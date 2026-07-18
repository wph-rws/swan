#!/usr/bin/env python3
"""Convert and lay out the Fortran sources of this fork.

The fork keeps the TU Delft code recognisable line by line, so that a routine
can be compared with upstream and ``git blame`` can follow a line back to the
upstream commit it came from. Two things make that possible: every source is
in one layout, and the same program that produced that layout can be run on a
new upstream release. This script is that program.

``convert`` turns a fixed-form source (``.ftn``, ``.f``, ``.for``: comments
in column 1, continuation in column 6, code in columns 7-72) into free form.
It changes no statement. Comment lines keep their full text. The version tags
that TU Delft writes in columns 73-80 of code lines are dropped, because the
compiler ignores them and free form has no such columns; they stay visible in
upstream. Lines that ``switch.pl`` activates (``!MPI``, ``!JAC`` and the
others) keep their prefix.

``format`` lays out free-form sources:

* code is indented by three spaces per level of nesting; the procedures of a
  module, and the internal procedures after a CONTAINS, start at the
  indentation of the unit that contains them, as the fork has always done;
* a continued line ends in `` &``; a continuation line has no leading ``&``
  (except inside a character constant, where Fortran needs it) and keeps its
  alignment relative to the first line of the statement;
* a code line longer than 132 characters, the limit of the standard, is
  wrapped at a comma or a blank;
* comment lines in column 1 are not touched; indented comment lines move with
  the code they describe;
* in a procedure with a TU Delft header, code that the fork placed before the
  header is moved to where upstream has it: USE statements below the row of
  asterisks, declarations of the arguments into section 4 of the header;
* trailing blanks are removed and runs of blank lines become one.

``format --check`` reports files whose layout differs and changes nothing.
The layout changes no token: building the sources before and after gives the
same object code, which ``tests/test_fortran_layout.py`` checks on a sample.
"""

import argparse
import difflib
import re
import sys
from pathlib import Path

LIMIT = 132        # line length of the Fortran standard
WRAP_AT = 100      # target width of the pieces of a wrapped line
STEP = 3           # spaces per level of nesting

# Prefixes that switch.pl removes to activate a line, longest first so that
# !NNCF is not read as !N plus NCF. The order of removal in switch.pl does not
# matter here: a line is code if it starts with any chain of these.
SWITCHES = sorted(['!!ESMF', '!ESMF', '!TIMG', '!JAC', '!WFR', '!FXFRO',
                   '!GRAPH', '!MPI', '!F95', '!DOS', '!UNIX', '!/Cray',
                   '!/SGI', '!/impi', '!CVIS', '!ADC', '!NADC', '!COH',
                   '!NCOH', '!METIS', '!NCF', '!NNCF', '!MatL4', '!MatL5'],
                  key=len, reverse=True)


# Pairs of switches of which a build activates exactly one; the lines of the
# two sides are alternatives (two DO statements with one END DO, say).
EXCLUSIVE = [('!JAC', '!WFR'), ('!FXFRO', '!GRAPH'), ('!ESMF', '!!ESMF'),
             ('!ADC', '!NADC'), ('!COH', '!NCOH'), ('!NCF', '!NNCF'),
             ('!MatL4', '!MatL5'), ('!DOS', '!UNIX')]


def exclusive(prefix, other):
    """True if no build activates both prefixes (one has !NCF, the other !NNCF)."""
    a, b = set(prefixes(prefix)), set(prefixes(other))
    return any((x in a and y in b) or (y in a and x in b) for x, y in EXCLUSIVE)


def prefixes(prefix):
    """The individual switches in a chain of prefixes."""
    out = []
    while prefix:
        for s in SWITCHES:
            if prefix.startswith(s):
                out.append(s)
                prefix = prefix[len(s):]
                break
        else:
            break
    return out


def split_switch(line):
    """Return (prefix, rest) where prefix is a chain of switch.pl prefixes."""
    prefix = ''
    while True:
        for s in SWITCHES:
            if line.startswith(s):
                # A switch prefix is followed by code or by blanks, never by
                # a letter that would make it a different word (!MPIX).
                nxt = line[len(s):len(s) + 1]
                if nxt and (nxt.isalnum() or nxt == '_') and not nxt.isdigit():
                    continue
                prefix += s
                line = line[len(s):]
                break
        else:
            return prefix, line


# ---------------------------------------------------------------- scanning

def scan(code, quote=None):
    """Scan free-form code text.

    Returns (code_end, quote_at_end): code_end is the index where an inline
    comment starts (len(code) if none), quote_at_end the open quote character
    at the end of the code part, or None.
    """
    i = 0
    n = len(code)
    while i < n:
        c = code[i]
        if quote:
            if c == quote:
                if i + 1 < n and code[i + 1] == quote:
                    i += 2
                    continue
                quote = None
        elif c in '\'"':
            quote = c
        elif c == '!':
            return i, None
        i += 1
    return n, quote


# ---------------------------------------------------------- fixed to free

# A version tag in columns 73-80: '40.41', sometimes two of them or a letter.
RE_TAG = re.compile(r'\s+\d\d\.\d\d[A-Za-z]?(\s+\d\d\.\d\d[A-Za-z]?)*\s*$')


def is_fixed_comment(line):
    if not line.strip():
        return False
    c = line[0]
    if c in 'Cc*!':
        # !$ in column 1-2 is an OpenMP sentinel; with code from column 7 it
        # is conditional code, which we keep as written.
        return True
    return False


def convert_fixed(text):
    """Convert fixed-form text to free form, keeping every statement."""
    raw = text.split('\n')
    if raw and raw[-1] == '':
        raw.pop()
    out = []            # list of output lines
    last_code = None    # index in out of the last code line (for '&')
    last_quote = None   # quote open at the end of that line
    last_prefix = ''
    last_dir = None     # index of the last OpenMP directive line
    last_cc = None      # index of the last line of conditional code (!$)
    group = []          # alternative lines (!NCF, !NNCF) that end the statement
    for line in raw:
        if line.startswith('\t') or line[:6].strip() == '' and '\t' in line[:6]:
            # a tab in the leading blanks moves to column 7; a tab further on
            # (inside a character constant, say) is kept as it is
            lead = len(line) - len(line.lstrip(' \t'))
            line = line[:lead].expandtabs(8) + line[lead:]
        prefix, body = split_switch(line)
        if not prefix and line[:2] == '!$':
            # OpenMP in fixed form: '!$OMP' directives and '!$' conditional
            # code, both with a continuation mark in column 6 and nothing
            # after column 72 (the version tags must go: with -fopenmp they
            # would be read as code).
            if line[2:5].lower() == 'omp':
                cont = line[5:6]
                content = line[6:72].rstrip()
                if cont not in ('', ' ', '0'):
                    if last_dir is None:
                        raise ValueError('directive continuation without a directive: %r' % line)
                    out[last_dir] = out[last_dir].rstrip() + ' &'
                out.append(line[:5] + ' ' + content.lstrip() if content.strip() else line[:5])
                last_dir = len(out) - 1
                continue
            body = line[:72]
            cont = body[5:6]
            stmt = body[6:].rstrip()
            if cont not in ('', ' ', '0'):
                if last_cc is None:
                    raise ValueError('continuation of conditional code without a start: %r' % line)
                out[last_cc] = '!$' + _add_amp(out[last_cc][2:])
                out.append('!$' + ' ' * 4 + stmt)
            else:
                lab = body[2:5].strip()
                head = (lab + ' ' * max(1, 4 - len(lab))) if lab else ' ' * 4
                out.append('!$' + head + stmt)
            last_cc = len(out) - 1
            continue
        if not prefix and is_fixed_comment(body):
            out.append('!' + body[1:].rstrip())
            continue
        if not prefix and not body.strip():
            out.append('')
            continue
        if prefix and is_fixed_comment(body):
            out.append(prefix + body.rstrip())
            continue
        # code line: columns 1-5 label, 6 continuation, 7-72 statement
        # For a prefixed line, switch.pl removes the prefix before the
        # compiler reads the columns, so they are counted after it. Text
        # after column 72 is not code; but where an inline comment starts
        # before column 72, its words often run on past it, and those are
        # kept (without the version tag).
        tail = body[72:]
        body = body[:72]
        if tail.strip():
            end, q = scan(body[6:])
            if end < len(body[6:]) and q is None:
                tail = RE_TAG.sub('', tail).rstrip()
                body = body + tail if tail.strip() else body
        label = body[:5]
        cont = body[5:6]
        stmt = body[6:]
        alternative = (last_code == len(out) - 1 and not last_quote
                       and exclusive(prefix, last_prefix))
        if cont not in ('', ' ', '0') and alternative:
            # A continuation under a switch that excludes the one of the line
            # just before (!MatL5 after !MatL4) is an alternative to that
            # line: both continue the same earlier line, which ends in '&'.
            out.append(prefix + ' ' * 6 + stmt.rstrip())
            group.append(len(out) - 1)
            last_code, last_quote, last_prefix = len(out) - 1, _quote_after(stmt, None), prefix
            continue
        if cont not in ('', ' ', '0'):
            # continuation of the previous code line
            if last_code is None:
                raise ValueError('continuation without a statement: %r' % line)
            for index in group[:-1]:
                # the statement goes on after alternative lines (!NCF and
                # !NNCF): every alternative needs its '&'
                out[index] = _add_amp(out[index])
            if last_quote:
                # Inside a character constant the blanks up to column 72 are
                # part of the string; free form needs '&' on both lines.
                prev = out[last_code]
                pad = 72 - len(_fixed_body(prev, last_prefix))
                out[last_code] = prev + ' ' * max(pad, 0) + '&'
                newline = prefix + ' ' * 6 + '&' + stmt.rstrip()
            else:
                out[last_code] = _add_amp(out[last_code])
                newline = prefix + ' ' * 6 + stmt.rstrip()
            q = _quote_after(stmt, last_quote)
            out.append(newline)
            last_code, last_quote, last_prefix = len(out) - 1, q, prefix
            group = [last_code]
            continue
        lab = label.strip()
        if lab:
            head = lab + ' ' * max(1, 6 - len(lab))
        else:
            head = ' ' * 6
        newline = prefix + head + stmt.rstrip()
        out.append(newline)
        group = group + [len(out) - 1] if alternative else [len(out) - 1]
        last_code, last_quote, last_prefix = len(out) - 1, _quote_after(stmt, None), prefix
    return '\n'.join(out) + '\n'


def _fixed_body(line, prefix):
    return line[len(prefix):]


def _quote_after(stmt, quote):
    end, q = scan(stmt, quote)
    return q


def _add_amp(line):
    """Append a continuation '&' to a free-form line, before any comment."""
    prefix, body = split_switch(line)
    end, q = scan(body)
    code, comment = body[:end].rstrip(), body[end:]
    if comment:
        return prefix + code + ' & ' + comment
    return prefix + code + ' &'


# ------------------------------------------------------------- statements

class Line:
    __slots__ = ('text', 'kind', 'prefix', 'sentinel', 'code', 'comment',
                 'indent', 'lead_amp', 'quote_in', 'quote_out', 'cont', 'owner',
                 'new_indent')

    def __init__(self, text):
        self.text = text
        self.kind = 'code'
        self.prefix = self.sentinel = self.code = self.comment = ''
        self.indent = 0
        self.lead_amp = self.cont = False
        self.quote_in = self.quote_out = None


def classify(lines):
    """Split lines into code and comment, and group the code into statements.

    Returns (stmts, seq): stmts is the list of statements in the order of
    their first line, each a list of its code lines; seq holds every line in
    file order. A comment line between the lines of a continued statement
    gets that statement as its owner.

    Upstream sources interleave alternatives: a statement under !NCF goes on
    on the next !NCF line, and the !NNCF line between them is a comment in
    that build. A continued statement therefore waits for the next line with
    its own prefix; a plain continued statement takes any next code line.
    """
    stmts, seq = [], []
    open_ = {}          # start prefix -> statement waiting for a continuation
    last_open = None
    for t in lines:
        ln = Line(t)
        ln.owner = None
        seq.append(ln)
        prefix, body = split_switch(t)
        ln.prefix = prefix
        s = body.lstrip()
        if '' in open_:
            target = open_['']
        elif prefix in open_:
            target = open_[prefix]
        elif not prefix and len(open_) > 1 and all(
                exclusive(a, b) for a in open_ for b in open_ if a != b):
            # alternatives (!NCF and !NNCF) that go on on a common line: in
            # each build that line continues the one alternative left
            target = last_open
            for key in list(open_):
                if open_[key] is not target:
                    open_.pop(key)
        else:
            target = None
        in_string = target is not None and _last_code(target).quote_out
        m = re.match(r'!\$(omp)?(\s|&|$)', s, re.I)
        if m and not in_string:
            # OpenMP directive or conditional compilation sentinel
            ln.sentinel = s[:2 + (3 if m.group(1) else 0)]
            body_code = s[len(ln.sentinel):]
        elif not s:
            ln.kind = 'blank'
        elif s.startswith('!') and not in_string:
            ln.kind = 'comment'
        else:
            body_code = s
        if ln.kind != 'code':
            ln.indent = len(body) - len(s) if s else 0
            ln.comment = s
            ln.owner = last_open if open_ else None
            continue
        ln.indent = len(body) - len(s)
        if target is None and not ln.sentinel:
            # a statement label stays in column 1; the indentation of the
            # statement is where its text starts
            m = RE_LABEL.match(s)
            if m:
                ln.indent += m.end()
        qin = _last_code(target).quote_out if target else None
        ln.quote_in = qin
        txt = body_code
        if target is not None and txt.lstrip().startswith('&') and (ln.sentinel or txt.startswith('&')):
            ln.lead_amp = True
            txt = txt.lstrip()[1:]
        end, q = scan(txt, qin)
        code, comment = txt[:end], txt[end:]
        stripped = code.rstrip()
        if stripped.endswith('&'):
            ln.cont = True
            code = stripped[:-1] if q is None else code[:code.rstrip().rfind('&')]
        ln.code = code if q else code.rstrip()
        ln.comment = comment
        ln.quote_out = q
        if target is None:
            target = [ln]
            stmts.append(target)
        else:
            target.append(ln)
        ln.owner = target
        key = target[0].prefix
        if ln.cont:
            open_[key] = target
            last_open = target
        else:
            open_.pop(key, None)
            last_open = next(reversed(open_.values()), None) if open_ else None
    return stmts, seq


def _last_code(stmt):
    return next(x for x in reversed(stmt) if x.kind == 'code')


def joined(stmt):
    """The code of a statement on one line, for recognising its kind."""
    parts = []
    for ln in stmt:
        if ln.kind == 'code':
            parts.append(ln.code if ln.quote_in else ln.code.strip())
    return ' '.join(parts).strip()


# ------------------------------------------------------------- nesting

TYPESPEC = (r'(integer|real|logical|complex|character|double\s*precision|'
            r'double\s*complex|type\s*\([^)]*\)|class\s*\([^)]*\))'
            r'(\s*\(\s*[^)]*\)|\s*\*\s*\d+)?')
PREFIX = r'((recursive|pure|elemental|impure|non_recursive|module)\s+)'
RE_PROC = re.compile(r'^%s*(%s\s+)?%s*(subroutine|function)\s+\w+' % (PREFIX, TYPESPEC, PREFIX), re.I)
RE_PROGRAM = re.compile(r'^(program\s+\w+|block\s*data(\s+\w+)?|submodule\s*\(.*\)\s*\w+)\s*$', re.I)
RE_MODULE = re.compile(r'^module\s+(?!procedure\b|subroutine\b|function\b)\w+\s*$', re.I)
RE_INTERFACE = re.compile(r'^(abstract\s+)?interface\b', re.I)
RE_TYPEDEF = re.compile(r'^type\s*(,[^:]*)?::\s*\w+|^type\s+(?!is\b)\w+\s*$', re.I)
RE_END = re.compile(r'^end\s*(do|if|select|where|forall|associate|block|critical|'
                    r'interface|type|module|submodule|subroutine|function|program|'
                    r'block\s*data|enum|team)?(\s+[\w ()=,.*]*)?$', re.I)
RE_NAME = re.compile(r'^(\w+)\s*:(?!:)\s*')
RE_LABEL = re.compile(r'^(\d{1,5})\s+')
RE_DO = re.compile(r'^do(?!\w)\s*(\d+)?', re.I)
RE_IFTHEN = re.compile(r'^if\s*\(', re.I)
RE_ELSE = re.compile(r'^else(\s*if\s*\(.*\)\s*then(\s+\w+)?|\s+\w+)?\s*$|^elseif\s*\(.*\)\s*then(\s+\w+)?\s*$', re.I)
RE_ELSEWHERE = re.compile(r'^else\s*where\b', re.I)
RE_SELECT = re.compile(r'^select\s*(case|type|rank)\s*\(', re.I)
RE_CASE = re.compile(r'^(case\s*(\(|default\b)|type\s+is\s*\(|class\s+(is\s*\(|default\b)|rank\s*(\(|default\b))', re.I)
RE_WHERE = re.compile(r'^(where|forall)\s*\(', re.I)
RE_ASSOC = re.compile(r'^(associate\s*\(|block\s*$|critical\b|enum\s*,)', re.I)
RE_CONTAINS = re.compile(r'^contains\s*$', re.I)


def close_paren(s, start):
    """Index after the parenthesis that matches s[start] == '('."""
    depth = 0
    quote = None
    for i in range(start, len(s)):
        c = s[i]
        if quote:
            if c == quote:
                quote = None
            continue
        if c in '\'"':
            quote = c
        elif c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return i + 1
    return len(s)


def kind_of(code):
    """Classify a statement: returns (kind, label, do_label)."""
    s = code
    label = None
    m = RE_LABEL.match(s)
    if m:
        label = m.group(1)
        s = s[m.end():]
    m = RE_NAME.match(s)
    named = False
    if m and not re.match(r'^(case|default)\b', s, re.I):
        s = s[m.end():]
        named = True
    low = s.lower()
    if RE_END.match(s) and not re.match(r'^end\s*=', s, re.I):
        return 'end', label, None
    if RE_CONTAINS.match(s):
        return 'contains', label, None
    if RE_ELSEWHERE.match(s) or RE_ELSE.match(s):
        return 'middle', label, None
    if RE_CASE.match(s):
        return 'case', label, None
    if RE_SELECT.match(s):
        return 'select', label, None
    m = RE_DO.match(s)
    if m:
        return 'open', label, m.group(1)
    if RE_IFTHEN.match(s):
        e = close_paren(s, s.index('('))
        if re.match(r'^\s*then\s*$', s[e:], re.I):
            return 'open', label, None
        return 'simple', label, None
    if RE_WHERE.match(s):
        e = close_paren(s, s.index('('))
        if not s[e:].strip():
            return 'open', label, None
        return 'simple', label, None
    if RE_ASSOC.match(s) or (named and re.match(r'^block\s*$', s, re.I)):
        return 'open', label, None
    if RE_INTERFACE.match(s):
        return 'unit', label, None
    if RE_TYPEDEF.match(s):
        return 'unit', label, None
    if RE_MODULE.match(s) or RE_PROGRAM.match(s):
        return 'unit', label, None
    if RE_PROC.match(s) and not re.match(r'^module\s+procedure', s, re.I):
        return 'unit', label, None
    return 'simple', label, None


def _indents(stmts, inactive):
    """Compute the indentation level of each statement.

    Returns a list with an integer level for code statements, None for
    comment and blank lines, and a flag that is False when the nesting did
    not close properly (then the caller leaves the file alone).
    """
    level = 0
    stack = []          # entries: (kind, opening level, do_label)
    out = []
    ok = True
    for st in stmts:
        first = st[0]
        if set(prefixes(first.prefix)) & inactive:
            out.append('skip')
            continue
        if first.sentinel.lower().startswith('!$omp'):
            out.append(level)       # directives do not nest the code
            continue
        k, label, do_label = kind_of(joined(st))
        if k == 'end':
            if stack:
                kind, lv, _ = stack.pop()
                level = lv
            else:
                ok = False
                PROBLEMS.append('END without an open block: %s' % joined(st)[:70])
            out.append(level)
        elif k == 'middle':
            lv = stack[-1][1] if stack else 0
            out.append(lv)
        elif k == 'case':
            lv = stack[-1][1] if stack else 0
            out.append(lv)
            level = lv + 1
        elif k == 'select':
            out.append(level)
            stack.append(('select', level, None))
            level += 1
        elif k == 'contains':
            lv = stack[-1][1] if stack else 0
            out.append(lv)
            level = lv
        elif k in ('open', 'unit'):
            out.append(level)
            stack.append((k, level, do_label))
            level += 1
        else:
            out.append(level)
        # a labelled statement closes every DO that ends on that label
        if label is not None and k != 'end':
            closed = False
            while stack and stack[-1][2] == label and stack[-1][0] == 'open':
                kind, lv, _ = stack.pop()
                level = lv
                closed = True
            if closed:
                out[-1] = level
    if stack:
        ok = False
        PROBLEMS.append('blocks left open: %s' % ', '.join('%s@%d' % (k, l) for k, l, _ in stack))
    return out, ok


def indents(stmts):
    """Indentation level of each statement (None for comments).

    Upstream sources still hold the switch lines of switch.pl. Of each pair
    of alternative switches only one side is active in a build, so the
    nesting is counted twice, once with each side; every line takes the
    level of the configuration in which it is active."""
    has = set()
    for st in stmts:
        has.update(prefixes(st[0].prefix))
    if not has:
        lv, ok = _indents(stmts, set())
        return lv, ok
    pos = {a for a, b in EXCLUSIVE}
    neg = {b for a, b in EXCLUSIVE}
    la, oka = _indents(stmts, neg)
    lb, okb = _indents(stmts, pos)
    out = []
    ok = oka and okb
    for st, x, y in zip(stmts, la, lb):
        if x == 'skip':
            out.append(y)
        elif y == 'skip':
            out.append(x)
        else:
            # a common line inside a block of only one side (an !NCF IF
            # around a plain statement): indent it as inside the block
            out.append(max(x, y))
    return out, ok


# ---------------------------------------------------------------- layout

def emit_code(ln, ind, first=False):
    pre = ln.prefix
    # after a switch prefix at least one blank, or the prefix would read as
    # part of a word
    head = ' ' * (max(ind, 1) if pre else ind)
    if ln.sentinel:
        if ln.sentinel.lower().startswith('!$omp'):
            head = head + ln.sentinel + ' '
            body = ln.code.lstrip() if not ln.quote_in else ln.code
        else:
            # conditional compilation: keep the sentinel, then the code
            head = head + ln.sentinel + ' '
            body = ln.code.lstrip() if not ln.quote_in else ln.code
    else:
        body = ln.code if ln.quote_in else ln.code.lstrip()
        m = RE_LABEL.match(body) if first else None
        if m and not pre:
            lab = m.group(1)
            head = lab + ' ' * max(1, ind - len(lab))
            body = body[m.end():]
    if ln.lead_amp and ln.quote_in:
        body = '&' + body
    text = pre + head + body
    if ln.cont:
        if ln.quote_out:
            text = text + '&'
        else:
            text = text.rstrip() + ' &'
    if ln.comment:
        sep = ' ' if not text.endswith(' ') else ''
        text = text + sep + ln.comment if text.strip() else text + ln.comment
    return text.rstrip() if not (ln.cont and ln.quote_out) else text


def wrap(text, ind, sentinel=''):
    """Wrap a code line that is longer than LIMIT. Returns a list of lines."""
    if len(text) <= LIMIT:
        return [text]
    end, q = scan(text)
    code, comment = text[:end].rstrip(), text[end:]
    if q is not None:
        return [text]          # an open string at the end: leave it
    if len(code) <= LIMIT and comment:
        # move the comment to its own line above
        return [' ' * ind + comment, code]
    trailing_amp = code.endswith('&')
    if trailing_amp:
        code = code[:-1].rstrip()
    pieces = []
    cont_ind = ind + 2 * STEP
    lead = ''
    open_quote = None
    while len(lead + code) > LIMIT - (2 if trailing_amp else 0):
        room = LIMIT - 2 - len(lead)
        cut, inside = _break_point(code, WRAP_AT - len(lead), room, open_quote)
        if cut is None:
            break
        if inside:
            # break a character constant: '&' ends this piece inside the
            # string and '&' starts the next piece, which continues it
            pieces.append(lead + code[:cut] + '&')
            open_quote = scan(code[:cut], open_quote)[1]
            code = code[cut:]
            lead = ' ' * cont_ind + (sentinel + ' ' if sentinel else '') + '&'
        else:
            pieces.append(lead + code[:cut].rstrip() + ' &')
            open_quote = None
            code = code[cut:].lstrip()
            lead = ' ' * cont_ind + (sentinel + ' ' if sentinel else '')
    last = lead + code + (' &' if trailing_amp else '')
    if comment:
        last = last + ' ' + comment
    pieces.append(last)
    return pieces


def _break_point(code, target, limit, quote=None):
    """Best position to break code, as (index, inside_string).

    `quote` is the quote character of a string that is still open at the
    start of `code`. Outside strings the code breaks after a comma or at a
    blank. Only if there is none in reach it breaks inside a character
    constant: preferably after a comma that separates format items (outside
    the inner quotes of a format), otherwise after a blank. Never beyond
    `limit`; as close to `target` as possible."""
    out_c, in_c, any_c = [], [], []
    inner = False
    start = -99 if quote else None
    for i, c in enumerate(code):
        if quote:
            if c == quote:
                if i + 1 < len(code) and code[i + 1] == quote:
                    continue
                if i > 0 and code[i - 1] == quote:
                    continue
                quote = None
                continue
            if c in '\'"':
                inner = not inner
            if i - start > 10 and i + 1 < len(code) and code[i + 1] != quote:
                if c == ',' and not inner:
                    in_c.append((i + 1, 0))
                elif c == ' ':
                    in_c.append((i + 1, 1))
                else:
                    any_c.append((i + 1, 2))
            continue
        if c in '\'"':
            quote = c
            start = i
            inner = False
        elif c == ',' and i + 1 < len(code):
            out_c.append((i + 1, 0))
        elif c == ' ' and i > 0 and code[i - 1] not in ' (':
            out_c.append((i, 1))
    for cands, inside in ((out_c, False), (in_c, True), (any_c, True)):
        ok = [p for p in cands if 20 < p[0] <= limit - (1 if inside else 0)]
        if not ok:
            continue
        under = [p for p in ok if p[0] <= target]
        if under:
            return max(under, key=lambda p: (p[1] == 0, p[0]))[0], inside
        return min(ok)[0], inside
    return None, False


def layout(text):
    del PROBLEMS[:]
    raw = text.split('\n')
    if raw and raw[-1] == '':
        raw.pop()
    stmts, seq = classify(raw)
    levels, ok = indents(stmts)
    if not ok:
        raise ValueError('nesting does not close: ' + '; '.join(PROBLEMS[-3:]))
    for st, lv in zip(stmts, levels):
        new = lv * STEP
        m = RE_LABEL.match(st[0].code.lstrip()) if not (st[0].sentinel or st[0].prefix) else None
        if m:
            # the statement after a label starts at least one blank after it
            new = max(new, len(m.group(1)) + 1)
        if st[0].prefix:
            new = max(new, 1)       # emit_code puts one blank after a prefix
        delta = new - st[0].indent
        for ln in st:
            if ln is st[0]:
                ln.new_indent = new
            elif ln.quote_in:
                ln.new_indent = max(ln.indent + delta, 0)
            else:
                ln.new_indent = max(ln.indent + delta, new + STEP)
    out = []
    pending = []
    for ln in seq:
        if ln.kind == 'code':
            shift = ln.new_indent - ln.indent
            for c in pending:
                out.append(_comment_line(c, shift))
            pending = []
            sentinel = ln.sentinel if ln.sentinel.lower().startswith('!$omp') else ''
            text = emit_code(ln, ln.new_indent, first=(ln is ln.owner[0]))
            if ln.quote_in or ln.quote_out or ln.prefix:
                out.append(text)
            else:
                out.extend(wrap(text, ln.new_indent, sentinel))
        elif ln.owner is not None:
            first = ln.owner[0]
            out.append(_comment_line(ln, first.new_indent - first.indent))
        else:
            pending.append(ln)
    for c in pending:
        out.append(_comment_line(c, 0))
    # blank lines: at most one in a row, none at the end
    res = []
    for line in out:
        line = line.rstrip()
        if not line and res and not res[-1]:
            continue
        res.append(line)
    while res and not res[-1]:
        res.pop()
    return '\n'.join(res) + '\n'


def _comment_line(ln, shift):
    if ln.kind == 'blank':
        return ''
    t = ln.text.rstrip()
    if ln.prefix or not t[:1].isspace():
        return t
    ind = len(t) - len(t.lstrip())
    return ' ' * max(1, ind + shift) + t.lstrip()


# ---------------------------------------------------- header placement

RE_BANNER = re.compile(r'^!\s+--\|-{10,}')
RE_STARS = re.compile(r'^\s*!\*{20,}\s*$')
RE_SEC4 = re.compile(r'^!\s+4\.\s+Argument variables', re.I)
RE_SEC5 = re.compile(r'^!\s+5\.\s', re.I)
RE_SEC_ANY = re.compile(r'^!\s*\d+\.\s+\w')
RE_USE = re.compile(r'^\s*use\b', re.I)
RE_IMPLICIT = re.compile(r'^\s*implicit\b', re.I)


def place_headers(lines):
    """Move fork code out of TU Delft headers to the upstream places."""
    out = list(lines)
    i = 0
    while i < len(out):
        s = out[i].strip()
        if s and not s.startswith('!') and RE_PROC.match(s) and not re.match(r'^end\b', s, re.I):
            j = i
            while out[j].rstrip().endswith('&'):
                j += 1
            k = j + 1
            banner = None
            while k < len(out) and k < j + 120:
                if RE_BANNER.match(out[k]):
                    banner = k
                    break
                t = out[k].strip()
                if t and not t.startswith('!') and (RE_PROC.match(t) or re.match(r'^end\b', t, re.I)):
                    break
                k += 1
            if banner is not None:
                out = _place_one(out, j + 1, banner, _stmt_text(out[i:j + 1]))
            i = j + 1
        else:
            i += 1
    return out


def _stmt_end(lines, i):
    while lines[i].rstrip().endswith('&'):
        i += 1
    return i


def _stmt_text(lines):
    return ' '.join(l.strip().rstrip('&').lstrip('&') for l in lines)


def _dummies(stmt):
    """Names of the dummy arguments in a SUBROUTINE or FUNCTION statement."""
    m = re.search(r'(subroutine|function)\s+\w+\s*\(([^)]*)\)', stmt, re.I)
    names = set()
    if m:
        names = {x.strip().lower() for x in m.group(2).split(',') if x.strip()}
    r = re.search(r'result\s*\(\s*(\w+)\s*\)', stmt, re.I)
    if r:
        names.add(r.group(1).lower())
    return names


def _declared(stmt):
    """(names, is_parameter) of a declaration, or None if it is not one."""
    m = re.match(r'^\s*' + TYPESPEC, stmt, re.I)
    if not m:
        return None
    rest = stmt[m.end():]
    if '::' in rest:
        attrs, ents = rest.split('::', 1)
    else:
        attrs, ents = '', rest
    if not re.match(r'^\s*(,|::)|^\s+\w', rest):
        return None
    depth, cur, parts = 0, '', []
    for c in ents:
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        if c == ',' and depth == 0:
            parts.append(cur)
            cur = ''
        else:
            cur += c
    parts.append(cur)
    names = set()
    for x in parts:
        n = re.match(r'\s*(\w+)', x.split('!')[0])
        if n:
            names.add(n.group(1).lower())
    return names, bool(re.search(r'\bparameter\b', attrs, re.I))


RE_SEC = {4: re.compile(r'^!\s+4\.\s+Argument variables', re.I),
          5: re.compile(r'^!\s+5\.\s+Parameter variables', re.I),
          6: re.compile(r'^!\s+6\.\s+Local variables', re.I)}

REPORT = []
PROBLEMS = []


def _place_one(lines, a, banner, proc):
    """Rearrange lines[a:banner], the part between procedure statement and
    TU Delft banner, the way upstream has it."""
    seg = lines[a:banner]
    dummies = _dummies(proc)
    noncode, uses, implicit, decls, other = [], [], [], {4: [], 5: [], 6: []}, []
    stars = None
    n = 0
    while n < len(seg):
        t = seg[n].strip()
        if t and not t.startswith('!'):
            e = _stmt_end(seg, n)
            block = seg[n:e + 1]
            txt = _stmt_text(block)
            if RE_USE.match(t):
                uses.extend(block)
            elif RE_IMPLICIT.match(t):
                implicit.extend(block)
            else:
                d = _declared(txt)
                if d is None:
                    other.extend(block)
                else:
                    names, par = d
                    sec = 4 if names & dummies else (5 if par else 6)
                    decls[sec].extend(block)
            n = e + 1
            continue
        if RE_STARS.match(seg[n]) and stars is None:
            stars = len(noncode)
        noncode.append(seg[n])
        n += 1
    if not any(decls.values()) and not other:
        # only USE/IMPLICIT: move USE lines that precede the asterisks
        first_code = next((k for k, t in enumerate(seg) if t.strip() and not t.strip().startswith('!')), None)
        if stars is None or first_code is None:
            return lines
        st_line = next(k for k, t in enumerate(seg) if RE_STARS.match(t))
        if first_code > st_line:
            return lines
    tail = lines[banner:]
    left = list(other)
    for sec in (4, 5, 6):
        if decls[sec]:
            new = _into_section(tail, decls[sec], sec)
            if new is None:
                left.extend(decls[sec])
                REPORT.append('no section %d for %s' % (sec, proc[:60]))
            else:
                tail = new
    # rebuild the part above the banner: comments in their order, the code
    # after the asterisks (or where the first code was)
    code = uses + ([''] + implicit if implicit else []) + ([''] + left if left else [])
    if stars is not None:
        k = stars + 1
        if k < len(noncode) and not noncode[k].strip().replace('!', ''):
            k += 1
        head = noncode[:k] + code + [''] + noncode[k:]
    else:
        head = code + noncode
    return lines[:a] + head + tail


def _into_section(tail, decls, sec):
    """Insert declarations in header section `sec`, before its first
    declaration, or at its end if it has none."""
    start = None
    for n, t in enumerate(tail):
        if RE_SEC[sec].match(t):
            start = n
            break
        if RE_SEC_ANY.match(t):
            m = re.match(r'^!\s*(\d+)\.', t)
            if m and int(m.group(1)) > sec:
                return None
    if start is None:
        return None
    n = start + 1
    while n < len(tail):
        t = tail[n].strip()
        if t and not t.startswith('!'):
            return tail[:n] + decls + tail[n:]
        if RE_SEC_ANY.match(tail[n]):
            m = n
            while m > start + 1 and not tail[m - 1].strip().replace('!', ''):
                m -= 1
            return tail[:m] + ['!'] + decls + ['!'] + tail[m:]
        n += 1
    return None


# ------------------------------------------------------- equivalence

def _norm(code):
    """Upper case and no blanks outside character constants."""
    out = []
    quote = None
    for c in code:
        if quote:
            out.append(c)
            if c == quote:
                quote = None
        elif c in '\'"':
            quote = c
            out.append(c)
        elif not c.isspace():
            out.append(c.upper())
    return ''.join(out)


def statements(text):
    """The statements of a free-form text, normalised, in order."""
    raw = text.split('\n')
    if raw and raw[-1] == '':
        raw.pop()
    res = []
    for st in classify(raw)[0]:
        code = ''.join(ln.code for ln in st)
        res.append(st[0].prefix + st[0].sentinel.lower() + _norm(code))
    return res


def _is_spec(stmt):
    return bool(re.match(r'^(USE|IMPLICIT)', stmt)) or bool(
        re.match(r'^(INTEGER|REAL|LOGICAL|COMPLEX|CHARACTER|DOUBLEPRECISION|TYPE\(|CLASS\()', stmt)
        and not re.search(r'(SUBROUTINE|FUNCTION)\w', stmt[:80]))


def equivalent(before, after):
    """True if two texts hold the same statements; within a run of
    specification statements the order may differ."""
    def runs(st):
        out, cur = [], []
        for x in st:
            if _is_spec(x):
                cur.append(x)
            else:
                if cur:
                    out.append(tuple(sorted(cur)))
                    cur = []
                out.append(x)
        if cur:
            out.append(tuple(sorted(cur)))
        return out
    return runs(statements(before)) == runs(statements(after))


# ------------------------------------------------------------- driver

def format_text(text, headers=True):
    lines = text.split('\n')
    if lines and lines[-1] == '':
        lines.pop()
    if headers:
        lines = place_headers(lines)
    return layout('\n'.join(lines) + '\n')


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    sub = p.add_subparsers(dest='cmd', required=True)
    c = sub.add_parser('convert', help='fixed form to free form (in place, or to --out)')
    c.add_argument('files', nargs='+')
    c.add_argument('--out', help='write to this file instead (one input only)')
    f = sub.add_parser('format', help='lay out free-form sources in place')
    f.add_argument('files', nargs='+')
    f.add_argument('--check', action='store_true', help='report, do not change')
    f.add_argument('--no-headers', action='store_true', help='leave header placement alone')
    a = p.parse_args(argv)
    bad = 0
    for name in a.files:
        path = Path(name)
        text = path.read_text(encoding='latin-1')
        try:
            if a.cmd == 'convert':
                new = convert_fixed(text)
            else:
                new = format_text(text, headers=not a.no_headers)
                if not equivalent(text, new):
                    raise ValueError('layout would change a statement; file left alone')
        except ValueError as e:
            print('%s: %s' % (name, e), file=sys.stderr)
            bad += 1
            continue
        if a.cmd == 'format' and a.check:
            if new != text:
                print('%s: layout differs' % name)
                bad += 1
            continue
        target = Path(a.out) if getattr(a, 'out', None) else path
        if new != text or target != path:
            target.write_text(new, encoding='latin-1')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
