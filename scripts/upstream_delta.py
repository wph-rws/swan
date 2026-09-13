#!/usr/bin/env python3
"""Leg de fork deck voor deck naast de vastgezette TU Delft-bron.

De bugfixtabel vertelt in proza welke reparaties de uitvoer verschuiven. Dat is
precies zo betrouwbaar als de discipline waarmee iemand die tabel bijhoudt. Dit
script maakt er een gecontroleerde lijst van: `doc/upstream-delta.json` noemt
per deck of het identiek aan upstream hoort te zijn of bewust afwijkt, met het
BF-nummer erbij, en deze poort rekent die bewering na tegen de werkelijkheid.

Een deck dat gaat afwijken zonder BF-verwijzing is daarmee een rode poort in
plaats van een ontdekking achteraf.

Elke run start met een vaste geheugenindeling (`setarch -R`), omdat upstream
anders van run tot run twee verschillende antwoorden geeft; zie `launcher`.

De vergelijking is streng: velden moeten exact gelijk zijn. Upstream en de fork
draaien hier op dezelfde compiler, dezelfde vlaggen en dezelfde machine, dus
alles wat verschilt is de code -- er is geen ruimte waarbinnen "ongeveer gelijk"
nog iets betekent. Waar de fork bewust afwijkt, staat dat in het manifest.

Statussen in het manifest:

  identiek      beide binaries draaien het deck en leveren gelijke uitvoer;
  wijkt af      beide draaien, de uitvoer verschilt; `bugfix` noemt de reparatie
                die het verschil maakt, of `onderzoek` zegt wat er gemeten is
                zolang de toewijzing nog niet rond is;
  alleen fork   upstream weigert het deck (een commando dat het niet kent),
                `bugfix` noemt de reparatie die het deck bereikbaar maakte;
  overgeslagen  bewust niet vergeleken, `reden` zegt waarom -- nooit stil;
  upstream instabiel
                upstream geeft op dit deck van run tot run een ander antwoord,
                dus een vergelijking zegt er niets over;
  fork instabiel
                de fork doet dat -- dat is altijd een fout van deze kant.

Gebruik:

  python3 scripts/upstream_delta.py                 # poort: bewering vs. werkelijkheid
  python3 scripts/upstream_delta.py --update        # neem de werkelijkheid over
  python3 scripts/upstream_delta.py --deck quick_test/quick_test.swn

`--update` maakt de poort nooit zelfstandig groen: een nieuw verschil krijgt
`"bugfix": null`, en daar valt de controle op tot iemand het verschil verklaart.
"""

from __future__ import annotations

import argparse
import filecmp
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parent.parent
MANIFEST = REPOSITORY / "doc" / "upstream-delta.json"
BUGFIX_TABLE = REPOSITORY / "doc" / "bugfixes-tov-tu-delft-41.51.md"
EXAMPLES = REPOSITORY / "examples"

sys.path.insert(0, str(EXAMPLES))
from reference_check import same_results  # noqa: E402

#  Uitvoer die niets over het model zegt: logboeken, foutbestanden en de
#  afdruk met tijdstempels en padnamen erin. Wat overblijft zijn de tabellen,
#  blokken en spectra -- het model zelf.
IGNORED_NAMES = {
    "INPUT",
    "norm_end",
    "swaninit",
    "ERRPTS",
    "console.log",
}
#  .bqf is de binaire interactietabel die de XNL-route naast zich aanlegt:
#  een werkbestand met opvulbytes, geen modeluitvoer.
IGNORED_SUFFIXES = {".prt", ".erf", ".log", ".egs", ".bqf"}
IGNORED_PREFIXES = ("PRINT", "Errfile", "ERRFILE", "screen")
#  Mappen naast het deck die invoer noch uitvoer zijn.
SKIPPED_DIRECTORIES = {"reference", "reference-jac", "references", "results", "__pycache__"}

#  Wordt door --geen-pin op False gezet; zie launcher().
PIN_MEMORY_LAYOUT = True

STATUS_IDENTICAL = "identiek"
STATUS_DIFFERS = "wijkt af"
STATUS_UPSTREAM_UNSTABLE = "upstream instabiel"
STATUS_FORK_UNSTABLE = "fork instabiel"
STATUS_FORK_ONLY = "alleen fork"
STATUS_SKIPPED = "overgeslagen"


@dataclass
class RunOutcome:
    """Wat één binary van één deck maakte."""

    returncode: int
    outputs: dict[str, Path]
    seconds: float
    timed_out: bool = False


@dataclass
class DeckVerdict:
    deck: str
    status: str
    detail: str


def load_manifest(path: Path) -> dict:
    if not path.is_file():
        raise SystemExit(
            f"manifest ontbreekt: {path}. Maak het aan met --update, of geef "
            "--manifest op."
        )
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def write_manifest(path: Path, manifest: dict) -> None:
    with path.open("w", encoding="utf-8") as handle:
        json.dump(manifest, handle, ensure_ascii=False, indent=2)
        handle.write("\n")


def known_bugfix_identifiers() -> set[str]:
    """De BF-nummers die de bugfixtabel werkelijk kent."""
    if not BUGFIX_TABLE.is_file():
        return set()
    text = BUGFIX_TABLE.read_text(encoding="utf-8")
    return set(re.findall(r"^\| (BF-\d+) \|", text, flags=re.MULTILINE))


def discover_decks() -> list[str]:
    """Elk deck in de voorbeeldsuite, als pad onder `examples/`."""
    return sorted(
        str(deck.relative_to(EXAMPLES)) for deck in EXAMPLES.rglob("*.swn")
    )


def ensure_upstream_source(worktree: Path, commit: str) -> None:
    """Zorg dat de vastgezette upstream-bron er staat.

    De worktree hoort in `.upstream/` te leven: dat pad staat in `.gitignore`,
    dus de referentiebron vervuilt de fork niet, en hij overleeft -- anders dan
    een checkout in /tmp -- een herstart van de machine.

    Het pad mag geen streepje bevatten. De upstream-bouw draait `switch.pl` om
    de fixed-form bron voor te bewerken, en die parseert zijn argumenten met
    `while ($ARGV[0]=~/-.*/)` zonder `shift` in de vangnettak: een bronpad met
    een streepje erin matcht die ongeankerde regex, wordt door geen enkele
    optie herkend, en het script draait voor eeuwig op 100% CPU. Beter een
    duidelijke fout dan een bouw die lijkt te hangen.
    """
    if "-" in str(worktree):
        raise SystemExit(
            f"upstream-pad {worktree} bevat een streepje. De upstream-switch.pl "
            "loopt daarop vast in een oneindige lus; kies een pad zonder streepje."
        )
    if (worktree / "CMakeLists.txt").is_file():
        head = subprocess.run(
            ["git", "-C", str(worktree), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            check=False,
        )
        if head.returncode == 0 and head.stdout.strip().startswith(commit[:12]):
            return
        raise SystemExit(
            f"upstream-worktree {worktree} staat niet op {commit[:12]}. "
            "Verwijder hem (git worktree remove) en draai opnieuw."
        )
    worktree.parent.mkdir(parents=True, exist_ok=True)
    print(f"upstream-bron uitchecken op {commit[:12]} in {worktree} ...")
    completed = subprocess.run(
        ["git", "-C", str(REPOSITORY), "worktree", "add", "--detach", str(worktree), commit],
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        raise SystemExit(
            "kon de upstream-worktree niet aanmaken:\n"
            f"{completed.stdout}\n{completed.stderr}"
        )


def launcher() -> list[str]:
    """Start elke run met een vaste geheugenindeling.

    Upstream 41.51 geeft op verscheidene decks van run tot run twee
    verschillende antwoorden op dezelfde invoer. Zet je de willekeurige
    geheugenindeling van Linux uit (`setarch -R`), dan geeft hij er nog maar
    een: het resultaat hangt dus af van waar het proces in het geheugen komt te
    liggen, de signatuur van een variabele die gelezen wordt zonder ooit
    geschreven te zijn. Zonder die pin knippert deze poort tussen "identiek" en
    "wijkt af" op dezelfde code, en dan bewaakt hij niets.

    Ontbreekt `setarch`, dan draait de poort gewoon door en waarschuwt. Met
    `--geen-pin` laat je de willekeur juist staan, om die instabiliteit zelf te
    zien: draai een deck dan een paar keer en vergelijk de uitkomsten.
    """
    if not PIN_MEMORY_LAYOUT:
        return []
    if shutil.which("setarch"):
        return ["setarch", "-R"]
    print("  let op: setarch ontbreekt, de vergelijking kan van run tot run wisselen")
    return []


def find_binary(build_directory: Path) -> Path | None:
    candidate = build_directory / "bin" / "swan.exe"
    if candidate.is_file():
        return candidate
    for found in build_directory.rglob("swan.exe"):
        if found.is_file():
            return found
    return None


def build_upstream(worktree: Path, build_directory: Path, jobs: int) -> Path:
    existing = find_binary(build_directory)
    if existing:
        return existing
    print(f"upstream bouwen in {build_directory} (eenmalig, duurt enkele minuten) ...")
    configure = subprocess.run(
        ["cmake", "-S", str(worktree), "-B", str(build_directory), "-DCMAKE_BUILD_TYPE=Release"],
        capture_output=True,
        text=True,
        check=False,
    )
    if configure.returncode != 0:
        raise SystemExit(f"cmake configure faalde:\n{configure.stdout}\n{configure.stderr}")
    build = subprocess.run(
        ["cmake", "--build", str(build_directory), "-j", str(jobs)],
        capture_output=True,
        text=True,
        check=False,
    )
    if build.returncode != 0:
        raise SystemExit(f"upstream-bouw faalde:\n{build.stdout[-4000:]}\n{build.stderr[-4000:]}")
    binary = find_binary(build_directory)
    if not binary:
        raise SystemExit(f"upstream-bouw leverde geen swan.exe in {build_directory}")
    return binary


def is_output(name: str) -> bool:
    if name in IGNORED_NAMES:
        return False
    if Path(name).suffix.lower() in IGNORED_SUFFIXES:
        return False
    return not name.startswith(IGNORED_PREFIXES)


def copy_case_files(source: Path, destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    for item in source.iterdir():
        if item.name in SKIPPED_DIRECTORIES:
            continue
        if item.is_dir():
            shutil.copytree(item, destination / item.name, dirs_exist_ok=True)
        else:
            shutil.copy2(item, destination / item.name)


def stage_case(deck: str, work_root: Path) -> Path:
    """Zet de invoer van één case klaar met het deck als INPUT.

    De case krijgt zijn eigen map met de oorspronkelijke naam, want een deck
    mag naar een buurcase verwijzen: `voordelta_unstructured` leest de
    bathymetrie als `../voordelta/voordelta.dep`. Die buren worden meegekopieerd
    op precies die relatieve plek, anders faalt het deck op een ontbrekend
    bestand en zou de poort dat als "wijkt af" tellen.
    """
    deck_path = EXAMPLES / deck
    case_directory = deck_path.parent
    run_directory = work_root / case_directory.name
    copy_case_files(case_directory, run_directory)
    for sibling in sorted(set(re.findall(r"\.\./([A-Za-z0-9_.-]+)/", deck_path.read_text(errors="replace")))):
        neighbour = case_directory.parent / sibling
        if neighbour.is_dir():
            copy_case_files(neighbour, work_root / sibling)
    shutil.copy2(deck_path, run_directory / "INPUT")
    return run_directory


def written_since(directory: Path, moment: float) -> dict[str, bytes]:
    """De bestanden die de run zelf heeft geschreven.

    Op de tijdstempel, niet op de inhoud. Een aantal cases heeft zijn eigen
    uitvoer naast het deck staan; schrijft de run daar exact hetzelfde in
    terug, dan is dat wel degelijk uitvoer die vergeleken moet worden. Een
    inhoudsvergelijking zou die stilzwijgend overslaan -- en juist bij de kant
    die het goed doet, wat de vergelijking op zijn kop zet.

    `shutil.copy2` bewaart de oorspronkelijke tijdstempel van de gekopieerde
    invoer, dus alles wat na de start van de run is aangeraakt komt van de run.
    """
    written: dict[str, bytes] = {}
    for path in directory.rglob("*"):
        if not path.is_file():
            continue
        if path.stat().st_mtime + 1.0 < moment:
            continue
        written[str(path.relative_to(directory))] = path.read_bytes()
    return written


def run_deck(binary: Path, deck: str, destination: Path, timeout: int) -> RunOutcome:
    """Draai één deck en bewaar wat het produceerde."""
    destination.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="swan-delta-") as temporary:
        work_directory = stage_case(deck, Path(temporary))
        timed_out = False
        moment = time.time()
        started = time.monotonic()
        try:
            completed = subprocess.run(
                [*launcher(), str(binary)],
                cwd=work_directory,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False,
            )
            returncode = completed.returncode
        except subprocess.TimeoutExpired:
            returncode = -1
            timed_out = True
        seconds = time.monotonic() - started
        outputs: dict[str, Path] = {}
        for name, content in written_since(work_directory, moment).items():
            if not is_output(name):
                continue
            target = destination / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(content)
            outputs[name] = target
    return RunOutcome(returncode=returncode, outputs=outputs, seconds=seconds, timed_out=timed_out)


def files_equal(first: Path, second: Path) -> bool:
    """Gelijk als tekstvelden exact overeenkomen, of als de bytes gelijk zijn.

    Tolerantie nul: dezelfde compiler, dezelfde vlaggen, dezelfde machine, dus
    elk verschil is code. `same_results` negeert alleen witruimte, wat het
    verschil tussen de seriële en de MPI-schrijfroute is en geen resultaat.
    """
    if filecmp.cmp(first, second, shallow=False):
        return True
    try:
        first.read_text()
        second.read_text()
    except (OSError, UnicodeDecodeError):
        return False
    return same_results(first, second, tolerance=0.0)


def compare_outputs(first: RunOutcome, second: RunOutcome) -> tuple[bool, str]:
    """Vergelijk wat twee runs produceerden."""
    names = sorted(set(first.outputs) | set(second.outputs))
    missing = [name for name in names if name not in first.outputs or name not in second.outputs]
    if missing:
        return False, "alleen aan één kant aanwezig: " + ", ".join(missing[:3])
    different = [name for name in names if not files_equal(first.outputs[name], second.outputs[name])]
    if different:
        return False, "verschilt in: " + ", ".join(different[:3])
    return True, f"{len(names)} uitvoerbestand(en) gelijk"


def assess_deck(
    deck: str,
    fork_binary: Path,
    upstream_binary: Path,
    root: Path,
    timeout: int,
    repeats: int,
) -> tuple[str, str, float]:
    """Bepaal de status van één deck, met een controle op zelfconsistentie.

    Een enkele vergelijking veronderstelt dat beide binaries op hetzelfde deck
    steeds hetzelfde antwoord geven. Dat is niet vanzelfsprekend: upstream 41.51
    blijkt op verscheidene decks van run tot run twee verschillende antwoorden
    te geven. Eén vergelijking zou dan de ene keer "identiek" en de andere keer
    "wijkt af" zeggen, en de poort zou willekeurig knipperen.

    Daarom wordt een gevonden verschil eerst tegen de kant zelf gehouden: geeft
    een binary op herhaling een ander antwoord dan zichzelf, dan is dát het
    resultaat -- instabiliteit, geen verschil tussen de twee lijnen. Herhalen
    gebeurt alleen bij een verschil, zodat de gelijke gevallen goedkoop blijven.
    """
    fork = run_deck(fork_binary, deck, root / "fork", timeout)
    upstream = run_deck(upstream_binary, deck, root / "upstream", timeout)
    seconds = fork.seconds + upstream.seconds

    #  Een tijdslimiet bewijst niets over het verschil; hij zegt alleen dat de
    #  poort te weinig tijd gaf. Dat hoort niet als "alleen fork" te eindigen,
    #  want dan leest een overschrijding als een eigenschap van de code.
    if fork.timed_out or upstream.timed_out:
        kant = "de fork" if fork.timed_out else "upstream"
        return (
            STATUS_DIFFERS,
            f"{kant} overschreed de tijdslimiet -- verhoog --timeout of sla het deck bewust over",
            seconds,
        )
    if fork.returncode != 0:
        return STATUS_DIFFERS, f"de fork stopte met code {fork.returncode}", seconds
    if upstream.returncode != 0:
        return STATUS_FORK_ONLY, f"upstream stopte met code {upstream.returncode}", seconds
    if not fork.outputs:
        return STATUS_DIFFERS, "de fork produceerde geen uitvoer om te vergelijken", seconds

    same, detail = compare_outputs(fork, upstream)
    if same:
        return STATUS_IDENTICAL, detail, seconds

    for kant, binary, first, status in (
        ("de fork", fork_binary, fork, STATUS_FORK_UNSTABLE),
        ("upstream", upstream_binary, upstream, STATUS_UPSTREAM_UNSTABLE),
    ):
        answers = 1
        for attempt in range(repeats):
            again = run_deck(binary, deck, root / f"{status}_{attempt}", timeout)
            seconds += again.seconds
            if not compare_outputs(first, again)[0]:
                answers += 1
        if answers > 1:
            return (
                status,
                f"{kant} geeft zichzelf {answers} verschillende antwoorden in {repeats + 1} runs; "
                "een vergelijking zegt hier niets",
                seconds,
            )
    return STATUS_DIFFERS, detail, seconds


def entries_by_deck(manifest: dict) -> dict[str, dict]:
    return {entry["deck"]: entry for entry in manifest.get("decks", [])}


def check_inventory(manifest: dict, decks: list[str]) -> list[str]:
    """Statische controles: is de inventaris zelf compleet en verklaard?"""
    problems: list[str] = []
    unattributed: list[str] = []
    entries = entries_by_deck(manifest)
    known = known_bugfix_identifiers()

    for deck in decks:
        if deck not in entries:
            problems.append(f"{deck}: staat niet in het manifest")
    for deck in entries:
        if deck not in decks:
            problems.append(f"{deck}: staat in het manifest maar bestaat niet meer")

    for deck, entry in sorted(entries.items()):
        status = entry.get("status")
        if status not in (
            STATUS_IDENTICAL,
            STATUS_DIFFERS,
            STATUS_FORK_ONLY,
            STATUS_SKIPPED,
            STATUS_UPSTREAM_UNSTABLE,
            STATUS_FORK_UNSTABLE,
        ):
            problems.append(f"{deck}: onbekende status {status!r}")
            continue
        #  Een instabiele fork is nooit een vastgelegde toestand: reproduceerbaar
        #  rekenen is de belofte van deze kant, dus dit is altijd rood.
        if status == STATUS_FORK_UNSTABLE:
            problems.append(f"{deck}: de fork geeft zichzelf verschillende antwoorden")
            continue
        if status in (STATUS_DIFFERS, STATUS_FORK_ONLY):
            bugfix = entry.get("bugfix")
            if bugfix:
                if known and bugfix not in known:
                    problems.append(f"{deck}: verwijst naar {bugfix}, dat de bugfixtabel niet kent")
            elif entry.get("onderzoek"):
                unattributed.append(deck)
            else:
                problems.append(
                    f"{deck}: {status} zonder BF-verwijzing en zonder onderzoeksnotitie -- "
                    "verklaar het verschil of repareer het"
                )
        if status == STATUS_SKIPPED and not entry.get("reden"):
            problems.append(f"{deck}: overgeslagen zonder reden")

    #  Ratel, geen belofte. Toen deze poort werd gebouwd bleken er meer
    #  afwijkende decks te zijn dan de bugfixtabel in proza noemde. Die
    #  achterstand is geteld en vastgezet: hij mag krimpen, nooit groeien.
    #  Zo is een nieuw onverklaard verschil rood terwijl de bestaande
    #  achterstand zichtbaar blijft in plaats van de poort onbruikbaar te maken.
    budget = manifest.get("onverklaard_budget")
    if budget is None:
        problems.append("het manifest noemt geen onverklaard_budget")
    elif len(unattributed) > budget:
        problems.append(
            f"{len(unattributed)} decks wijken onverklaard af, budget is {budget}: "
            + ", ".join(sorted(unattributed))
        )
    elif len(unattributed) < budget:
        problems.append(
            f"nog maar {len(unattributed)} onverklaarde decks tegen een budget van "
            f"{budget}: zet het budget omlaag zodat de ratel vasthoudt"
        )
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--manifest", default=str(MANIFEST), help="pad naar de delta-inventaris")
    parser.add_argument("--fork-binary", help="swan.exe van de fork (default: build/bin/swan.exe)")
    parser.add_argument("--upstream-binary", help="swan.exe van upstream (default: zelf bouwen)")
    parser.add_argument(
        "--upstream-build",
        default=str(REPOSITORY / ".upstream"),
        help="map voor de upstream-worktree en -bouw (default: %(default)s)",
    )
    parser.add_argument("--deck", action="append", help="beperk tot dit deck (herhaalbaar)")
    parser.add_argument("--include-skipped", action="store_true", help="draai ook de overgeslagen decks")
    parser.add_argument("--update", action="store_true", help="neem de gemeten werkelijkheid over in het manifest")
    parser.add_argument("--inventory-only", action="store_true", help="alleen de statische controles, niets draaien")
    parser.add_argument("--timeout", type=int, default=600, help="secondenlimiet per deckrun (default: %(default)s)")
    parser.add_argument(
        "--repeats",
        type=int,
        default=2,
        help="extra runs per kant bij een gevonden verschil, om instabiliteit van een "
        "binary te onderscheiden van een echt verschil (default: %(default)s)",
    )
    parser.add_argument("--jobs", type=int, default=0, help="parallelle bouwtaken (default: alle kernen)")
    parser.add_argument("--keep-results", help="bewaar de uitvoer van beide binaries hier")
    parser.add_argument(
        "--geen-pin",
        action="store_true",
        help="draai zonder vaste geheugenindeling; laat de instabiliteit van upstream juist zien",
    )
    arguments = parser.parse_args()

    global PIN_MEMORY_LAYOUT
    PIN_MEMORY_LAYOUT = not getattr(arguments, "geen_pin", False)

    manifest_path = Path(arguments.manifest).resolve()
    manifest = load_manifest(manifest_path)
    decks = discover_decks()

    problems = check_inventory(manifest, decks)
    if arguments.inventory_only:
        for problem in problems:
            print(f"  [!] {problem}")
        print(f"\n{len(decks)} deck(s) in de suite, {len(entries_by_deck(manifest))} in het manifest.")
        return 1 if problems else 0

    jobs = arguments.jobs or len(os.sched_getaffinity(0))
    upstream_root = Path(arguments.upstream_build).resolve()
    if arguments.upstream_binary:
        upstream_binary = Path(arguments.upstream_binary).resolve()
        if not upstream_binary.is_file():
            raise SystemExit(f"upstream-binary niet gevonden: {upstream_binary}")
    else:
        commit = manifest["upstream"]["commit"]
        ensure_upstream_source(upstream_root / "src", commit)
        upstream_binary = build_upstream(upstream_root / "src", upstream_root / "build", jobs)

    if arguments.fork_binary:
        fork_binary = Path(arguments.fork_binary).resolve()
    else:
        fork_binary = REPOSITORY / "build" / "bin" / "swan.exe"
    if not fork_binary.is_file():
        raise SystemExit(f"fork-binary niet gevonden: {fork_binary}. Bouw de fork eerst.")

    entries = entries_by_deck(manifest)
    selected = arguments.deck or decks
    results_root = Path(arguments.keep_results).resolve() if arguments.keep_results else None

    verdicts: list[DeckVerdict] = []
    with tempfile.TemporaryDirectory(prefix="swan-delta-runs-") as scratch:
        for deck in selected:
            entry = entries.get(deck, {})
            if entry.get("status") == STATUS_SKIPPED and not arguments.include_skipped:
                verdicts.append(DeckVerdict(deck, STATUS_SKIPPED, entry.get("reden", "")))
                continue
            root = Path(results_root or scratch) / deck.replace("/", "_")
            status, detail, seconds = assess_deck(
                deck, fork_binary, upstream_binary, root, arguments.timeout, arguments.repeats
            )
            verdicts.append(DeckVerdict(deck, status, detail))
            print(f"  {status:<18} {deck}  ({seconds:.1f} s)")

    width = max(len(verdict.deck) for verdict in verdicts) if verdicts else 10
    print()
    for verdict in verdicts:
        entry = entries.get(verdict.deck, {})
        claimed = entry.get("status", "-- niet in manifest --")
        #  Bij een overgeslagen deck staat de reden al in de detailkolom; hem
        #  ook links zetten levert dezelfde zin twee keer op.
        attribution = entry.get("bugfix") or ("" if verdict.status == STATUS_SKIPPED else entry.get("reden", ""))
        marker = "[x]" if claimed == verdict.status else "[!]"
        print(f"  {marker} {verdict.deck:<{width}}  {verdict.status:<18} {attribution:<8} {verdict.detail}")

    mismatched = [
        verdict
        for verdict in verdicts
        if entries.get(verdict.deck, {}).get("status") != verdict.status
    ]

    if arguments.update:
        for verdict in verdicts:
            entry = entries.get(verdict.deck)
            if entry is None:
                entry = {"deck": verdict.deck}
                manifest.setdefault("decks", []).append(entry)
            if entry.get("status") != verdict.status:
                entry["status"] = verdict.status
                if verdict.status in (STATUS_DIFFERS, STATUS_FORK_ONLY):
                    #  Een nieuw verschil wordt niet stilzwijgend goedgekeurd:
                    #  zonder BF-nummer valt de controle er alsnog op.
                    entry.setdefault("bugfix", None)
            entry["gemeten"] = verdict.detail
        manifest["decks"] = sorted(manifest["decks"], key=lambda item: item["deck"])
        write_manifest(manifest_path, manifest)
        print(f"\nmanifest bijgewerkt: {manifest_path}")
        problems = check_inventory(manifest, decks)

    for problem in problems:
        print(f"  [!] {problem}")
    for verdict in mismatched:
        claimed = entries.get(verdict.deck, {}).get("status", "-- niet in manifest --")
        print(f"  [!] {verdict.deck}: manifest zegt {claimed!r}, gemeten {verdict.status!r}")

    if problems or (mismatched and not arguments.update):
        print("\nDe delta-inventaris klopt niet meer met de werkelijkheid.")
        return 1
    print("\nElk verschil met upstream staat in het manifest en heeft een verklaring.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
