"""Every bug fix in the table is tied to the commit that made it.

`scripts/bf_commits.py check` compares the section "Commit of each fix" of
doc/bug-fixes-vs-tu-delft-41.51.md with the `Bug-fix: BF-NN` trailers in the
history. In a shallow clone the history is incomplete and the check passes
without checking; CI fetches the full history for this test.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

REPOSITORY = Path(__file__).resolve().parent.parent
SCRIPT = REPOSITORY / "scripts" / "bf_commits.py"


def _git_available() -> bool:
    result = subprocess.run(["git", "rev-parse", "HEAD"], cwd=REPOSITORY,
                            capture_output=True, text=True)
    return result.returncode == 0


@pytest.mark.skipif(not _git_available(), reason="not a git checkout")
def test_every_fix_is_tied_to_its_commit():
    result = subprocess.run([sys.executable, str(SCRIPT), "check"], cwd=REPOSITORY,
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
