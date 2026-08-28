"""Terminal execution tests."""

import pytest

from src.runtime.terminal import TerminalError, run_command, split_command_line
from src.workspace.fs import Workspace


@pytest.fixture
def ws(tmp_path):
    return Workspace(tmp_path / "ws")


def test_split_rejects_shell_operators():
    with pytest.raises(TerminalError):
        split_command_line("echo hi; rm -rf /")


def test_run_python_version(ws):
    result = run_command(ws, ["python3", "--version"])
    assert result.timed_out is False
    assert result.exit_code == 0
    assert "Python" in result.stdout or "Python" in result.stderr


def test_missing_command(ws):
    with pytest.raises(TerminalError, match="not found"):
        run_command(ws, ["definitely-not-a-command-xyz"])


def test_cwd_stays_in_workspace(ws):
    ws.mkdir("sub")
    ws.write_text("sub/note.txt", "ok")
    result = run_command(ws, ["ls"], cwd="sub")
    assert result.exit_code == 0
    assert "note.txt" in result.stdout
