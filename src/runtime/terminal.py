"""Run argv inside the workspace. No shell. Timeout and output caps enforced."""

from __future__ import annotations

import os
import shlex
import shutil
import subprocess
import time
from dataclasses import dataclass
from typing import List, Optional, Sequence

from src.workspace.fs import Workspace

DEFAULT_TIMEOUT_SEC = 15
MAX_TIMEOUT_SEC = 60
MAX_OUTPUT_BYTES = 262_144


class TerminalError(Exception):
    def __init__(self, message: str, status_code: int = 400):
        super().__init__(message)
        self.status_code = status_code


@dataclass
class CommandResult:
    argv: List[str]
    cwd: str
    exit_code: Optional[int]
    stdout: str
    stderr: str
    timed_out: bool
    duration_ms: int

    def to_dict(self):
        return {
            "argv": self.argv,
            "cwd": self.cwd,
            "exit_code": self.exit_code,
            "stdout": self.stdout,
            "stderr": self.stderr,
            "timed_out": self.timed_out,
            "duration_ms": self.duration_ms,
        }


def _decode(raw: bytes) -> str:
    if raw is None:
        return ""
    if len(raw) > MAX_OUTPUT_BYTES:
        raw = raw[:MAX_OUTPUT_BYTES] + b"\n...[truncated]..."
    return raw.decode("utf-8", errors="replace")


def _resolve_executable(program: str) -> str:
    if not program or program.strip() == "":
        raise TerminalError("Command is empty")
    if program.startswith("-"):
        raise TerminalError("Invalid program name")
    if "/" in program or "\\" in program:
        raise TerminalError("Program must be a PATH command, not a path")
    resolved = shutil.which(program)
    if not resolved:
        raise TerminalError("Command not found: {0}".format(program), 404)
    return resolved


def split_command_line(line: str) -> List[str]:
    if line is None:
        raise TerminalError("command is required")
    stripped = line.strip()
    if not stripped:
        raise TerminalError("command is empty")
    if any(token in stripped for token in [";", "|", "&", "`", "$(", "\n"]):
        raise TerminalError(
            "Shell operators are not allowed. Enter one program and its arguments."
        )
    try:
        parts = shlex.split(stripped, posix=True)
    except ValueError as exc:
        raise TerminalError(str(exc))
    if not parts:
        raise TerminalError("command is empty")
    return parts


def run_command(
    workspace: Workspace,
    argv: Sequence[str],
    cwd: Optional[str] = None,
    timeout_sec: Optional[float] = None,
    stdin_text: Optional[str] = None,
) -> CommandResult:
    if not argv or not isinstance(argv, (list, tuple)):
        raise TerminalError("argv must be a non-empty list")
    if any(not isinstance(part, str) for part in argv):
        raise TerminalError("argv entries must be strings")

    timeout = DEFAULT_TIMEOUT_SEC if timeout_sec is None else float(timeout_sec)
    if timeout <= 0 or timeout > MAX_TIMEOUT_SEC:
        raise TerminalError(
            "timeout_sec must be between 0 and {0}".format(MAX_TIMEOUT_SEC)
        )

    workdir = workspace.resolve(cwd or ".")
    if not workdir.exists() or not workdir.is_dir():
        raise TerminalError("Working directory not found", 404)

    program = _resolve_executable(argv[0])
    full_argv = [program, *list(argv[1:])]
    env = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": str(workspace.root),
        "LANG": os.environ.get("LANG", "C.UTF-8"),
        "TERM": "dumb",
        "MO_WORKSPACE": str(workspace.root),
    }

    started = time.monotonic()
    timed_out = False
    exit_code: Optional[int]
    try:
        completed = subprocess.run(
            full_argv,
            cwd=str(workdir),
            env=env,
            input=None if stdin_text is None else stdin_text.encode("utf-8"),
            capture_output=True,
            text=False,
            timeout=timeout,
            check=False,
        )
        exit_code = completed.returncode
        stdout = _decode(completed.stdout or b"")
        stderr = _decode(completed.stderr or b"")
    except subprocess.TimeoutExpired as exc:
        timed_out = True
        exit_code = None
        stdout = _decode(exc.stdout or b"")
        stderr = _decode(exc.stderr or b"") + (
            "\nCommand timed out after {0} seconds".format(timeout)
        )
    duration_ms = int((time.monotonic() - started) * 1000)
    return CommandResult(
        argv=list(argv),
        cwd=workspace.relpath(workdir),
        exit_code=exit_code,
        stdout=stdout,
        stderr=stderr,
        timed_out=timed_out,
        duration_ms=duration_ms,
    )
