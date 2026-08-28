"""Process execution."""

from src.runtime.terminal import (
    CommandResult,
    TerminalError,
    run_command,
    split_command_line,
)

__all__ = ["CommandResult", "TerminalError", "run_command", "split_command_line"]
