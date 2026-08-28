"""Workspace filesystem tests."""

from pathlib import Path

import pytest

from src.workspace.fs import Workspace, WorkspaceError


@pytest.fixture
def ws(tmp_path):
    return Workspace(tmp_path / "ws")


def test_write_and_read_roundtrip(ws):
    ws.write_text("hello.txt", "hi", overwrite=False)
    data = ws.read_text("hello.txt")
    assert data["content"] == "hi"
    assert data["path"] == "hello.txt"


def test_overwrite_requires_flag(ws):
    ws.write_text("a.txt", "one")
    with pytest.raises(WorkspaceError, match="overwrite"):
        ws.write_text("a.txt", "two", overwrite=False)
    ws.write_text("a.txt", "two", overwrite=True)
    assert ws.read_text("a.txt")["content"] == "two"


def test_rejects_escape(ws):
    with pytest.raises(WorkspaceError, match="escapes"):
        ws.resolve("../secret")


def test_rejects_absolute(ws):
    with pytest.raises(WorkspaceError, match="Absolute"):
        ws.resolve("/etc/passwd")


def test_delete_requires_confirm(ws):
    ws.write_text("gone.txt", "x")
    with pytest.raises(WorkspaceError, match="confirm"):
        ws.delete("gone.txt", confirm=False)
    ws.delete("gone.txt", confirm=True)
    with pytest.raises(WorkspaceError):
        ws.read_text("gone.txt")


def test_list_and_tree(ws):
    ws.write_text("dir/n.txt", "n")
    names = {e["name"] for e in ws.list_dir(".")}
    assert "dir" in names
    tree = ws.tree(".")
    assert tree["type"] == "dir"
    assert any(child["name"] == "dir" for child in tree["children"])


def test_mkdir_and_nonempty_dir(ws):
    ws.mkdir("box")
    ws.write_text("box/f.txt", "f")
    with pytest.raises(WorkspaceError, match="not empty"):
        ws.delete("box", confirm=True)
