"""Workspace filesystem rooted at one directory. Paths cannot escape it."""

from __future__ import annotations

import os
import stat
from pathlib import Path
from typing import Any, Dict, List, Optional

MAX_FILE_BYTES = 1_048_576
TEXT_SAMPLE = 4096


class WorkspaceError(Exception):
    def __init__(self, message: str, status_code: int = 400):
        super().__init__(message)
        self.status_code = status_code


class Workspace:
    def __init__(self, root: Path):
        self.root = Path(root).resolve()
        self.root.mkdir(parents=True, exist_ok=True)

    def resolve(self, rel: Optional[str]) -> Path:
        raw = "." if rel is None or str(rel).strip() == "" else str(rel)
        raw = raw.replace("\\", "/")
        if raw.startswith("/"):
            raise WorkspaceError("Absolute paths are not allowed")
        candidate = (self.root / raw).resolve()
        try:
            candidate.relative_to(self.root)
        except ValueError:
            raise WorkspaceError("Path escapes the workspace root")
        return candidate

    def relpath(self, path: Path) -> str:
        rel = path.resolve().relative_to(self.root)
        return "." if str(rel) == "." else str(rel).replace("\\", "/")

    def list_dir(self, rel: Optional[str] = ".") -> List[Dict[str, Any]]:
        path = self.resolve(rel)
        if not path.exists():
            raise WorkspaceError("Directory not found", 404)
        if not path.is_dir():
            raise WorkspaceError("Not a directory")
        entries: List[Dict[str, Any]] = []
        children = sorted(
            path.iterdir(), key=lambda p: (not p.is_dir(), p.name.lower())
        )
        for child in children:
            if child.name.startswith("."):
                continue
            item: Dict[str, Any] = {
                "name": child.name,
                "path": self.relpath(child),
                "type": "dir" if child.is_dir() else "file",
            }
            if child.is_file():
                item["size"] = child.stat().st_size
            entries.append(item)
        return entries

    def tree(self, rel: Optional[str] = ".", depth: int = 4) -> Dict[str, Any]:
        path = self.resolve(rel)
        if not path.exists():
            raise WorkspaceError("Path not found", 404)

        def walk(node: Path, remaining: int) -> Dict[str, Any]:
            data: Dict[str, Any] = {
                "name": node.name if node != self.root else self.root.name,
                "path": self.relpath(node),
                "type": "dir" if node.is_dir() else "file",
            }
            if node.is_file():
                data["size"] = node.stat().st_size
                return data
            data["children"] = []
            if remaining <= 0:
                return data
            try:
                kids = sorted(
                    node.iterdir(), key=lambda p: (not p.is_dir(), p.name.lower())
                )
            except OSError as exc:
                raise WorkspaceError(str(exc), 500)
            for child in kids:
                if child.name.startswith("."):
                    continue
                data["children"].append(walk(child, remaining - 1))
            return data

        return walk(path, depth)

    def read_text(self, rel: str) -> Dict[str, Any]:
        path = self.resolve(rel)
        if not path.exists():
            raise WorkspaceError("File not found", 404)
        if not path.is_file():
            raise WorkspaceError("Not a file")
        size = path.stat().st_size
        if size > MAX_FILE_BYTES:
            raise WorkspaceError(
                f"File exceeds editor limit of {MAX_FILE_BYTES} bytes", 413
            )
        try:
            sample = path.read_bytes()[:TEXT_SAMPLE]
        except OSError as exc:
            raise WorkspaceError(str(exc), 500)
        if b"\x00" in sample:
            raise WorkspaceError("Binary files cannot be opened in the editor")
        try:
            content = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            raise WorkspaceError("File is not valid UTF-8")
        return {
            "path": self.relpath(path),
            "content": content,
            "size": size,
            "encoding": "utf-8",
        }

    def write_text(
        self, rel: str, content: str, overwrite: bool = False
    ) -> Dict[str, Any]:
        if not isinstance(content, str):
            raise WorkspaceError("Content must be a string")
        encoded = content.encode("utf-8")
        if len(encoded) > MAX_FILE_BYTES:
            raise WorkspaceError(
                f"Content exceeds editor limit of {MAX_FILE_BYTES} bytes", 413
            )
        path = self.resolve(rel)
        existed = path.exists()
        if existed:
            if path.is_dir():
                raise WorkspaceError("Cannot write to a directory")
            if not overwrite:
                raise WorkspaceError(
                    "File exists; pass overwrite=true to replace it", 409
                )
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        return {
            "path": self.relpath(path),
            "size": path.stat().st_size,
            "overwritten": existed,
        }

    def mkdir(self, rel: str) -> Dict[str, Any]:
        path = self.resolve(rel)
        if path.exists():
            if path.is_dir():
                return {"path": self.relpath(path), "existed": True}
            raise WorkspaceError("A file already exists at that path", 409)
        path.mkdir(parents=True, exist_ok=True)
        return {"path": self.relpath(path), "existed": False}

    def delete(self, rel: str, confirm: bool = False) -> Dict[str, Any]:
        if not confirm:
            raise WorkspaceError("Delete requires confirm=true")
        path = self.resolve(rel)
        if path == self.root:
            raise WorkspaceError("Refusing to delete the workspace root")
        if not path.exists():
            raise WorkspaceError("Path not found", 404)
        if path.is_dir():
            try:
                next(path.iterdir())
                raise WorkspaceError("Directory is not empty")
            except StopIteration:
                path.rmdir()
        else:
            path.unlink()
        return {"deleted": self.relpath(path)}

    def seed_if_empty(self) -> None:
        visible = [p for p in self.root.iterdir() if not p.name.startswith(".")]
        if visible:
            return
        (self.root / "welcome.txt").write_text(
            "Miniature Office IDE workspace\n"
            "==============================\n\n"
            "Files saved here persist on disk.\n"
            "The terminal runs with this directory as its working root.\n",
            encoding="utf-8",
        )

    def info(self) -> Dict[str, Any]:
        mode = self.root.stat().st_mode
        return {
            "root": str(self.root),
            "writable": bool(mode & stat.S_IWUSR),
            "max_file_bytes": MAX_FILE_BYTES,
        }


_workspace: Optional[Workspace] = None


def get_workspace(root: Optional[Path] = None) -> Workspace:
    global _workspace
    if root is not None:
        _workspace = Workspace(root)
        return _workspace
    if _workspace is None:
        env_root = os.getenv("MO_WORKSPACE")
        chosen = Path(env_root) if env_root else Path.cwd() / "user_workspace"
        _workspace = Workspace(chosen)
        _workspace.seed_if_empty()
    return _workspace
