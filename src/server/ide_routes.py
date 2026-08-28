"""IDE routes: workspace, editor, terminal. Real file and process operations."""

from __future__ import annotations

import os
from pathlib import Path

from flask import Flask, jsonify, request

from src.core.audit import EventType, get_audit_log
from src.runtime.terminal import TerminalError, run_command, split_command_line
from src.workspace.fs import WorkspaceError, get_workspace


def _error(exc):
    status = getattr(exc, "status_code", 400)
    return jsonify({"error": str(exc)}), status


def register_ide_routes(app: Flask) -> None:
    @app.route("/api/ide/workspace", methods=["GET"])
    def ide_workspace_info():
        ws = get_workspace()
        info = ws.info()
        info["tree"] = ws.tree(".", depth=5)
        return jsonify(info)

    @app.route("/api/ide/files", methods=["GET"])
    def ide_list_or_read():
        ws = get_workspace()
        rel = request.args.get("path", ".")
        try:
            target = ws.resolve(rel)
            if target.is_dir() or rel in ("", "."):
                return jsonify(
                    {"path": ws.relpath(target), "entries": ws.list_dir(rel)}
                )
            return jsonify(ws.read_text(rel))
        except WorkspaceError as exc:
            return _error(exc)

    @app.route("/api/ide/files", methods=["PUT"])
    def ide_write():
        payload = request.get_json(silent=True) or {}
        rel = payload.get("path")
        content = payload.get("content")
        overwrite = bool(payload.get("overwrite", False))
        if not rel:
            return jsonify({"error": "path is required"}), 400
        if content is None:
            return jsonify({"error": "content is required"}), 400
        ws = get_workspace()
        try:
            result = ws.write_text(rel, content, overwrite=overwrite)
        except WorkspaceError as exc:
            return _error(exc)
        get_audit_log().log_event(
            EventType.WORKSPACE_WRITE,
            actor_id="operator",
            target_id=result["path"],
            data={"size": result["size"], "overwritten": result["overwritten"]},
        )
        return jsonify(result)

    @app.route("/api/ide/mkdir", methods=["POST"])
    def ide_mkdir():
        payload = request.get_json(silent=True) or {}
        rel = payload.get("path")
        if not rel:
            return jsonify({"error": "path is required"}), 400
        try:
            return jsonify(get_workspace().mkdir(rel))
        except WorkspaceError as exc:
            return _error(exc)

    @app.route("/api/ide/files", methods=["DELETE"])
    def ide_delete():
        payload = request.get_json(silent=True) or {}
        rel = payload.get("path") or request.args.get("path")
        confirm = bool(payload.get("confirm", False))
        if not rel:
            return jsonify({"error": "path is required"}), 400
        ws = get_workspace()
        try:
            result = ws.delete(rel, confirm=confirm)
        except WorkspaceError as exc:
            return _error(exc)
        get_audit_log().log_event(
            EventType.WORKSPACE_DELETE,
            actor_id="operator",
            target_id=result["deleted"],
            data={},
        )
        return jsonify(result)

    @app.route("/api/ide/terminal", methods=["POST"])
    def ide_terminal():
        payload = request.get_json(silent=True) or {}
        argv = payload.get("argv")
        command = payload.get("command")
        cwd = payload.get("cwd")
        timeout_sec = payload.get("timeout_sec")
        try:
            if argv is None and command is not None:
                argv = split_command_line(command)
            if not argv:
                return jsonify({"error": "argv or command is required"}), 400
            result = run_command(
                get_workspace(),
                argv,
                cwd=cwd,
                timeout_sec=timeout_sec,
            )
        except (TerminalError, WorkspaceError) as exc:
            return _error(exc)
        get_audit_log().log_event(
            EventType.TERMINAL_RUN,
            actor_id="operator",
            target_id=result.cwd,
            data={
                "argv": result.argv,
                "exit_code": result.exit_code,
                "timed_out": result.timed_out,
            },
        )
        return jsonify(result.to_dict())

    @app.route("/api/ide/health", methods=["GET"])
    def ide_health():
        ws = get_workspace()
        info = ws.info()
        audit = get_audit_log()
        return jsonify(
            {
                "workspace": info,
                "audit_events": len(audit.graph.events),
                "audit_chain_ok": audit.verify_chain(),
                "persist_path": str(audit.persist_path) if audit.persist_path else None,
                "data_dir": os.getenv("MO_DATA_DIR"),
            }
        )


def configure_ide_defaults() -> None:
    """Attach process workspace and optional persisted audit log."""
    workspace_root = os.getenv("MO_WORKSPACE")
    if workspace_root:
        get_workspace(Path(workspace_root)).seed_if_empty()
    else:
        get_workspace().seed_if_empty()

    data_dir = os.getenv("MO_DATA_DIR")
    if data_dir:
        from src.core import audit as audit_mod

        persist = Path(data_dir) / "audit.jsonl"
        if audit_mod._audit_log.persist_path is None:
            if persist.exists():
                audit_mod.reset_audit_log(persist)
            else:
                audit_mod._audit_log.persist_path = persist
                persist.parent.mkdir(parents=True, exist_ok=True)
