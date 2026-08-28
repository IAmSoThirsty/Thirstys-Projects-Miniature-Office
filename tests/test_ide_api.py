"""API tests for workspace and terminal routes."""

import json
from pathlib import Path

import pytest

from src.core.audit import AuditLog
from src.core.entity import EntityRegistry
from src.departments.department import DepartmentRegistry
from src.server import app as app_module
from src.tools.supply_store import SupplyStore
from src.workspace import fs as fs_mod


@pytest.fixture
def ide_client(tmp_path):
    app_module.app.config["TESTING"] = True
    fs_mod._workspace = None
    ws = fs_mod.get_workspace(tmp_path / "user_workspace")
    ws.seed_if_empty()

    app_module.simulation = None
    import src.core.entity as entity_mod

    entity_mod._registry = EntityRegistry()
    import src.core.audit as audit_mod

    audit_mod._audit_log = AuditLog()
    import src.departments.department as dept_mod

    dept_mod._department_registry = DepartmentRegistry()
    import src.core.world as world_mod

    world_mod._world = None
    import src.tools.supply_store as supply_mod

    supply_mod._supply_store = SupplyStore()
    app_module.simulation = app_module.init_simulation()

    with app_module.app.test_client() as client:
        yield client

    fs_mod._workspace = None


def test_workspace_info(ide_client):
    res = ide_client.get("/api/ide/workspace")
    assert res.status_code == 200
    data = res.get_json()
    assert data["writable"] is True
    assert "tree" in data


def test_write_read_via_api(ide_client):
    put = ide_client.put(
        "/api/ide/files",
        data=json.dumps({"path": "n.txt", "content": "alpha"}),
        content_type="application/json",
    )
    assert put.status_code == 200
    got = ide_client.get("/api/ide/files?path=n.txt")
    assert got.status_code == 200
    assert got.get_json()["content"] == "alpha"


def test_escape_rejected(ide_client):
    res = ide_client.get("/api/ide/files?path=../secret")
    assert res.status_code == 400


def test_terminal_python(ide_client):
    res = ide_client.post(
        "/api/ide/terminal",
        data=json.dumps({"command": "python3 --version"}),
        content_type="application/json",
    )
    assert res.status_code == 200
    body = res.get_json()
    assert body["timed_out"] is False
    assert body["exit_code"] == 0
