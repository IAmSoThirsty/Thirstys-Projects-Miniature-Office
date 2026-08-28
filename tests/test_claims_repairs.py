"""Tests for the honesty-pass repairs: health, PWA, SQL floor, token, analyzers."""

import json
from pathlib import Path

from src.analysis.ast_analyzer import ASTAnalyzer
from src.analysis.dependency_analyzer import DependencyAnalyzer
from src.analysis.metrics_calculator import MetricsCalculator
from src.analysis.pattern_detector import PatternDetector, PatternType
from src.server import app as app_module


def test_health_without_simulation_is_liveness(monkeypatch):
    app_module.app.config["TESTING"] = True
    app_module.simulation = None
    with app_module.app.test_client() as client:
        response = client.get("/health")
    assert response.status_code == 200
    body = response.get_json()
    assert body["status"] == "healthy"


def test_pwa_manifest_and_service_worker():
    app_module.app.config["TESTING"] = True
    with app_module.app.test_client() as client:
        manifest = client.get("/manifest.json")
        sw = client.get("/sw.js")
    assert manifest.status_code == 200
    data = manifest.get_json()
    assert data["name"] == "Miniature Office IDE"
    assert sw.status_code == 200
    assert b"miniature-office-shell" in sw.data


def test_sql_floor_has_sql():
    schema = Path("floors/sql/schema.sql")
    assert schema.is_file()
    text = schema.read_text(encoding="utf-8")
    assert "CREATE TABLE" in text


def test_singleton_pattern_detected():
    source = "class Singleton:\n    _instance = None\n    def __new__(cls):\n        return cls._instance\n"
    root, error = ASTAnalyzer().parse_source(source)
    assert error is None
    patterns = PatternDetector().detect_patterns(root)
    assert any(p.pattern_type == PatternType.SINGLETON for p in patterns)


def test_imports_appear_in_dependency_graph():
    root, error = ASTAnalyzer().parse_source("import os\nimport sys\n")
    assert error is None
    graph = DependencyAnalyzer().analyze_dependencies(root)
    assert "os" in graph.nodes
    assert "sys" in graph.nodes


def test_complexity_not_constant_one_for_branches():
    source = "def foo(x):\n    if x:\n        return 1\n    elif x == 2:\n        return 2\n    return 0\n"
    root, error = ASTAnalyzer().parse_source(source)
    assert error is None
    metrics = MetricsCalculator().calculate_complexity(root)
    assert metrics.cyclomatic_complexity > 1


def test_ide_token_rejects_terminal(monkeypatch, tmp_path):
    from src.workspace import fs as fs_mod

    monkeypatch.setenv("MO_IDE_TOKEN", "secret-token")
    app_module.app.config["TESTING"] = True
    fs_mod._workspace = None
    fs_mod.get_workspace(tmp_path / "user_workspace").seed_if_empty()
    with app_module.app.test_client() as client:
        denied = client.post(
            "/api/ide/terminal",
            data=json.dumps({"command": "python3 --version"}),
            content_type="application/json",
        )
        allowed = client.post(
            "/api/ide/terminal",
            data=json.dumps({"command": "python3 --version"}),
            content_type="application/json",
            headers={"X-MO-Token": "secret-token"},
        )
    assert denied.status_code == 401
    assert allowed.status_code == 200


def test_ci_bandit_json_uses_severity_floor():
    text = Path(".github/workflows/ci.yml").read_text(encoding="utf-8")
    bandit_lines = [
        line.strip()
        for line in text.splitlines()
        if "bandit " in line and not line.strip().startswith("#")
    ]
    assert bandit_lines
    for line in bandit_lines:
        assert "-ll" in line, line
    assert "'3.9'" not in text
    assert "'3.10'" in text


def test_operator_docs_do_not_deny_pwa():
    install = Path("INSTALL.md").read_text(encoding="utf-8")
    started = Path("GETTING_STARTED.md").read_text(encoding="utf-8")
    docs = Path("DOCS.md").read_text(encoding="utf-8")
    assert "service worker are absent" not in install
    assert "this repo is not a PWA" not in started
    assert "no web-app manifest" not in started
    assert "Not a PWA" not in docs
