# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an optional unsigned audit JSONL, language “floors,” a template-based code-generation pipeline, a jailed workspace / no-shell terminal API, and a browser editor/file-tree/terminal UI.

This README reports the **measured** state of [`a41e1f8`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a41e1f866cad6452c678495e23f6cf9d97ec6231) (`a41e1f866cad6452c678495e23f6cf9d97ec6231`). That commit shipped the IDE UI, a production secret gate, and a bandit job that can fail CI. Canonical files on that commit still described `aa7b439` (no editor UI, compose placeholder `SECRET_KEY`, security `|| true`). Independent pytest of `a41e1f8`: **1,558 passed**, 1 skipped. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Score that must sum: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md). Index: [DOCS.md](DOCS.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![Status](https://img.shields.io/badge/status-experimental-orange.svg)](CLAIMS_AUDIT.md)
[![CI](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml)

## What this actually is

| Piece | What exists |
| --- | --- |
| Web app | Flask + Flask-SocketIO, `python3 run.py`, port 5000 |
| Domain model | `src/core/entity.py` — 7 entity types, 8 relations, in-memory registry with `threading.RLock` |
| Audit log | `src/core/audit.py` — SHA-256 chain (`prev_hash` + parent hashes). Optional unsigned JSONL when `MO_DATA_DIR` is set. Not a ledger. Not signed |
| IDE core | Jailed workspace FS, no-shell argv terminal, 7 `/api/ide/*` routes. Real disk and `subprocess.run`. Browser UI in `src/client/index.html`: file tree, textarea editor, terminal form |
| Code pipeline | `src/core/code_civilization.py` — **1,364-line** template generator (not 49,741 lines). Inserts `TODO` bodies and assumes generated tests pass |
| Floors | 28 directories under `floors/`. Every floor README is marked a toy. The SQL floor is Python. See [floors/README.md](floors/README.md) |
| Tests | **1,558 passing**, 1 skipped (independent pytest of `a41e1f8`) |
| CI / CD | Unit tests **green** on 3.9–3.12. Whole CI **red** on `a41e1f8` ([run 33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573)) because bandit `-ll` fails on B104 (`host="0.0.0.0"`). CD **red** ([run 33201115545](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115545)): `test-docker` still curls `/health` (503) while compose healthchecks `/api/ide/health`. `safety` still `\|\| true` |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured 28 August 2026 on `a41e1f8`)

| Metric | Claimed (old README) | Measured (`a41e1f8`) |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | **23,876** total / **18,542** non-comment (53 files; bandit loc=18,542) |
| `code_civilization.py` | 49,741 lines | **1,364 lines** (50,430 bytes; original error treated bytes as lines) |
| Tests | 1,537 passing | **1,558 passed**, 1 skipped (`def test_` grep = 1,591) |
| Coverage | 99% of the system | Fresh `pytest --cov=src`: **7,194 / 7,364** imported statements (**97.69%**). `integrated_specs/` still omitted. Not 99% of the tree |
| Language floors | 30+ native, working | 28 dirs; mixed; SQL floor is Python; each README is bannered as a toy |
| Flask routes | 45+ | **71** `@app.route` entries (64 in `app.py` + 7 IDE) |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |
| Editor UI | (docs on `a41e1f8` still said “no chrome”) | **Present** — workspace tree, textarea editor, terminal, wired to `/api/ide/*` |
| Audit score | 8 hold / 6 partial / 2 inflated / 3 false | **6 / 8 / 2 / 3** of 19 (sums) |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and in-memory “canonical bundle” JSON views
- Real workspace files under `MO_WORKSPACE` (default `./user_workspace`), path-jailed
- Real terminal: one PATH program + args, no shell operators, 15s default timeout
- Browser IDE chrome: file tree, editor, terminal (HTTP API, not Monaco/LSP)
- Docker Compose files that start gunicorn on port 5000 (`docker compose up --build`). No default `SECRET_KEY`. Production refuses placeholders. The CD compose probe currently fails
- GitHub Actions unit-test job (3.9–3.12) — green on `a41e1f8`. The security job is red

## What does not work as advertised

- Generated code is scaffolding with `TODO: Implement actual logic`
- Generated tests are not executed (`# For this implementation, we assume tests pass`)
- `src/analysis/pattern_detector.py`, `flow_analyzer.py`, `metrics_calculator.py`, and `dependency_analyzer.py` are placeholders (empty graphs, constant A-grade maintainability)
- Audit events are unsigned. Without `MO_DATA_DIR` they live in process memory
- `safety` uses `|| true` and cannot fail the build. Bandit **can** fail, and **does** (B104)
- Compose interpolates `SECRET_KEY` with no default; `.env.example` still contains a placeholder
- There is no PWA (no `manifest.json`, no service worker) and no WebXR
- No authentication on the API, including `/api/ide/terminal`
- Docker volume `./user_workspace` is not writable by the image user on CD (`PermissionError: welcome.txt`)

## Quick start

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python3 run.py
```

Open `http://127.0.0.1:5000`.

Docker:

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
docker compose up --build
```

Linux: `./install.sh` then `./start.sh`. macOS: `./install.sh` then `./start.command` (or `./start.sh`). Windows: `install.ps1` then `start.bat`.

## Documentation

| File | Role |
| --- | --- |
| [DOCS.md](DOCS.md) | Which files are canonical vs historical |
| [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) | Claim-by-claim evidence. Canonical for status. |
| [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) | Score that must sum (6/8/2/3 = 19) |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |
| [floors/README.md](floors/README.md) | Floor inventory and stub markings |
| [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) | What “runs everywhere” actually means |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. Each now starts with a historical banner. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
