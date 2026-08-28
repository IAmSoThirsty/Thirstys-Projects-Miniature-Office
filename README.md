# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an optional unsigned audit JSONL, language “floors,” a template-based code-generation pipeline, and a jailed workspace / no-shell terminal API.

This README reports the **measured** state of `aa7b439`, the working tree that repairs [`6196f8a`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/6196f8afb941351c8fb435e5ea68199add48b0ae) (PR #15). That merge added IDE files but did not register the routes or persist the audit log. Independent pytest on `6196f8a`: **5 failed**, 1,553 passed. GitHub Actions CI [run 33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388) **failed** on that commit. This tree wires the routes and optional JSONL persist. Independent pytest here: **1,558 passed**, 1 skipped. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Score that must sum: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md). Index: [DOCS.md](DOCS.md).

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
| IDE core | Jailed workspace FS, no-shell argv terminal, 7 `/api/ide/*` routes. Real disk and `subprocess.run`. No UI editor chrome yet |
| Code pipeline | `src/core/code_civilization.py` — **1,364-line** template generator (not 49,741 lines). Inserts `TODO` bodies and assumes generated tests pass |
| Floors | 28 directories under `floors/`. Every floor README is marked a toy. The SQL floor is Python. See [floors/README.md](floors/README.md) |
| Tests | **1,558 passing**, 1 skipped (independent pytest of this tree) |
| CI / CD | **Red** on `6196f8a`. Repair **CI green** on `aa7b439` ([run 33198450337](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33198450337)). CD not claimed until a run succeeds. Security still `\|\| true` |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured 28 August 2026)

| Metric | Claimed (old README) | Measured (this tree) |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | **23,858** total / **18,527** non-comment (53 files) |
| `code_civilization.py` | 49,741 lines | **1,364 lines** (50,430 bytes; original error treated bytes as lines) |
| Tests | 1,537 passing | **1,558 passed**, 1 skipped (`def test_` grep = 1,591) |
| Coverage | 99% of the system | Fresh `pytest --cov=src`: **7,192 / 7,356** imported statements (**97.8%**). `integrated_specs/` still omitted. Not 99% of the tree |
| Language floors | 30+ native, working | 28 dirs; mixed; SQL floor is Python; each README is bannered as a toy |
| Flask routes | 45+ | **71** `@app.route` entries (64 in `app.py` + 7 IDE) |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |
| Audit score | 8 hold / 6 partial / 2 inflated / 3 false | **6 / 8 / 2 / 3** of 19 (sums). CI moved to Partial. IDE core added as Holds |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and in-memory “canonical bundle” JSON views
- Real workspace files under `MO_WORKSPACE` (default `./user_workspace`), path-jailed
- Real terminal: one PATH program + args, no shell operators, 15s default timeout
- Docker Compose files that start gunicorn on port 5000 (`docker compose up --build`)
- Browser UI at `src/client/index.html` (office metaphor; the IDE API is HTTP, not a full editor UI)
- GitHub Actions unit-test job (3.9–3.12) — last green on `f560d651`

## What does not work as advertised

- Generated code is scaffolding with `TODO: Implement actual logic`
- Generated tests are not executed (`# For this implementation, we assume tests pass`)
- `src/analysis/pattern_detector.py`, `flow_analyzer.py`, `metrics_calculator.py`, and `dependency_analyzer.py` are placeholders (empty graphs, constant A-grade maintainability)
- Audit events are unsigned. Without `MO_DATA_DIR` they live in process memory
- CI security jobs use `|| true` and cannot fail the build
- Default compose `SECRET_KEY` is `change-this-secret-key`
- There is no PWA (no `manifest.json`, no service worker) and no WebXR
- No authentication on the API, including `/api/ide/terminal`

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
