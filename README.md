# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an in-memory audit log, language “floors,” and a template-based code-generation pipeline.

This README reports the **measured** state of `main` at commit [`537c469`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/537c469a8ce34d952525ac25886ed8a85a629f82), audited 28 August 2026. Earlier documents described a civilization-tier production IDE. Those claims were not true of the code. The evidence is in [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![Status](https://img.shields.io/badge/status-experimental-orange.svg)](CLAIMS_AUDIT.md)

## What this actually is

| Piece | What exists |
| --- | --- |
| Web app | Flask + Flask-SocketIO, `python3 run.py`, port 5000 |
| Domain model | `src/core/entity.py` — entities, relationship types, in-memory registry |
| Audit log | `src/core/audit.py` — SHA-256 of each event’s own fields, in memory, not a hash chain, not persisted |
| Code pipeline | `src/core/code_civilization.py` — 1,233-line template generator (not 49,741 lines). Inserts `TODO` bodies and assumes tests pass |
| Floors | 28 directories under `floors/`. Completeness varies. Elixir has no `.ex` source. The SQL floor is Python |
| Tests | 28 files, 1,570 `test_*` functions. Coverage.json reports 98.7% of **tracked** statements and omits 5,346 lines of Python |
| CI | Workflows exist. The last 15 runs of `CI - Test and Lint` all failed, including HEAD |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured)

| Metric | Claimed (old README) | Measured |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | 21,566 total / 16,313 non-comment |
| `code_civilization.py` | 49,741 lines | **1,233 lines** (48,308 bytes) |
| Tests | 1,537 passing | 1,570 functions present; CI red |
| Coverage | 99% of the system | 98.7% of 6,438 tracked statements; `design_analyzer.py` and `integrated_specs/` omitted |
| Language floors | 30+ native, working | 28 dirs; mixed; some stubs |
| Flask routes | 45+ | 64 `@app.route` entries |
| macOS `start.command` | Documented | **File does not exist** |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and many in-memory “canonical bundle” JSON views
- Docker and docker-compose files that start gunicorn on port 5000
- Browser UI at `src/client/index.html`

## What does not work as advertised

- Generated code is scaffolding with `TODO: Implement actual logic`
- Generated tests are not executed (`# For this implementation, we assume tests pass`)
- `src/analysis/pattern_detector.py`, `flow_analyzer.py`, `metrics_calculator.py`, and `dependency_analyzer.py` are placeholders (empty graphs, constant A-grade maintainability)
- Audit events live in process memory
- `datetime.utcnow` remains in nine source files
- CI security jobs use `|| true` and cannot fail the build

## Quick start

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python3 run.py
```

Open `http://localhost:5000`.

Docker:

```bash
docker-compose up --build
```

Linux/macOS helper: `./install.sh` then `./start.sh`. Windows: `install.ps1` then `start.bat`. There is no `start.command`.

## Documentation

| File | Role |
| --- | --- |
| [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) | Claim-by-claim evidence. Canonical for status. |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
