# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an in-memory audit log, language “floors,” and a template-based code-generation pipeline.

This README reports the **measured** state of `main`. Earlier documents described a civilization-tier production IDE. Those claims were not true of the code. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![Status](https://img.shields.io/badge/status-experimental-orange.svg)](CLAIMS_AUDIT.md)

## What this actually is

| Piece | What exists |
| --- | --- |
| Web app | Flask + Flask-SocketIO, `python3 run.py`, port 5000 |
| Domain model | `src/core/entity.py` — entities, relationship types, in-memory registry with `threading.RLock` |
| Audit log | `src/core/audit.py` — SHA-256 of each event’s own fields, in memory, not a hash chain, not persisted |
| Code pipeline | `src/core/code_civilization.py` — 1,233-line template generator (not 49,741 lines). Inserts `TODO` bodies and assumes generated tests pass |
| Floors | 28 directories under `floors/`. Elixir now has Mix modules under `lib/`. The SQL floor is still Python |
| Tests | **1,537 passing**, 1 skipped (pytest, Python 3.10, 28 Aug 2026) |
| CI | **CI - Test and Lint is green** on `5e182f2`. CD still used `docker-compose` (v1) which GitHub runners no longer ship |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured)

| Metric | Claimed (old README) | Measured |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | 21,566 total / 16,313 non-comment (pre-format count at `537c469`) |
| `code_civilization.py` | 49,741 lines | **1,233 lines** (byte count was misread as line count) |
| Tests | 1,537 passing | **1,537 passed**, 1 skipped |
| Coverage | 99% of the system | 98.7% of 6,438 tracked statements; `design_analyzer.py` and `integrated_specs/` omitted |
| Language floors | 30+ native, working | 28 dirs; mixed; SQL floor is Python |
| Flask routes | 45+ | 64 `@app.route` entries |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and many in-memory “canonical bundle” JSON views
- Docker and docker-compose files that start gunicorn on port 5000
- Browser UI at `src/client/index.html`
- `start.sh`, `start.bat`, and `start.command`

## What does not work as advertised

- Generated code is scaffolding with `TODO: Implement actual logic`
- Generated tests are not executed (`# For this implementation, we assume tests pass`)
- `src/analysis/pattern_detector.py`, `flow_analyzer.py`, `metrics_calculator.py`, and `dependency_analyzer.py` are placeholders (empty graphs, constant A-grade maintainability)
- Audit events live in process memory
- CI security jobs still use `|| true` and cannot fail the build
- There is no WebXR / VR client

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

Linux: `./install.sh` then `./start.sh`. macOS: `./install.sh` then `./start.command` (or `./start.sh`). Windows: `install.ps1` then `start.bat`.

## Documentation

| File | Role |
| --- | --- |
| [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) | Claim-by-claim evidence. Canonical for status. |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. Each now starts with a historical banner. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
