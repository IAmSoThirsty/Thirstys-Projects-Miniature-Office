# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an in-memory audit log, language “floors,” and a template-based code-generation pipeline.

This README reports the **measured** state of `main` at [`fe9cdf1`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fe9cdf16a0cb0a660e5ba921a3a604a93ddd6f77) (28 August 2026). Earlier documents described a civilization-tier production IDE. Those claims were not true of the code. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Score that must sum: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md). Index: [DOCS.md](DOCS.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![Status](https://img.shields.io/badge/status-experimental-orange.svg)](CLAIMS_AUDIT.md)
[![CI](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml)

## What this actually is

| Piece | What exists |
| --- | --- |
| Web app | Flask + Flask-SocketIO, `python3 run.py`, port 5000 |
| Domain model | `src/core/entity.py` — entities, relationship types, in-memory registry with `threading.RLock` |
| Audit log | `src/core/audit.py` — in-memory SHA-256 **chain** (`prev_hash` + parent hashes). Not persisted, not signed |
| Code pipeline | `src/core/code_civilization.py` — **1,364-line** template generator (not 49,741 lines). Inserts `TODO` bodies and assumes generated tests pass |
| Floors | 28 directories under `floors/`. Every floor README is marked a toy. The SQL floor is Python. See [floors/README.md](floors/README.md) |
| Tests | **1,541 passing**, 1 skipped (local pytest on `fe9cdf1`; GitHub Actions CI green on the same commit) |
| CI / CD | **CI and CD are green** on `fe9cdf1` ([CI run 33179509134](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33179509134), [CD run 33179509146](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33179509146)). Security scans still use `\|\| true` |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured 28 August 2026)

| Metric | Claimed (old README) | Measured |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | **23,250** total / **17,991** non-comment |
| `code_civilization.py` | 49,741 lines | **1,364 lines** (50,430 bytes; original error treated bytes as lines) |
| Tests | 1,537 passing | **1,541 passed**, 1 skipped (`def test_` grep = 1,574) |
| Coverage | 99% of the system | Committed `coverage.json` is 98.7% of 6,438 tracked statements and omits `design_analyzer.py` + `integrated_specs/`. Fresh `pytest --cov=src` is 99% of 6,971 *imported* statements; `integrated_specs/` is still omitted |
| Language floors | 30+ native, working | 28 dirs; mixed; SQL floor is Python; each README is bannered as a toy |
| Flask routes | 45+ | 64 `@app.route` entries |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |
| Audit score | 8 hold / 6 partial / 2 inflated / 3 false | **6 / 7 / 2 / 3** of 18 (sums; VR is Partial) |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and many in-memory “canonical bundle” JSON views
- Docker Compose files that start gunicorn on port 5000 (`docker compose up --build`)
- Browser UI at `src/client/index.html`
- GitHub Actions unit-test job (3.9–3.12) and compose health check

## What does not work as advertised

- Generated code is scaffolding with `TODO: Implement actual logic`
- Generated tests are not executed (`# For this implementation, we assume tests pass`)
- `src/analysis/pattern_detector.py`, `flow_analyzer.py`, `metrics_calculator.py`, and `dependency_analyzer.py` are placeholders (empty graphs, constant A-grade maintainability)
- Audit events live in process memory. Each event hashes `prev_hash` and parent hashes; restart drops the chain
- CI security jobs use `|| true` and cannot fail the build
- Default compose `SECRET_KEY` is `change-this-secret-key`
- There is no PWA (no `manifest.json`, no service worker) and no WebXR

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
| [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) | Score that must sum (6/7/2/3 = 18) |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |
| [floors/README.md](floors/README.md) | Floor inventory and stub markings |
| [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) | What “runs everywhere” actually means |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. Each now starts with a historical banner. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
