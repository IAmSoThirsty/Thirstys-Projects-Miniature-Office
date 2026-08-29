# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an optional HMAC-tagged audit JSONL, language “floors,” a template-based code-generation pipeline, a jailed workspace / no-shell terminal API, a browser editor/file-tree/terminal UI, and a small PWA shell.

This README reports the **code pin** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — the last commit that changed `src/` or `tests/`. Independent pytest on that SHA: **1,573 passed**, 1 skipped, 13.14s. Fresh `--cov=src`: **7,494 / 7,749** (96.71%). Re-run 29 August 2026 on then-HEAD `32a70dc` (src/tests identical to the pin): **1,573 passed**, 1 skipped, 13.18s, same coverage XML. Later docs-only commits do not retarget the pin. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Score that must sum: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md). Index: [DOCS.md](DOCS.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.10+-blue.svg)](https://www.python.org/downloads/)
[![Status](https://img.shields.io/badge/status-experimental-orange.svg)](CLAIMS_AUDIT.md)
[![CI](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/workflows/ci.yml)

## What this actually is

| Piece | What exists |
| --- | --- |
| Web app | Flask + Flask-SocketIO, `python3 run.py`, port 5000 |
| Domain model | `src/core/entity.py` — 7 entity types, 8 relations, in-memory registry with `threading.RLock` |
| Audit log | `src/core/audit.py` — SHA-256 chain (`prev_hash` + parent hashes). Optional HMAC-SHA256 when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set. Not a ledger |
| IDE core | Jailed workspace FS, no-shell argv terminal, 7 `/api/ide/*` routes. Token gate when `MO_IDE_TOKEN` is set (required in production). Browser UI in `src/client/index.html` |
| PWA | `manifest.json` + `sw.js`. No WebXR |
| Code pipeline | `src/core/code_civilization.py` — **1,421-line** template generator (not 49,741 lines). Python identity bodies; generated pytest is executed |
| Floors | 28 directories under `floors/`. SQL floor includes `schema.sql`. Every floor README is marked a toy. See [floors/README.md](floors/README.md) |
| Tests | **1,573 passing**, 1 skipped |
| CI / CD | **Both green** on the code pin `fdd9762`: [CI 33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987), [CD 33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992). Also green on later docs-only commits `1a103bf` ([CI 33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008), [CD 33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012)) and `32a70dc` ([CI 33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458), [CD 33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461)). Matrix is Python 3.10–3.12; bandit JSON dump uses `-ll` |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured 28 August 2026)

| Metric | Claimed (old README) | Measured (`fdd9762`) |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | **24,441** total / **19,058** non-comment (53 files) |
| `code_civilization.py` | 49,741 lines | **1,421 lines** (52,653 bytes; original error treated bytes as lines) |
| Tests | 1,537 passing | **1,573 passed**, 1 skipped (anchored `def test_` in `tests/` = 1,606; unanchored = 1,608) |
| Coverage | 99% of the system | Fresh `pytest --cov=src`: **7,494 / 7,749** imported statements (**96.71%**). `integrated_specs/` still omitted |
| Language floors | 30+ native, working | 28 dirs; SQL has `schema.sql`; each README is bannered as a toy |
| Flask routes | 45+ | **74** `@app.route` entries (67 in `app.py` + 7 IDE) |
| Python | 3.9+ | **3.10+** (`pytest==9.0.3` requires ≥3.10) |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |
| Editor UI | (older docs denied it) | **Present** |
| PWA | Claimed | **Present** (`manifest.json`, `sw.js`); no WebXR |
| Audit score | 8 hold / 6 partial / 2 inflated / 3 false | **9 / 6 / 1 / 3** of 19 (sums). CI/CD Holds: both jobs green on code pin `fdd9762` and on later docs-only commits |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and in-memory “canonical bundle” JSON views
- Real workspace files under `MO_WORKSPACE` (default `./user_workspace`), path-jailed
- Real terminal: one PATH program + args, no shell operators, 15s default timeout
- Browser IDE chrome: file tree, editor, terminal (HTTP API, not Monaco/LSP)
- AST-backed pattern / flow / metrics / dependency analyzers (small named set)
- Docker Compose files that start gunicorn on port 5000 (`docker compose up --build`). No default `SECRET_KEY`. Production refuses placeholders. CD `test-docker` green on `fdd9762`.
- GitHub Actions unit-test + security jobs (bandit `-ll`, `pip-audit`); Python 3.10–3.12 — **green** on `fdd9762` and on later docs-only commits

## What does not work as advertised

- Generated Python is an identity transform (`result = data`), not spec-faithful logic. Non-Python tests are not executed
- Analyzers cover 5 patterns and 4 anti-patterns, not “23+ SOLID / 17 smells”
- Audit HMAC is optional. Without a real key the chain is unsigned
- Bandit still reports 13 **low** findings
- There is no WebXR
- `/api/ide/*` is open unless `MO_IDE_TOKEN` is set
- Docker is a compose healthcheck, not a hardened stack (in-memory world, `chmod 777` in CD)

## Quick start

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python3 run.py
```

Open `http://127.0.0.1:5000`. Python 3.10 or newer is required.

Docker:

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
chmod 777 user_workspace data logs
docker compose up --build
```

Linux: `./install.sh` then `./start.sh`. macOS: `./install.sh` then `./start.command` (or `./start.sh`). Windows: `install.ps1` then `start.bat`.

## Documentation

| File | Role |
| --- | --- |
| [DOCS.md](DOCS.md) | Which files are canonical vs historical |
| [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) | Claim-by-claim evidence. Canonical for status. |
| [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) | Score that must sum (9/6/1/3 = 19) |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |
| [floors/README.md](floors/README.md) | Floor inventory and stub markings |
| [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) | What “runs everywhere” actually means |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. Each now starts with a historical banner. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
