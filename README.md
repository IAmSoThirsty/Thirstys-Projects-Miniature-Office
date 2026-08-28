# Miniature Office

**Status: experimental prototype — not production-ready.**

A Flask simulation of a spatial office metaphor for software work: typed entities, an optional HMAC-tagged audit JSONL, language “floors,” a template-based code-generation pipeline, a jailed workspace / no-shell terminal API, a browser editor/file-tree/terminal UI, and a small PWA shell.

This README reports the **measured** state of the honesty-repair pin on parent [`8f7ee8be`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/8f7ee8be10ef4a64599415db84b07cefe535ca88). Independent pytest: **1,567 passed**, 1 skipped. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Score that must sum: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md). Index: [DOCS.md](DOCS.md).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
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
| Tests | **1,567 passing**, 1 skipped |
| CI / CD | Workflows rewritten: bandit `-ll` with documented B104 nosec, `pip-audit` (no `|| true`), CD curls `/health` and `/api/ide/health`. Actions on this SHA have not yet run |

It is **not** a production IDE, not VR-native, not a cryptographic ledger, and not a polyglot runtime that authors real code in 30 languages.

## Honest metrics (measured 28 August 2026)

| Metric | Claimed (old README) | Measured (this pin) |
| --- | --- | --- |
| Production status | Production ready | Experimental prototype |
| `src/` Python lines | 18,285 | **24,441** total / **19,058** non-comment (53 files) |
| `code_civilization.py` | 49,741 lines | **1,421 lines** (52,653 bytes; original error treated bytes as lines) |
| Tests | 1,537 passing | **1,567 passed**, 1 skipped (`def test_` grep = 1,600) |
| Coverage | 99% of the system | Fresh `pytest --cov=src`: **7,493 / 7,749** imported statements (**96.70%**). `integrated_specs/` still omitted |
| Language floors | 30+ native, working | 28 dirs; SQL has `schema.sql`; each README is bannered as a toy |
| Flask routes | 45+ | **74** `@app.route` entries (67 in `app.py` + 7 IDE) |
| macOS `start.command` | Documented | **Present** — launches `start.sh` |
| Editor UI | (older docs denied it) | **Present** |
| PWA | Claimed | **Present** (`manifest.json`, `sw.js`); no WebXR |
| Audit score | 8 hold / 6 partial / 2 inflated / 3 false | **8 / 7 / 1 / 3** of 19 (sums) |

## What still works as a prototype

- Entity / department / agent / supply-store objects in Python
- Task state machine in `src/core/mission.py`
- Flask routes for world state, agents, departments, audit events, and in-memory “canonical bundle” JSON views
- Real workspace files under `MO_WORKSPACE` (default `./user_workspace`), path-jailed
- Real terminal: one PATH program + args, no shell operators, 15s default timeout
- Browser IDE chrome: file tree, editor, terminal (HTTP API, not Monaco/LSP)
- AST-backed pattern / flow / metrics / dependency analyzers (small named set)
- Docker Compose files that start gunicorn on port 5000 (`docker compose up --build`). No default `SECRET_KEY`. Production refuses placeholders
- GitHub Actions unit-test + security jobs (bandit `-ll`, `pip-audit`)

## What does not work as advertised

- Generated Python is an identity transform (`result = data`), not spec-faithful logic. Non-Python tests are not executed
- Analyzers cover 5 patterns and 4 anti-patterns, not “23+ SOLID / 17 smells”
- Audit HMAC is optional. Without a real key the chain is unsigned
- Bandit still reports 13 **low** findings
- There is no WebXR
- `/api/ide/*` is open unless `MO_IDE_TOKEN` is set
- GitHub Actions on this SHA have not been observed yet

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
| [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) | Score that must sum (8/7/1/3 = 19) |
| [LIMITATIONS.md](LIMITATIONS.md) | Current limitations, without contradictory percentages |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Design notes — treat as intent, not a completion certificate |
| [PRODUCTION_READY.md](PRODUCTION_READY.md) | Historical. Superseded. |
| [floors/README.md](floors/README.md) | Floor inventory and stub markings |
| [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) | What “runs everywhere” actually means |

Root-level `IMPLEMENTATION_COMPLETE*.md` and `MAXIMUM_ALLOWED_*.md` files are agent-generated delivery notes. Each now starts with a historical banner. They are not evidence that the described system exists.

## License

Apache License 2.0 — see [LICENSE](LICENSE).
