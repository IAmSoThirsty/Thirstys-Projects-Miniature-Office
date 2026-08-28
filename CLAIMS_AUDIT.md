# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)

**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`

**Honesty-pass code:** `fe9cdf1`

**Docs pin (pre-repair):** `8f7ee8be` of `a41e1f8` — score 6/8/2/3 of 19

**This pin:** [`ffd9b5e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/ffd9b5e7310194c713473941a06eaf797cfdfd38) — honesty-repair of remaining *code* defects that made canonical claims fail (health 503, bandit B104, safety `|| true`, unsigned JSONL, placeholder analyzers, assumed tests, SQL-is-Python, no PWA).

**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main` at `8f7ee8be`.
2. Implement the remaining LIMITATIONS.md roadmap items that could be made true without pretending the product is a civilization-tier IDE.
3. Count lines in `src/**/*.py` (`wc` equivalent) and `def test_` in `tests/`.
4. Independent pytest: **1,567 passed**, 1 skipped, 4.37s. Fresh `--cov=src`: **7,493 / 7,749** (96.70% of imported statements).
5. `bandit -r src -ll`: 0 medium/high (B104 nosec on `run_server`). `pip-audit -r requirements.txt`: clean.
6. Black + isort check on `src/`: pass.

## Score

Of 19 headline claims: **8 hold**, **7 are partial**, **1 is inflated**, **3 are false**.

That is `8 + 7 + 1 + 3 = 19`. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

The three **false** rows and the **inflated** coverage row are *historical* claims. Canonical README / LIMITATIONS / this file no longer assert them. Remaining **partial** rows are still true of the code. The product is still not a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. Template codegen. Auth only if `MO_IDE_TOKEN` is set. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Fresh `pytest --cov=src`: **7,493 / 7,749** (96.70% of imported statements). `integrated_specs/` still omitted. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,567 passed**, 1 skipped. `def test_` grep = 1,600 (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **24,441** total / **19,058** non-comment (53 files). |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,421 lines** (52,653 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. SQL floor now includes `schema.sql`. Department runtime is Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Python identity bodies; generated pytest **is executed**. Other languages still assumed. |
| 23+ patterns, SOLID, 17+ smells | **Partial** | Full design analysis | AST walkers detect 5 named patterns and 4 anti-patterns. Not a 23+/SOLID product. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | SHA-256 chain. Optional HMAC-SHA256 when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set. Not a ledger. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI + `manifest.json` / `sw.js`. No WebXR. |
| 45+ API endpoints | **Holds** | 45+ | **74** Flask `@app.route` entries (67 in `app.py` + 7 IDE). |
| CI / CD healthy | **Partial** | Live green badges | Workflows rewritten (bandit nosec, pip-audit, CD curls `/health` and `/api/ide/health`, workspace chmod). No Actions run on this SHA yet. |
| 0 lint / 0 vulns | **Holds** | Clean | Critical flake8, Black, isort pass. Bandit `-ll` clean. `pip-audit` clean. 13 bandit **low** findings remain. |
| Registry thread-safe via GIL | **Holds** (restated) | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Compose has **no** default `SECRET_KEY`. Production refuses placeholders. CD script now matches `/health`. Actions on this SHA not yet observed. |
| Docs consistent | **Holds** (this pin) | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 8/7/1/3 of 19. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |
| Real workspace / editor / terminal | **Holds** | PR #15: real IDE core | Jailed `Workspace`, no-shell `subprocess.run`, 7 `/api/ide/*` routes, `register_ide_routes(app)` called. Token gate when `MO_IDE_TOKEN` is set. Browser UI has a file tree, textarea editor, and terminal form. Not Monaco, not LSP. |

## What this pin implemented so the remaining claims could be true

- `/health` is liveness (HTTP 200). Simulation starts lazily. Gunicorn no longer 503s because `FLASK_ENV` defaults to development.
- `run_server(host="0.0.0.0")` has `# nosec B104` with a reason. Bandit `-ll` is clean.
- `safety check || true` was replaced with `pip-audit`. Requirements bumped (Flask 3.1.3, python-socketio 5.16.2, python-dotenv 1.2.2, pytest 9.0.3). Audit is clean.
- CD `test-docker` curls `/health` **and** `/api/ide/health`, creates workspace dirs, `chmod 777`, sets `SECRET_KEY`.
- Audit events HMAC-sign the content hash when a real key is present.
- Pattern / flow / metrics / dependency analyzers walk the AST instead of returning empty/constant values.
- Python codegen no longer inserts `TODO: Implement actual logic` and **runs** the generated pytest.
- SQL floor ships `schema.sql`.
- PWA: `manifest.json` + `sw.js`.
- `MO_IDE_TOKEN` gates `/api/ide/*` except health; required in production.

## Internal contradictions (original smoking gun)

`LIMITATIONS.md` (pre-audit) contained all of:

- “Production Ready — 99% (1288 tests passing)”
- “Test coverage stat in README (32%) is accurate”
- “Phase 1 complete” with a real pipeline
- “underlying code generation functionality is not operational”

`PRODUCTION_READY.md` certifies production from **22 tests and 32% coverage**.

`README.md` certified production from **1,537 tests and 99% coverage**.

Those contradictions are why the canonical files were rewritten. Historical files are bannered, not deleted.

## Pipeline evidence

Python generation writes `result = data` (identity) and `_run_python_tests` executes pytest in a temp directory. Non-Python languages still skip execution.

## CI

Workflow files: `.github/workflows/ci.yml`, `.github/workflows/deploy.yml`.

- Security job: `pip-audit` (can fail) then `bandit -r src -ll` (can fail).
- CD `test-docker`: liveness + readiness, writable workspace dirs.

## Ecosystem

[thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That is false of current `main`. The portal is stale.

The product remains a Flask office metaphor with templates, an optional HMAC-tagged audit file, a jailed workspace API, a browser editor, a PWA shell, and in-memory world state. It is not a civilization-tier IDE.
