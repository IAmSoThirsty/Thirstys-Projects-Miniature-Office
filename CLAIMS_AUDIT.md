# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)

**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`

**Honesty-pass code:** `fe9cdf1`

**Docs pin (pre-repair):** `8f7ee8be` of `a41e1f8` — score 6/8/2/3 of 19

**Honesty-repair merge:** [`f24ae5c`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f24ae5c3d419f9bb14388591b05b8fb40ab70cc0) (PR #18). CI on that SHA was red.

**Prior pin:** [`590b90c`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/590b90ce53bd859d8baf32a2155820e3169a93e8) — CI [33211771675](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33211771675) and CD [33211771674](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33211771674) green. Coverage was recorded as 7,493 / 7,749.

**Code pin:** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — last commit that changed `src/` or `tests/`. Independently measured. Coverage XML is **7,494 / 7,749** (96.71%).

**Docs after the pin:** later commits (`1a103bf`, `c783357`, `32a70dc`, and successors) are docs-only. `git log fdd9762..HEAD -- src tests` is empty. Do not name a docs SHA as “on `main` at this writing” — that sentence is false the moment the next docs merge lands, and retargeting the pin to it recreates an infinite SHA loop.

**Independent remeasure (29 August 2026):** clone of then-HEAD `32a70dc` (src/tests identical to the code pin). Pytest **1,573 passed**, 1 skipped, 13.18s. Coverage XML **7,494 / 7,749** (96.71%). Line/route/floor counts match the pin. Bandit `-ll` clean. `pip-audit` clean. CI [33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458) and CD [33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461) **succeeded** on that docs HEAD.

**Convention:** pin the code tree, not HEAD. Retargeting every docs merge recreates an infinite SHA loop and mistakes coverage XML variance (one statement) for a tree change.

**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main`. Identify the last commit that changed `src/` or `tests/` (`fdd9762`). Confirm later docs commits have an empty `git log fdd9762..HEAD -- src tests`.
2. Count lines in `src/**/*.py`. Count `def test_` in `tests/` two ways: anchored (`^\s*def test_`) and unanchored.
3. Independent pytest on `fdd9762`: **1,573 passed**, 1 skipped, 13.14s. Fresh `--cov=src`: **7,494 / 7,749** (96.71% of imported statements). Re-run 29 August 2026 on then-HEAD `32a70dc` (identical `src/`/`tests/`): **1,573 passed**, 1 skipped, 13.18s, same coverage XML. A later machine may report 7,493; that is XML variance, not a tree change.
4. `bandit -r src -ll`: 0 medium/high (B104 nosec on `run_server`). `pip-audit -r requirements.txt`: clean.
5. Observe GitHub Actions on the **code pin** `fdd9762`:
   - CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) **succeeded**.
   - CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) **succeeded**.
6. Same jobs on later docs-only commits also **succeeded**: `1a103bf` ([CI 33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008), [CD 33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012)) and `32a70dc` ([CI 33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458), [CD 33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461)).
7. Anchored `def test_` grep in `tests/` = **1,606**. Unanchored in `tests/` = **1,608**. Do not report 1,612.
8. Do not record “docs commit on `main` at this writing.” That sentence is false as soon as the next docs merge lands.

## Score

Of 19 headline claims: **9 hold**, **6 are partial**, **1 is inflated**, **3 are false**.

That is `9 + 6 + 1 + 3 = 19`. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

The three **false** rows and the **inflated** coverage row are *historical* claims. Canonical README / LIMITATIONS / this file no longer assert them. Remaining **partial** rows are still true of the code. The product is still not a production IDE.

CI/CD remains **Holds** because both jobs were observed green on the code pin `fdd9762` and on later docs-only commits. Docker stays Partial: a green compose healthcheck is not a hardened stack.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. Template codegen. Auth only if `MO_IDE_TOKEN` is set. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Fresh `pytest --cov=src`: **7,494 / 7,749** (96.71% of imported statements). `integrated_specs/` still omitted. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,573 passed**, 1 skipped. Anchored `def test_` = 1,606; unanchored = 1,608 (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **24,441** total / **19,058** non-comment (53 files). |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,421 lines** (52,653 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. SQL floor includes `schema.sql`. Department runtime is Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Python identity bodies; generated pytest **is executed**. Other languages still assumed. |
| 23+ patterns, SOLID, 17+ smells | **Partial** | Full design analysis | AST walkers detect 5 named patterns and 4 anti-patterns. Not a 23+/SOLID product. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | SHA-256 chain. Optional HMAC-SHA256 when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set. Not a ledger. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI + `manifest.json` / `sw.js`. No WebXR. |
| 45+ API endpoints | **Holds** | 45+ | **74** Flask `@app.route` entries (67 in `app.py` + 7 IDE). |
| CI / CD healthy | **Holds** | Live green badges | Green on code pin `fdd9762` ([CI 33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987), [CD 33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992)) and on later docs-only commits `1a103bf` ([CI 33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008), [CD 33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012)) and `32a70dc` ([CI 33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458), [CD 33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461)). |
| 0 lint / 0 vulns | **Holds** | Clean | Critical flake8, Black, isort pass. Bandit `-ll` clean. `pip-audit` clean. 13 bandit **low** findings remain. |
| Registry thread-safe via GIL | **Holds** (restated) | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Compose has **no** default `SECRET_KEY`. Production refuses placeholders. CD `test-docker` **succeeded** on `fdd9762` and on later docs-only commits. Still not hardened (in-memory world, `chmod 777` in the workflow). |
| Docs consistent | **Holds** (this pin) | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 9/6/1/3 of 19 and on code pin `fdd9762`. They do not name a docs-only SHA as HEAD. EASY_ACCESS no longer denies the PWA shell. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |
| Real workspace / editor / terminal | **Holds** | PR #15: real IDE core | Jailed `Workspace`, no-shell `subprocess.run`, 7 `/api/ide/*` routes, `register_ide_routes(app)` called. Token gate when `MO_IDE_TOKEN` is set. Browser UI has a file tree, textarea editor, and terminal form. Not Monaco, not LSP. |

## What landed so the remaining claims could be true

- `/health` is liveness (HTTP 200). Simulation starts lazily.
- `run_server(host="0.0.0.0")` has `# nosec B104` with a reason. Bandit `-ll` is clean.
- `safety check || true` was replaced with `pip-audit`. Requirements bumped. Audit is clean.
- CD `test-docker` curls `/health` **and** `/api/ide/health`, creates workspace dirs, `chmod 777`, sets `SECRET_KEY`. Observed green: [33209448004](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33209448004).
- Audit events HMAC-sign the content hash when a real key is present.
- Pattern / flow / metrics / dependency analyzers walk the AST instead of returning empty/constant values.
- Python codegen no longer inserts `TODO: Implement actual logic` and **runs** the generated pytest.
- SQL floor ships `schema.sql`.
- PWA: `manifest.json` + `sw.js`.
- `MO_IDE_TOKEN` gates `/api/ide/*` except health; required in production.
- CI matrix is Python **3.10–3.12**. Bandit JSON dump uses `-ll`. Observed green: [33209447993](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33209447993).

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

- Security job: `pip-audit` (can fail) then `bandit -r src -ll` (can fail). JSON dump also uses `-ll`.
- Test matrix: 3.10, 3.11, 3.12. Python 3.9 dropped because pytest 9 does not install.
- CD `test-docker`: liveness + readiness, writable workspace dirs — **green** on `fdd9762`.
- CI on `fdd9762` was **green** (33212776987). Later docs-only commits were also green: `1a103bf` (33215760008 / 33215760012), `32a70dc` (33250434458 / 33250434461). Parent `590b90c` was green (33211771675 / 33211771674). `f24ae5c` was red (bandit JSON dump without `-ll`; pytest 9 vs Python 3.9).

## Ecosystem

[thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That is false of current `main`. The portal is stale.

The product remains a Flask office metaphor with templates, an optional HMAC-tagged audit file, a jailed workspace API, a browser editor, a PWA shell, and in-memory world state. It is not a civilization-tier IDE.
