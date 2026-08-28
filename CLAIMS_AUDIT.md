# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Honesty-pass code:** `fe9cdf1`  
**Docs pin (pre-IDE):** `27d7fdf` / `f560d651` — score 6/7/2/3 of 18  
**PR #15 merge:** `6196f8a` — IDE files landed, routes unhooked, CI red  
**IDE-wiring repair:** `aa7b439` — registered routes + unsigned JSONL persist  
**HEAD measured:** `a41e1f8` (`a41e1f866cad6452c678495e23f6cf9d97ec6231`) — IDE UI, production secret gate, bandit job that can fail. Canonical files on that commit still described `aa7b439`.  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main` (HEAD `a41e1f8`).
2. Count lines in `src/**/*.py` (`wc -l`) and `def test_` in `tests/`.
3. Compare README / LIMITATIONS numbers to those counts.
4. Open modules the README treats as complete (pipeline, analyzers, audit log, floors, IDE).
5. List Flask routes in `src/server/app.py` and `src/server/ide_routes.py`.
6. List GitHub Actions runs of `CI - Test and Lint` and `CD - Build and Deploy`.
7. Independent pytest of `a41e1f8`: **1,558 passed**, 1 skipped, 11.64s. Fresh `--cov=src`: **7,194 / 7,364** (97.69% of imported statements).
8. CI [33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573) **failure** — unit-test matrix green; security job failed on bandit B104 (`host="0.0.0.0"` in `app.py:1533`). CD [33201115545](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115545) **failure** — `test-docker` curls `/health` (503) while compose healthchecks `/api/ide/health`.

## Score

Of 19 headline claims: **6 hold**, **8 are partial**, **2 are inflated**, **3 are false**.

That is `6 + 8 + 2 + 3 = 19`. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

The three **false** and two **inflated** rows are *historical* claims. Canonical README / LIMITATIONS / this file no longer assert them. Remaining **partial** rows are still true of the code. The product is still not a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. Template codegen. Unauthenticated API. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Fresh `pytest --cov=src`: **7,194 / 7,364** (97.69% of imported statements). `integrated_specs/` still omitted. Not 99% of the tree. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,558 passed**, 1 skipped on `a41e1f8`. `def test_` grep = 1,591 (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **23,876** total / **18,542** non-comment (53 files). Bandit loc matches 18,542. |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,364 lines** (50,430 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Floor READMEs open with a toy/prototype banner. SQL floor is still Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | SHA-256 chain (`prev_hash` + parent hashes). Optional unsigned JSONL when `MO_DATA_DIR` is set. Not signed. Not a ledger. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI on port 5000 with editor/tree/terminal. `start.command` exists. No WebXR, no PWA. |
| 45+ API endpoints | **Holds** | 45+ | **71** Flask `@app.route` entries (64 in `app.py` + 7 IDE). |
| CI / CD healthy | **Partial** | Live green badges | Unit tests green 3.9–3.12. CI **failed** on `a41e1f8` ([33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573)) — bandit B104. CD **failed** ([33201115545](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115545)) — `/health` 503; workspace `PermissionError` on `welcome.txt`. `safety` remains `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/`. Bandit `-ll` fails (B104 bind-all-interfaces). Safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (separate processes). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Compose has **no** default `SECRET_KEY`. Production refuses placeholders. CD compose probe fails. Image user cannot write `./user_workspace`. |
| Docs consistent | **Holds** (this pin) | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 6/8/2/3 of 19 **of `a41e1f8`**. On `a41e1f8` itself they still described `aa7b439` — that was the defect this pin repairs. Historical `*_COMPLETE.md` files stay bannered. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |
| Real workspace / editor / terminal | **Holds** | PR #15: real IDE core | Jailed `Workspace`, no-shell `subprocess.run`, 7 `/api/ide/*` routes, `register_ide_routes(app)` called. `src/client/index.html` has a file tree, textarea editor, and terminal form. Not Monaco, not LSP, no auth. |

## What `a41e1f8` already implemented that the docs denied

`a41e1f8` (“Ship IDE UI, secret gate, and failing bandit scan”) changed four files and did not update the ledger. Independent re-measure found these live contradictions on that commit:

- README said “No UI editor chrome yet.” `src/client/index.html` has workspace tree + editor + terminal.
- README said compose `SECRET_KEY` defaults to `change-this-secret-key`. Compose interpolates `${SECRET_KEY}` with no default. `ide_routes._require_real_secret()` refuses placeholders when `FLASK_ENV=production`.
- README said “CI security jobs use `|| true`.” Bandit no longer has `|| true` and failed the job. `safety` still has `|| true`.
- Line counts pinned 23,858 / 18,527; tree is 23,876 / 18,542.
- Coverage pinned 7,192 / 7,356; fresh run is 7,194 / 7,364.
- CI pinned green on `aa7b439`; HEAD CI is red.

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

From `src/core/code_civilization.py`:

```python
result = data  # TODO: Implement actual logic
```

```python
# For this implementation, we assume tests pass
```

`_testing_mandate` emits `assert result is not None`. It does not run pytest on generated code.

## Analyzer stubs

`src/analysis/pattern_detector.py` — `detect_patterns` / `detect_antipatterns` return empty lists. File labels itself `placeholder`.

`src/analysis/metrics_calculator.py` — maintainability is hard-coded `index=100.0, grade="A"`.

`src/analysis/flow_analyzer.py` / `dependency_analyzer.py` — labelled placeholders; empty graphs.

## CI

Workflow files: `.github/workflows/ci.yml`, `.github/workflows/deploy.yml`.

- `CI - Test and Lint` on `a41e1f8`: **failure** ([run 33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573)). Test matrix (3.9–3.12) succeeded. Security job failed: bandit B104 at `src/server/app.py:1533` (`def run_server(host="0.0.0.0", ...)`). Bandit loc=18,542; 1 medium, 11 low.
- Last unit-test-green docs pin: `aa7b439` ([run 33198450337](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33198450337)).
- `safety check --json || true` cannot fail the job.
- CD `test-docker` curls `http://localhost:5000/health` (Flask/gunicorn 503). Compose healthcheck is `/api/ide/health`.

## Ecosystem

[thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That is false of current `main`. The portal is stale.

The product remains a Flask office metaphor with templates, an optional unsigned audit file, a jailed workspace API, a browser editor, and in-memory world state. It is not a civilization-tier IDE.
