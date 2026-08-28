# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Honesty-pass code:** `fe9cdf1`  
**Docs pin (pre-IDE):** `27d7fdf` / `f560d651` — score 6/7/2/3 of 18  
**PR #15 merge:** `6196f8a` — IDE files landed, routes unhooked, CI red  
**This repair commit:** `aa7b439` — independent clone of `6196f8a`, then wire of the unhooked IDE core.  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main` (HEAD `6196f8a`).
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS numbers to those counts.
4. Open modules the README treats as complete (pipeline, analyzers, audit log, floors, IDE).
5. List Flask routes in `src/server/app.py` and `src/server/ide_routes.py`.
6. List GitHub Actions runs of `CI - Test and Lint` and `CD - Build and Deploy`.
7. Independent pytest on `6196f8a`: **5 failed**, 1,553 passed, 1 skipped. Failures: `test_persist_and_reload` (`AuditLog` had no `persist_path`), four `/api/ide/*` tests (404 — `register_ide_routes` never called). CI [33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388) **failure**.
8. Repair: persist + new `EventType`s in `audit.py`; `register_ide_routes(app)` + `configure_ide_defaults()` in `app.py`. Independent pytest of this tree: **1,558 passed**, 1 skipped. Fresh `--cov=src`: **7,192 / 7,356**.

## Score

Of 19 headline claims: **6 hold**, **8 are partial**, **2 are inflated**, **3 are false**.

That is `6 + 8 + 2 + 3 = 19`. The previous 18-row score (`6 / 7 / 2 / 3`) described `27d7fdf`. PR #15 added a 19th headline (real workspace / editor / terminal). CI/CD moved from Holds to Partial because `6196f8a` is red. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

The three **false** and two **inflated** rows are *historical* claims. Canonical README / LIMITATIONS / this file no longer assert them. Remaining **partial** rows are still true of the code. The product is still not a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. Template codegen. Unauthenticated API. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Fresh `pytest --cov=src`: **7,192 / 7,356** (97.8% of imported statements). `integrated_specs/` still omitted. Not 99% of the tree. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,558 passed**, 1 skipped on this tree. `def test_` grep = 1,591 (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **23,858** total / **18,527** non-comment (53 files). |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,364 lines** (50,430 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Floor READMEs open with a toy/prototype banner. SQL floor is still Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | SHA-256 chain (`prev_hash` + parent hashes). Optional unsigned JSONL when `MO_DATA_DIR` is set. Not signed. Not a ledger. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI on port 5000. `start.command` exists. No WebXR, no PWA. |
| 45+ API endpoints | **Holds** | 45+ | **71** Flask `@app.route` entries (64 in `app.py` + 7 IDE). |
| CI / CD healthy | **Partial** | Live green badges | **Failed** on `6196f8a` ([CI 33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388)). Repair **CI succeeded** on `aa7b439` ([33198450337](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33198450337)). CD not claimed until a run succeeds. Security steps remain `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/` on this tree. Bandit/safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (separate processes). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Default `SECRET_KEY` is `change-this-secret-key`. `MO_WORKSPACE` / `MO_DATA_DIR` volumes are mounted. |
| Docs consistent | **Holds** | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 6/8/2/3 of 19. Historical `*_COMPLETE.md` files stay bannered. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |
| Real workspace / editor / terminal | **Holds** (this repair) | PR #15: real IDE core | On `6196f8a` the files existed and the tests 404'd. This tree calls `register_ide_routes(app)`. Jailed `Workspace`, no-shell `subprocess.run`, 7 `/api/ide/*` routes. Independent pytest 1,558 passed. There is still no full editor UI in `src/client/index.html`. |

## What was not true on `6196f8a`

PR #15 (“IDE core: real workspace, editor API, terminal”) merged files that the running app did not use:

- `src/server/app.py` never imported `register_ide_routes`.
- `src/core/audit.py` had no `persist_path`, no `WORKSPACE_WRITE` / `TERMINAL_RUN` event types, no `reset_audit_log`.
- `src/server/ide_routes.py` therefore could not load; `/api/ide/*` returned 404.
- Coverage of `ide_routes.py` on that commit: **0%**.

That is why the canonical files still pinned `27d7fdf` numbers — and why those numbers were already stale, and why “CI healthy” was already false of HEAD.

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

- `CI - Test and Lint` on `6196f8a`: **failure** ([run 33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388)).
- Last green CI on the docs-pin commit `f560d651`: [run 33194264370](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33194264370).
- Security `safety` / `bandit` still use `|| true`.

## Ecosystem

[thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That is false of current `main`. The portal is stale.

The product remains a Flask office metaphor with templates, an optional unsigned audit file, a jailed workspace API, and in-memory world state. It is not a civilization-tier IDE.
