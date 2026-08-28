# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Code follow-up:** `5e182f2` (cheap claims made true: tests, `start.command`, locks, Mix modules, Black/isort, CI)  
**This file:** 28 August 2026 — re-measured on HEAD `8132127`  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone the default branch (`8132127`).
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS / PRODUCTION_READY numbers to those counts and to `coverage.json`.
4. Open modules that the README treats as complete (pipeline, analyzers, audit log, floors).
5. List Flask routes in `src/server/app.py`.
6. List GitHub Actions runs of `CI - Test and Lint` and `CD - Build and Deploy`.
7. Local pytest (Python 3.10) at the code follow-up: **1,537 passed, 1 skipped**. Confirmed by GitHub Actions CI success on `5e182f2` and on `8132127`.
8. Sweep remaining operator docs (PLATFORM_SUPPORT, EASY_ACCESS, INSTALL, GETTING_STARTED) for VR / PWA / native-app / production language.

## Score

Of 18 headline claims: **8 hold**, **6 are partial**, **2 are inflated**, **3 are false**.

Was 5 / 8 / 2 / 3 after the CI/CD pass. Extra holds this pass: desktop/mobile access docs no longer claim a VR product, and operator docs now agree with the audit. The product is still not a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. In-memory. Template codegen. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | 98.7% of 6,438 tracked statements. Omits ~5,548 lines (`design_analyzer.py` 1,333 + `integrated_specs/`). |
| 1,537 tests passing | **Holds** | 1,537 passing | **1,537 passed**, 1 skipped. GitHub Actions `CI - Test and Lint` succeeded on `8132127`. 1,572 `test_*` functions exist (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **23,196** total / **17,943** non-comment at `8132127`. |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,364 lines** (50,430 bytes) at `8132127`. Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Elixir has Mix modules. SQL floor is still Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists; omitted from coverage. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | In-memory. Per-event SHA-256 of own fields. Parent **ids**, not parent hashes. Not persisted. |
| Desktop / mobile / VR | **Holds** | Any device including VR | Browser UI on port 5000. `start.command` exists. No WebXR, no PWA. Operator docs now say that. |
| 45+ API endpoints | **Holds** | 45+ | 64 Flask routes |
| CI / CD healthy | **Holds** | Live green badges | **CI** [run 33172395544](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33172395544) and **CD** [run 33172395561](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33172395561) both succeeded on `8132127`. Security steps remain `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/` (CI green). Committed `flake8_remaining*.txt` dumps remain. Bandit/safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (separate processes). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`) |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Default `SECRET_KEY` is `change-this-secret-key`. CD compose health check succeeded on `8132127`. |
| Docs consistent | **Holds** | Maximum detail, no omission | Canonical README / LIMITATIONS / this file agree. Operator how-tos no longer claim VR/PWA/native production. Historical certificates are bannered. [floors/README.md](floors/README.md) no longer stamps every floor “Implemented.” |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |

## Internal contradictions (original smoking gun)

`LIMITATIONS.md` (pre-audit) contained all of:

- “Production Ready — 99% (1288 tests passing)”
- “Test coverage stat in README (32%) is accurate”
- “Phase 1 complete” with a real pipeline
- “underlying code generation functionality is not operational”

`PRODUCTION_READY.md` certifies production from **22 tests and 32% coverage**.

`README.md` certified production from **1,537 tests and 99% coverage**.

`AUDIT_SUMMARY.md` said the README status was changed to ALPHA. It was not (until the 28 Aug rewrite). It also claimed `datetime.utcnow` was gone before it actually was.

Those contradictions are why the canonical files were rewritten. Historical files are bannered, not deleted.

## Coverage omissions

`coverage.json` (still the pre-format snapshot) does not include:

- `src/analysis/design_analyzer.py` (1,333 lines at `8132127`)
- `src/core/integrated_specs/canonical_bundle.py` (1,800)
- `src/core/integrated_specs/domain_base.py` (332)
- `src/core/integrated_specs/governance.py` (671)
- `src/core/integrated_specs/identity.py` (906)
- `src/core/integrated_specs/meta_identity.py` (506)

Reporting 99% while excluding the largest analysis module is not 99% of the codebase.

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

`design_analyzer.py` is a real module. It is still omitted from `coverage.json`.

## CI

Workflow files: `.github/workflows/ci.yml`, `.github/workflows/deploy.yml`.

- `CI - Test and Lint` on `8132127`: **success** ([run 33172395544](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33172395544)).
- `CD - Build and Deploy` on `8132127`: **success** ([run 33172395561](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33172395561)).
- Security `safety` / `bandit` still use `|| true`.

## What we changed to make claims accurate

### Docs (first pass)

Canonical README / LIMITATIONS / this file. `PRODUCTION_READY.md` superseded.

### Code (second pass, `5e182f2`)

1. Added `start.command`.
2. Replaced `datetime.utcnow`.
3. `EntityRegistry` uses `threading.RLock`.
4. Elixir Mix modules so `mix.exs` is not dangling.
5. Tests repaired; local and GitHub pytest green (1,537 passed).
6. CI: `upload-artifact@v4`, `cache@v4`, Black + isort.
7. Bannered historical completion certificates.
8. Removed VR-as-product language from some operator how-tos.

### Third pass (CI/CD + floors)

1. Re-measured line counts after Black (`code_civilization.py` is 1,364, not 1,233).
2. Switched CD to `docker compose` with a `/health` retry.
3. Honest [floors/README.md](floors/README.md); added [DOCS.md](DOCS.md).
4. GitHub About description no longer claims a civilization-tier IDE.

### This pass (`8132127` remeasure)

1. Re-measured HEAD. Line counts, routes, floors, and `code_civilization.py` size are unchanged from `5e182f2`.
2. Recorded CI + CD **success** on `8132127`.
3. Rewrote remaining operator docs that still claimed a VR product, a PWA, native apps, or production Docker (`PLATFORM_SUPPORT.md`, `EASY_ACCESS.md`, `INSTALL.md`, `GETTING_STARTED.md`).
4. Bannered `ARCHITECTURE.md` as design intent; bannered `QUICK_REFERENCE_FLOORS_20-22-23.md` and `floors/erlang/README.md` so “nine-nines” is not an SLA.
5. `.env.example` no longer titles itself a production config.

The product remains a Flask office metaphor with templates and in-memory state. It is not a civilization-tier IDE.
