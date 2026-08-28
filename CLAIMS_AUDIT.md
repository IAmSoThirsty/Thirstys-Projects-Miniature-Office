# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Code follow-up:** `5e182f2` (cheap claims made true: tests, `start.command`, locks, Mix modules, Black/isort, CI)  
**This file:** 28 August 2026 — numbers re-measured on `5e182f2`; CD `docker-compose` v1 failure patched  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone the default branch.
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS / PRODUCTION_READY numbers to those counts and to `coverage.json`.
4. Open modules that the README treats as complete (pipeline, analyzers, audit log, floors).
5. List Flask routes in `src/server/app.py`.
6. List GitHub Actions runs of `CI - Test and Lint` and `CD - Build and Deploy`.
7. Local pytest (Python 3.10) at the code follow-up: **1,537 passed, 1 skipped**. Confirmed by GitHub Actions CI success on `5e182f2`.

## Score

Of 18 headline claims: **4 hold**, **9 are partial**, **2 are inflated**, **3 are false**.

Was 3 / 4 / 4 / 7 at the docs-only pass, then 4 / 8 / 2 / 4 after the code follow-up. The extra hold/partials come from making cheap claims true (tests, `start.command`, locks, Mix modules, banners, CI green). CD was still red on `docker-compose` (v1); this docs/CD patch does not make the product a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. In-memory. Template codegen. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | 98.7% of 6,438 tracked statements. Omits ~5,548 lines (`design_analyzer.py` 1,333 + `integrated_specs/`). |
| 1,537 tests passing | **Holds** | 1,537 passing | **1,537 passed**, 1 skipped. GitHub Actions `CI - Test and Lint` succeeded on `5e182f2`. 1,570 `test_*` functions exist (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **23,196** total / **17,943** non-comment at `5e182f2` (was 21,566 / 16,313 at `537c469` before Black). |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,364 lines** (50,430 bytes) at `5e182f2`. Original misread **bytes as lines** (48,308 bytes ≈ 1,233 lines at `537c469`). Same error on `cognitive_contract.py` and `constitutional_mutation.py`. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Elixir has Mix modules. SQL floor is still Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists; omitted from coverage. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | In-memory. Per-event SHA-256 of own fields. Parent **ids**, not parent hashes. Not persisted. `datetime.utcnow` removed. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI on port 5000. `start.command` exists. No WebXR. Access docs no longer say VR is a product. |
| 45+ API endpoints | **Holds** | 45+ | 64 Flask routes |
| CI / CD healthy | **Partial** | Live green badges | `CI - Test and Lint` **succeeded** on `5e182f2`. `CD - Build and Deploy` failed: `docker-compose: command not found` (v1). Workflow now uses `docker compose`. Security steps remain `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/` (CI green). `datetime.utcnow` gone. Committed `flake8_remaining*.txt` dumps remain. Bandit/safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (separate processes). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`) |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Default `SECRET_KEY` is `change-this-secret-key`. CD compose test used a missing v1 binary until this patch. |
| Docs consistent | **Partial** | Maximum detail, no omission | Canonical README / LIMITATIONS / this file agree. Historical certificates are bannered. [floors/README.md](floors/README.md) no longer stamps every floor “Implemented.” [DOCS.md](DOCS.md) classifies files. |
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

- `src/analysis/design_analyzer.py` (1,333 lines at `5e182f2`)
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

- `CI - Test and Lint` on `5e182f2`: **success** (52s).
- `CD - Build and Deploy` on `5e182f2`: **failure** — `docker-compose: command not found`. Ubuntu runners ship Compose v2 as `docker compose`. This patch switches the job and waits for `/health`.

Security `safety` / `bandit` still use `|| true`.

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
8. Removed VR-as-product language from operator how-tos.

### This pass

1. Re-measured line counts after Black (`code_civilization.py` is 1,364, not 1,233).
2. Recorded CI success / CD `docker-compose` failure.
3. Switched CD to `docker compose` with a `/health` retry.
4. Honest [floors/README.md](floors/README.md); added [DOCS.md](DOCS.md).
5. GitHub About description no longer claims a civilization-tier IDE.

The product remains a Flask office metaphor with templates and in-memory state. It is not a civilization-tier IDE.
