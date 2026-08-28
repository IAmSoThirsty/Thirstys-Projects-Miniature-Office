# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Follow-up (this file):** 28 August 2026 — docs aligned, then cheap claims made true in code  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone the default branch.
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS / PRODUCTION_READY numbers to those counts and to `coverage.json`.
4. Open modules that the README treats as complete (pipeline, analyzers, audit log, floors).
5. List Flask routes in `src/server/app.py`.
6. List GitHub Actions runs of `CI - Test and Lint`.
7. Run `pytest tests/` locally (Python 3.10): **1,537 passed, 1 skipped**.

## Score

Of 18 headline claims: **4 hold**, **8 are partial**, **2 are inflated**, **4 are false**.

Was 3 / 4 / 4 / 7 at the docs-only pass. The extra holds and partials come from making cheap claims true in the tree (tests, `start.command`, locks, Mix modules, banners), not from becoming a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. In-memory. Template codegen. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | 98.7% of 6,438 tracked statements. Omits 5,346 lines (`design_analyzer.py`, `integrated_specs/`). |
| 1,537 tests passing | **Holds** | 1,537 passing | **1,537 passed**, 1 skipped (pytest 3.10). Nine tests that never interpolated fixtures/f-strings were repaired. |
| 18,285 src lines | **False** | 18,285 | 21,566 total / 16,313 non-comment at `537c469` (Black later reformatted files; that is not a feature) |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,233 lines** (48,308 bytes). Same byte-as-line error on `cognitive_contract.py` and `constitutional_mutation.py`. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Elixir now has `lib/department_floor*.ex`. SQL floor is still Python. |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists and some smell tests pass. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | In-memory. Per-event SHA-256 of own fields. Parent **ids**, not parent hashes. Not persisted. `datetime.utcnow` removed. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI on port 5000. `start.command` now exists. No WebXR. Access docs no longer say VR is a product. |
| 45+ API endpoints | **Holds** | 45+ | 64 Flask routes |
| CI / CD healthy | **False** | Live green badges | Before this change: 15+ consecutive failures. Root causes were `upload-artifact@v3` (hard-fail) and `black --check` (34 files). Workflow bumped to v4; Black/isort applied. **GitHub Actions must still go green on this push.** Security steps remain `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/`. `datetime.utcnow` gone. Committed `flake8_remaining*.txt` dumps remain. Bandit/safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (processes, not a single GIL). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`) |
| Docker / compose | **Partial** | Verified, hardened | Files exist and look coherent. Default `SECRET_KEY` is `change-this-secret-key`. |
| Docs consistent | **Partial** | Maximum detail, no omission | Canonical README / LIMITATIONS / this file agree. 29 historical certificates now carry a “not current status” banner but still contain old prose below it. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |

## Internal contradictions (original smoking gun)

`LIMITATIONS.md` (pre-audit) contained all of:

- “Production Ready — 99% (1288 tests passing)”
- “Test coverage stat in README (32%) is accurate”
- “Phase 1 complete” with a real pipeline
- “underlying code generation functionality is not operational”

`PRODUCTION_READY.md` certifies production from **22 tests and 32% coverage**.

`README.md` certified production from **1,537 tests and 99% coverage**.

`AUDIT_SUMMARY.md` said the README status was changed to ALPHA. It was not (until the 28 Aug rewrite).

Those contradictions are why the canonical files were rewritten. Historical files are bannered, not deleted.

## Coverage omissions

`coverage.json` does not include:

- `src/analysis/design_analyzer.py` (1,256 lines)
- `src/core/integrated_specs/canonical_bundle.py` (1,680)
- `src/core/integrated_specs/domain_base.py` (327)
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

`src/analysis/pattern_detector.py` — `detect_patterns` / `detect_antipatterns` return empty lists.

`src/analysis/metrics_calculator.py` — maintainability is hard-coded `index=100.0, grade="A"`.

`src/analysis/flow_analyzer.py` / `dependency_analyzer.py` — labelled placeholders; empty graphs.

`design_analyzer.py` is a real module (god-class / ISP tests now actually parse the intended source). It is still omitted from `coverage.json`.

## CI

Workflow file: `.github/workflows/ci.yml` (`CI - Test and Lint`).

Observed before this change: every inspected run concluded **failure**. Two mechanical causes on HEAD:

1. `actions/upload-artifact@v3` is rejected by GitHub (security job).
2. `black --check src` would reformat 34 files (test job never reached pytest).

This change upgrades artifact/cache actions to v4, formats with Black + isort (`--profile black`), and keeps Codecov from failing the job. Security `safety` / `bandit` still use `|| true`.

Local pytest is green (1,537 passed). The GitHub badge is true only after the next Actions run succeeds.

## What we changed to make claims accurate

### Docs (first pass)

1. This file — evidence table.
2. `README.md` — measured status only.
3. `LIMITATIONS.md` — one set of numbers, no “production ready”.
4. `PRODUCTION_READY.md` — marked superseded.

### Code and remaining docs (second pass)

1. Added `start.command` (macOS launcher → `start.sh`).
2. Replaced `datetime.utcnow` in `src/`, `tests/`, and floor Python.
3. `EntityRegistry` now uses `threading.RLock` (`GlobalRegistry` already did).
4. Elixir floor: `lib/department_floor.ex` + Application + CLI so `mix.exs` is not a dangling manifest.
5. Status reports in `creative_autonomy.py` / `off_duty_city.py` are f-strings (they previously printed `{self.version}` literally).
6. Tests that built source with `{methods}` / `{long_line}` in non-f-strings now interpolate; production-secret test no longer hard-codes a GitHub Actions path.
7. CI: `upload-artifact@v4`, `cache@v4`, isort `--profile black`, Codecov `continue-on-error`.
8. Bannered 29 historical completion certificates.
9. Removed VR-as-product language from INSTALL / EASY_ACCESS / PLATFORM_SUPPORT / QUICK_REFERENCE.

The product remains a Flask office metaphor with templates and in-memory state. It is not a civilization-tier IDE.
