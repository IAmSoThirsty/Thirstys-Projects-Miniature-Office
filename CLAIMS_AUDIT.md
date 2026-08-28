# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Commit audited:** `537c469a8ce34d952525ac25886ed8a85a629f82` (`main`)  
**Date:** 28 August 2026  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone the default branch.
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS / PRODUCTION_READY numbers to those counts and to `coverage.json`.
4. Open modules that the README treats as complete (pipeline, analyzers, audit log, floors).
5. List Flask routes in `src/server/app.py`.
6. List the last 15 GitHub Actions runs of `CI - Test and Lint`.

## Score

Of 18 headline claims: **3 hold**, **4 are partial**, **4 are inflated**, **7 are false**.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. CI red. In-memory. Template codegen. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | 98.7% of 6,438 tracked statements. Omits 5,346 lines (`design_analyzer.py`, `integrated_specs/`). `code_civilization.py` 96.9%, `semantic_analyzer.py` 65.2%. |
| 1,537 tests passing | **Inflated** | 1,537 passing | 1,570 `test_*` functions exist. CI history on this branch is failure. Older docs say 22 tests. |
| 18,285 src lines | **False** | 18,285 | 21,566 total / 16,313 non-comment |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,233 lines** (48,308 bytes). Same byte-as-line error on `cognitive_contract.py` (457 lines, claimed 15,773) and `constitutional_mutation.py` (487 lines, claimed 17,818). |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Elixir: `mix.exs` + README only. SQL floor is Python. |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` excluded from coverage. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | In-memory. Per-event SHA-256 of own fields. Parent **ids**, not parent hashes. Not persisted. |
| Desktop / mobile / VR | **Inflated** | Any device including VR | Browser UI on port 5000. No WebXR. `start.command` is missing. |
| 45+ API endpoints | **Holds** | 45+ | 64 Flask routes |
| CI / CD healthy | **False** | Live green badges | 15 consecutive CI failures including HEAD. `safety`/`bandit` use `\|\| true`. |
| 0 lint / 0 vulns | **False** | Clean | Committed `flake8_remaining*.txt`. `datetime.utcnow` in 9 files. Security job cannot fail. |
| Registry thread-safe via GIL | **False** | Thread-safe | GIL is not a lock. No `Lock`. Gunicorn uses 4 eventlet workers. |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`, 169 lines) |
| Docker / compose | **Partial** | Verified, hardened | Files exist and look coherent. Default `SECRET_KEY` is `change-this-secret-key`. CD not shown green. |
| Docs consistent | **False** | Maximum detail, no omission | LIMITATIONS asserts 99% **and** 32%; pipeline complete **and** not operational. AUDIT_SUMMARY says README was changed to ALPHA; README still says PRODUCTION READY. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |

## Internal contradictions (smoking gun)

`LIMITATIONS.md` (pre-audit) contained all of:

- “Production Ready — 99% (1288 tests passing)”
- “Test coverage stat in README (32%) is accurate”
- “Phase 1 complete” with a real pipeline
- “underlying code generation functionality is not operational”

`PRODUCTION_READY.md` certifies production from **22 tests and 32% coverage**.

`README.md` certified production from **1,537 tests and 99% coverage**.

`AUDIT_SUMMARY.md` says the README status was changed to ALPHA. It was not.

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

## CI

Workflow file: `.github/workflows/ci.yml` (`CI - Test and Lint`).

Observed: every inspected run through 21 March 2026 concluded **failure**, including the HEAD push “feat: Add Floor 10 (Java Jurisdiction)”.

## What we changed to make claims accurate

Documentation, not a rewrite of the simulator:

1. This file — evidence table.
2. `README.md` — measured status only.
3. `LIMITATIONS.md` — one set of numbers, no “production ready”.
4. `PRODUCTION_READY.md` — marked superseded.

The product remains what the code is: a Flask office metaphor with templates and in-memory state.
