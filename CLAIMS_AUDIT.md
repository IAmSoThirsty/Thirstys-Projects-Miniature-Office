# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)  
**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`  
**Code follow-up:** `5e182f2` (cheap claims made true: tests, `start.command`, locks, Mix modules, Black/isort, CI)  
**Docs follow-up:** `b8827e7` (operator how-tos, floor inventory)  
**Honesty pass:** `fe9cdf1` — remaining live overclaims removed; in-memory hash chain implemented  
**Docs pin:** `27d7fdf` — score 6/7/2/3; CLAIMS_LEDGER + claims.json  
**This re-measure:** 28 August 2026 — independent clone of `27d7fdf`. Tree numbers identical to `fe9cdf1`. CI [33189144183](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144183) and CD [33189144211](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144211) green on current HEAD.  
**Score correction:** 28 August 2026 evening — table arithmetic was 8/6/2/3 (19). The 18-row table is **6 holds / 7 partial / 2 inflated / 3 false** once VR is Partial. Independent re-measure of `27d7fdf` confirmed the tree numbers.  
**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main` at `27d7fdf`.
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Compare README / LIMITATIONS / PRODUCTION_READY numbers to those counts and to `coverage.json`.
4. Open modules that the README treats as complete (pipeline, analyzers, audit log, floors).
5. List Flask routes in `src/server/app.py`.
6. List GitHub Actions runs of `CI - Test and Lint` and `CD - Build and Deploy`.
7. Independent pytest on `27d7fdf`: **1,541 passed, 1 skipped** (7.09s). GitHub Actions CI success on the same commit ([run 33189144183](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144183)). Fresh `pytest --cov=src`: **6,908 / 6,971** statements (99% of imported modules).
8. Sweep remaining live operator docs (`DEMO.txt`, `QUICKSTART.md`, `package.json`, per-floor READMEs) for VR / PWA / ledger / “spatialized IDE” language.

## Score

Of 18 headline claims: **6 hold**, **7 are partial**, **2 are inflated**, **3 are false**.

That is `6 + 7 + 2 + 3 = 18`. The previous score line (`8 hold / 6 partial / 2 inflated / 3 false`) summed to 19 and cannot describe an 18-row table. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

The three **false** and two **inflated** rows are *historical* claims. Canonical README / LIMITATIONS / this file no longer assert them. Remaining **partial** rows are still true of the code (template codegen, toy floors, unsigned in-memory audit, `|| true` security, placeholder compose secret, no VR). The product is still not a production IDE.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. In-memory. Template codegen. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Committed `coverage.json`: 98.7% of 6,438 statements, omitting `design_analyzer.py` + `integrated_specs/`. Fresh `pytest --cov=src`: 99% of 6,971 *imported* statements; `integrated_specs/` still omitted. Not 99% of the tree. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,541 passed**, 1 skipped. Independent pytest on `27d7fdf`. Four hash-chain tests added on `fe9cdf1`. `def test_` grep = 1,574 (grep ≠ collected). |
| 18,285 src lines | **False** | 18,285 | **23,250** total / **17,991** non-comment. |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,364 lines** (50,430 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. Floor READMEs open with a toy/prototype banner. SQL floor is still Python. Nested `floors/java/bin/README.md` is committed build output. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Templates with `TODO`. Comment: “we assume tests pass.” Tests are not run on generated code. |
| 23+ patterns, SOLID, 17+ smells | **Inflated** | Full design analysis | `pattern_detector.py` returns `[]`. Flow/metrics/dependency analyzers are placeholders. `design_analyzer.py` exists; omitted from committed `coverage.json`. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | In-memory SHA-256 **chain**: each event hashes `prev_hash` plus parent hashes. Not persisted, not signed. Restart drops it. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI on port 5000. `start.command` exists. No WebXR, no PWA. A Holds verdict is not compatible with a missing VR surface. |
| 45+ API endpoints | **Holds** | 45+ | 64 Flask `@app.route` entries |
| CI / CD healthy | **Holds** | Live green badges | **CI** [run 33189144183](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144183) and **CD** [run 33189144211](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144211) succeeded on `27d7fdf`. Security steps remain `\|\| true`. |
| 0 lint / 0 vulns | **Partial** | Clean | Critical flake8, Black, and isort pass on `src/` (CI green). Committed `flake8_remaining*.txt` dumps remain. Bandit/safety cannot fail CI. |
| Registry thread-safe via GIL | **Partial** | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. Gunicorn uses 4 eventlet workers (separate processes). |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Partial** | Verified, hardened | Files exist. Default `SECRET_KEY` is `change-this-secret-key`. CD compose health check succeeded on `27d7fdf`. |
| Docs consistent | **Holds** | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 6/7/2/3. Historical `*_COMPLETE.md` files stay bannered. |
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

- `src/analysis/design_analyzer.py` (1,333 lines)
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

`design_analyzer.py` is a real module. It is still omitted from committed `coverage.json`. A full `pytest --cov=src` on this tree reports **99% of 6,971 imported statements** (analyzer included) and still omits `src/core/integrated_specs/`.

## CI

Workflow files: `.github/workflows/ci.yml`, `.github/workflows/deploy.yml`.

- `CI - Test and Lint` on `27d7fdf`: **success** ([run 33189144183](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144183)).
- `CD - Build and Deploy` on `27d7fdf`: **success** ([run 33189144211](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144211)).
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

### `fe9cdf1` remaining live claims

1. `DEMO.txt` no longer says “You're in VR with Miniature Office.”
2. Quickstart no longer claims an event hash proves a tamper-proof ledger.
3. `package.json` no longer describes a spatialized IDE or a phantom `src/client/main.js`.
4. Floor READMEs open with a toy/prototype banner (SQL: “not SQL”).
5. `src/core/audit.py` now hashes `prev_hash` and parent hashes (in-memory chain). Pytest: **1,541 passed**, 1 skipped.
6. Re-measured `src/`: **23,250** total / **17,991** non-comment.

### This correction (score + evidence pin)

1. Score line was 8/6/2/3 (sums to 19). Corrected to **6/7/2/3** (sums to 18). VR is Partial.
2. CI/CD evidence pinned to green runs on `fe9cdf1`, not the older `b8827e7`.
3. Independent pytest re-run: 1,541 passed, 1 skipped; 6,908 / 6,971 imported statements covered.
4. LIMITATIONS roadmap no longer lists “hash over previous hash” as undone work; hashing exists, persistence/signing do not.
5. Nested `floors/java/bin/README.md` gets a toy banner so it cannot be cited as an enterprise runtime.

### This re-measure (`27d7fdf`)

1. Independent clone of current `main`. Line counts, route count, floor count, and `code_civilization.py` bytes/lines match the honesty-pass numbers.
2. Independent pytest: **1,541 passed**, 1 skipped; `--cov=src` **6,908 / 6,971**.
3. CI/CD evidence moved from `fe9cdf1` to green runs on `27d7fdf`.
4. [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469`. That portal is stale.

The product remains a Flask office metaphor with templates and in-memory state. It is not a civilization-tier IDE.
