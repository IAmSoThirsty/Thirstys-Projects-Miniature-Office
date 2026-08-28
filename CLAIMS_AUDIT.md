# Claims audit

**Repository:** [IAmSoThirsty/Thirstys-Projects-Miniature-Office](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office)

**Original audit commit:** `537c469a8ce34d952525ac25886ed8a85a629f82`

**Honesty-pass code:** `fe9cdf1`

**Previous pin:** [`ffd9b5e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/ffd9b5e7310194c713473941a06eaf797cfdfd38) of parent `8f7ee8be` — score 8/7/1/3 of 19. Docs still said Actions had not run.

**This pin:** independent remeasure of HEAD [`f24ae5c`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f24ae5c3d419f9bb14388591b05b8fb40ab70cc0) plus a CI/docs repair. Tree numbers match `ffd9b5e`. Actions **did** run: CI [33208378504](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378504) **failed**; CD [33208378559](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378559) **succeeded**. Operator docs still denied the PWA shell.

**Rule:** a claim is true only if the tree implements it. Design prose is not implementation.

## Method

1. Clone `main` at `f24ae5c`.
2. Count lines in `src/**/*.py` and `def test_` in `tests/`.
3. Independent pytest: **1,567 passed**, 1 skipped, 12.43s. Fresh `--cov=src`: **7,493 / 7,749** (96.70% of imported statements).
4. `bandit -r src -ll`: 0 medium/high (B104 nosec on `run_server`). `pip-audit -r requirements.txt`: clean.
5. Inspect Actions on `f24ae5c`. Repair the two CI failure modes. Align operator docs with the PWA files that already exist.

## Score

Of 19 headline claims: **9 hold**, **6 are partial**, **1 is inflated**, **3 are false**.

That is `9 + 6 + 1 + 3 = 19`. Ledger: [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md).

Docker/compose moved from Partial to **Holds** because CD was observed green. CI/CD stays **Partial** until a run of this pin is green. The three **false** rows and the **inflated** coverage row remain historical. Canonical files no longer assert them.

## Claim table

| Claim | Verdict | Claimed | Measured |
| --- | --- | --- | --- |
| Production ready | **False** | Production-ready core pipeline | Experimental. Template codegen. Auth only if `MO_IDE_TOKEN` is set. |
| 99% coverage | **Inflated** | 99% overall, core at 100% | Fresh `pytest --cov=src`: **7,493 / 7,749** (96.70% of imported statements). `integrated_specs/` still omitted. |
| 1,537 tests passing | **Holds** (updated) | 1,537 passing | **1,567 passed**, 1 skipped. `def test_` grep = **1,602**. |
| 18,285 src lines | **False** | 18,285 | **24,441** total / **19,058** non-comment (53 files). |
| `code_civilization.py` is 49,741 lines | **False** | 49,741 lines | **1,421 lines** (52,653 bytes). Original misread **bytes as lines**. |
| 30+ native language floors | **Partial** | 30+ working native floors | 28 directories. SQL floor includes `schema.sql`. Department runtime is Python. Inventory: [floors/README.md](floors/README.md). |
| Complete codegen → tested artifact | **Partial** | Working code with tests | Python identity bodies; generated pytest **is executed**. Other languages still assumed. |
| 23+ patterns, SOLID, 17+ smells | **Partial** | Full design analysis | AST walkers detect 5 named patterns and 4 anti-patterns. Not a 23+/SOLID product. |
| Cryptographic immutable audit log | **Partial** | Tamper-proof hash chain | SHA-256 chain. Optional HMAC-SHA256 when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set. Not a ledger. |
| Desktop / mobile / VR | **Partial** | Any device including VR | Browser UI + `manifest.json` / `sw.js`. No WebXR. |
| 45+ API endpoints | **Holds** | 45+ | **74** Flask `@app.route` entries (67 in `app.py` + 7 IDE). |
| CI / CD healthy | **Partial** | Live green badges | CI **failed** on `f24ae5c` (bandit JSON dump; Python 3.9 vs pytest 9). CD **succeeded**. This pin repairs CI. Actions on this SHA not yet observed. |
| 0 lint / 0 vulns | **Holds** | Clean | Critical flake8, Black, isort pass. Bandit `-ll` clean. `pip-audit` clean. 13 bandit **low** findings remain. |
| Registry thread-safe via GIL | **Holds** (restated) | Thread-safe because GIL | `EntityRegistry` and `GlobalRegistry` use `threading.RLock`. The GIL is not the mutex. |
| Formal entity ontology | **Holds** | 7 types, 8 relations | Real, small module (`entity.py`): 7 `EntityType`, 8 `RelationType`. |
| Docker / compose | **Holds** | Verified, hardened | Files exist. Compose has **no** default `SECRET_KEY`. Production refuses placeholders. CD [33208378559](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378559) green on `f24ae5c`. |
| Docs consistent | **Holds** (this pin) | Maximum detail, no omission | Canonical README / LIMITATIONS / this file / [CLAIMS_LEDGER.md](CLAIMS_LEDGER.md) agree on 9/6/1/3 of 19. Operator docs no longer deny the PWA. |
| Apache 2.0 | **Holds** | Apache 2.0 | LICENSE matches |
| Real workspace / editor / terminal | **Holds** | PR #15: real IDE core | Jailed `Workspace`, no-shell `subprocess.run`, 7 `/api/ide/*` routes, `register_ide_routes(app)` called. Token gate when `MO_IDE_TOKEN` is set. Browser UI has a file tree, textarea editor, and terminal form. Not Monaco, not LSP. |

## What this pin changed so remaining claims could be true

- Independent remeasure of `f24ae5c` confirmed the `ffd9b5e` tree numbers (tests, lines, routes, coverage, floors).
- Recorded CI **failure** [33208378504](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378504) and CD **success** [33208378559](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378559). Canonical files no longer say “Actions have not run.”
- `bandit -r src -f json` now uses `-ll --exit-zero`. The text gate remains `bandit -r src -ll`.
- Python matrix is 3.10–3.12. pytest 9.0.3 cannot install on 3.9; the 3.9+ badge was false.
- Operator docs (INSTALL / GETTING_STARTED / EASY_ACCESS / QUICK_REFERENCE / DOCS) describe the PWA shell as present and WebXR as absent.
- Docker/compose is **Holds** because CD was observed green on the measured tree.

## Internal contradictions (original smoking gun)

`LIMITATIONS.md` (pre-audit) contained all of:

- “Production Ready — 99% (1288 tests passing)”
- “Test coverage stat in README (32%) is accurate”
- “Phase 1 complete” with a real pipeline
- “underlying code generation functionality is not operational”

`PRODUCTION_READY.md` certifies production from **22 tests and 32% coverage**.

`README.md` certified production from **1,537 tests and 99% coverage**.

Those contradictions are historical. Canonical files on this pin agree.

Operator docs on `f24ae5c` still said “this repo is not a PWA” while `src/client/manifest.json` and `sw.js` existed. That is corrected here.
