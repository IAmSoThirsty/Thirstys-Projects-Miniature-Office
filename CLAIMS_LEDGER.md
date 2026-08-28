# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Measured tree:** [`590b90ce53bd859d8baf32a2155820e3169a93e8`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/590b90ce53bd859d8baf32a2155820e3169a93e8)

**Status:** experimental prototype — not production-ready

**Rule:** a claim is true only if the tree implements it.

**Independent pytest:** **1,571 passed / 1 skipped** on `590b90c` (13.89s). This pin adds 2 honesty-lock tests (**1,573 passed** / 1 skipped). `src/` **24,441** lines / **19,058** non-comment; **74** `@app.route`; 28 floor dirs; `code_civilization.py` **1,421** lines / **52,653** bytes; fresh `--cov=src` **7,493 / 7,749** (96.70% of imported statements). `def test_` grep = 1,606. Bandit `-ll` clean (B104 nosec). `pip-audit` clean.

**Actions on `590b90c`:** CI [33211771675](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33211771675) **succeeded**. CD [33211771674](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33211771674) **succeeded**.

This pin is docs + two honesty-lock tests (EASY_ACCESS must not deny the PWA; canonical files must cite the same measured commit). Tree numbers other than the new tests are those of `590b90c`.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 9 | 6 | 1 | 3 | 19 |

`9 + 6 + 1 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (9)**

- Tests passing (1,573 passed, 1 skipped; 1,571 on `590b90c` plus 2 honesty-lock tests)
- 45+ API endpoints (74 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin; EASY_ACCESS no longer denies the PWA shell)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)
- Lint / known-vuln gates (Black, isort, flake8 E9/F63/F7/F82; bandit `-ll` 0 medium/high with documented B104 nosec; `pip-audit` clean). 13 bandit **low** findings remain.
- Registry thread-safety via `threading.RLock` (the GIL is not the mutex)
- CI / CD healthy (CI 33211771675 and CD 33211771674 both **green** on `590b90c`)

**Partial (6)**

- 30+ native language floors (28 toy dirs; SQL now has `schema.sql`; department runtime is Python)
- Complete codegen → tested artifact (identity bodies; Python tests **are** executed; other languages still assumed)
- Cryptographic immutable audit log (SHA-256 chain; optional HMAC-SHA256 when a real key is set; not a ledger)
- Desktop / mobile / VR (browser UI + PWA manifest/service worker; no WebXR)
- Docker / compose (files exist; CD `test-docker` green on `590b90c`; not hardened — in-memory world, `chmod 777`)
- 23+ patterns / SOLID / smells (AST walkers exist for 5 patterns and 4 anti-patterns; not a 23+/SOLID product)

**Inflated (1)**

- 99% coverage (96.70% of imported statements; `integrated_specs/` omitted)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 24,441 / 19,058 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,421; bytes were counted as lines)

VR is **Partial**, not Holds: PWA exists; WebXR does not.

CI/CD is **Holds**, not Partial: both jobs were observed green on `590b90c`. Docker stays Partial.

See [claims.json](claims.json) for the same table as JSON.

## Remaining product gaps

- Template codegen for non-Python; identity bodies for Python
- 28 toy floors
- HMAC is optional and is not a public ledger
- IDE HTTP API is open unless `MO_IDE_TOKEN` is set (required only in production)
- Bandit still reports 13 low findings
- Docker compose is a healthchecked container, not a hardened service
- [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That portal is stale.
