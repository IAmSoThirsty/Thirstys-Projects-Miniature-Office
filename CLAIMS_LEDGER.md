# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Measured tree:** [`fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a)

**Status:** experimental prototype — not production-ready

**Rule:** a claim is true only if the tree implements it.

**Independent pytest on `fdd9762`:** **1,573 passed / 1 skipped** (13.14s). `src/` **24,441** lines / **19,058** non-comment; **74** `@app.route`; 28 floor dirs; `code_civilization.py` **1,421** lines / **52,653** bytes; fresh `--cov=src` **7,494 / 7,749** (96.71% of imported statements). `def test_` grep = 1,606. Bandit `-ll` clean (B104 nosec). `pip-audit` clean.

**Actions on `fdd9762`:** CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) **succeeded**. CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) **succeeded**.

Parent pin `590b90c` reported coverage **7,493 / 7,749** (96.70%). Independent re-measure of HEAD is **7,494 / 7,749** (96.71%) — one extra covered statement. Tree numbers other than coverage and the CI/CD run IDs match the parent pin.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 9 | 6 | 1 | 3 | 19 |

`9 + 6 + 1 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (9)**

- Tests passing (1,573 passed, 1 skipped)
- 45+ API endpoints (74 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin; files cite `fdd9762`)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)
- Lint / known-vuln gates (Black, isort, flake8 E9/F63/F7/F82; bandit `-ll` 0 medium/high with documented B104 nosec; `pip-audit` clean). 13 bandit **low** findings remain.
- Registry thread-safety via `threading.RLock` (the GIL is not the mutex)
- CI / CD healthy (CI 33212776987 and CD 33212776992 both **green** on `fdd9762`)

**Partial (6)**

- 30+ native language floors (28 toy dirs; SQL now has `schema.sql`; department runtime is Python)
- Complete codegen → tested artifact (identity bodies; Python tests **are** executed; other languages still assumed)
- Cryptographic immutable audit log (SHA-256 chain; optional HMAC-SHA256 when a real key is set; not a ledger)
- Desktop / mobile / VR (browser UI + PWA manifest/service worker; no WebXR)
- Docker / compose (files exist; CD `test-docker` green on `fdd9762`; not hardened — in-memory world, `chmod 777`)
- 23+ patterns / SOLID / smells (AST walkers exist for 5 patterns and 4 anti-patterns; not a 23+/SOLID product)

**Inflated (1)**

- 99% coverage (96.71% of imported statements; `integrated_specs/` omitted)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 24,441 / 19,058 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,421; bytes were counted as lines)

VR is **Partial**, not Holds: PWA exists; WebXR does not.

CI/CD is **Holds**, not Partial: both jobs were observed green on `fdd9762`. Docker stays Partial.

See [claims.json](claims.json) for the same table as JSON.

## Remaining product gaps

- Template codegen for non-Python; identity bodies for Python
- 28 toy floors
- HMAC is optional and is not a public ledger
- IDE HTTP API is open unless `MO_IDE_TOKEN` is set (required only in production)
- Bandit still reports 13 low findings
- Docker compose is a healthchecked container, not a hardened service
- [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That portal is stale.
