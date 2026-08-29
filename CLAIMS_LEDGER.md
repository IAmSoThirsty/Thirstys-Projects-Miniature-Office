# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Code pin (measured tree):** [`fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — last commit that changed `src/` or `tests/`.

**Docs commit observed on `main`:** [`c783357`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/c7833576720d381322a2186a26b610715fd6f388) (docs-only successor of `1a103bf`). `src/` and `tests/` are identical to the code pin. A later docs merge will have a different SHA; do not retarget the pin to it.

**Independent remeasure of HEAD `c783357` (29 August 2026):** shallow clone counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `def test_` = 1,606. Matches the code-pin metrics. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) is still stale (pins `537c469`, still describes a PRODUCTION READY README badge).

**Status:** experimental prototype — not production-ready

**Rule:** a claim is true only if the tree implements it.

**Independent pytest on `fdd9762`:** **1,573 passed / 1 skipped** (13.14s). `src/` **24,441** lines / **19,058** non-comment; **74** `@app.route`; 28 floor dirs; `code_civilization.py` **1,421** lines / **52,653** bytes; fresh `--cov=src` **7,494 / 7,749** (96.71% of imported statements). Anchored `def test_` = 1,606; unanchored in `tests/` = 1,608. Bandit `-ll` clean (B104 nosec). `pip-audit` clean.

**Actions on code pin `fdd9762`:** CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) **succeeded**. CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) **succeeded**.

**Actions on docs commit `1a103bf`:** CI [33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008) **succeeded**. CD [33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012) **succeeded**.

Re-running coverage on an identical tree can swing by one statement (7,493 vs 7,494). That is not evidence the parent pin was wrong. Do not report `def test_` = 1,612.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 9 | 6 | 1 | 3 | 19 |

`9 + 6 + 1 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (9)**

- Tests passing (1,573 passed, 1 skipped; anchored `def test_` = 1,606)
- 45+ API endpoints (74 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin; files cite code pin `fdd9762`, not a docs-only HEAD)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)
- Lint / known-vuln gates (Black, isort, flake8 E9/F63/F7/F82; bandit `-ll` 0 medium/high with documented B104 nosec; `pip-audit` clean). 13 bandit **low** findings remain.
- Registry thread-safety via `threading.RLock` (the GIL is not the mutex)
- CI / CD healthy (green on `fdd9762` and on docs commit `1a103bf`)

**Partial (6)**

- 30+ native language floors (28 toy dirs; SQL now has `schema.sql`; department runtime is Python)
- Complete codegen → tested artifact (identity bodies; Python tests **are** executed; other languages still assumed)
- Cryptographic immutable audit log (SHA-256 chain; optional HMAC-SHA256 when a real key is set; not a ledger)
- Desktop / mobile / VR (browser UI + PWA manifest/service worker; no WebXR)
- Docker / compose (files exist; CD `test-docker` green on `fdd9762` and `1a103bf`; not hardened — in-memory world, `chmod 777`)
- 23+ patterns / SOLID / smells (AST walkers exist for 5 patterns and 4 anti-patterns; not a 23+/SOLID product)

**Inflated (1)**

- 99% coverage (96.71% of imported statements; `integrated_specs/` omitted)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 24,441 / 19,058 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,421; bytes were counted as lines)

VR is **Partial**, not Holds: PWA exists; WebXR does not.

CI/CD is **Holds**, not Partial: both jobs were observed green on `fdd9762`. Docker stays Partial.

See [claims.json](claims.json) for the same table as JSON. See [INDEPENDENT_REMEASURE.md](INDEPENDENT_REMEASURE.md) for the 29 August 2026 clone counts.

## Remaining product gaps

- Template codegen for non-Python; identity bodies for Python
- 28 toy floors
- HMAC is optional and is not a public ledger
- IDE HTTP API is open unless `MO_IDE_TOKEN` is set (required only in production)
- Bandit still reports 13 low findings
- Docker compose is a healthchecked container, not a hardened service
- [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still says the README badges PRODUCTION READY. That portal is stale.
