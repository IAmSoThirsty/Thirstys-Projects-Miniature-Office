# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Measured tree:** honesty-repair pin on parent `8f7ee8be`

**Status:** experimental prototype — not production-ready

**Rule:** a claim is true only if the tree implements it.

**Independent pytest:** **1,567 passed / 1 skipped** (4.37s). `src/` **24,441** lines / **19,058** non-comment; **74** `@app.route`; 28 floor dirs; `code_civilization.py` **1,421** lines / **52,653** bytes; fresh `--cov=src` **7,493 / 7,749** (96.70% of imported statements). Bandit `-ll` clean (B104 nosec). `pip-audit` clean. GitHub Actions on this pin have not yet run.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 8 | 7 | 1 | 3 | 19 |

`8 + 7 + 1 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (8)**

- Tests passing (1,567 passed, 1 skipped)
- 45+ API endpoints (74 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)
- Lint / known-vuln gates (Black, isort, flake8 E9/F63/F7/F82; bandit `-ll` 0 medium/high with documented B104 nosec; `pip-audit` clean). 13 bandit **low** findings remain.
- Registry thread-safety via `threading.RLock` (the GIL is not the mutex)

**Partial (7)**

- 30+ native language floors (28 toy dirs; SQL now has `schema.sql`; department runtime is Python)
- Complete codegen → tested artifact (identity bodies; Python tests **are** executed; other languages still assumed)
- Cryptographic immutable audit log (SHA-256 chain; optional HMAC-SHA256 when a real key is set; not a ledger)
- Desktop / mobile / VR (browser UI + PWA manifest/service worker; no WebXR)
- Docker / compose (files exist; CD script now curls `/health` and `/api/ide/health` and chmods workspace dirs; Actions on this pin not yet observed)
- CI / CD healthy (unit-test and security workflows rewritten to be able to pass; no run on this SHA yet)
- 23+ patterns / SOLID / smells (AST walkers exist for 5 patterns and 4 anti-patterns; not a 23+/SOLID product)

**Inflated (1)**

- 99% coverage (96.70% of imported statements; `integrated_specs/` omitted)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 24,441 / 19,058 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,421; bytes were counted as lines)

VR is **Partial**, not Holds: PWA exists; WebXR does not.

See [claims.json](claims.json) for the same table as JSON.

## Remaining product gaps

- Template codegen for non-Python; identity bodies for Python
- 28 toy floors
- HMAC is optional and is not a public ledger
- IDE HTTP API is open unless `MO_IDE_TOKEN` is set (required only in production)
- GitHub Actions on this pin have not been observed yet
- Bandit still reports 13 low findings
