# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Commit measured:** `27d7fdf` (28 August 2026)  
**Honesty-pass code:** `fe9cdf1` (docs-only delta to `27d7fdf`; tree numbers identical)  
**Status:** experimental prototype — not production-ready  
**Rule:** a claim is true only if the tree implements it.  
**Independent re-measure of `27d7fdf`:** pytest **1,541 passed / 1 skipped**; `src/` **23,250** lines / **17,991** non-comment; 64 `@app.route`; 28 floor dirs; `code_civilization.py` **1,364** lines / **50,430** bytes; fresh `--cov=src` **6,908 / 6,971**. CI [33189144183](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144183) and CD [33189144211](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33189144211) green on `27d7fdf`.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 6 | 7 | 2 | 3 | 18 |

`6 + 7 + 2 + 3 = 18`. The previous score line in CLAIMS_AUDIT (`8 hold / 6 partial / 2 inflated / 3 false`) sums to 19.

## Verdicts (must match the audit table)

**Holds (6)**

- Tests passing (1,541 passed, 1 skipped on `27d7fdf`)
- 45+ API endpoints (64 routes)
- CI / CD healthy on `27d7fdf`
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent
- Apache 2.0

**Partial (7)**

- 30+ native language floors (28 toy dirs; SQL is Python)
- Complete codegen → tested artifact (TODO templates)
- Cryptographic immutable audit log (in-memory unsigned chain)
- Desktop / mobile / VR (browser only; no WebXR, no PWA)
- 0 lint / 0 vulns (`|| true` on security jobs)
- Registry thread-safe via GIL (actually `RLock`; 4 gunicorn workers)
- Docker / compose (files exist; placeholder `SECRET_KEY`)

**Inflated (2)**

- 99% coverage (subset; `integrated_specs/` omitted)
- 23+ patterns / SOLID / smells (placeholder analyzers)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 23,250 / 17,991 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,364; bytes were counted as lines)

VR is **Partial**, not Holds: there is no WebXR and no PWA.

See [claims.json](claims.json) for the same table as JSON.

Ecosystem claim [EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469`. That commit is the smoking-gun contradictions, not current `main`. The README no longer badges PRODUCTION READY.

## Remaining product gaps (unchanged)

- Template codegen with `TODO` bodies; generated tests are not executed
- 28 toy floors; SQL floor is Python
- In-memory unsigned audit chain
- Security CI uses `|| true`
- Compose `SECRET_KEY` placeholder
