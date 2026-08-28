# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Docs HEAD:** `8f7ee8be` (`8f7ee8be10ef4a64599415db84b07cefe535ca88`) — docs-only pin  
**Measured code:** `a41e1f8` (`a41e1f866cad6452c678495e23f6cf9d97ec6231`)  
**Status:** experimental prototype — not production-ready  
**Rule:** a claim is true only if the tree implements it.  
**Independent pytest of `a41e1f8`:** **1,558 passed / 1 skipped** (11.64s). `src/` **23,876** lines / **18,542** non-comment; **71** `@app.route`; 28 floor dirs; `code_civilization.py` **1,364** lines / **50,430** bytes; fresh `--cov=src` **7,194 / 7,364** (97.69% of imported statements).  
**CI on `a41e1f8`:** [33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573) **failed** (bandit B104).  
**CI on HEAD `8f7ee8be`:** [33202552780](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33202552780) **failed** (same B104).  
**CD on `a41e1f8`:** [33201115545](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115545) **failed** (`/health` 503).  
**CD on HEAD `8f7ee8be`:** [33202552838](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33202552838) **failed** (same probe mismatch).

This branch repairs CD/Dockerfile to curl `/api/ide/health` and makes the
workspace volume writable. **Do not mark CI/CD Holds until a run on the
repair SHA is independently green.** Bandit B104 is unchanged.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 6 | 8 | 2 | 3 | 19 |

`6 + 8 + 2 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (6)**

- Tests passing (1,558 passed, 1 skipped on `a41e1f8`)
- 45+ API endpoints (71 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin of `a41e1f8` / HEAD `8f7ee8be`)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)

**Partial (8)**

- 30+ native language floors (28 toy dirs; SQL is Python)
- Complete codegen → tested artifact (TODO templates)
- Cryptographic immutable audit log (unsigned JSONL optional; in-memory otherwise)
- Desktop / mobile / VR (browser UI exists; no WebXR, no PWA)
- 0 lint / 0 vulns (lint clean; bandit fails B104; safety `|| true`)
- Registry thread-safe via GIL (actually `RLock`; 4 gunicorn workers)
- Docker / compose (files exist; no compose default `SECRET_KEY`; CD probe was mismatched; volume was not writable by image user)
- CI / CD healthy (unit tests green; security red; CD red on HEAD)

**Inflated (2)**

- 99% coverage (97.69% of imported statements; `integrated_specs/` omitted)
- 23+ patterns / SOLID / smells (placeholder analyzers)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 23,876 / 18,542 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,364; bytes were counted as lines)

VR is **Partial**, not Holds: there is no WebXR and no PWA.

See [claims.json](claims.json) for the same table as JSON.

Ecosystem claim [EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still describes a PRODUCTION READY badge. That portal is stale relative to current `main`.

## Remaining product gaps (unchanged)

- Template codegen with `TODO` bodies; generated tests are not executed
- 28 toy floors; SQL floor is Python
- Unsigned audit chain (JSONL is not a signature)
- Safety CI uses `|| true`; bandit fails on expected bind-all-interfaces
- IDE HTTP API has no auth
