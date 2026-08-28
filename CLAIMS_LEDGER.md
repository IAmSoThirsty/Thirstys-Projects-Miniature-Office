# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Parent measured:** `6196f8a` (PR #15 merge — IDE files present, routes unhooked, CI red)  
**This repair:** `aa7b439` — wires `register_ide_routes`, optional unsigned JSONL persist  
**Status:** experimental prototype — not production-ready  
**Rule:** a claim is true only if the tree implements it.  
**Independent pytest of this tree:** **1,558 passed / 1 skipped** (7.14s). `src/` **23,858** lines / **18,527** non-comment; **71** `@app.route`; 28 floor dirs; `code_civilization.py` **1,364** lines / **50,430** bytes; fresh `--cov=src` **7,192 / 7,356** (97.8% of imported statements). CI [33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388) **failed** on `6196f8a`. Last green: `f560d651`.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 6 | 8 | 2 | 3 | 19 |

`6 + 8 + 2 + 3 = 19`. The previous 18-row score (`6 / 7 / 2 / 3`) described `27d7fdf`. PR #15 added a 19th headline (real IDE core) and moved CI/CD from Holds to Partial because `6196f8a` is red.

## Verdicts (must match the audit table)

**Holds (6)**

- Tests passing (1,558 passed, 1 skipped on this tree)
- 45+ API endpoints (71 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes)

**Partial (8)**

- 30+ native language floors (28 toy dirs; SQL is Python)
- Complete codegen → tested artifact (TODO templates)
- Cryptographic immutable audit log (unsigned JSONL optional; in-memory otherwise)
- Desktop / mobile / VR (browser only; no WebXR, no PWA)
- 0 lint / 0 vulns (`|| true` on security jobs)
- Registry thread-safe via GIL (actually `RLock`; 4 gunicorn workers)
- Docker / compose (files exist; placeholder `SECRET_KEY`)
- CI / CD healthy (red on `6196f8a`; this repair not yet Actions-green)

**Inflated (2)**

- 99% coverage (97.8% of imported statements; `integrated_specs/` omitted)
- 23+ patterns / SOLID / smells (placeholder analyzers)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 23,858 / 18,527 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,364; bytes were counted as lines)

VR is **Partial**, not Holds: there is no WebXR and no PWA.

See [claims.json](claims.json) for the same table as JSON.

Ecosystem claim [EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still describes a PRODUCTION READY badge. That portal is stale relative to current `main`.

## Remaining product gaps (unchanged)

- Template codegen with `TODO` bodies; generated tests are not executed
- 28 toy floors; SQL floor is Python
- Unsigned audit chain (JSONL is not a signature)
- Security CI uses `|| true`
- Compose `SECRET_KEY` placeholder
- IDE HTTP API has no auth
