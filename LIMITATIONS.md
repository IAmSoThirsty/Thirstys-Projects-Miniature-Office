# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Measured 28 August 2026 on `aa7b439` (repairs `6196f8a`).

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files start with a historical banner. Index: [DOCS.md](DOCS.md).

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`)
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a SHA-256 **chain** (`prev_hash` + parent hashes). Optional unsigned JSONL when `MO_DATA_DIR` is set. Not signed, not a ledger
- Jailed workspace filesystem (`src/workspace/fs.py`) and no-shell terminal (`src/runtime/terminal.py`) via `/api/ide/*`
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness; each README is a toy banner; SQL floor is Python ([floors/README.md](floors/README.md))
- Docker Compose / GitHub Actions **files**; CI **failed** on `6196f8a`; last green docs pin `f560d651`
- **1,558 tests passing**, 1 skipped (independent pytest of this tree)
- `start.sh`, `start.bat`, `start.command`

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | Default compose `SECRET_KEY` is a placeholder. World state is in-memory. API has no auth. |
| 99% system coverage | Fresh `pytest --cov=src` is 7,192 / 7,356 imported statements (97.8%) and still omits `integrated_specs/`. |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,364 lines** (bytes were originally counted as lines). |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR / PWA / native apps | Browser only. No WebXR, no `manifest.json`, no service worker. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results (`design_analyzer.py` is a real exception). |
| Tamper-proof ledger | Hashing exists. JSONL persist is optional and **unsigned**. |
| Security CI as a gate | `safety` and `bandit` cannot fail the job (`\|\| true`). |
| CI / CD on HEAD | Red on `6196f8a`. Repair CI green on `aa7b439` ([33198450337](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33198450337)). CD not claimed until a run succeeds. |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Independent pytest of this tree | 1,558 passed, 1 skipped | Collected tests |
| Independent pytest of `6196f8a` | 5 failed, 1,553 passed | Unhooked IDE core |
| GitHub Actions CI on `6196f8a` | Failure [run 33197912388](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33197912388) | Same commit |
| `def test_` grep | 1,591 | Functions defined, not the same as collected |
| Fresh `pytest --cov=src` | 7,192 / 7,356 statements (97.8%) | Imported modules only; still omits `integrated_specs/` |
| Omitted Python | `integrated_specs/` (~4,215 lines) | Not imported by the test run |
| `src/` Python lines | 23,858 total / 18,527 non-comment | Whole tree, not coverage |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Historical |

There is no single 99% of the whole tree.

## Persistence

- World / simulation / registries: in-process. Restart drops them.
- Workspace files: on disk under `MO_WORKSPACE` (default `./user_workspace`).
- Audit chain: in-process unless `MO_DATA_DIR` is set, in which case `audit.jsonl` is appended. Unsigned.

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose default is insecure
- Dependency/security CI: informational only
- No authentication on the API, including the terminal route
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Fail the job on security findings instead of `|| true`.
2. Sign the audit chain. Hashing over `prev_hash` already exists; JSONL is not a signature.
3. Replace template TODOs with real generation, or stop claiming a pipeline.
4. Include every `src/` module in coverage reports.
5. Add authentication before treating `/api/ide/terminal` as a product surface.
6. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Add a test that would fail if the route or module is not wired.
2. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) in the same change if a headline number moves.
3. Do not describe planned work as done.
