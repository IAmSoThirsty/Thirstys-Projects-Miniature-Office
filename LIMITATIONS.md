# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Measured 28 August 2026 on `fe9cdf1`.

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files start with a historical banner. Index: [DOCS.md](DOCS.md).

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`)
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a SHA-256 **chain** (`prev_hash` + parent hashes). In-memory only. Not signed, not durable
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness; each README is a toy banner; SQL floor is Python ([floors/README.md](floors/README.md))
- Docker Compose / GitHub Actions **files**; CI and CD are green on `fe9cdf1`
- **1,541 tests passing**, 1 skipped (independent pytest on `fe9cdf1`; GitHub Actions CI green on the same commit)
- `start.sh`, `start.bat`, `start.command`

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | Default compose `SECRET_KEY` is a placeholder. State is in-memory. |
| 99% system coverage | Committed `coverage.json` is 98.7% of a subset. Fresh `pytest --cov=src` is 99% of imported modules and still omits `integrated_specs/`. |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,364 lines** (bytes were originally counted as lines). |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR / PWA / native apps | Browser only. No WebXR, no `manifest.json`, no service worker. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results (`design_analyzer.py` is a real exception). |
| Durable audit / hash chain | Memory dict with `prev_hash` + parent hashes. Hashing exists. Not signed. Not persisted. |
| Security CI as a gate | `safety` and `bandit` cannot fail the job (`\|\| true`). |
| CD | Green on `fe9cdf1` ([run 33179509146](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33179509146)). Uses `docker compose`. Default `SECRET_KEY` is still a placeholder. |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Independent pytest on `fe9cdf1` | 1,541 passed, 1 skipped (Python 3.12) | Collected tests |
| GitHub Actions CI | Green [run 33179509134](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33179509134) | Same commit |
| `def test_` grep | 1,574 | Functions defined, not the same as collected |
| `coverage.json` totals | 6,354 / 6,438 statements (98.7%) | Snapshot that omitted `design_analyzer.py` |
| Fresh `pytest --cov=src` | 6,908 / 6,971 statements (99%) | Imported modules only; still omits `integrated_specs/` |
| Omitted Python | `integrated_specs/` (~4,215 lines) | Not imported by the test run |
| `src/` Python lines | 23,250 total / 17,991 non-comment | Whole tree, not coverage |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Historical |

There is no single 99% of the whole tree.

## Persistence

Everything lives in the process. Restarting the server drops world state, audit events, and generated artifacts.

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose default is insecure
- Dependency/security CI: informational only
- No authentication on the API
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Fail the job on security findings instead of `|| true`.
2. Persist and sign the audit chain. Hashing over `prev_hash` already exists in memory; restart still drops it.
3. Replace template TODOs with real generation, or stop claiming a pipeline.
4. Include every `src/` module in coverage reports.
5. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Write a test that fails first.
2. Update this file if a limitation is removed.
3. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) if a public claim changes verdict.
4. Do not describe planned work as shipped.
