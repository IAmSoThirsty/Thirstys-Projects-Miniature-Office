# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Measured on `8132127` (28 August 2026).

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files start with a historical banner. Index: [DOCS.md](DOCS.md).

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`)
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a per-event SHA-256 of the event’s own fields (not a chain, not durable)
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness; Elixir has Mix modules; SQL floor is Python ([floors/README.md](floors/README.md))
- Docker Compose / GitHub Actions **files**; CI and CD are green on `8132127`
- **1,537 tests passing**, 1 skipped (`pytest tests/`; GitHub Actions CI green)
- `start.sh`, `start.bat`, `start.command`

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | Default compose `SECRET_KEY` is a placeholder. State is in-memory. |
| 99% system coverage | `coverage.json` is 98.7% of **tracked** statements and skips `design_analyzer.py` plus `integrated_specs/` (~5,548 lines). |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,364 lines** (bytes were originally counted as lines). |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR / PWA / native apps | Browser only. No WebXR, no `manifest.json`, no service worker. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results (`design_analyzer.py` is a real exception). |
| Durable audit / hash chain | Memory dict. Parent event **ids**, not parent hashes. |
| Security CI as a gate | `safety` and `bandit` cannot fail the job (`\|\| true`). |
| CD | Green on `8132127` ([run 33172395561](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33172395561)). Uses `docker compose`. Default `SECRET_KEY` is still a placeholder. |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Local + GitHub pytest | 1,537 passed, 1 skipped | Collected tests |
| `def test_` grep | 1,572 | Functions defined, not the same as collected |
| `coverage.json` totals | 6,354 / 6,438 statements (98.7%) | Only files pytest was told to measure |
| Omitted Python | ~5,548 lines | `design_analyzer.py` + `integrated_specs/` at `8132127` |
| `src/` Python lines | 23,196 total / 17,943 non-comment | Whole tree, not coverage |
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
2. Persist audit events; hash each event over the **previous hash**.
3. Replace template TODOs with real generation, or stop claiming a pipeline.
4. Include every `src/` module in coverage reports.
5. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Write a test that fails first.
2. Update this file if a limitation is removed.
3. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) if a public claim changes verdict.
4. Do not describe planned work as shipped.
