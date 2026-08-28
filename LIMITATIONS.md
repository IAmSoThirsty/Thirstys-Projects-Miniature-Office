# Known limitations

**Status: experimental prototype.** This file is the operator-facing limitation list. For claim-by-claim evidence see [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files now start with a historical banner.

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`)
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a per-event SHA-256 of the event’s own fields (not a chain, not durable)
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness; Elixir has Mix modules
- Docker / docker-compose / GitHub Actions **files**
- **1,537 tests passing**, 1 skipped (`pytest tests/`, Python 3.10)
- `start.sh`, `start.bat`, `start.command`

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | Default compose `SECRET_KEY` is a placeholder. State is in-memory. |
| 99% system coverage | `coverage.json` is 98.7% of **tracked** statements and skips `design_analyzer.py` plus `integrated_specs/` (5,346 lines). |
| 49,741-line civilization module | `src/core/code_civilization.py` is 1,233 lines. |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR | Browser only. No WebXR. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results (`design_analyzer.py` is a real exception). |
| Durable audit / hash chain | Memory dict. Parent event **ids**, not parent hashes. |
| Security CI as a gate | `safety` and `bandit` cannot fail the job (`\|\| true`). |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

LIMITATIONS previously said both “Phase 1 complete” and “code generation is not operational.” The second statement is the true one for *execution*; the first is true only for *having functions named after the steps*.

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Local pytest (28 Aug 2026) | 1,537 passed, 1 skipped | Collected tests, not `def test_` grep |
| `coverage.json` totals | 6,354 / 6,438 statements (98.7%) | Only files pytest was told to measure |
| Omitted Python | 5,346 lines | `design_analyzer.py` + `integrated_specs/` |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Earlier snapshot, still in that file |
| README (old) | 1,537 tests, 99% | Test count now holds; 99% does not |

There is no single 99% of the whole tree.

## Persistence

Everything lives in the process. Restarting the server drops world state, audit events, and generated artifacts unless they were written to disk by something else (they are not).

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose default is insecure
- Dependency/security CI: informational only
- No authentication on the API
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Confirm GitHub Actions is green on the repaired workflow (Black/isort/pytest across 3.9–3.12).
2. Fail the job on security findings instead of `|| true`.
3. Persist audit events; hash each event over the **previous hash**.
4. Replace template TODOs with real generation, or stop claiming a pipeline.
5. Include every `src/` module in coverage reports.
6. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Write a test that fails first.
2. Update this file if a limitation is removed.
3. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) if a public claim changes verdict.
4. Do not describe planned work as shipped.
