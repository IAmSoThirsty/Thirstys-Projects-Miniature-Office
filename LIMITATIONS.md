# Known limitations

**Status: experimental prototype.** This file is the operator-facing limitation list. For claim-by-claim evidence see [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Numbers below were measured on commit `537c469` (28 August 2026).

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status.

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`)
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- Audit events with a per-event SHA-256 of the event’s own fields (not a chain, not durable)
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness
- Docker / docker-compose / GitHub Actions **files**
- 1,570 unit-test functions under `tests/` (presence ≠ CI green)

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | No evidence of a healthy CD. Default compose `SECRET_KEY` is a placeholder. State is in-memory. |
| 99% system coverage | `coverage.json` is 98.7% of **tracked** statements and skips `design_analyzer.py` plus `integrated_specs/` (5,346 lines). |
| 49,741-line civilization module | `src/core/code_civilization.py` is 1,233 lines. |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | Elixir has no `.ex` sources. SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR / `start.command` | Browser only. `start.command` is absent. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results. |
| Durable audit / hash chain | Memory dict. Parent event **ids**, not parent hashes. |
| Clean CI | Last 15 `CI - Test and Lint` runs failed. `safety` and `bandit` cannot fail the job (`\|\| true`). |
| `datetime.utcnow` cleanup | Still present in nine `src/` files including `audit.py`. |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

LIMITATIONS previously said both “Phase 1 complete” and “code generation is not operational.” The second statement is the true one for *execution*; the first is true only for *having functions named after the steps*.

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| `coverage.json` totals | 6,354 / 6,438 statements (98.7%) | Only files pytest was told to measure |
| Omitted Python | 5,346 lines | `design_analyzer.py` + `integrated_specs/` |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Earlier snapshot, still in that file |
| README (old) | 1,537 tests, 99% | Inflated / unreconciled |

There is no single 99% of the whole tree.

## Persistence

Everything lives in the process. Restarting the server drops world state, audit events, and generated artifacts unless they were written to disk by something else (they are not).

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose default is insecure
- Dependency/security CI: informational only
- No authentication on the API

## Roadmap (not done)

1. Make CI actually pass (black/isort/pytest) and fail on security findings.
2. Persist audit events; hash each event over the **previous hash**.
3. Replace template TODOs with real generation, or stop claiming a pipeline.
4. Either implement missing floor languages or mark those directories as stubs in `floors/README.md`.
5. Include every `src/` module in coverage reports.
6. Delete or archive agent-generated `*_COMPLETE.md` certificates so they cannot be cited as fact.

## Contributing

When you add behavior:

1. Write a test that fails first.
2. Update this file if a limitation is removed.
3. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) if a public claim changes verdict.
4. Do not describe planned work as shipped.
