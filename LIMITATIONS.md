# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Measured 28 August 2026 on `a41e1f8`.

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files start with a historical banner. Index: [DOCS.md](DOCS.md).

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`) with file tree, textarea editor, and terminal
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a SHA-256 **chain** (`prev_hash` + parent hashes). Optional unsigned JSONL when `MO_DATA_DIR` is set. Not signed, not a ledger
- Jailed workspace filesystem (`src/workspace/fs.py`) and no-shell terminal (`src/runtime/terminal.py`) via `/api/ide/*`
- Template code generation for Python / JavaScript / Rust snippets
- 28 language-floor directories of uneven completeness; each README is a toy banner; SQL floor is Python ([floors/README.md](floors/README.md))
- Docker Compose / GitHub Actions **files**; unit tests green on `a41e1f8`; CI security job **failed** (bandit B104); CD docker probe **failed**
- **1,558 tests passing**, 1 skipped (independent pytest of `a41e1f8`)
- `start.sh`, `start.bat`, `start.command`
- Production refuses placeholder `SECRET_KEY` values. Compose has no default secret

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | World state is in-memory. API has no auth. CD compose probe fails. Workspace volume is not writable by the image user. |
| 99% system coverage | Fresh `pytest --cov=src` is 7,194 / 7,364 imported statements (97.69%) and still omits `integrated_specs/`. |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,364 lines** (bytes were originally counted as lines). |
| Executed tests of generated code | Pipeline comment: “we assume tests pass.” |
| Native 30-language runtime | SQL floor is Python. Several floors are JSON-RPC toys, not compilers. |
| VR / PWA / native apps | Browser only. No WebXR, no `manifest.json`, no service worker. |
| Pattern / flow / metrics analysis | Detector and analyzer modules return empty or constant results (`design_analyzer.py` is a real exception). |
| Tamper-proof ledger | Hashing exists. JSONL persist is optional and **unsigned**. |
| Security CI as a clean gate | Bandit **fails** the job (B104 bind `0.0.0.0`). `safety` still cannot fail (`\|\| true`). |
| CI / CD on HEAD | Unit tests green. CI red on `a41e1f8` ([33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573)). CD red ([33201115545](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115545)). |

## Code generation pipeline

Steps 1–6 exist as Python methods. They are a **template printer**:

- Implementation sprint writes `TODO: Implement actual logic`
- Testing mandate writes `assert result is not None`
- Manager seal does not run the generated tests

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Independent pytest of `a41e1f8` | 1,558 passed, 1 skipped | Collected tests |
| GitHub Actions unit tests on `a41e1f8` | Success (3.9–3.12) | Same tree, matrix job |
| GitHub Actions CI on `a41e1f8` | Failure [run 33201115573](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33201115573) | Bandit B104 |
| `def test_` grep | 1,591 | Functions defined, not the same as collected |
| Fresh `pytest --cov=src` | 7,194 / 7,364 statements (97.69%) | Imported modules only; still omits `integrated_specs/` |
| Omitted Python | `integrated_specs/` (~4,215 lines) | Not imported by the test run |
| `src/` Python lines | 23,876 total / 18,542 non-comment | Whole tree, not coverage |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Historical |

There is no single 99% of the whole tree.

## Persistence

- World / simulation / registries: in-process. Restart drops them.
- Workspace files: on disk under `MO_WORKSPACE` (default `./user_workspace`).
- Audit chain: in-process unless `MO_DATA_DIR` is set, in which case `audit.jsonl` is appended. Unsigned.

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose has no default; production refuses placeholders; `.env.example` still has one
- Bandit: can fail the job, currently does (B104)
- Safety: informational only (`|| true`)
- No authentication on the API, including the terminal route
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Either bind explicitly and `# nosec B104` with a reason, or accept that bandit stays red. Make `safety` a gate instead of `|| true`.
2. Point CD `test-docker` at `/api/ide/health` and make `./user_workspace` writable by the image user.
3. Sign the audit chain. Hashing over `prev_hash` already exists; JSONL is not a signature.
4. Replace template TODOs with real generation, or stop claiming a pipeline.
5. Include every `src/` module in coverage reports.
6. Add authentication before treating `/api/ide/terminal` as a product surface.
7. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Add a test that would fail if the route or module is not wired.
2. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) in the same change if a headline number moves.
3. Do not describe planned work as done.
