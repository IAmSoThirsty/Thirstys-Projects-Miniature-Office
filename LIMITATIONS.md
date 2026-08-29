# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Independently re-measured 28 August 2026 on **code pin** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) (last `src/` / `tests/` change). CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) and CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) green on that pin. Later docs-only commits are identical in `src/` and `tests/` and also green, including `1a103bf` ([CI 33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008), [CD 33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012)) and `32a70dc` ([CI 33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458), [CD 33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461)). Later docs-only `a0910d4` is also green ([CI 33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717), [CD 33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743)), as is `32b08d8` ([CI 33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624), [CD 33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630)). Independent pytest on then-HEAD `32a70dc` (29 August 2026): **1,573 passed**, 1 skipped, 13.18s; coverage XML **7,494 / 7,749** (96.71%).

Do not treat `IMPLEMENTATION_COMPLETE*.md`, `PRODUCTION_READY.md`, or `MAXIMUM_ALLOWED_*.md` as status. Those files start with a historical banner. Index: [DOCS.md](DOCS.md).

## What is implemented

- Flask app + Socket.IO (`src/server/app.py`), static client (`src/client/index.html`) with file tree, textarea editor, and terminal
- PWA shell: `manifest.json` + `sw.js`. No WebXR
- Entity registry, departments, agents, supply store, task state machine — in-process Python objects
- `threading.RLock` on `EntityRegistry` and `GlobalRegistry`
- Audit events with a SHA-256 **chain**. Optional HMAC-SHA256 when `MO_AUDIT_HMAC_KEY` or a non-placeholder `SECRET_KEY` is set
- Jailed workspace filesystem (`src/workspace/fs.py`) and no-shell terminal (`src/runtime/terminal.py`) via `/api/ide/*`, token-gated when `MO_IDE_TOKEN` is set
- Template code generation; Python tests of generated code **are executed**
- AST-backed pattern / flow / metrics / dependency analyzers (5 patterns, 4 anti-patterns)
- 28 language-floor directories; SQL floor includes `schema.sql` ([floors/README.md](floors/README.md))
- Docker Compose / GitHub Actions **files**; security job uses bandit `-ll` + `pip-audit` (no `|| true`)
- **1,573 tests passing**, 1 skipped
- `start.sh`, `start.bat`, `start.command`
- Production refuses placeholder `SECRET_KEY` values. Compose has no default secret
- `/health` is a liveness probe (HTTP 200)
- Python **3.10+** (`pytest==9.0.3` does not install on 3.9)
- CI and CD **green** on code pin `fdd9762` and on later docs-only commits

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | World state is in-memory. IDE API is open unless `MO_IDE_TOKEN` is set. |
| 99% system coverage | Fresh `pytest --cov=src` is 7,494 / 7,749 imported statements (96.71%) and still omits `integrated_specs/`. |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,421 lines** (bytes were originally counted as lines). |
| Spec-faithful generated code | Python generation is an identity transform. Non-Python tests are not executed. |
| Native 30-language runtime | 28 toy directories. SQL has a schema file; department logic is Python. |
| VR | Browser + PWA only. No WebXR. |
| 23+ SOLID / 17 smells | 5 patterns + 4 anti-patterns from AST walks. |
| Tamper-proof ledger | Hashing exists. HMAC is optional. |
| Hardened Docker | CD `test-docker` green. World is in-memory. Workflow `chmod 777`s host dirs. |

## Code generation pipeline

Steps 1–6 exist as Python methods. Python:

- Implementation sprint writes `result = data`
- Testing mandate writes pytest and **runs it** in a temp directory
- Other languages still skip execution

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Independent pytest | 1,573 passed, 1 skipped | Collected tests on code pin `fdd9762` |
| Anchored `def test_` grep | 1,606 | `^\s*def test_` in `tests/` (not the same as collected) |
| Unanchored `def test_` in `tests/` | 1,608 | Includes comment/string hits. Not 1,612. |
| Fresh `pytest --cov=src` | 7,494 / 7,749 statements (96.71%) | Imported modules only; still omits `integrated_specs/` |
| `src/` Python lines | 24,441 total / 19,058 non-comment | Whole tree, not coverage |
| PRODUCTION_READY.md (old) | 22 tests, 32% | Historical |

There is no single 99% of the whole tree.

## Persistence

- World / simulation / registries: in-process. Restart drops them.
- Workspace files: on disk under `MO_WORKSPACE` (default `./user_workspace`).
- Audit chain: in-process unless `MO_DATA_DIR` is set, in which case `audit.jsonl` is appended. HMAC-tagged when a real key is set.

## Security

- Non-root Docker user: yes
- Security headers module: yes
- SECRET_KEY: compose has no default; production refuses placeholders; `.env.example` still has one
- Bandit `-ll`: clean (B104 nosec on `run_server`). JSON dump in CI also uses `-ll`.
- pip-audit: clean on `fdd9762`
- `MO_IDE_TOKEN` required when `FLASK_ENV=production`
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Docker stays Partial until the stack is more than a compose healthcheck (in-memory world, `chmod 777`).
2. Turn HMAC on by default in compose via a generated key.
3. Replace identity codegen with spec-mapped generators, or stop listing a pipeline.
4. Include every `src/` module in coverage reports.
5. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Add a test that would fail if the route or module is not wired.
2. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) in the same change if a headline number moves.
3. Do not describe planned work as done.
