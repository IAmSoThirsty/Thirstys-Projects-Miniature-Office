# Known limitations

**Status: experimental prototype.** Operator-facing limitation list. Evidence: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Measured 28 August 2026 on parent `f24ae5c` (code pin `ffd9b5e`) plus the CI/docs repair on this pin.

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
- Docker Compose / GitHub Actions **files**; security job uses bandit `-ll` + `pip-audit` (no `|| true`). CD green on `f24ae5c`
- **1,567 tests passing**, 1 skipped
- `start.sh`, `start.bat`, `start.command`
- Production refuses placeholder `SECRET_KEY` values. Compose has no default secret
- `/health` is a liveness probe (HTTP 200)
- Python **3.10+** (pytest 9.0.3)

## What is not implemented (but was claimed)

| Topic | Reality |
| --- | --- |
| Production deployment | World state is in-memory. IDE API is open unless `MO_IDE_TOKEN` is set. |
| 99% system coverage | Fresh `pytest --cov=src` is 7,493 / 7,749 imported statements (96.70%) and still omits `integrated_specs/`. |
| 49,741-line civilization module | `src/core/code_civilization.py` is **1,421 lines** (bytes were originally counted as lines). |
| Spec-faithful generated code | Python generation is an identity transform. Non-Python tests are not executed. |
| Native 30-language runtime | 28 toy directories. SQL has a schema file; department logic is Python. |
| VR | Browser + PWA only. No WebXR. |
| 23+ SOLID / 17 smells | 5 patterns + 4 anti-patterns from AST walks. |
| Tamper-proof ledger | Hashing exists. HMAC is optional. |
| Python 3.9 | pytest 9.0.3 requires 3.10+. The 3.9+ badge was false. |
| CI on `f24ae5c` | Failed ([33208378504](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33208378504)). This pin repairs the two causes. Actions on this SHA not yet observed. |

## Code generation pipeline

Steps 1–6 exist as Python methods. Python:

- Implementation sprint writes `result = data`
- Testing mandate writes pytest and **runs it** in a temp directory
- Other languages still skip execution

## Test coverage (do not mix these numbers)

| Source | Number | Meaning |
| --- | --- | --- |
| Independent pytest | 1,567 passed, 1 skipped | Collected tests |
| `def test_` grep | 1,602 | Functions defined, not the same as collected |
| Fresh `pytest --cov=src` | 7,493 / 7,749 statements (96.70%) | Imported modules only; still omits `integrated_specs/` |
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
- Bandit `-ll`: clean (B104 nosec on `run_server`)
- pip-audit: clean on this pin
- `MO_IDE_TOKEN` required when `FLASK_ENV=production`
- `datetime.utcnow` has been replaced with `datetime.now(timezone.utc)`

## Roadmap (not done)

1. Observe GitHub Actions on this SHA and move CI/CD to **Holds** if the unit-test and security jobs are green.
2. Turn HMAC on by default in compose via a generated key.
3. Replace identity codegen with spec-mapped generators, or stop listing a pipeline.
4. Include every `src/` module in coverage reports.
5. Archive historical `*_COMPLETE.md` files so they cannot be cited even below the banner.

## Contributing

When you add behavior:

1. Add a test that would fail if the route or module is not wired.
2. Update [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) in the same change if a headline number moves.
3. Do not describe planned work as done.
