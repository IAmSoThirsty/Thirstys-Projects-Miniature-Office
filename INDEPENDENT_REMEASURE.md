# Independent remeasure log

**Rule:** a claim is true only if the tree implements it.

**Code pin:** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — last commit that changed `src/` or `tests/`. This file is a measurement log. It does **not** retarget the pin. Do not record “docs commit on `main` at this writing.”

Score remains **9 hold / 6 partial / 1 inflated / 3 false** of 19.

## 10 September 2026 18:12 UTC — observed main `e3d316e` (pytest re-run; live JSON Cognitive IDE / bundle complete / health running)

Independent clone of live main [`e3d316e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/e3d316e7243ec003381a81251c5d41b131091755) (PR [#56](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/56), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `e3d316e` itself. Honesty PRs [#52](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/52)–[#55](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/55) were closed unmerged when #56 landed.

Present-tense miss on `e3d316e`:

1. `GET /api` returns `"name": "Miniature Office - Cognitive IDE"` and `"description": "A spatialized, agent-orchestrated development environment"`. Canonical README status is experimental Flask prototype.
2. `GET /health` body `"simulation": "running"` / `"status": "healthy"` means the global object exists (lazy init). Independent `GET /api/world/state` `"is_running"` is **false** until START.
3. `GET /api/canonical-bundle` `is_complete: true` — `verify_bundle_completeness()` only checks 27 slots are not `None`. Empty archives still count. Report says “Complete: Yes”.
4. Charter `is_immutable: true`. `digital_signature` is `hashlib.sha256(b"charter-001").hexdigest()`. `verify_signature` always returns True.
5. Purpose lock `overall_locked: true` with `subsystems_checked: 0`. Authority ledger grants 0.
6. Consigliere JSON: `can_alter_execution` / `can_issue_commands` / `can_manage_agents` are methods that `return True`. Head of Security JSON: `can_force_rearchitecture` / `can_freeze_building` true. `src/client/index.html` never calls those routes. Tick does not import them.
7. Shipped client loads Socket.IO from `https://cdn.socket.io/4.5.4/socket.io.min.js`. START live refresh is `tick_end` then HTTP GET. STEP / REFRESH are same-origin `fetch`.
8. Security Model still said “Operations: Logged with cryptographic hashes.” Only `AuditLog.log_event` writes the chain.

Independent `init_simulation()` + `sim.tick()`:

- 11 `EntityType.AGENT` (10 assistants + Alice); **0** `EntityType.MANAGER`
- Python `office-1.manager` = Alice; **`office-1.agents == []`**
- All 11 agents stay `idle` after 1 tick
- Tools: Python Interpreter, PyTest Framework
- Registered tasks: **0**

| Metric | Value |
| --- | --- |
| Observed main | [`e3d316e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/e3d316e7243ec003381a81251c5d41b131091755) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| Default `EntityType.MANAGER` objects | **0** |
| Default `EntityType.AGENT` objects | **11** |
| `office-1.agents` | **[]** |
| Registered tasks (`GET /api/tasks`) | **[]** |
| `GET /api` name | Miniature Office - Cognitive IDE |
| `GET /health` simulation | **running** (object exists) |
| `GET /api/world/state` is_running | **false** |
| Canonical bundle is_complete | **true** (27 non-None slots) |
| Charter verify_signature | **always True** |
| Purpose-lock subsystems_checked | **0** |
| Consigliere / Security UI chrome | **none** |
| Socket.IO client | `cdn.socket.io/4.5.4` |
| Anchored `def test_` in `tests/` | 1,606 |
| Pytest | **1,573 passed**, 1 skipped, **13.83s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) matching the pin |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| CI on `e3d316e` | [34507228912](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34507228912) succeeded |
| CD on `e3d316e` | [34507228822](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34507228822) succeeded |

Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Open Dependabot PR [#13](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/13) (`flatted` in toy `floors/typescript`) is outside the Python pip-audit / bandit `-ll` gate.

## 10 September 2026 17:15 UTC — observed main `88e23a5` (pytest re-run; Core Innovation / MCP tools / ghost task audit)

Independent clone of live main [`88e23a5`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/88e23a5d830f5a5634f4e57a45f4de58c073ae9f) (PR [#51](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/51), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `88e23a5` itself.

Open honesty PRs [#52](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/52)–[#55](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/55) already record Layer 3 meetings / unregistered `task-001`, Layer 4 empty domains / no tick consensus, Layer 9 unread execution path, and Design Principles / scarcity / canvas Agents: 0. Present-tense miss beyond those: [ARCHITECTURE.md](ARCHITECTURE.md) Core Innovation still said Miniature Office “organizes code through” autonomous workers, elevators, and meeting rooms. Layer 1 listed Tools as “Compilers, linters, MCP servers.” Independent seed supply store is Python Interpreter (`COMPILER`) + PyTest Framework (`TEST_FRAMEWORK`). `EntityType.ARCHITECTURE` / `CONTRACT` / `MANAGER` / registered `ARTIFACT` are **0**. Constructing unregistered `task-001` still writes `directive_created` + `task_state_changed` targeting `task-001` while `GET /api/tasks` is `[]`. Performance still said consensus walks “the current agent list”; `process_manager` walks `managed_agents` (default `[]`).

Independent `init_simulation()` + `sim.tick()`:

- 11 `EntityType.AGENT` (10 assistants + Alice); **0** `EntityType.MANAGER`
- Python `office-1.manager` = Alice; **`office-1.agents == []`**
- All 11 agents stay `idle` after 1 tick
- All 11 `capabilities.domains == set()`
- Alice capabilities empty, clearance 1; `managed_agents == []`
- Tools: Python Interpreter, PyTest Framework
- Assistant names use `role.value.title()`, so **Assistant Doc_Agent**
- Registered tasks: **0**. Meeting transcripts: **0**. Resource spend: **0**
- Audit after init: `entity_created` 30, `agent_action` 11, `directive_created` 1, `task_state_changed` 1 (ghost `task-001`)
- After tick: +2 `agent_action` (`state_persisted` etc.); still 11 idle
- `src.core.simulation` / `src.server.app` do not import `scarcity_economics`
- WORLD canvas `Agents:` **0** (`office.roles`); Metrics Agents **11**

| Metric | Value |
| --- | --- |
| Observed main | [`88e23a5`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/88e23a5d830f5a5634f4e57a45f4de58c073ae9f) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| Entity types / relations | 7 / 8 |
| Default `EntityType.MANAGER` objects | **0** |
| Default `EntityType.AGENT` objects | **11** |
| Default `EntityType.ARCHITECTURE` / `CONTRACT` | **0** / **0** |
| `office-1.agents` | **[]** |
| Registered tasks (`GET /api/tasks`) | **[]** |
| Ghost `task-001` audit events | `directive_created` + `task_state_changed` |
| Tick imports `scarcity_economics` | **no** |
| Resource spend on `sim.tick()` | **0** |
| WORLD canvas `Agents:` | **0** (`office.roles`) |
| Metrics Agents | **11** (`GET /api/agents`) |
| UI simulation buttons | STEP / START / STOP / REFRESH |
| UI metric labels | Floors / Agents / Tasks / Tools |
| Docker gunicorn workers | **4** (split in-memory world) |
| Checkout capability match | **comment only** |
| Default supply-store MCP / linter Tool | **no** |
| Anchored `def test_` in `tests/` | 1,606 |
| Pytest | **1,573 passed**, 1 skipped, **12.57s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) matching the pin |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| CI on `88e23a5` | [34488611556](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34488611556) succeeded |
| CD on `88e23a5` | [34488611517](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34488611517) succeeded |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 10 September 2026 14:20 UTC — observed main `d976c5d` (pytest re-run; gunicorn workers / Layer 11 chrome)

Independent clone of live main [`d976c5d`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d976c5d0b096c6e6afb677faacbc6abcb7e5f86e) (PR [#50](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/50), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `d976c5d` itself.

Present-tense miss: [ARCHITECTURE.md](ARCHITECTURE.md) Layer 11 still named chrome “Control Panel”, “Metrics Dashboard”, “Agent List”, “Event Log” after PR #50 recorded the shipped labels. Scaling still said the engine is one Python process. Dockerfile CMD is `gunicorn --workers 4 --worker-class eventlet src.server.app:app`. Each worker has its own in-memory `simulation`. There is no Socket.IO message queue. Layer 6 / Security Model said checkout checks capabilities; `check_out_tool` only requires the agent id to exist.

Independent `init_simulation()` + `sim.step()`:

- 11 `EntityType.AGENT` (10 assistants + Alice); **0** `EntityType.MANAGER`
- Python `office-1.manager` = Alice; **`office-1.agents == []`**
- All 11 agents stay `idle` after 1 tick
- Tools: Python Interpreter, PyTest Framework
- Assistant names use `role.value.title()`, so **Assistant Doc_Agent**

| Metric | Value |
| --- | --- |
| Observed main | [`d976c5d`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d976c5d0b096c6e6afb677faacbc6abcb7e5f86e) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| Entity types / relations | 7 / 8 |
| Default `EntityType.MANAGER` objects | **0** |
| Default `EntityType.AGENT` objects | **11** |
| `office-1.agents` | **[]** |
| UI simulation buttons | STEP / START / STOP / REFRESH |
| UI metric labels | Floors / Agents / Tasks / Tools |
| Docker gunicorn workers | **4** (split in-memory world) |
| Checkout capability match | **comment only** |
| Anchored `def test_` in `tests/` | 1,606 |
| Pytest | **1,573 passed**, 1 skipped, **12.73s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| CI on `d976c5d` | [34481698942](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34481698942) succeeded |
| CD on `d976c5d` | [34481698924](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34481698924) succeeded |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 10 September 2026 13:15 UTC — observed main `7542ad6` (pytest re-run; UI chrome)


Independent clone of live main [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) (PR [#49](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/49), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `7542ad6` itself.

Present-tense miss: after PR #50 recorded the empty `office-1.agents` seed, [QUICKSTART.md](QUICKSTART.md) and [GETTING_STARTED.md](GETTING_STARTED.md) still described UI chrome the HTML does not ship.

Shipped `src/client/index.html` (right panel):

- Buttons: **STEP**, **START**, **STOP**, **REFRESH**
- Heading **Metrics** with labels Floors / Agents / Tasks / Tools
- Heading **Agents**
- Heading **Log**

QUICKSTART on main / earlier PR #50 still said “STEP (+1 Tick)”, “REFRESH STATE”, “World Metrics”, “Tools Available”, “Active Agents”, “Event Log.” GETTING_STARTED “What Now?” told operators to “Watch the simulation.” Independent `init_simulation()` + `sim.step()`:

- 11 `EntityType.AGENT` (10 assistants + Alice); **0** `EntityType.MANAGER`
- Python `office-1.manager` = Alice; **`office-1.agents == []`**
- All 11 agents stay `idle` after 1 tick
- Tools: Python Interpreter, PyTest Framework
- Assistant names use `role.value.title()`, so **Assistant Doc_Agent**

| Metric | Value |
| --- | --- |
| Observed main | [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| Entity types / relations | 7 / 8 |
| Default `EntityType.MANAGER` objects | **0** |
| Default `EntityType.AGENT` objects | **11** |
| `office-1.agents` | **[]** |
| UI simulation buttons | STEP / START / STOP / REFRESH |
| UI metric labels | Floors / Agents / Tasks / Tools |
| Anchored `def test_` in `tests/` | 1,606 |
| Pytest | **1,573 passed**, 1 skipped, **12.56s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| CI on `7542ad6` | [34418505373](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505373) succeeded |
| CD on `7542ad6` | [34418505473](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505473) succeeded |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 10 September 2026 12:20 UTC — observed main `7542ad6` (pytest re-run; office.agents)

Independent clone of live main [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) (PR [#49](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/49), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `7542ad6` itself.

Present-tense miss: even after correcting “one Manager per department,” [QUICKSTART.md](QUICKSTART.md) still nested the five Python assistants under `office-1`. Independent `init_simulation()`:

- Python `office-1.manager` is Alice (`mgr-001`); **`office-1.agents == []`**
- `init_simulation()` never calls `Office.add_agent`
- `OfficeProcessor.process_office` walks `office.get_agents()` then `process_manager`. Default assistants are not ticked (status unchanged after 1 tick)
- `GET /api/agents` returns 11 `EntityType.AGENT` (10 assistants + Alice). `EntityType.MANAGER` count is **0** (`Manager` subclasses `Agent`)
- JavaScript: 5 department assistants, no office, no manager
- Tools: Python Interpreter, PyTest Framework

| Metric | Value |
| --- | --- |
| Observed main | [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| Entity types / relations | 7 / 8 |
| Default `EntityType.MANAGER` objects | **0** |
| Default `EntityType.AGENT` objects | **11** |
| `office-1.agents` | **[]** |
| Anchored `def test_` in `tests/` | 1,606 |
| Pytest | **1,573 passed**, 1 skipped, **12.54s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| CI on `7542ad6` | [34418505373](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505373) succeeded |
| CD on `7542ad6` | [34418505473](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505473) succeeded |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 10 September 2026 11:20 UTC — observed main `7542ad6` (pytest re-run)

Independent clone of live main [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) (PR [#49](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/49), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `7542ad6` itself.

Present-tense miss: [QUICKSTART.md](QUICKSTART.md) Example Workflow still said two departments, “each fully staffed with 5 required roles,” plus “one Manager per department.” `src/server/app.py` `init_simulation()` auto-spawns the five required roles (architect, builder, verifier, security, doc_agent) when a department is registered. Only the Python floor gets `office-1` and Manager Alice (`mgr-001`). The JavaScript floor has no office and no manager. `GET /api/agents` lists `EntityType.AGENT`; `Manager` subclasses `Agent` and registers as that type, so Alice appears in the list. [ARCHITECTURE.md](ARCHITECTURE.md) Layer 5 now records that seed.

| Metric | Value |
| --- | --- |
| Observed main | [`7542ad6`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/7542ad64de7c20d06e530c8f340d6e69d06893fc) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.90s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | flask / prototype / agents / experimental |
| Start scripts activate a venv | **no** (`python3 run.py` / `python run.py`) |
| Default world managers | **1** (Python Alice only; not one per department) |
| CI on `7542ad6` | [34418505373](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505373) succeeded |
| CD on `7542ad6` | [34418505473](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34418505473) succeeded |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 23:45 UTC — observed main `d5de225` (pytest re-run)


Independent clone of live main [`d5de225`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d5de2256c2a5bdb4a85f6db11e0aab1c4e9a62e6) (PR [#48](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/48), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `d5de225` itself.

Present-tense miss: [INSTALL.md](INSTALL.md) Method 3 (Termux) still told operators to `pkg install python` and `pip install -r requirements.txt` without the Python **3.10+** gate that `install.sh` / `install.ps1` / INSTALL troubleshooting require (`pytest==9.0.3` does not install on 3.9). [QUICKSTART.md](QUICKSTART.md) clone/pip path also omitted 3.10+. Termux is not a supported platform.

| Metric | Value |
| --- | --- |
| Observed main | [`d5de225`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d5de2256c2a5bdb4a85f6db11e0aab1c4e9a62e6) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.24s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | flask / prototype / agents / experimental |
| Start scripts activate a venv | **no** (`python3 run.py` / `python run.py`) |
| Termux / QUICKSTART 3.10+ gate | **missing** at clone (fixed in this pass) |
| CI on `d5de225` | [34417563379](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34417563379) succeeded |
| CD on `d5de225` | [34417563253](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34417563253) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 23:27 UTC — observed main `d433ff2` (pytest re-run)

Independent clone of live main [`d433ff2`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d433ff233796dbf52d97bbecc94e2cb490cd2502) (PR [#47](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/47), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `d433ff2` itself.

Present-tense miss: [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) still said start scripts “Activates the venv and runs `run.py`” after PR #45 recorded that the installer does not create a venv. `start.sh` runs `python3 run.py`; `start.bat` runs `python run.py`; `start.command` execs `start.sh`. [QUICK_REFERENCE.md](QUICK_REFERENCE.md) still said “access it from ANY device on your network.” `install.sh` still printed `brew install python3` after [INSTALL.md](INSTALL.md) required 3.10+ (`pytest==9.0.3` does not install on 3.9). EASY_ACCESS now leads with experimental-prototype status.

| Metric | Value |
| --- | --- |
| Observed main | [`d433ff2`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d433ff233796dbf52d97bbecc94e2cb490cd2502) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.68s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | flask / prototype / agents / experimental |
| Start scripts activate a venv | **no** (`python3 run.py` / `python run.py`) |
| CI on `d433ff2` | [34416782846](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34416782846) succeeded |
| CD on `d433ff2` | [34416782810](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34416782810) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 23:18 UTC — observed main `99ea1b5` (pytest re-run)

Independent clone of live main started at [`f25adcf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f25adcf8602663631acc54e7f1777b536fada945); after squash-merge of PR [#46](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/46) observed main is [`99ea1b5`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/99ea1b55c2f80adcce1d0e0ad9ed33ddf8af7f56) (docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `f25adcf` itself (identical `src/`/`tests/` to `99ea1b5`).

Present-tense miss: [INSTALL.md](INSTALL.md) still said “**Edit the run.py file** or use environment variables” for bind address after [`.env.example`](.env.example) (PR #45) listed `HOST` / `PORT` as unread. It also led without experimental-prototype status, called the pip scripts a “One-Click Installer”, and said the app is accessible from “any device with a modern web browser.” [ARCHITECTURE.md](ARCHITECTURE.md) Vertical Scaling still said “Simulation tick rate is configurable (`tick_duration_ms`, default 100)” after Layer 9 / Performance recorded Flask `init_simulation()` hardcoding 1000ms and `TICK_DURATION_MS` unread. GETTING_STARTED troubleshooting now names Python 3.10+ and the unread `HOST` / `PORT` vars.

| Metric | Value |
| --- | --- |
| Observed main | [`99ea1b5`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/99ea1b55c2f80adcce1d0e0ad9ed33ddf8af7f56) |
| Pytest tree | [`f25adcf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f25adcf8602663631acc54e7f1777b536fada945) (src/tests identical) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.98s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | flask / prototype / agents / experimental |
| CI on `f25adcf` | [34412193707](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193707) succeeded |
| CD on `f25adcf` | [34412193733](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193733) succeeded |
| CI on `99ea1b5` | [34416318379](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34416318379) succeeded |
| CD on `99ea1b5` | [34416318389](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34416318389) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 22:33 UTC — observed main `f25adcf` (pytest re-run)

Independent clone of live main [`f25adcf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f25adcf8602663631acc54e7f1777b536fada945) (PR [#45](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/45), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. Pytest ran on `f25adcf` itself.

Present-tense miss: [GETTING_STARTED.md](GETTING_STARTED.md) opened “Welcome to Miniature Office! This guide will help you get started no matter what device you're using.” Other operator how-tos (QUICKSTART, INSTALL, QUICK_REFERENCE) lead with experimental / not production-ready / not WebXR. GETTING_STARTED now does too. Lint/vulns Holds is unchanged: `pip-audit` of `requirements.txt` and `bandit -ll` on `src/`. Open Dependabot PR [#13](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/13) (`flatted` 3.3.3 → 3.4.2, CWE-1321) is in toy `floors/typescript` and is not that Python gate.

| Metric | Value |
| --- | --- |
| Observed main | [`f25adcf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/f25adcf8602663631acc54e7f1777b536fada945) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **7.63s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | flask / prototype / agents / experimental |
| CI on `f25adcf` | [34412193707](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193707) succeeded |
| CD on `f25adcf` | [34412193733](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193733) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 22:22 UTC — observed main `8fdaca0` (pytest re-run)

Independent clone of live main [`8fdaca0`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/8fdaca0d561bc6c572913a460de9e7d5c1409e8f) (PR [#44](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/44), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. `git log fdd9762..HEAD -- src tests` empty. Pytest ran on `8fdaca0` itself.

Present-tense miss: `install.sh` / `install.ps1` still said “Python 3.9 or higher” after README / INSTALL / LIMITATIONS required 3.10+ (`pytest==9.0.3`). [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) said the installer “Creates a venv”; both scripts `pip install -r requirements.txt` into the current interpreter. `ARCHITECTURE.md` Layer 9 / Performance / Troubleshooting treated `SimulationConfig.tick_duration_ms = 100` as the running tick; `src/server/app.py` `init_simulation()` hardcodes `tick_duration_ms=1000`, and `TICK_DURATION_MS` is never `getenv`'d. `.env.example` listed HOST/PORT/WORKERS/TICK_DURATION_MS/AUTO_ASSIGN_TASKS/LOG_FILE; compose passed HOST/PORT/WORKERS/LOG_LEVEL; `run.py` and the Docker gunicorn CMD ignore them. Installer message is now 3.10+ with a version check. `.env.example` and compose only list read vars. Layer 1 now states `declare_relationship()` is a side effect, not a runtime gate.

| Metric | Value |
| --- | --- |
| Observed main | [`8fdaca0`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/8fdaca0d561bc6c572913a460de9e7d5c1409e8f) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.34s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| Installer Python floor | 3.10+ (was 3.9+) |
| Installer venv | none (pip into current interpreter) |
| Flask init tick | 1000ms hardcoded; dataclass default 100ms; env unread |
| CI on `8fdaca0` | [34406238644](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34406238644) succeeded |
| CD on `8fdaca0` | [34406238661](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34406238661) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 21:16 UTC — observed main `d61d253` (pytest re-run)

Independent clone of live main [`d61d253`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d61d253e7e9ebd93d9f5e1c7d7b244e610966d7c) (PR [#43](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/43), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. `git log fdd9762..HEAD -- src tests` empty. Pytest ran on `d61d253` itself.

Present-tense miss: `package.json` keywords still listed `pixel-art` after PR [#41](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/41) established the shipped canvas is `fillRect` / `strokeRect` / `fillText` rectangles, not pixel-art sprites. `ARCHITECTURE.md` Contributing said “See main README for contribution guidelines” and “Follow the Codex principles”; README has no contribution section, there is no `CONTRIBUTING.md`, and historical `*_CODEX.md` files are bannered. Keywords now `flask` / `prototype` / `agents` / `experimental`. Contributing now points at [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

| Metric | Value |
| --- | --- |
| Observed main | [`d61d253`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/d61d253e7e9ebd93d9f5e1c7d7b244e610966d7c) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28, all toy-bannered |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| `k8s/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **13.16s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `package.json` keywords | no longer `pixel-art` |
| CI on `d61d253` | [34393910856](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34393910856) succeeded |
| CD on `d61d253` | [34393910832](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34393910832) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 19:11 UTC — observed main `a153b92` (pytest re-run)

Independent clone of live main [`a153b92`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a153b923db3c0bcf6f92f757fb72c249d59c0808) (PR [#42](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/42), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. `git log fdd9762..HEAD -- src tests` empty. Pytest ran on `a153b92` itself.

Present-tense miss in design notes: `ARCHITECTURE.md` Layer 7 showed a contract DSL the tree does not parse (`Contract <Name> { API: ... }`); `ElevatorProtocol.check_compatibility` is “consumer exists in the registry.” Layer 8 said entities were JSON Schema compliant (`codex/office.json`); there is no `codex/` directory. Layer 9 showed `persistState(world)` as persistence; `SimulationEngine.persist_state` logs an `agent_action` and the world stays in-memory. Performance claimed a ~10–50ms tick and O(log n) causality indexing; `tick_duration_ms` defaults to 100 and events are a list. Testing Strategy said audit tests verify immutability. Banner already said intent-not-done; those layers now match the files.

| Metric | Value |
| --- | --- |
| Observed main | [`a153b92`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a153b923db3c0bcf6f92f757fb72c249d59c0808) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| `codex/` directory | absent |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **12.56s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `k8s/` directory | absent |
| CI on `a153b92` | [34358022421](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34358022421) succeeded |
| CD on `a153b92` | [34358022556](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34358022556) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 13:26 UTC — observed main `a1db8ae` (pytest re-run)

Independent clone of live main [`a1db8ae`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a1db8ae7351fadc8d499a2ee4f764e973bf72c72) (PR [#41](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/41), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. `git log fdd9762..HEAD -- src tests` empty. Pytest ran on `a1db8ae` itself.

Present-tense miss: `DEPLOYMENT.md` still read as a production guide (`docker-compose up -d`, systemd, Kubernetes `k8s/deployment.yaml`) and said `GET /health` returns 503 at startup. Measured: no `k8s/` directory; `/health` is liveness HTTP 200 (`test_health_without_simulation_is_liveness`); `/metrics` is the route that can 503 when `simulation` is None. `ARCHITECTURE.md` Layer 2 named `src/core/audit.py` an immutable ledger with tamper detection on every read. The module is a SHA-256 chain with optional HMAC, not a ledger.

| Metric | Value |
| --- | --- |
| Observed main | [`a1db8ae`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a1db8ae7351fadc8d499a2ee4f764e973bf72c72) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **13.03s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| `k8s/` directory | absent |
| CI on `a1db8ae` | [34351639115](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34351639115) succeeded |
| CD on `a1db8ae` | [34351639093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34351639093) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 9 September 2026 12:30 UTC — observed main `526721e` (pytest re-run)

Independent clone of live main [`526721e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/526721e2faf1ec81d31229f71e0c377cbf61d78b) (PR [#40](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/40), docs-only). `src/` tree `fafbad684ed9d61bd5fd347098276eeea4b911d3` and `tests/` tree `1ddf08f8a24d9003054c0a395e06c95470009fe0` match code pin `fdd9762`. `git log fdd9762..HEAD -- src tests` empty. Pytest ran on `526721e` itself.

Present-tense miss in design notes: `ARCHITECTURE.md` Layer 11 named `src/client/index.html` and described a Vault-Tec / pixel-art / CRT UI. The shipped canvas is `fillRect` rectangles; there is no scanline shader. Banner already said intent-not-done; Layer 11 now matches the file.

| Metric | Value |
| --- | --- |
| Observed main | [`526721e`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/526721e2faf1ec81d31229f71e0c377cbf61d78b) |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **13.09s** |
| Coverage XML `--cov=src` | **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| Compose `SECRET_KEY` default | none |
| CI on `526721e` | [34347045367](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34347045367) succeeded |
| CD on `526721e` | [34347045102](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34347045102) succeeded |
| Live EC-013 | Verified (experimental prototype; pin `fdd9762`; limitation line still names docs HEAD `268058c`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 6 September 2026 23:10 UTC — observed main `0aaf783` (pytest re-run)

Independent clone of [`0aaf783`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/0aaf7839d6dd2943647a2d0b87da83b2608a1d56) (PR #35, docs-only). `git log fdd9762..HEAD -- src tests` empty. Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **13.09s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `0aaf783` | [33817498446](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33817498446) succeeded |
| CD on `0aaf783` | [33817498306](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33817498306) succeeded |
| Live EC-013 | matches tree (experimental prototype; pin `fdd9762`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 30 August 2026 — docs successor `268058c` (pytest re-run)

Independent clone of [`268058c`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/268058c1b17408691807c20b01e2ba91fa54f4ce). `git log fdd9762..HEAD -- src tests` empty. Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 (33 module-level + 1,573 class methods) |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | **1,619** (not 1,626) |
| Pytest | **1,573 passed**, 1 skipped, **13.75s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high |
| Bandit low findings | 13 |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `268058c` | [33263264093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264093) succeeded |
| CD on `268058c` | [33263264131](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264131) succeeded |

Headline metrics match the code pin. The only live numeric error in canonical files was `claims.json` `def_test_grep_whole_repo_unanchored` **1626**, which independent `*.py` grep (excluding `.git`) measures as **1619**. That field is corrected in the same docs change. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

GitHub's HTML README on 30 August 2026 matches the raw experimental-prototype text (the earlier `27d7fdf` HTML-staleness note was clone-time history).


## 29 August 2026 — then-HEAD `32a70dc` (pytest run)

Clone of [`32a70dc`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/32a70dc7a73d3143a0f06c36361ff2b64a1a9124). `git log fdd9762..HEAD -- src tests` empty.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Pytest | **1,573 passed**, 1 skipped, **13.18s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `32a70dc` | [33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458) succeeded |
| CD on `32a70dc` | [33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461) succeeded |

These numbers match [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) for the code pin.

## 29 August 2026 — docs successor `a0910d4` (counts only)

Clone of [`a0910d4`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a0910d4768f0a9ce05c65b91761ef7dc62e54110). Counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `def test_` = 1,606. Matches the code pin. Pytest was **not** re-run in this clone (last pytest: `32a70dc`). CI [33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717) **succeeded**. CD [33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743) **succeeded**. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**.



## 29 August 2026 — docs successor `32b08d8` (counts only)

Clone of [`32b08d8`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/32b08d8a7cf5808bd188077ff0b32db56c69ee7a) (PR #31 squash). Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`. Counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `^\s*def test_` = 1,606 (33 module-level + 1,573 class methods), unanchored = 1,608. Pytest was **not** re-run (last pytest: `32a70dc`). CI [33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624) **succeeded**. CD [33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630) **succeeded**. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. GitHub's HTML landing page was still serving a stale `27d7fdf` README at the time of this clone; the API and raw files at `32b08d8` are the measured tree.

## 29 August 2026 — then-HEAD `c783357` (counts only)

Clone of [`c783357`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/c7833576720d381322a2186a26b610715fd6f388). Counts matched the pin. Pytest was **not** run in that clone.

## Outside this repository (updated 6 September 2026 23:10 UTC)

Live [thirstysystems.com/claims](https://www.thirstysystems.com/claims) EC-013 now reports that the README no longer badges Production Ready and pins `CLAIMS_AUDIT.md` at `fdd9762`. [/systems/miniature-office](https://www.thirstysystems.com/systems/miniature-office) now cites 1,573 tests and 28 floors. The February `537c469` / PRODUCTION READY sentence is historical. Production-ready remains false.
