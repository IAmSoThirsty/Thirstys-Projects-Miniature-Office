# Miniature Office - Architecture Documentation

> **Design notes, not a completion certificate.** This file describes intended architecture. Measured status is [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). The running system is an experimental Flask prototype with in-memory state and template codegen. Do not read the layers below as shipped capabilities.

## System Overview

The intended product is a spatial office metaphor for software work. The running tree is an experimental Flask prototype, not a completed “Civilization-Tier Cognitive IDE.” The layers below describe that intent.

## Core Innovation: Spatial Cognition

Traditional IDEs organize code through:
- Files and folders
- Tabs and windows
- Text-based navigation

**Intended, not the default seed.** The running tree is a Flask HTML page. The WORLD canvas paints `fillRect` rectangles for two floors and one office.

Miniature Office *intends* to organize code through:
- **Spatial floors** (departments by language/domain) — default seed has two in-memory floors (Python, JavaScript), not the 28 toy `floors/` directories
- **Office rooms** (teams and projects) — only Python `office-1`; `office.agents` is `[]`
- **Agent entities** (autonomous workers with roles) — 11 `EntityType.AGENT` objects. Independent `sim.tick()` leaves them `idle`. They are not autonomous in the default seed
- **Physical metaphors** (supply store, elevators, meeting rooms) — supply store is a Python object with **2** tools. `ElevatorProtocol.check_compatibility` is “consumer exists in the registry.” `MeetingSystem.hold_meeting()` is a library call the tick never makes. There is no meeting-room rectangle on the canvas

## Architecture Layers

### Layer 1: Entity Ontology (`src/core/entity.py`)

**Shipped module:**
- 7 `EntityType` values and 8 `RelationType` values
- In-memory `EntityRegistry` / `GlobalRegistry` with `threading.RLock`
- `declare_relationship()` appends a `Relationship` dataclass. `Department.add_agent` calls it as a side effect
- There is **no** runtime gate that refuses interaction when no relationship was declared
- `Manager` subclasses `Agent` and registers as `EntityType.AGENT`. Default `init_simulation()` has **0** `EntityType.MANAGER` objects. The enum value exists; the shipped Manager class does not use it

**Intended, not implemented:** a relationship matrix that blocks undeclared interaction. A distinct runtime Manager entity type.

All objects inherit from `Entity` with formal types (enum values; default seed counts in parentheses):
- **Architectures** - Structural blueprints (**0** objects)
- **Departments** - Language/runtime domains (**2**: Python Development, Frontend Development)
- **Agents** - Autonomous workers (**11** `EntityType.AGENT`; all `idle` after `sim.tick()`)
- **Managers** - Meta-agents for consensus (enum exists; shipped `Manager` class registers as `AGENT`; **0** `EntityType.MANAGER`)
- **Tools** - Compilers, linters, MCP servers are `ToolTag` values. Default seed tools are **Python Interpreter** (`COMPILER`) and **PyTest Framework** (`TEST_FRAMEWORK`). No linter Tool and no MCP Tool are in the supply store. Verifier capability sets include the string `"linter"`; that is not a Tool entity
- **Artifacts** - Code outputs, documents, and `Task`/`Directive` objects (**0** registered; unregistered `task-001` still writes audit events)
- **Contracts** - Inter-department interfaces (**0** objects)

### Layer 2: Audit chain (`src/core/audit.py`)

**Shipped module:**
- SHA-256 of each event's fields plus `prev_hash` (previous event, or 64 zero hex digits for genesis) and parent hashes
- Optional HMAC-SHA256 over the content hash when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set
- Optional JSONL append when `MO_DATA_DIR` / `persist_path` is set
- In-memory by default; restart drops the chain unless JSONL persistence is on
- Not a public ledger, not PKI, not tamper-proof on every read

**Intended, not implemented:** an immutable cryptographic ledger with tamper detection on every read.

### Layer 3: Mission Logic (`src/core/mission.py`)

**Shipped module:**
- `TaskState` enum and `Directive` / `Task` objects with optional precondition / postcondition / acceptance callables
- `Task` does **not** auto-register in `EntityRegistry` (Agent / Tool / Department do). `GET /api/tasks` lists `EntityType.ARTIFACT` instances that are `Task`
- Default `init_simulation()` constructs `task-001` ("Implement User Authentication") as a **local**. It is never `registry.register`'d and never `assign_task`'d. After init returns, `GET /api/tasks` is `[]`. The Metrics **Tasks** count is 0
- Constructing that local still logs `directive_created` and `task_state_changed` targeting `task-001`. The audit JSONL/API can show a task that `GET /api/tasks` cannot
- Preconditions added as description strings without a `checker` callable leave `is_satisfied` False. `all([])` is True only when the list is empty
- `MeetingSystem.hold_meeting()` exists and writes a `DecisionTranscript` (unit-tested). The tick loop does **not** call it
- `AgentExecutionEngine.process_agent`: if `task.needs_meeting()`, sets `agent.status = "in_meeting"` and returns. No transcript
- `SimulationConfig.auto_assign_tasks` and `auto_resolve_meetings` are stored on the dataclass and **never read**. Flask init hardcodes `auto_assign_tasks=True` with no effect
- Alice's `managed_agents` is `[]`. `ManagerDecisionProtocol.process_manager` walks that list

**Intended, not implemented:** a registered, assigned default task; tick-loop meetings that call `hold_meeting()`; auto-assign from the config flag.

**Directive Tree (in-process objects, not a running pipeline):**
```
User Intent
  └── Architect Intent
       └── Task Nodes
```

Each node *can* have:
- **Preconditions** - optional callables; no checker means `is_satisfied` stays False
- **Postconditions** - same
- **Acceptance Criteria** - same

**Task Lifecycle (enum + `can_transition_to`, not the default seed):**
```
Scheduled → InReview → Blocked → Approval → Merged → Deployed
```

**Meeting System (library, not the tick):** `hold_meeting()` produces a Decision Transcript when a caller invokes it. Ambiguity ≥ threshold does not, by itself, hold a meeting.

### Layer 4: Agent System (`src/agents/agent.py`)

**Shipped module:**
- `AgentRole` enum: architect, builder, verifier, security, doc_agent, manager
- Required department roles are the first five. **Manager is not required**
- `Agent.__init__` always registers `EntityType.AGENT`. `Manager` inherits that
- CapabilityProfile, ConsensusVote, ConsensusDecision exist as in-process dataclasses

**Intended, not implemented:** a live consensus loop over default-seed assistants (they are not in `office.agents`, so they are not ticked).

**Required Roles (per department):**
1. **Architect** - Design authority
2. **Builder** - Implementation
3. **Verifier** - Correctness testing
4. **Security** - Threat modeling
5. **DocAgent** - Documentation & communication

**Capability Profiles:** `CapabilityProfile` fields exist. Independent default seed:
- Assistants get `languages={department.domain.lower()}`, role skills, and a clearance. **All 11 `domains == set()`** — `_default_capabilities_for_role` never writes `domains`
- Builder tools `{"compiler", "interpreter"}`; Verifier tools `{"linter", "test_framework"}`. Those are strings on the profile, not Tool checkouts
- Alice (`Manager.__init__`) does **not** call that helper. Her languages/tools/domains/skills are empty; clearance **1**; `managed_agents == []`

**Consensus System (library, not the default tick):**
- `ConsensusVote` / `ConsensusDecision` dataclasses exist. `issue_override` writes `override_log`
- `ManagerDecisionProtocol.process_manager` walks `manager.managed_agents`. Default Alice list is `[]`, so the tick does not initiate consensus, does not call `assign_task`, and does not call `can_handle_task`
- Threshold default 2/3 exists on the in-process helper

### Layer 5: Department Management (`src/departments/department.py`)

**Shipped module:**
- `Department.get_missing_roles()` checks five required roles: architect, builder, verifier, security, doc_agent. **Manager is not required.**
- `DepartmentRegistry.register_department()` calls `auto_spawn_assistants()` for those missing roles. Assistants are added to the **department**, not to an office
- Default Flask world (`init_simulation()` in `src/server/app.py`) seeds **Python and JavaScript** departments only. Python gets `office-1` with `office.manager = Alice` (`mgr-001`). **`office-1.agents` is `[]`** — `init_simulation()` never calls `Office.add_agent`. JavaScript gets auto-spawned assistants, no office, no manager
- Independent seed: 11 `EntityType.AGENT` (10 assistants + Alice), 0 `EntityType.MANAGER`
- The 28 toy `floors/` directories are not this in-memory world

**Intended, not implemented:** one office and one manager per language floor; assistants sitting in `office.agents` so the tick loop processes them.

Each floor *in the design* has a department representing a language domain.


### Layer 6: Tool & Supply Store (`src/tools/supply_store.py`)

**Shipped module:**
- Tool metadata: tag, version, trust_score, security_rating, capabilities set, `requires_justification`
- `check_out_tool` refuses a missing tool, an unavailable tool, a missing justification when required, and a missing agent id
- The comment “Check agent capabilities match tool requirements” is **not implemented**. There is no comparison of agent capabilities to `tool.metadata_info.capabilities`
- Default seed does not check out tools. Assistants are not ticked

**Intended, not implemented:** capability matching at checkout; default-seed agents using the supply store.

Tools have metadata:
- Tag (compiler, linter, test framework, etc.)
- Version (semver)
- Trust score (0.0-1.0)
- Security rating (1-5)
- Capabilities set

**Checkout Protocol (partial):**
1. Caller invokes `check_out_tool(tool_id, agent_id, justification)`
2. Agent id must exist in the registry. Capabilities are **not** checked
3. If `requires_justification`, a non-empty justification string is required
4. Tool marked unavailable while checked out
5. `check_in_tool` marks it available again

### Layer 7: Contract System (`src/interfaces/contract.py`)

**Shipped module:**
- `Contract` is a Python `Entity` with `APIEndpoint`, `VersionBoundary`, and `FailureMode` dataclasses
- There is no parsed contract DSL. The `Contract <Name> { API: ... }` block below is design prose, not a grammar the tree implements
- `ElevatorProtocol` registers contracts, records `InvocationRecord`s, and treats compatibility as “consumer exists in the registry”
- Not a service mesh. It does not enforce “no implicit coupling”

**Intended, not implemented:** a typed contract language with automated compatibility beyond registry presence.

**Contract Syntax (intent, not a parser):**
```
Contract <Name> {
    API: /<path>(<params>) -> <return>
    Version: <semver>
    Fails: <error_codes>
}
```

### Layer 8: World Structure (`src/core/world.py`)

**Shipped module:**
- `World` / `Floor` / `Office` Python objects with `to_dict()` and `*Schema` dataclasses
- There is **no `codex/` directory** and no JSON Schema files (`codex/office.json` is not in the tree)
- Default world in `src/server/app.py` seeds **Python and JavaScript** floors, not the 28 toy language-floor directories
- Python `office-1.agents` is empty. JavaScript has no office. Assistants live on the department registry

**Intended, not implemented:** JSON Schema-validated world files; every floor has an office whose `agents` list is the department staff.

```
World
  └── Floor (Python + JavaScript in the default seed)
       └── Office (only Python office-1; manager set; agents list empty)
```

### Layer 9: Simulation Engine (`src/core/simulation.py`)

**Shipped module:**
- Tick loop processes floors / offices in-process. `OfficeProcessor.process_office` returns immediately if `office.manager` is missing; otherwise it walks `office.get_agents()` then `process_manager`
- Default seed: `office-1.agents` is empty, so assistants are **not ticked**. Alice still goes through `process_manager`. JavaScript has no office, so its assistants are not ticked either
- `persist_state()` logs an `agent_action` whose data says `state_persisted`. World and registries stay **in-memory**
- Dataclass default is `SimulationConfig.tick_duration_ms = 100`
- The shipped Flask `init_simulation()` in `src/server/app.py` hardcodes `tick_duration_ms=1000` (1 second per tick)
- `.env.example` lists `TICK_DURATION_MS`; that name is **not** `getenv`'d
- `auto_assign_tasks` / `auto_resolve_meetings` are dataclass fields. Flask init sets `auto_assign_tasks=True`. **Neither flag is read.** `OfficeProcessor.process_office` does not assign tasks
- Default seed `task-001` is not in the registry, so there is nothing to assign

**Intended, not implemented:** database or file persistence of world state; ticking department-level assistants that were never added to an office; auto-assign / auto-resolve from the config flags.

**Tick-based Processing (in-process; persist is a log label):**
```python
while world.isActive:
    world.time += 1
    for floor in floors:
        processFloor(floor)
            for office in floor.offices:
                processOffice(office)
                    for agent in office.agents:
                        processAgent(agent)
                    processManager(office.manager)
    persistState(world)  # logs agent_action; does not write the world
```

**Agent Execution (only agents already in `office.agents` with `current_task_id`):**
1. If no current task: return
2. `task.check_preconditions()` — optional callables, not agent-capability matching
3. If `needs_meeting()`: set `in_meeting` and return (no `hold_meeting()`, no transcript)
4. Else SCHEDULED → IN_REVIEW, or IN_REVIEW → APPROVAL when postconditions pass

**Manager Decision (walks `manager.managed_agents`, default `[]`):**
1. Review tasks in approval state on those managed agents
2. Initiate consensus if `is_ready_for_commit()`
3. Manager casts a weight-2 vote and finalizes
4. Transition MERGED only if consensus finalizes

### Layer 10: API Server (`src/server/app.py`)

**Shipped:** Flask + Flask-SocketIO, **74** `@app.route` entries (67 in `app.py` + 7 `/api/ide/*`). World state is in-memory.

**Live JSON that is not the product status.** Independent Flask test client on this tree:

- `GET /api` returns `"name": "Miniature Office - Cognitive IDE"` and `"description": "A spatialized, agent-orchestrated development environment"`. That is a route label. Canonical status is experimental Flask prototype — not a Cognitive IDE
- `GET /health` is HTTP 200 liveness. Body `"simulation": "running"` means the global `simulation` object is not `None` (the handler lazy-inits it). Independent `GET /api/world/state` is `"is_running": false` until `POST /api/world/start`. `"status": "healthy"` is the liveness string, not a production probe
- `GET /api/canonical-bundle` returns `"is_complete": true`, `"missing_artifacts": []`. `verify_bundle_completeness()` only checks that 27 dataclass slots are not `None`. Empty archives still count. The report title is “NON-DESIGN CANONICAL BUNDLE” / “Complete: Yes”
- `GET /api/canonical-bundle/charter` JSON keys are `charter_id`, `version`, `issued_date`, `axioms`, `is_immutable`, `human_readable`. There is **no** `digital_signature` field. The `sha256(b"charter-001")` hex appears only inside `human_readable` (`to_human_readable()`). `CivilizationCharter.verify_signature` **always returns True** and ignores its `public_key` argument
- `GET /api/canonical-bundle/purpose-lock` returns `"overall_locked": true` with `"subsystems_checked": 0`
- `GET /api/canonical-bundle/authority-ledger` returns `total_grants` **0** / `active_grants` **0**
- `GET /api/consigliere` returns `"role": "Chief Operating Executive"` with `can_alter_execution` / `can_issue_commands` / `can_manage_agents` **true**. Those are hardcoded methods that `return True`. `src/client/index.html` never calls `/api/consigliere*`. The tick does not import Consigliere
- `GET /api/security` returns `"role": "Executive Authority - Security Sovereign"` with `can_force_rearchitecture` / `can_freeze_building` **true**, `policies` **3**, lockdowns **0**. No UI chrome. The tick does not import Head of Security
- `POST /api/consigliere/preview` of `"freeze building"` returns canned consequences (`"All execution stops immediately"`, `"No new tasks can start"`, `resource_costs.system_availability: -100`). `preview_consequences()` is a keyword table (`"freeze" in proposed_action.lower()`), not a simulation. After `POST /api/security/lockdown` `scope=building` (`is_active: true`): freeze-protocol stays `is_frozen: false`; STEP still ticks; START returns `success: true`; `POST /api/ide/terminal` `echo hello` exits 0
- `GET /api/agents/mgr-001` is Flask **404** HTML. The shipped route is `GET /api/agents/<id>/status` (Alice `idle`, `current_task_id` null). `POST /api/security/audit` `type=floor` is **400** `Unknown audit type: floor` (only `full_system` / `cross_floor`)

**REST Endpoints (subset of the 74):**
- `GET /api` - JSON index. Names “Cognitive IDE”; does not list the 28 `/api/canonical-bundle*` routes
- `GET /api/world/state` - **HTTP 500** until `/health` lazy-inits. After that: in-memory world (`is_running` is the START loop flag). `world.floors` is **2** (`floor-python` / `floor-javascript`); `office-1.roles` is `[]`
- `GET /metrics` - **503** until lazy-init. HELP `minioffice_floors_total` counts `len(world.floors)` (**2**), not 28 `floors/` dirs
- `POST /api/world/step` - Advance one tick
- `POST /api/world/start` - Start continuous simulation
- `POST /api/world/stop` - Stop simulation
- `GET /api/agents` - List all agents
- `GET /api/tasks` - List registered `Task` artifacts (default `[]`)
- `GET /api/departments` - List departments
- `GET /api/supply-store` - Tool inventory
- `GET /api/audit/events` - Audit trail
- `GET /health` - liveness 200; body `"simulation"` is object-exists, not START
- `GET /api/ide/*` - jailed workspace / editor / terminal (token-gated when `MO_IDE_TOKEN` is set)
- `GET /api/consigliere` / `GET /api/security` / `GET /api/canonical-bundle*` - in-memory JSON views. Not UI chrome. Completeness / immutability / LOCKED are slot defaults, not evidence

**WebSocket Events:**
- `tick_start` - Tick begins (Flask-SocketIO emit from the worker that ran the tick)
- `tick_end` - Tick completes. Shipped `index.html` listens and then **HTTP GET** `/api/world/state`
- `state_update` - emitted on `request_state`, not automatically on every tick. The shipped client does not send `request_state`
- The shipped client loads Socket.IO from `https://cdn.socket.io/4.5.4/socket.io.min.js`. STEP / REFRESH are same-origin `fetch`. START live refresh needs that CDN plus `tick_end`
### Layer 11: Spatial UI (`src/client/index.html`)

**Shipped client** (green-on-navy Flask HTML, not a Vault-Tec product):
- Color palette in the file is orange `#ff9f00` and green `#00ff41` on navy `#0d1f2a`
- Courier New monospace
- World canvas is `fillRect` / `strokeRect` / `fillText` rectangles, not pixel-art sprites
- No scanline or CRT shader
- Textarea editor, file tree, HTTP terminal form — not Monaco, not LSP

**Intended, not implemented:** a richer spatial / pixel-art office visualization.

**Components that exist as HTML** (`src/client/index.html`; names are the `<h2>` / button labels):
1. **World Canvas** — `fillRect` rectangles for floors and offices. Each office box labels `Agents: ` + `office.roles.length`. `Office.to_schema()` sets `roles=self.agents`. Default `office-1.agents` is `[]`, so the canvas paints **Agents: 0**. Metrics **Agents** is `GET /api/agents` (**11**). The WORLD tab does not draw the 10 department assistants or Alice inside the office rectangle
2. **Simulation** — buttons **STEP / START / STOP / REFRESH** (not a “Control Panel”)
3. **Metrics** — labels Floors / Agents / Tasks / Tools (not “Metrics Dashboard”). Default counts: Floors **2**, Agents **11**, Tasks **0**, Tools **2**
4. **Agents** (not “Agent List”) — lists `GET /api/agents` (11 rows, all `idle`)
5. **Log** (not “Event Log”)

## Design Principles

**Shipped vs intended.** The four headings below were written as running laws. They are design prose unless a caller invokes the matching library. Independent `init_simulation()` + `sim.tick()` does not enforce any of them.

### 1. Law of Least Ambiguity

**Intended.** There is no runtime gate that refuses an interface until ambiguity is resolved. `Task.needs_meeting()` is a boolean on an in-process object.

### 2. Decoupling Principle

**Intended.** Departments do not “integrate only through formal contracts.” `ElevatorProtocol.check_compatibility` is “consumer exists in the registry.” Layer 7 already records that it does not enforce “no implicit coupling.” Default `EntityType.CONTRACT` count is **0**.

### 3. Safety First Doctrine

**Partial library.** Tools have `trust_score` / `security_rating`. Agents have `security_clearance`. `check_out_tool` does not compare capabilities. `MO_IDE_TOKEN` gates `/api/ide/*` only when set.

### 4. Economic Resource Allocation

**Library, not the tick.** `src/core/scarcity_economics.py` defines `ResourceType` (`agent_time`, `manager_attention`, `consensus_bandwidth`, `tool_slots`, `simulation_budget`) and a ledger. Unit tests cover the module. `SimulationEngine.tick` and `init_simulation()` do **not** import it. Independent `sim.tick()` does not spend those resources. Default allocations stay 0.

**Intended, not implemented:** tick-time budget consumption that forces judgment.

## Data Flow Example

**Intended pipeline, not the default seed.** `init_simulation()` constructs unregistered `task-001` and never assigns it. Assistants are not in `office.agents`. `auto_assign_tasks` is unread. Independent `sim.tick()` does not run the steps below. Constructing the local task still writes `directive_created` / `task_state_changed` to the audit log.

**User wants to implement authentication (design prose):**

1. **Directive Created:** User intent → Architect intent → Task nodes
2. **Task Assigned:** *Intended.* Manager does not auto-assign. `assign_task` is a method a caller must invoke
3. **Agent Executes:** *Intended.* `process_agent` only runs for agents in `office.agents`
4. **State Transition:** *Intended.* SCHEDULED → IN_REVIEW when a ticked agent has a task
5. **Verification:** *Intended.* No default verifier loop
6. **Security Review:** *Intended.* No default security loop
7. **Meeting (if needed):** *Intended.* Tick only sets `in_meeting`. `hold_meeting()` is a library call
8. **Consensus:** *Intended.* `process_manager` walks `managed_agents` (default `[]`)
9. **Approval:** *Intended.* MERGED only if consensus finalizes
10. **Audit Trail:** SHA-256 chain of events that actually ran. Default init logs `directive_created` + `task_state_changed` for unregistered `task-001`. Default tick logs `state_persisted`

## Scaling Considerations

The shipped `python3 run.py` path is **one Python process** with in-memory world state.

The Docker image CMD is `gunicorn --bind 0.0.0.0:5000 --workers 4 --worker-class eventlet ... src.server.app:app`. That is **four** processes, each with its own `simulation` global. There is no Flask-SocketIO message queue. `POST /api/world/step` on one worker is not visible to `GET /api/world/state` on another. Operator `docker compose up --build` is this 4-worker CMD, not `run.py`.

**Intended, not implemented:**
- Each department on a separate worker
- Async contract invocations as a service mesh
- Audit log sharded by time window
- Multi-world instances

### Vertical Scaling (partial, in-process)

- Simulation tick rate: dataclass default is 100ms; shipped Flask `init_simulation()` hardcodes 1000ms. `TICK_DURATION_MS` is not read
- Agent pool sizes are whatever the in-memory registry holds
- There is no measured production capacity figure

## Future Extensions

1. **MCP Server Integration** - Full Model Context Protocol implementation
2. **Spatial Pathfinding** - Agents physically move through the office
3. **Resource Marketplace** - Agents trade capabilities
4. **Department Mergers** - Dynamic organizational restructuring
5. **AI Planning** - Agents autonomously decompose user intents
6. **Visual Debugging** - Step through causality graph visually
7. **Multi-world Instances** - Parallel simulation universes
8. **Historical Replay** - Time-travel through audit log

## Security Model

### Trust Levels
- **Tools:** Trust score 0.0-1.0, security rating 1-5 (fields on Tool metadata)
- **Agents:** Security clearance 1-5 (Alice default is **1**)
- **Operations:** only events passed to `AuditLog.log_event` join the SHA-256 chain. `GET /api`, `GET /health`, `GET /api/canonical-bundle*`, and `GET /api/consigliere` do **not** write audit events. Charter `verify_signature` always returns True

### Audit Integrity
- SHA-256 chain per logged event (shipped)
- HMAC tag only when a real key is set
- Not tamper detection on every read; not an immutable public ledger
- Canonical-bundle `is_immutable` / purpose-lock `LOCKED` are dataclass defaults on empty in-memory objects, not this chain
### Capability Enforcement
- `check_out_tool` records a `USES` relationship if the agent id exists
- It does **not** compare agent capabilities to `tool.metadata_info.capabilities` (comment-only)
- Justification is required only when `requires_justification` is set
- Manager approval for production transitions is design prose

## Performance Characteristics

These are not SLOs.

- **Simulation Tick:** shipped Flask init hardcodes 1000ms sleep. Dataclass default is 100ms. Neither is a measured SLA. `TICK_DURATION_MS` is not read
- **Audit Log Write:** in-memory list append; optional JSONL when `MO_DATA_DIR` is set
- **Causality Query:** not implemented as O(log n) indexed lookup. Events are a list
- **Consensus Calculation:** `process_manager` walks `manager.managed_agents` (default `[]`). It is not a walk of the current agent list
- **State Serialization:** `to_dict()` of in-memory objects. Not lazy

## Testing Strategy

Measured suite: **1,573 passed**, 1 skipped on code pin `fdd9762`. See [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

1. **Unit Tests:** pytest under `tests/`
2. **Integration Tests:** Flask client tests against in-memory app
3. **Simulation Tests:** tick / step helpers
4. **UI Tests:** not a Playwright/Cypress suite; the browser UI is exercised indirectly
5. **Audit Tests:** hash-chain and optional HMAC tests exist; they do **not** prove immutability
6. **Consensus Tests:** voting helpers in-process

## Troubleshooting

**Import Errors:** Ensure Python path includes project root
**Port Conflicts:** Change port in `run.py`
**Memory Growth:** in-memory audit list grows until process exit. There is no shipped “archive after N events” job.
**Slow Ticks:** shipped Flask init sleeps 1000ms per tick; dataclass default is 100ms. There is no env tuner.

## Contributing

There is no `CONTRIBUTING.md`, and [README.md](README.md) has no contribution section.

Measured status is [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Historical `*_CODEX.md` files are bannered; they are not a current contribution gate. The tree does not require capability profiles or contracts as a merge check.
