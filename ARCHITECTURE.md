# Miniature Office - Architecture Documentation

> **Design notes, not a completion certificate.** This file describes intended architecture. Measured status is [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). The running system is an experimental Flask prototype with in-memory state and template codegen. Do not read the layers below as shipped capabilities.

## System Overview

The intended product is a spatial office metaphor for software work. The running tree is an experimental Flask prototype, not a completed “Civilization-Tier Cognitive IDE.” The layers below describe that intent.

## Core Innovation: Spatial Cognition

Traditional IDEs organize code through:
- Files and folders
- Tabs and windows
- Text-based navigation

Miniature Office organizes code through:
- **Spatial floors** (departments by language/domain)
- **Office rooms** (teams and projects)
- **Agent entities** (autonomous workers with roles)
- **Physical metaphors** (supply store, elevators, meeting rooms)

## Architecture Layers

### Layer 1: Entity Ontology (`src/core/entity.py`)

**Shipped module:**
- 7 `EntityType` values and 8 `RelationType` values
- In-memory `EntityRegistry` / `GlobalRegistry` with `threading.RLock`
- `declare_relationship()` appends a `Relationship` dataclass. `Department.add_agent` calls it as a side effect
- There is **no** runtime gate that refuses interaction when no relationship was declared
- `Manager` subclasses `Agent` and registers as `EntityType.AGENT`. Default `init_simulation()` has **0** `EntityType.MANAGER` objects. The enum value exists; the shipped Manager class does not use it

**Intended, not implemented:** a relationship matrix that blocks undeclared interaction. A distinct runtime Manager entity type.

All objects inherit from `Entity` with formal types:
- **Architectures** - Structural blueprints
- **Departments** - Language/runtime domains
- **Agents** - Autonomous workers
- **Managers** - Meta-agents for consensus
- **Tools** - Compilers, linters, MCP servers
- **Artifacts** - Code outputs, documents
- **Contracts** - Inter-department interfaces

### Layer 2: Audit chain (`src/core/audit.py`)

**Shipped module:**
- SHA-256 of each event's fields plus `prev_hash` (previous event, or 64 zero hex digits for genesis) and parent hashes
- Optional HMAC-SHA256 over the content hash when `MO_AUDIT_HMAC_KEY` or a real `SECRET_KEY` is set
- Optional JSONL append when `MO_DATA_DIR` / `persist_path` is set
- In-memory by default; restart drops the chain unless JSONL persistence is on
- Not a public ledger, not PKI, not tamper-proof on every read

**Intended, not implemented:** an immutable cryptographic ledger with tamper detection on every read.

### Layer 3: Mission Logic (`src/core/mission.py`)

**Directive Tree:**
```
User Intent
  └── Architect Intent
       └── Task Nodes
```

Each node has:
- **Preconditions** - Must be true to start
- **Postconditions** - Must be true when done
- **Acceptance Criteria** - For production readiness

**Task Lifecycle:**
```
Scheduled → InReview → Blocked → Approval → Merged → Deployed
```

**Meeting System:** Tasks with ambiguity ≥ threshold trigger meetings that produce Decision Transcripts.

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

**Capability Profiles:** Each agent has:
- Languages (e.g., Python, Rust)
- Tools (e.g., pytest, cargo)
- Domains (e.g., backend, frontend)
- Skills (e.g., testing, security)
- Security clearance (1-5)

**Consensus System:**
- Managers initiate consensus decisions
- Agents vote with weights
- Threshold determines outcome (default: 2/3 majority)
- Overrides are logged (nothing silently overrides)

### Layer 5: Department Management (`src/departments/department.py`)

**Shipped module:**
- `Department.get_missing_roles()` checks five required roles: architect, builder, verifier, security, doc_agent. **Manager is not required.**
- `DepartmentRegistry.register_department()` calls `auto_spawn_assistants()` for those missing roles. Assistants are added to the **department**, not to an office
- Default Flask world (`init_simulation()` in `src/server/app.py`) seeds **Python and JavaScript** departments only. Python gets `office-1` with `office.manager = Alice` (`mgr-001`). **`office-1.agents` is `[]`** — `init_simulation()` never calls `Office.add_agent`. JavaScript gets auto-spawned assistants, no office, no manager
- Independent seed on `7542ad6`: 11 `EntityType.AGENT` (10 assistants + Alice), 0 `EntityType.MANAGER`
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

**Intended, not implemented:** database or file persistence of world state; ticking department-level assistants that were never added to an office.

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

**Agent Execution:**
1. Check if agent has task
2. Verify capabilities cover preconditions
3. Execute or request support
4. Errors trigger blocking

**Manager Decision:**
1. Review tasks in approval state
2. Initiate consensus if needed
3. Approve or reject based on voting
4. Transition task state

### Layer 10: API Server (`src/server/app.py`)

**Shipped:** Flask + Flask-SocketIO, **74** `@app.route` entries (67 in `app.py` + 7 `/api/ide/*`). World state is in-memory.

**REST Endpoints (subset of the 74):**
- `GET /api/world/state` - Current simulation state
- `POST /api/world/step` - Advance one tick
- `POST /api/world/start` - Start continuous simulation
- `POST /api/world/stop` - Stop simulation
- `GET /api/agents` - List all agents
- `GET /api/tasks` - List all tasks
- `GET /api/departments` - List departments
- `GET /api/supply-store` - Tool inventory
- `GET /api/audit/events` - Audit trail
- `GET /health` - liveness 200
- `GET /api/ide/*` - jailed workspace / editor / terminal (token-gated when `MO_IDE_TOKEN` is set)

**WebSocket Events:**
- `tick_start` - Tick begins
- `tick_end` - Tick completes
- `state_update` - emitted on `request_state`, not automatically on every tick

### Layer 11: Spatial UI (`src/client/index.html`)

**Shipped client** (green-on-navy Flask HTML, not a Vault-Tec product):
- Color palette in the file is orange `#ff9f00` and green `#00ff41` on navy `#0d1f2a`
- Courier New monospace
- World canvas is `fillRect` / `strokeRect` / `fillText` rectangles, not pixel-art sprites
- No scanline or CRT shader
- Textarea editor, file tree, HTTP terminal form — not Monaco, not LSP

**Intended, not implemented:** a richer spatial / pixel-art office visualization.

**Components that exist as HTML** (`src/client/index.html`; names are the `<h2>` / button labels):
1. **World Canvas** — `fillRect` rectangles for floors and offices
2. **Simulation** — buttons **STEP / START / STOP / REFRESH** (not a “Control Panel”)
3. **Metrics** — labels Floors / Agents / Tasks / Tools (not “Metrics Dashboard”)
4. **Agents** (not “Agent List”)
5. **Log** (not “Event Log”)

## Design Principles

### 1. Law of Least Ambiguity
Every interface must resolve unambiguously before use. No implicit assumptions.

### 2. Decoupling Principle
Departments integrate only through formal contracts. No direct coupling.

### 3. Safety First Doctrine
Security constraints are first-class citizens, not add-ons.

### 4. Economic Resource Allocation
Compute and agent time are finite resources with budgeting.

## Data Flow Example

**User wants to implement authentication:**

1. **Directive Created:** User intent → Architect intent → Task nodes
2. **Task Assigned:** Manager finds idle Builder agent with matching capabilities
3. **Agent Executes:** Builder checks preconditions, starts work
4. **State Transition:** Task moves from Scheduled → InReview
5. **Verification:** Verifier agent runs tests (postconditions)
6. **Security Review:** Security agent performs threat analysis
7. **Meeting (if needed):** If ambiguity ≥ threshold, meeting produces Decision Transcript
8. **Consensus:** Manager initiates consensus vote
9. **Approval:** If 2/3 agents approve → Task transitions to Merged
10. **Audit Trail:** Every step logged with causality links

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
- **Tools:** Trust score 0.0-1.0, security rating 1-5
- **Agents:** Security clearance 1-5
- **Operations:** Logged with cryptographic hashes

### Audit Integrity
- SHA-256 chain per event (shipped)
- HMAC tag only when a real key is set
- Not tamper detection on every read; not an immutable public ledger

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
- **Consensus Calculation:** in-process walk of the current agent list
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
