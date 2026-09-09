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

All objects in the system inherit from `Entity` with formal types:
- **Architectures** - Structural blueprints
- **Departments** - Language/runtime domains
- **Agents** - Autonomous workers
- **Managers** - Meta-agents for consensus
- **Tools** - Compilers, linters, MCP servers
- **Artifacts** - Code outputs, documents
- **Contracts** - Inter-department interfaces

**Relationship Matrix:** Entities must declare relationships before interaction.

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

Each floor has a department representing a language domain (Python, JavaScript, etc.).

**Auto-staffing:** If any required role is missing, assistant agents are auto-spawned.

### Layer 6: Tool & Supply Store (`src/tools/supply_store.py`)

Tools have metadata:
- Tag (compiler, linter, test framework, etc.)
- Version (semver)
- Trust score (0.0-1.0)
- Security rating (1-5)
- Capabilities set

**Checkout Protocol:**
1. Agent requests tool
2. System checks capabilities match
3. If requires justification, agent must provide
4. Tool marked unavailable while checked out
5. Agent returns tool when done

### Layer 7: Contract System (`src/interfaces/contract.py`)

**Contract Syntax:**
```
Contract <Name> {
    API: /<path>(<params>) -> <return>
    Version: <semver>
    Fails: <error_codes>
}
```

**Elevator Protocol:**
- Carries contracts between departments
- Performs automated compatibility checks
- Records invocation telemetry
- Enforces no implicit coupling

### Layer 8: World Structure (`src/core/world.py`)

```
World
  └── Floor (one per language)
       └── Office (team within department)
            ├── Manager
            └── Agents
```

**JSON Schema Compliant:** All entities serialize to formal schema.

### Layer 9: Simulation Engine (`src/core/simulation.py`)

**Tick-based Processing:**
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
    persistState(world)
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

**REST Endpoints:**
- `GET /api/world/state` - Current simulation state
- `POST /api/world/step` - Advance one tick
- `POST /api/world/start` - Start continuous simulation
- `POST /api/world/stop` - Stop simulation
- `GET /api/agents` - List all agents
- `GET /api/tasks` - List all tasks
- `GET /api/departments` - List departments
- `GET /api/supply-store` - Tool inventory
- `GET /api/audit/events` - Audit trail

**WebSocket Events:**
- `tick_start` - Tick begins
- `tick_end` - Tick completes
- `state_update` - State changed

### Layer 11: Spatial UI (`src/client/index.html`)

**Shipped client** (green-on-navy Flask HTML, not a Vault-Tec product):
- Color palette in the file is orange `#ff9f00` and green `#00ff41` on navy `#0d1f2a`
- Courier New monospace
- World canvas is `fillRect` / `strokeRect` / `fillText` rectangles, not pixel-art sprites
- No scanline or CRT shader
- Textarea editor, file tree, HTTP terminal form — not Monaco, not LSP

**Intended, not implemented:** a richer spatial / pixel-art office visualization.

**Components that exist as HTML:**
1. **World Canvas** - colored rectangles for floors and offices
2. **Control Panel** - simulation controls
3. **Metrics Dashboard** - real-time counts
4. **Agent List** - live agent status
5. **Event Log** - scrolling audit events

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

### Horizontal Scaling
- Each department can run on separate worker
- Contract invocations are async-ready
- Audit log can be sharded by time window

### Vertical Scaling
- Simulation tick rate is configurable
- Agent pool sizes are dynamic
- Tool concurrency limits prevent resource exhaustion

### Governance Scaling
- Codex itself is versioned
- Agents can propose amendments
- Meta-agents evaluate proposals
- Backward compatibility enforced

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
- Tools require matching agent capabilities
- Justification required for sensitive operations
- Manager approval for production transitions

## Performance Characteristics

- **Simulation Tick:** ~10-50ms depending on entity count
- **Audit Log Write:** O(1) append
- **Causality Query:** O(log n) with indexing
- **Consensus Calculation:** O(voters) linear
- **State Serialization:** O(entities) but lazy

## Testing Strategy

1. **Unit Tests:** Each system layer independently
2. **Integration Tests:** Cross-layer interactions
3. **Simulation Tests:** Full tick cycles
4. **UI Tests:** Browser automation
5. **Audit Tests:** Verify immutability and integrity
6. **Consensus Tests:** Edge cases in voting

## Troubleshooting

**Import Errors:** Ensure Python path includes project root
**Port Conflicts:** Change port in `run.py`
**Memory Growth:** Audit log can be archived after N events
**Slow Ticks:** Reduce agent count or tick frequency

## Contributing

See main README for contribution guidelines. Key points:
- Follow the Codex principles
- All changes must be auditable
- New agents must have capability profiles
- Contracts required for new integrations
