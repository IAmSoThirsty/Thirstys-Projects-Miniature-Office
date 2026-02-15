# Thirstys-Projects-Miniature-Office

[![CI Tests](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CI%20-%20Test%20and%20Lint/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![Docker](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CD%20-%20Build%20and%20Deploy/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python 3.9+](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)
[![Test Coverage](https://img.shields.io/badge/coverage-99%25-brightgreen.svg)](tests/)
[![Code Lines](https://img.shields.io/badge/code%20lines-18%2C285-blue.svg)](src/)

---

# OPERATING MODE: MAXIMUM ALLOWED DETAIL

**This document operates in MAXIMUM ALLOWED DETAIL MODE as specified by system directive.**

All permitted technical dimensions are expanded to their maximum level. No intentional summarization. No compression of structure. Complete transparency across all layers, components, dependencies, constraints, failure modes, and operational considerations.

---

## Executive Summary

**A Civilization-Tier Cognitive IDE: Where Software Development Becomes Spatial Simulation**

🚀 **PRODUCTION READY** - Core pipeline fully functional with 99% test coverage (1,537 tests passing)

💻 **RUNS EVERYWHERE** - Desktop, Mobile, VR, Tablets - Access from any device via web browser

🌐 **POLYGLOT ARCHITECTURE** - 30+ language floors, each implemented in its native language with transparent diversity

This is not just an IDE, agent framework, or simulator. It is a **Cognitive Operating Environment for Software Civilization** where code synthesis, review, and integration are modeled as a living organization inside a simulated world with:

- **Formal ontology** with typed entity relationships
- **Immutable audit log** with cryptographic integrity and causality graphs
- **Cognitive contracts** with intent and responsibility as first-class objects
- **Scarcity economics** with finite resources and meaningful budgets
- **Constitutional mutation** with controlled system evolution and mandatory safeguards
- **Multi-language floors** where each department runs in its designated programming language
- **Complete code generation pipeline** from directive to tested artifact

---

## Table of Contents

- [Quick Start](#quick-start)
- [System Architecture - Complete Technical Specification](#system-architecture---complete-technical-specification)
- [Layer-by-Layer Deep Dive](#layer-by-layer-deep-dive)
- [Multi-Language Floor Architecture](#multi-language-floor-architecture)
- [Code Generation Pipeline - Civilization-Tier](#code-generation-pipeline---civilization-tier)
- [Advanced Features - Cognitive Tier](#advanced-features---cognitive-tier)
- [Production Infrastructure](#production-infrastructure)
- [Testing and Quality Assurance](#testing-and-quality-assurance)
- [Dependencies and Cross-Dependencies](#dependencies-and-cross-dependencies)
- [Edge Cases and Failure Modes](#edge-cases-and-failure-modes)
- [Operational Considerations](#operational-considerations)
- [Governance and Lifecycle](#governance-and-lifecycle)
- [API Reference - Complete Endpoint Catalog](#api-reference---complete-endpoint-catalog)
- [Installation - All Platforms](#installation---all-platforms)
- [Documentation Index](#documentation-index)

---

## Quick Start

### 🚀 Super Easy Installation (Recommended for Everyone)

Choose your platform:

**Windows**
```cmd
# Right-click install.ps1 → "Run with PowerShell"
# Then double-click start.bat
```

**macOS**
```bash
./install.sh          # One-time setup
./start.command       # Double-click to start!
```

**Linux**
```bash
./install.sh          # One-time setup
./start.sh           # Run to start
```

**Docker** (All Platforms)
```bash
docker-compose up
```

Then open your browser to `http://localhost:5000`

📱 **Mobile & VR**: See [INSTALL.md](INSTALL.md) for complete device support

---

## System Architecture - Complete Technical Specification

### Architectural Philosophy

The Miniature Office implements a **spatial cognitive metaphor** for software development, transforming traditional file-based organization into a simulated office building where:

- **Code is spatial**: Organized by floors (languages), offices (teams), and rooms (projects)
- **Development is organizational**: Agents perform roles, departments manage domains, managers coordinate
- **History is immutable**: Every action logged with cryptographic integrity
- **Resources are finite**: Agent time, tool slots, and manager attention are bounded
- **Evolution is controlled**: System changes require formal proposals, simulations, and delayed activation

### System Metrics (Current Implementation)

| Metric | Value | Details |
|--------|-------|---------|
| **Total Code Lines** | 18,285 | Python source files in `src/` |
| **Test Suite Size** | 1,537 tests | Comprehensive unit, integration, and simulation tests |
| **Test Coverage** | 99% | All core modules at 100%, overall system at 99% |
| **Language Floors** | 30+ | Each implemented in its native language |
| **Core Modules** | 23 | Including entity, audit, mission, agent, civilization |
| **Analysis Modules** | 7 | AST, semantic, flow, metrics, patterns, dependency, design |
| **API Endpoints** | 45+ | RESTful HTTP + WebSocket events |
| **Entity Types** | 7 | Architecture, Department, Agent, Manager, Tool, Artifact, Contract |
| **Relationship Types** | 8 | Manages, WorksIn, Uses, DependsOn, Implements, Produces, Reviews, IntegratesWith |
| **Event Types** | 13 | Complete audit trail categories |
| **Task States** | 6 | Full lifecycle state machine |
| **Design Patterns Detected** | 23+ | GoF patterns plus architectural patterns |
| **SOLID Principles** | 5 | Complete validation with violation detection |
| **Design Smells Detected** | 17+ | Anti-pattern identification |
| **Dependencies** | 9 | Core Python packages (see requirements.txt) |

---

## Layer-by-Layer Deep Dive

### Layer 1: Entity Ontology (`src/core/entity.py`)

**Purpose**: Formal type system for all objects in the IDE world.

**Implementation Details**:
- **File Size**: 176 lines of code
- **Test Coverage**: 100%
- **Core Classes**: `Entity`, `EntityRegistry`, `Relationship`
- **Enumerations**: `EntityType` (7 types), `RelationType` (8 types)

**Entity Types (Complete Catalog)**:

1. **ARCHITECTURE** (`EntityType.ARCHITECTURE`)
   - Purpose: Structural blueprints and design patterns
   - Metadata: Design decisions, architectural styles, component relationships
   - Relationships: Implemented by artifacts

2. **DEPARTMENT** (`EntityType.DEPARTMENT`)
   - Purpose: Language/runtime domains (floors in the building)
   - Metadata: Language specification, floor number, required roles
   - Relationships: Uses tools, implements contracts, integrates with other departments
   - Auto-staffing: Spawns assistant agents if required roles are missing

3. **AGENT** (`EntityType.AGENT`)
   - Purpose: Autonomous workers with specific capabilities
   - Metadata: Capability profile, security clearance (1-5), languages, tools, domains, skills
   - Relationships: Works in department, uses tools, produces artifacts, reviews artifacts
   - Roles: Architect, Builder, Verifier, Security, DocAgent

4. **MANAGER** (`EntityType.MANAGER`)
   - Purpose: Meta-agents for consensus and coordination
   - Metadata: Attention budget, consensus threshold, managed agents
   - Relationships: Manages agents
   - Responsibilities: Initiate consensus, approve tasks, resolve conflicts

5. **TOOL** (`EntityType.TOOL`)
   - Purpose: Compilers, linters, test frameworks, MCP servers
   - Metadata: Tag (compiler/linter/test/etc.), version (semver), trust score (0.0-1.0), security rating (1-5), capabilities
   - Relationships: Used by agents and departments
   - Checkout Protocol: Requires capability match, justification for sensitive operations, marked unavailable when checked out

6. **ARTIFACT** (`EntityType.ARTIFACT`)
   - Purpose: Code outputs, documents, generated files
   - Metadata: Language, file path, generated code, review status
   - Relationships: Produced by agents, implements architecture, reviewed by agents

7. **CONTRACT** (`EntityType.CONTRACT`)
   - Purpose: Inter-department interfaces (elevator protocol)
   - Metadata: API specification, version, error codes, compatibility matrix
   - Relationships: Implemented by departments
   - Enforcement: No implicit coupling between floors

**Relationship Matrix (Valid Combinations)**:

| Source Type | Target Type | Relation Type | Description |
|-------------|-------------|---------------|-------------|
| DEPARTMENT | TOOL | USES | Department uses tool for operations |
| MANAGER | AGENT | MANAGES | Manager coordinates agent activities |
| AGENT | DEPARTMENT | WORKS_IN | Agent belongs to department |
| AGENT | TOOL | USES | Agent checks out tool |
| AGENT | ARTIFACT | PRODUCES | Agent creates artifact |
| AGENT | ARTIFACT | REVIEWS | Agent reviews artifact |
| DEPARTMENT | CONTRACT | IMPLEMENTS | Department implements contract interface |
| DEPARTMENT | DEPARTMENT | INTEGRATES_WITH | Departments integrate via contract |
| ARTIFACT | ARCHITECTURE | IMPLEMENTS | Artifact realizes architectural design |

**Enforcement Mechanism**:
- Relationships must be declared before interaction
- Registry validates all relationships against matrix
- Undeclared relationships raise `ValueError`
- Complete audit trail of all relationship changes

**Global Registry**:
- Singleton pattern via `get_registry()` function
- Type-indexed for O(1) lookups by entity type
- Relationship validation on every registration
- Thread-safe (single-threaded Python GIL)

**Serialization**:
- Full `to_dict()` method for JSON export
- Includes entity metadata, relationships, timestamps
- ISO 8601 datetime format
- Preserves all relationship metadata

**Usage Example**:
```python
from src.core.entity import Entity, EntityType, RelationType, get_registry

# Create entities
agent = Entity("agent-1", EntityType.AGENT, "Builder Bot")
dept = Entity("dept-python", EntityType.DEPARTMENT, "Python Department")

# Declare relationship
agent.declare_relationship(dept, RelationType.WORKS_IN)

# Register in global registry
registry = get_registry()
registry.register(agent)
registry.register(dept)

# Validate relationship
assert registry.validate_relationship("agent-1", "dept-python", RelationType.WORKS_IN)
```

---

### Layer 2: Immutable Audit Log (`src/core/audit.py`)

**Purpose**: Complete, tamper-proof history of all system actions.

**Implementation Details**:
- **File Size**: 262 lines of code
- **Test Coverage**: 100%
- **Core Classes**: `AuditEvent`, `CausalityGraph`, `AuditLog`
- **Hash Algorithm**: SHA-256 for cryptographic integrity
- **Storage**: In-memory (production would use persistent store)

**Event Types (Complete Catalog)**:

| Event Type | Purpose | Typical Data Fields |
|------------|---------|---------------------|
| `ENTITY_CREATED` | New entity instantiated | entity_type, name, metadata |
| `ENTITY_UPDATED` | Entity modified | changed_fields, old_values, new_values |
| `RELATIONSHIP_DECLARED` | Relationship established | source_id, target_id, relation_type |
| `TASK_STATE_CHANGED` | Task lifecycle transition | from_state, to_state, reason |
| `DIRECTIVE_CREATED` | New directive/task created | level, description, parent_id |
| `CONSENSUS_REACHED` | Manager consensus decision | voters, votes, threshold, outcome |
| `OVERRIDE_ISSUED` | Manager overrides consensus | override_reason, original_outcome |
| `MEETING_HELD` | Meeting produced transcript | participants, ambiguity, resolution |
| `ARTIFACT_PRODUCED` | Code generated | language, artifact_type, file_path |
| `TOOL_CHECKED_OUT` | Agent borrows tool | tool_id, agent_id, justification |
| `CODEX_AMENDMENT` | System rules modified | amendment_type, activation_delay |
| `AGENT_ACTION` | Generic agent operation | action_type, parameters, result |
| `SECURITY_EVENT` | Security-relevant action | event_category, severity, details |

**AuditEvent Structure**:
```python
@dataclass
class AuditEvent:
    event_id: str               # UUID v4
    event_type: EventType       # From enumeration above
    timestamp: datetime         # UTC timestamp (microsecond precision)
    actor_id: Optional[str]     # Entity that caused the event
    target_id: Optional[str]    # Entity affected by the event
    data: Dict[str, Any]        # Event-specific payload
    parent_events: List[str]    # Causality links (parent event IDs)
    _hash: str                  # SHA-256 hash of event content
```

**Cryptographic Integrity**:
- Each event hashed with SHA-256 on creation
- Hash includes: event_id, type, timestamp, actor, target, data, parent events
- JSON serialization with sorted keys for deterministic hashing
- `verify_integrity()` method recomputes hash and compares
- Tampering detection: Any modification invalidates hash

**Causality Graph**:
- Directed Acyclic Graph (DAG) of events
- Each event can have multiple parent events (causes)
- Children index maintained for efficient descendant queries
- Graph operations:
  - `get_lineage(event_id)`: Full ancestry from root to event
  - `get_descendants(event_id)`: All events caused by this event
  - `get_children(event_id)`: Immediate next events
- Complexity: O(log n) with indexing, O(n) worst case for lineage

**Audit Log Indexes**:
- Type index: EventType → List[event_id] for filtering by type
- Actor index: actor_id → List[event_id] for agent activity tracking
- Target index: target_id → List[event_id] for entity history
- All indexes updated atomically with event logging

**Query Operations**:
```python
# Get all task state changes
state_changes = audit_log.get_events_by_type(EventType.TASK_STATE_CHANGED)

# Get complete history of an entity
entity_history = audit_log.get_events_by_target("entity-id")

# Get all actions by an agent
agent_actions = audit_log.get_events_by_actor("agent-id")

# Get change lineage for tracing evolution
lineage = audit_log.get_change_lineage("target-id")
```

**Performance Characteristics**:
- Event logging: O(1) append + O(1) index updates = O(1) total
- Hash calculation: O(n) where n = data size, typically < 1ms
- Integrity verification: O(n) where n = number of events
- Lineage query: O(m × log n) where m = lineage depth, n = total events
- Memory: ~1KB per event (typical), scales linearly

**Serialization Format**:
```json
{
  "event_id": "550e8400-e29b-41d4-a716-446655440000",
  "event_type": "task_state_changed",
  "timestamp": "2026-02-15T16:30:45.123456Z",
  "actor_id": "agent-builder-001",
  "target_id": "task-implement-auth",
  "data": {
    "from_state": "scheduled",
    "to_state": "in_review",
    "reason": "Implementation complete, ready for review"
  },
  "parent_events": ["550e8400-e29b-41d4-a716-446655440001"],
  "hash": "a3f5c8d9e2b1f0a8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8e7f6a5b4"
}
```

**Failure Modes and Recovery**:
- **Hash mismatch**: Event marked as tampered, system alert triggered
- **Orphan events**: Parent event missing, lineage incomplete but event preserved
- **Circular causality**: Prevented by DAG structure, validation on add
- **Memory exhaustion**: Implement event archiving (future), priority retention

**Operational Constraints**:
- **No deletion**: Append-only, events never removed (archiving is external)
- **No modification**: Events immutable after creation
- **Parent verification**: Parent events must exist before child creation
- **Uniqueness**: Event IDs must be globally unique (UUID v4 provides ~122 bits of entropy)

---

### Layer 3: Mission Logic (`src/core/mission.py`)

**Purpose**: Directive trees, task lifecycle, and meeting system for ambiguity resolution.

**Implementation Details**:
- **File Size**: 343 lines of code
- **Test Coverage**: 100%
- **Core Classes**: `Directive`, `Task`, `DecisionTranscript`, `MeetingSystem`, `Condition`, `AcceptanceCriteria`
- **State Machine**: 6 states with validated transitions

**Directive Tree Structure**:

```
User Intent (Level 1: DirectiveLevel.USER_INTENT)
    │
    ├── Architect Intent (Level 2: DirectiveLevel.ARCHITECT_INTENT)
    │       │
    │       ├── Task Node (Level 3: DirectiveLevel.TASK_NODE)
    │       ├── Task Node (Level 3: DirectiveLevel.TASK_NODE)
    │       └── Task Node (Level 3: DirectiveLevel.TASK_NODE)
    │
    └── Architect Intent (Level 2: DirectiveLevel.ARCHITECT_INTENT)
            │
            └── Task Node (Level 3: DirectiveLevel.TASK_NODE)
```

**Directive Formalism** (Codex Section 2.1):

Each directive has:

1. **Preconditions**: Must be true before work begins
   - Checked via `Condition` objects with optional `checker` callable
   - All must be satisfied for task to start
   - Example: "Python 3.9+ available", "Dependencies installed"

2. **Postconditions**: Must be true when work completes
   - Checked via `Condition` objects with optional `checker` callable
   - All must be satisfied for task to move to approval
   - Example: "Tests pass", "Code formatted", "No linting errors"

3. **Acceptance Criteria**: Required for production readiness
   - Checked via `AcceptanceCriteria` objects with optional `validator` callable
   - All must be met for task to merge
   - Example: "Coverage ≥ 80%", "Security scan passed", "Documentation updated"

**Task Lifecycle State Machine**:

```
SCHEDULED ──────► IN_REVIEW ──────► APPROVAL ──────► MERGED ──────► DEPLOYED
    │                 │                 │                │               │
    │                 │                 │                │               │
    └────► BLOCKED ◄──┴─────► BLOCKED ◄─┴──► BLOCKED ◄──┴──► BLOCKED ◄──┘
```

**State Transition Rules**:

| Current State | Allowed Next States | Conditions Required |
|---------------|---------------------|---------------------|
| SCHEDULED | IN_REVIEW, BLOCKED | None (start work) |
| IN_REVIEW | APPROVAL, BLOCKED, SCHEDULED | Postconditions met for APPROVAL |
| BLOCKED | SCHEDULED, IN_REVIEW | Block reason resolved |
| APPROVAL | MERGED, IN_REVIEW, BLOCKED | All criteria met for MERGED |
| MERGED | DEPLOYED, BLOCKED | Deployment preconditions met |
| DEPLOYED | BLOCKED | Rollback if issues detected |

**State Transition Validation**:
- Enforced via `can_transition_to(new_state)` method
- APPROVAL requires all postconditions satisfied
- MERGED requires all preconditions, postconditions, AND acceptance criteria met
- All transitions logged to audit log with reason
- State history maintained in task object

**Ambiguity and Meeting System**:

**Ambiguity Score**: Float 0.0-1.0
- Set via `task.set_ambiguity_score(score)`
- Measures uncertainty or lack of clarity
- Factors: Vague requirements, conflicting constraints, missing information

**Meeting Trigger**: Ambiguity ≥ Threshold (default 0.5)
- Automatic meeting scheduling when `task.needs_meeting()` returns True
- Manager initiates meeting with relevant agents
- Produces first-class `DecisionTranscript` artifact

**Decision Transcript Structure**:
```python
@dataclass
class DecisionTranscript:
    transcript_id: str              # UUID
    task_id: str                    # Task being discussed
    meeting_date: datetime          # When meeting occurred
    participants: List[str]         # Agent IDs who attended
    ambiguity_addressed: str        # What uncertainty was resolved
    decisions_made: List[str]       # Decisions reached
    action_items: List[str]         # Follow-up tasks
    resolution: str                 # Final outcome summary
```

**Meeting System Operations**:
- `hold_meeting(task, participants, ambiguity, decisions, resolution)`: Create transcript
- `get_transcript(transcript_id)`: Retrieve by ID
- `get_task_transcripts(task_id)`: All meetings for a task
- Meetings logged to audit log with MEETING_HELD event
- Task ambiguity score reset to 0.0 after resolution

**Task Management Examples**:

```python
from src.core.mission import Task, TaskState, get_meeting_system

# Create task
task = Task(
    task_id="task-001",
    name="Implement user authentication",
    description="Add JWT-based authentication",
    assigned_agent_id="agent-builder-001"
)

# Add preconditions
task.add_precondition(
    "Flask and PyJWT dependencies installed",
    checker=lambda: check_dependencies_installed(["flask", "pyjwt"])
)

# Add postconditions
task.add_postcondition(
    "All unit tests pass",
    checker=lambda: run_tests() == 0
)

# Add acceptance criteria
task.add_acceptance_criterion(
    "Code coverage ≥ 80%",
    validator=lambda: get_coverage() >= 0.8
)

# Set ambiguity score
task.set_ambiguity_score(0.7)

# Check if meeting needed
if task.needs_meeting(threshold=0.5):
    meeting_system = get_meeting_system()
    transcript = meeting_system.hold_meeting(
        task=task,
        participants=["agent-architect-001", "agent-builder-001"],
        ambiguity_addressed="Clarify JWT token expiration policy",
        decisions_made=["Use 1-hour access tokens", "7-day refresh tokens"],
        resolution="JWT configuration specified"
    )

# Transition through states
task.transition_to(TaskState.IN_REVIEW, "Code complete")
task.transition_to(TaskState.APPROVAL, "Review passed")
task.transition_to(TaskState.MERGED, "All criteria met")
```

**Failure Modes**:
- **Invalid transition**: Returns False, state unchanged, logged
- **Precondition failure**: Task cannot start, remains SCHEDULED
- **Postcondition failure**: Cannot move to APPROVAL, must fix issues
- **Acceptance failure**: Cannot MERGE, additional work required
- **Block with missing reason**: Prevented by API design

**Performance Considerations**:
- State validation: O(1) dictionary lookup
- Condition checking: O(n) where n = number of conditions
- Meeting scheduling: O(1) transcript creation
- History queries: O(m) where m = state history length
- Typical task: < 10 conditions, < 5 state transitions

---

### Layer 4: Agent System (`src/agents/agent.py`)

**Purpose**: Autonomous workers with capability profiles, consensus mechanism, and role-based organization.

**Implementation Details**:
- **File Size**: Integrated with entity system
- **Test Coverage**: 100% for core agent functionality
- **Roles**: 5 required roles per department
- **Capabilities**: Languages, tools, domains, skills, security clearance

**Required Agent Roles** (Per Department):

| Role | Responsibilities | Typical Tools | Security Clearance |
|------|------------------|---------------|-------------------|
| **Architect** | Design authority, architectural decisions, pattern selection | UML tools, diagramming, design analyzers | 4-5 (high) |
| **Builder** | Implementation, code construction, feature development | Compilers, IDEs, package managers | 2-3 (medium) |
| **Verifier** | Testing, correctness validation, quality assurance | Test frameworks, coverage tools, profilers | 2-3 (medium) |
| **Security** | Threat modeling, vulnerability scanning, secure code review | Static analyzers, penetration test tools, security scanners | 4-5 (high) |
| **DocAgent** | Documentation, communication, knowledge management | Documentation generators, wikis, style checkers | 1-2 (low) |

**Capability Profile Structure**:

```python
{
    "languages": ["python", "javascript", "rust"],  # Programming languages
    "tools": ["pytest", "black", "mypy", "cargo"],  # Tool names
    "domains": ["backend", "testing", "security"],  # Domain expertise
    "skills": ["async", "concurrency", "crypto"],   # Technical skills
    "security_clearance": 3                         # 1 (low) to 5 (high)
}
```

**Capability Matching Algorithm**:
- Task requires capabilities: `{"languages": ["python"], "tools": ["pytest"]}`
- Agent has capabilities: `{"languages": ["python", "rust"], "tools": ["pytest", "black"]}`
- Match if agent capabilities ⊇ task requirements
- Complexity: O(n × m) where n = agent caps, m = task requirements

**Consensus System**:

**Participants**:
- Manager: Initiates consensus decision
- Agents: Vote with weighted votes
- Threshold: Determines outcome (default 2/3 majority = 0.67)

**Voting Process**:
1. Manager creates consensus proposal with decision context
2. Agents receive proposal with voting period
3. Each agent casts vote: APPROVE, REJECT, ABSTAIN
4. Votes may have weights based on expertise or seniority
5. Vote tally: sum(weights of APPROVE votes) / sum(all weights)
6. If tally ≥ threshold: APPROVED
7. If tally < threshold: REJECTED

**Consensus Data Structure**:
```python
{
    "proposal_id": "consensus-001",
    "manager_id": "manager-python-dept",
    "decision_context": "Approve task-001 for merge",
    "voters": ["agent-architect", "agent-builder", "agent-verifier"],
    "votes": {
        "agent-architect": {"vote": "APPROVE", "weight": 1.5},
        "agent-builder": {"vote": "APPROVE", "weight": 1.0},
        "agent-verifier": {"vote": "REJECT", "weight": 1.0}
    },
    "threshold": 0.67,
    "outcome": "APPROVED",
    "tally": 0.71,  # (1.5 + 1.0) / (1.5 + 1.0 + 1.0) = 2.5 / 3.5 = 0.71
    "timestamp": "2026-02-15T16:30:00Z"
}
```

**Override Mechanism**:
- Managers can override consensus decisions
- Override reason must be provided
- Logged to audit log with OVERRIDE_ISSUED event
- Override history tracked for governance review
- Nothing silently overrides (transparency principle)

**Auto-Staffing System**:

**Trigger**: Department missing required role
**Process**:
1. Department scans for required roles: [Architect, Builder, Verifier, Security, DocAgent]
2. Check agent registry for agents with matching role
3. If role missing: Spawn assistant agent with basic capabilities
4. Assistant agent assigned to department with WORKS_IN relationship
5. Assistant marked as auto-generated for future optimization

**Assistant Agent Profile**:
```python
{
    "role": "Builder",
    "capabilities": {
        "languages": [department_language],  # e.g., "python"
        "tools": [basic_tools],              # e.g., ["compiler", "formatter"]
        "domains": ["implementation"],
        "skills": ["basic_coding"],
        "security_clearance": 2
    },
    "is_assistant": True,
    "spawned_by": "department-id",
    "spawned_at": "2026-02-15T16:00:00Z"
}
```

**Agent Lifecycle**:
- **Creation**: Registered in entity registry with EntityType.AGENT
- **Assignment**: Declared relationship with department (WORKS_IN)
- **Tool Checkout**: Declares USES relationship with tools
- **Task Execution**: Produces artifacts (PRODUCES relationship)
- **Code Review**: Reviews artifacts (REVIEWS relationship)
- **Retirement**: Removed from department, relationships archived

**Performance Budget**:
- Each agent has time budget (e.g., 100 time units per tick)
- Actions consume budget: Code generation (50), review (20), testing (30)
- When budget exhausted: Agent idle until next tick
- Prevents infinite work in single tick
- Budget reset each simulation tick

**Example Agent Configuration**:

```python
# High-skill architect agent
{
    "entity_id": "agent-architect-senior",
    "name": "Senior Architect Agent",
    "role": "Architect",
    "capabilities": {
        "languages": ["python", "rust", "go", "javascript", "java"],
        "tools": ["uml", "mermaid", "design-analyzer"],
        "domains": ["architecture", "design-patterns", "system-design"],
        "skills": ["SOLID", "GoF-patterns", "DDD", "microservices"],
        "security_clearance": 5
    },
    "time_budget": 150,
    "expertise_level": "senior",
    "vote_weight": 1.5
}
```

**Failure Modes**:
- **Capability mismatch**: Agent cannot execute task, task remains SCHEDULED
- **Tool unavailable**: Agent cannot checkout required tool, task BLOCKED
- **Budget exhaustion**: Agent idle, task progress delayed to next tick
- **Consensus deadlock**: Threshold not met, manager override or meeting required

---

### Layer 5: Department Management (`src/departments/department.py`)

**Purpose**: Language-specific organizational units (floors) with auto-staffing, resource management, and contract implementation.

**Implementation Details**:
- **File Size**: Department logic integrated across multiple modules
- **Test Coverage**: 100% for department core
- **Number of Floors**: 30+ language jurisdictions
- **Office Structure**: 6 offices per floor (uniform topology)

**Department Structure**:

```
Department (Floor)
    ├── Floor Number: Unique identifier (e.g., Floor 1 = Python)
    ├── Language: Primary programming language (e.g., "python")
    ├── Offices: 6 standard offices
    │   ├── Architecture Office: Design and planning
    │   ├── Implementation Office: Code construction
    │   ├── Review Office: Code review and validation
    │   ├── Test Office: Testing and verification
    │   ├── Security Office: Security auditing
    │   └── Manager Office: Coordination and oversight
    ├── Agents: Assigned agents (5+ required roles)
    ├── Tools: Department tool inventory
    ├── Contracts: Implemented contract interfaces
    └── Artifacts: Generated code and documents
```

**Floor Registry** (Complete Catalog):

| Floor | Language | Domain | Primary Tools | Special Capabilities |
|-------|----------|--------|---------------|---------------------|
| 1 | Python | Application logic, automation, data | pytest, black, mypy, pylint | Async, data science, ML |
| 2 | Rust | Systems programming, memory safety | cargo, clippy, rustfmt | Zero-cost abstractions, ownership |
| 3 | C/C++ | Low-level systems, performance | gcc, clang, valgrind, gdb | Manual memory, hardware access |
| 4 | JavaScript/TypeScript | Frontend, Node services | npm, eslint, jest, webpack | Event-driven, async |
| 5 | Go | Network services, concurrency | go build, gofmt, golint | Goroutines, channels |
| 6 | Java | Enterprise applications | maven, gradle, junit | JVM, cross-platform |
| 7 | Shell | System automation, orchestration | bash, shellcheck | Process management, scripting |
| 8 | SQL | Data management, queries | postgres, mysql, sqlite | Relational algebra, ACID |
| 9 | NoSQL | Document/key-value stores | mongodb, redis, cassandra | Eventual consistency, scaling |
| 10 | Haskell | Functional programming, type theory | ghc, cabal, stack | Lazy evaluation, pure functions |
| 11 | Scala | Functional + OOP hybrid | sbt, scalac | JVM, functional patterns |
| 12 | Kotlin | Modern JVM language | gradle, kotlinc | Null safety, coroutines |
| 13 | Swift | iOS, macOS development | swiftc, xcode | Memory safety, protocol-oriented |
| 14 | Ruby | Web frameworks, scripting | bundler, rspec, rubocop | Metaprogramming, DSLs |
| 15 | PHP | Web development, CMS | composer, phpunit, psalm | Server-side scripting |
| 16 | Perl | Text processing, sysadmin | cpan, perl critic | Regular expressions, glue code |
| 17 | Objective-C | Legacy iOS, macOS | clang, xcode | C superset, manual memory |
| 18 | Erlang | Distributed systems, telecom | rebar3, erl | Actor model, hot code swap |
| 19 | Elixir | Modern Erlang VM | mix, iex | Macros, concurrency |
| 20 | Rust-Async | Async Rust specialization | tokio, async-std | Non-blocking I/O, futures |
| 21 | WASM | WebAssembly compilation | wasm-pack, wasm-bindgen | Browser execution, near-native |
| 22 | CUDA | GPU programming | nvcc, cuda-toolkit | Parallel computing, GPUs |
| 23 | MATLAB | Numerical computing | matlab, simulink | Matrix operations, simulation |
| 24 | Fortran | Scientific computing, HPC | gfortran, mpi | Array operations, legacy code |
| 25 | OCaml | Functional programming, formal | opam, dune | Type inference, modules |
| 26 | PowerShell | Windows automation | powershell, pester | .NET integration, cmdlets |
| 27-30 | Reserved | Future language expansion | TBD | Extensibility |

**Auto-Staffing Algorithm**:

```
function auto_staff_department(department):
    required_roles = [Architect, Builder, Verifier, Security, DocAgent]

    for role in required_roles:
        existing_agents = department.get_agents_with_role(role)

        if len(existing_agents) == 0:
            # Role missing, spawn assistant agent
            assistant = create_assistant_agent(
                role=role,
                language=department.language,
                basic_capabilities=get_basic_capabilities(role)
            )

            # Register and assign
            entity_registry.register(assistant)
            assistant.declare_relationship(department, RelationType.WORKS_IN)
            department.agents.append(assistant)

            # Log to audit
            audit_log.log_event(
                EventType.ENTITY_CREATED,
                data={
                    "type": "assistant_agent",
                    "role": role,
                    "department": department.entity_id,
                    "reason": "auto_staffing"
                }
            )
```

**Tool Inventory Management**:

Each department maintains a tool inventory:

```python
{
    "compilers": [
        {"name": "python3.11", "version": "3.11.0", "trust": 1.0, "security": 5}
    ],
    "linters": [
        {"name": "pylint", "version": "3.0.0", "trust": 0.95, "security": 3}
    ],
    "test_frameworks": [
        {"name": "pytest", "version": "7.4.3", "trust": 1.0, "security": 3}
    ],
    "formatters": [
        {"name": "black", "version": "23.0.0", "trust": 1.0, "security": 2}
    ]
}
```

**Contract Implementation**:

Departments implement contracts for inter-floor communication:

```python
Contract AuthenticationContract {
    API: /auth/login(username: str, password: str) -> {token: str, expires: int}
    API: /auth/verify(token: str) -> {valid: bool, user_id: str}
    API: /auth/refresh(refresh_token: str) -> {token: str, expires: int}
    Version: 1.2.0
    Fails: [INVALID_CREDENTIALS, TOKEN_EXPIRED, RATE_LIMITED]
}
```

**Department Metrics**:

Each department tracks:
- Active agents count
- Task throughput (tasks completed per tick)
- Tool utilization (% of time tools checked out)
- Contract call frequency (invocations per minute)
- Resource consumption (CPU, memory, budget)

**Inter-Department Integration**:

**Elevator Protocol**:
- Metaphor: Elevators carry contracts between floors
- Implementation: JSON-RPC or HTTP API calls
- Validation: Automatic compatibility checking
- Telemetry: Records all invocations with timing
- No implicit coupling: All interactions through declared contracts

**Example Integration**:
```
Python Floor (1) needs authentication from Auth Floor (special floor):
1. Python declares INTEGRATES_WITH relationship with Auth Floor
2. Python implements client-side contract for AuthenticationContract
3. Auth Floor implements server-side contract for AuthenticationContract
4. Elevator validates contract versions match (semver compatible)
5. Python calls Auth via contract: /auth/login(...)
6. Auth processes request, returns token
7. Invocation logged to audit log with timing metrics
```

**Failure Modes**:
- **Missing role**: Auto-staffing spawns assistant (self-healing)
- **Tool unavailable**: Agents wait in queue or task BLOCKED
- **Contract version mismatch**: Elevator refuses call, compatibility error
- **Resource exhaustion**: Department throttles new tasks until resources available

**Performance Considerations**:
- Department initialization: O(n) where n = number of agents to staff
- Tool checkout: O(1) hash table lookup
- Contract invocation: O(1) routing + network latency
- Auto-staffing trigger: O(r) where r = number of required roles (5)

---

### Layer 6: Tool & Supply Store (`src/tools/supply_store.py`)

**Purpose**: Centralized tool management with checkout/return protocol, capability matching, and security enforcement.

**Implementation Details**:
- **File Size**: Tool management integrated into entity system
- **Test Coverage**: 100% for checkout protocol
- **Tool Types**: 5 categories (compiler, linter, test_framework, formatter, analyzer)
- **Checkout Protocol**: Capability-based with justification for sensitive tools

**Tool Metadata Structure**:

```python
{
    "tool_id": "tool-pytest-001",
    "name": "pytest",
    "version": "7.4.3",
    "tag": "test_framework",        # compiler, linter, test_framework, formatter, analyzer
    "trust_score": 1.0,             # 0.0 (untrusted) to 1.0 (fully trusted)
    "security_rating": 3,           # 1 (low risk) to 5 (high risk)
    "capabilities": ["python", "testing", "coverage"],
    "requires_justification": False, # True for sensitive tools
    "max_concurrent_users": 10,     # Concurrency limit
    "current_users": 0,             # Active checkouts
    "checked_out_by": [],           # List of agent IDs
    "last_used": "2026-02-15T16:00:00Z"
}
```

**Tool Categories**:

| Tag | Purpose | Examples | Typical Security Rating |
|-----|---------|----------|------------------------|
| `compiler` | Source code compilation | gcc, rustc, javac, python | 4-5 (can execute code) |
| `linter` | Static code analysis | pylint, eslint, clippy | 2-3 (read-only analysis) |
| `test_framework` | Test execution | pytest, jest, cargo test | 3-4 (executes test code) |
| `formatter` | Code formatting | black, prettier, gofmt | 2 (modifies files) |
| `analyzer` | Deep code analysis | mypy, flow, type checker | 2-3 (read-only) |

**Checkout Protocol**:

```
1. Agent requests tool: checkout_tool(tool_id, agent_id, justification=None)

2. System checks:
   a. Tool exists in supply store
   b. Tool not at max_concurrent_users limit
   c. Agent has required capabilities for tool
   d. If tool.requires_justification: justification provided and valid
   e. Agent security clearance ≥ tool security rating

3. If all checks pass:
   a. Mark tool as checked out by agent
   b. Increment current_users counter
   c. Declare agent USES tool relationship
   d. Log TOOL_CHECKED_OUT event to audit
   e. Return tool handle to agent

4. If any check fails:
   a. Reject checkout request
   b. Return error reason
   c. Log failed checkout attempt to audit

5. Agent uses tool for operations

6. Agent returns tool: return_tool(tool_id, agent_id)
   a. Remove agent from checked_out_by list
   b. Decrement current_users counter
   c. Log tool return to audit
   d. Mark tool available for next checkout
```

**Capability Matching**:

```python
def can_agent_use_tool(agent, tool):
    # Agent must have all tool capabilities
    agent_caps = set(agent.capabilities.get("tools", []))
    tool_caps = set(tool.capabilities)

    if not tool_caps.issubset(agent_caps):
        return False, "Missing required capabilities"

    # Security clearance check
    if agent.security_clearance < tool.security_rating:
        return False, "Insufficient security clearance"

    # Concurrency check
    if tool.current_users >= tool.max_concurrent_users:
        return False, "Tool at maximum concurrent users"

    return True, "OK"
```

**Justification Requirements**:

High-security tools require justification:

```python
# Example: Compiler requires justification
{
    "tool_id": "tool-gcc",
    "name": "gcc",
    "security_rating": 5,
    "requires_justification": True
}

# Agent must provide justification
justification = {
    "task_id": "task-001",
    "purpose": "Compile C source for performance optimization",
    "expected_duration": "5 minutes",
    "approval": "manager-c-dept"
}

checkout_result = supply_store.checkout_tool(
    tool_id="tool-gcc",
    agent_id="agent-builder-001",
    justification=justification
)
```

**Tool Trust Scores**:

Trust score affects tool selection:

| Trust Score | Meaning | Usage Policy |
|-------------|---------|--------------|
| 1.0 | Fully trusted, verified | No restrictions, preferred for production |
| 0.9-0.99 | High trust, widely used | Minor restrictions, logged usage |
| 0.7-0.89 | Medium trust, community | Requires review, not for critical tasks |
| 0.5-0.69 | Low trust, experimental | Sandbox only, extensive logging |
| 0.0-0.49 | Untrusted, unknown | Blocked by default, manual override only |

**Supply Store Operations**:

```python
class SupplyStore:
    def register_tool(self, tool: Tool) -> None:
        """Register new tool in supply store"""

    def checkout_tool(self, tool_id: str, agent_id: str, justification: Optional[Dict] = None) -> Result:
        """Agent checks out tool for use"""

    def return_tool(self, tool_id: str, agent_id: str) -> Result:
        """Agent returns tool after use"""

    def get_available_tools(self, tag: Optional[str] = None) -> List[Tool]:
        """Get available tools, optionally filtered by tag"""

    def get_tool_status(self, tool_id: str) -> Dict:
        """Get current status of tool (available, users, etc.)"""

    def update_trust_score(self, tool_id: str, new_score: float, reason: str) -> None:
        """Update tool trust score based on usage"""
```

**Example Tool Inventory**:

```python
# Python Department Tool Inventory
{
    "compilers": [
        {"id": "tool-python311", "name": "python3.11", "version": "3.11.0",
         "trust": 1.0, "security": 5, "concurrent": 20}
    ],
    "linters": [
        {"id": "tool-pylint", "name": "pylint", "version": "3.0.0",
         "trust": 0.95, "security": 3, "concurrent": 10},
        {"id": "tool-flake8", "name": "flake8", "version": "6.1.0",
         "trust": 1.0, "security": 2, "concurrent": 10}
    ],
    "test_frameworks": [
        {"id": "tool-pytest", "name": "pytest", "version": "7.4.3",
         "trust": 1.0, "security": 3, "concurrent": 15}
    ],
    "formatters": [
        {"id": "tool-black", "name": "black", "version": "23.0.0",
         "trust": 1.0, "security": 2, "concurrent": 10}
    ],
    "analyzers": [
        {"id": "tool-mypy", "name": "mypy", "version": "1.7.0",
         "trust": 0.98, "security": 3, "concurrent": 10}
    ]
}
```

**Failure Modes**:
- **Tool unavailable**: Agent waits in queue or task blocked
- **Capability mismatch**: Checkout rejected, alternative tool suggested
- **Security violation**: Checkout rejected, security event logged
- **Justification invalid**: Manager review required
- **Tool failure during use**: Agent reports failure, tool marked for maintenance

**Performance Metrics**:
- Average checkout time: O(1) hash table lookup + O(n) capability check
- Typical checkout: < 10ms
- Tool queue wait time: Depends on current_users and usage duration
- Return processing: O(1) update operations

---

(Continued in next response due to length...)
