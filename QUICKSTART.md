# Quick Start Guide

**Status: experimental Flask prototype — not production-ready.** Measured status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

Python **3.10+** is required (`pytest==9.0.3` does not install on 3.9).

## Installation

```bash
# Clone the repository
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office

# Install dependencies
pip install -r requirements.txt
```

## Running the Application

```bash
# Start the server
python3 run.py
```

The server will start on `http://localhost:5000`

## Using the Web Interface

1. **Open Browser:** Navigate to `http://localhost:5000`

2. **Simulation Controls:**
   - **STEP (+1 Tick):** Advance simulation by one time step
   - **START:** Begin continuous simulation
   - **STOP:** Halt simulation
   - **REFRESH STATE:** Update UI with latest data

3. **World Metrics:** View real-time counts of:
   - Floors (departments)
   - Agents (workers)
   - Tasks (work items)
   - Tools Available (in supply store)

4. **Active Agents:** See all agents and their current status:
   - `idle` - Waiting for work
   - `working` - Executing task
   - `blocked` - Waiting on dependencies
   - `in_meeting` - Resolving ambiguity

5. **Event Log:** Scrolling audit trail of all actions

## Using the API

### Get World State
```bash
curl http://localhost:5000/api/world/state | python3 -m json.tool
```

### Step Simulation
```bash
curl -X POST http://localhost:5000/api/world/step
```

### List Agents
```bash
curl http://localhost:5000/api/agents | python3 -m json.tool
```

### List Departments
```bash
curl http://localhost:5000/api/departments | python3 -m json.tool
```

### View Supply Store
```bash
curl http://localhost:5000/api/supply-store | python3 -m json.tool
```

### Query Audit Log
```bash
# Get last 10 events
curl "http://localhost:5000/api/audit/events?limit=10" | python3 -m json.tool

# Get events by type
curl "http://localhost:5000/api/audit/events?type=task_state_changed" | python3 -m json.tool

# Get events for specific agent
curl "http://localhost:5000/api/audit/events?actor_id=mgr-001" | python3 -m json.tool
```

## Example Workflow

### 1. Check Initial State
```bash
curl http://localhost:5000/api/departments
```

You should see 2 departments (Python Development and Frontend Development). Each is auto-staffed with the **5 required roles** (architect, builder, verifier, security, doc_agent). Manager is **not** a required role.

### 2. View Auto-Spawned Agents
```bash
curl http://localhost:5000/api/agents | python3 -m json.tool
```

`GET /api/agents` lists `EntityType.AGENT`. The `Manager` class subclasses `Agent`, so Alice is in this list. The seed from `init_simulation()` is:

- Python: 5 assistant agents plus **one** Manager (`mgr-001`, Alice Manager) on `office-1`
- JavaScript: 5 assistant agents, **no** manager, **no** office

There is not one Manager per department.

Assistant names come from `Assistant {role.value.title()}`:
- Assistant Architect
- Assistant Builder
- Assistant Verifier
- Assistant Security
- Assistant Doc_Agent



### 3. Run Simulation Steps
```bash
# Advance 5 ticks
for i in {1..5}; do
  curl -X POST http://localhost:5000/api/world/step
  sleep 1
done
```

### 4. Check Audit Trail
```bash
curl "http://localhost:5000/api/audit/events?limit=20" | python3 -m json.tool
```

You'll see logged events such as:
- Entity creation (`entity_created`)
- Agent actions (`agent_action`)
- A per-tick `agent_action` whose data says `state_persisted` — that is a log label. World state stays in-memory.

There is no separate world-tick or persistence event type.

### 5. View Supply Store Tools
```bash
curl http://localhost:5000/api/supply-store | python3 -m json.tool
```

Should show 2 tools:
- Python Interpreter (compiler)
- PyTest Framework (test framework)

## Understanding the World Structure

```
World: "Miniature Office IDE"
├── Floor: Python
│   ├── Department: Python Development
│   └── Office: office-1
│       ├── Manager: Alice Manager
│       └── Agents: 5 assistant agents (one per required role)
└── Floor: JavaScript
    ├── Department: Frontend Development
    └── (no office, no manager)
        └── Agents: 5 assistant agents on the department
```

The 28 toy language-floor directories under `floors/` are **not** seeded into this default world.


## Creating Custom Content

### Add a New Department

```python
from src.departments.department import Department, get_department_registry
from src.core.world import get_world

# Create Rust department
rust_dept = Department("dept-rust", "Systems Programming", "Rust")
registry = get_department_registry()
registry.register_department(rust_dept)

# Add to world
from src.core.world import Floor
rust_floor = Floor("floor-rust", "Rust")
rust_floor.department = rust_dept
get_world().add_floor(rust_floor)
```

### Add a Tool

```python
from src.tools.supply_store import Tool, ToolMetadata, ToolTag, get_supply_store

cargo_tool = Tool(
    "tool-cargo",
    "Cargo Build System",
    ToolMetadata(
        tag=ToolTag.BUILD_SYSTEM,
        version="1.70.0",
        trust_score=0.95,
        security_rating=5,
        capabilities={"rust", "build", "package"}
    )
)

get_supply_store().add_tool(cargo_tool)
```

### Create a Task

```python
from src.core.mission import Task, TaskState

task = Task(
    "task-002",
    "Implement API Endpoint",
    "Create REST endpoint for user registration",
    None,  # parent_directive_id
    None   # assigned_agent_id
)

# Add criteria
task.add_precondition("API specification document exists")
task.add_postcondition("Endpoint returns 201 on success")
task.add_acceptance_criterion("Integration tests pass")
task.add_acceptance_criterion("Security review complete")
task.add_acceptance_criterion("Documentation updated")
```

### Assign Task to Agent

```python
from src.core.entity import EntityType, get_registry
from src.agents.agent import AgentRole

# Get an idle builder agent
agents = get_registry().get_by_type(EntityType.AGENT)
builder = next((a for a in agents if getattr(a, "role", None) == AgentRole.BUILDER and a.status == 'idle'), None)

if builder:
    builder.assign_task(task)
```


## Monitoring & Debugging

### Watch Simulation in Real-Time

Open two terminals:

**Terminal 1:** Start simulation
```bash
python3 run.py
```

**Terminal 2:** Watch events
```bash
while true; do
  curl -s "http://localhost:5000/api/audit/events?limit=5" | python3 -m json.tool
  sleep 2
done
```

### Check Agent Status

```bash
# List all agents
curl http://localhost:5000/api/agents | python3 -c "
import sys, json
data = json.load(sys.stdin)
for agent in data['agents']:
    print(f\"{agent['name']:30} {agent['role']:15} {agent['status']:10}\")
"
```

### Inspect an event hash

```bash
# Get an event hash
curl -s "http://localhost:5000/api/audit/events?limit=1" | python3 -c "
import sys, json
data = json.load(sys.stdin)
event = data['events'][0]
print('Event ID:', event['event_id'])
print('Hash:', event['hash'])
print('prev_hash:', event.get('prev_hash'))
print('Timestamp:', event['timestamp'])
"
```

Each event stores a SHA-256 of its own fields plus `prev_hash` (the previous event). By default that chain is in-memory, unsigned, and dropped on process restart. If `MO_DATA_DIR` is set, events also append to `audit.jsonl`. If `MO_AUDIT_HMAC_KEY` or a non-placeholder `SECRET_KEY` is set, events carry HMAC-SHA256 over the content hash. That is still a local integrity tag, not a production-grade ledger.

## Troubleshooting

### Server Won't Start
- Check if port 5000 is already in use: `lsof -i :5000`
- Try a different port: Edit `run.py` and change port number

### Import Errors
- Ensure you're running from project root
- Check Python version: `python3 --version` (needs 3.10+)
- Reinstall dependencies: `pip install -r requirements.txt`

### UI Not Loading
- Check browser console for errors
- Verify server is running: `curl http://localhost:5000/api`
- Clear browser cache

### Agents Not Working
- Check agent status: `curl http://localhost:5000/api/agents`
- View audit log: `curl http://localhost:5000/api/audit/events`
- Verify capabilities match task requirements

## Next Steps

- Read [ARCHITECTURE.md](ARCHITECTURE.md) for intended design (not a completion certificate)
- Read [LIMITATIONS.md](LIMITATIONS.md) for what is not implemented
- Explore the code in `src/` directories

## Getting Help

- Check the audit log for error events
- Review agent capabilities and task requirements
- Ensure all required roles are filled in departments
- Verify tool availability in supply store
