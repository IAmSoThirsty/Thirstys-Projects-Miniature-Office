# Floor 1 - Python Jurisdiction

**Language:** Python  
**Floor Number:** 1  
**Domain:** Application logic, Automation, Data processing, Glue code

## Architecture

This floor implements department logic in pure Python, demonstrating:
- **Language Sovereignty**: All code written in Python
- **Identical Topology**: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
- **Contract-Bound Operation**: All operations via JSON-RPC protocol
- **Non-Creative Mandate**: Strict adherence to requests
- **Failure Escalation**: All failures are explicit and reported

## Running

```bash
python3 department_floor.py
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## API

### Get Floor Info
```json
{"method": "get_info"}
```

### Add Agent
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_1",
    "name": "Alice",
    "role": "Architect",
    "capabilities": ["design", "planning"]
  }
}
```

### Create Task
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_1",
    "title": "Implement feature X",
    "assigned_to": "agent_1"
  }
}
```

### Process Code
```json
{
  "method": "process_code",
  "params": {
    "code": "def hello(): pass",
    "operation": "analyze"
  }
}
```

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | python3 department_floor.py
```
