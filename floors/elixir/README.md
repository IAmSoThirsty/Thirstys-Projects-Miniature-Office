# Floor 22 - Elixir Jurisdiction

**Language:** Elixir  
**Floor Number:** 22  
**Domain:** Distributed systems, Fault-tolerant services, Concurrent applications

## Architectural Laws

This floor operates under OTP principles and Elixir best practices:
- **Let It Crash**: Embrace failure, use supervisors for recovery
- **Supervision Trees**: Hierarchical process supervision for fault tolerance
- **Process Isolation**: Each process has isolated state, no shared memory
- **Message Passing**: All communication via asynchronous message passing
- **Immutable Data**: All data structures are immutable

## Security Doctrine

- **Process Isolation**: Processes cannot corrupt each other's state
- **No Shared State**: Message passing eliminates data races
- **Fail Fast**: Errors are caught early, systems self-heal
- **Supervision**: Failed processes are automatically restarted
- **Defense in Depth**: Multiple layers of fault tolerance

## Agent Staffing

The floor employs specialized agents across 6 offices, all supervised for fault tolerance:

### Architecture Office
- Distributed system design
- OTP architecture patterns
- Supervision tree design

### Implementation Office
- GenServer implementation
- Process-based concurrency
- Message passing patterns

### Review Office
- Code review with OTP pattern verification
- Supervision tree audit
- Process design review

### Test Office
- ExUnit testing
- Property-based testing with StreamData
- Concurrency testing

### Security Office
- Process isolation audit
- Message validation
- Fault-tolerance verification

### Manager Office
- Project coordination
- Mix task management
- Dependency oversight (Hex packages)

## Prerequisites

- Elixir 1.14+ with Erlang/OTP 25+

Install via:
```bash
# macOS
brew install elixir

# Ubuntu/Debian
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt-get install elixir

# Or use asdf
asdf install elixir 1.14.0
```

## Building

Get dependencies:
```bash
mix deps.get
```

Compile:
```bash
mix compile
```

Build escript (standalone executable):
```bash
mix escript.build
```

## Running

Run with Mix:
```bash
mix run --no-halt -e "DepartmentFloor.CLI.main([])"
```

Or use the compiled escript:
```bash
./department_floor
```

## API

The floor implements JSON-RPC over stdin/stdout with fault-tolerant GenServers.

### Get Floor Info
```json
{"method": "get_info"}
```

Response includes floor metadata, supervised agents, tasks, and OTP architecture info.

### Add Agent
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_1",
    "name": "Alice",
    "role": "Architect",
    "capabilities": ["design", "supervision"]
  }
}
```

Agents are stored in the AgentService GenServer with fault-tolerant state.

### Create Task
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_1",
    "title": "Design supervision tree",
    "assigned_to": "agent_1"
  }
}
```

Tasks are managed by TaskManager GenServer.

### Process Code
```json
{
  "method": "process_code",
  "params": {
    "code": "defmodule MyApp do\n  use GenServer\nend",
    "operation": "analyze"
  }
}
```

Operations:
- `analyze`: Code analysis (lines, functions, modules, GenServers, concurrency score)
- `format`: Code formatting (would use Code.format_string! in production)
- `lint`: Linting for OTP patterns and best practices

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | mix run -e "DepartmentFloor.CLI.main([])"
```

Or with escript:
```bash
echo '{"method": "get_info"}' | ./department_floor
```

Test code analysis:
```bash
echo '{"method": "process_code", "params": {"code": "defmodule Test do\n  use GenServer\nend", "operation": "analyze"}}' | ./department_floor
```

Run tests:
```bash
mix test
```

## Development

Format code:
```bash
mix format
```

Check for warnings:
```bash
mix compile --warnings-as-errors
```

Analyze with Credo (if installed):
```bash
mix credo
```

Interactive console:
```bash
iex -S mix
```

## OTP Architecture

The floor uses a supervision tree:
```
DepartmentFloor.Supervisor (one_for_one)
├── DepartmentFloor.AgentService (GenServer)
├── DepartmentFloor.TaskManager (GenServer)
└── DepartmentFloor.CodeAnalyzer (GenServer)
```

Each GenServer is independently supervised. If one crashes, it's restarted without affecting others.

## Jurisdiction

This floor has exclusive authority over:
- Distributed system implementation
- OTP design patterns
- Fault-tolerant service design
- Concurrent application architecture
- Message-passing systems
- Real-time applications

## Inter-Floor Communication

Communicates with other floors via JSON-RPC, maintaining process isolation and fault tolerance at boundaries.
