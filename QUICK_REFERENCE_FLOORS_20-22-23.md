# Quick Reference: Floors 20, 22, 23

## Floor 20 - Haskell (Pure Functional Programming)

**Location**: `floors/haskell/`  
**Language**: Haskell (GHC 9.2+)  
**Paradigm**: Pure functional with type safety

### Quick Start
```bash
cd floors/haskell
cabal build --allow-newer
echo '{"method": "get_info"}' | cabal run department-floor
```

### Key Commands
```bash
# Build
cabal build

# Run
cabal run department-floor

# Test
echo '{"method": "get_info"}' | cabal run department-floor

# Format
stylish-haskell -i *.hs

# Lint
hlint .
```

### Code Analysis Features
- Purity score (percentage of pure functions)
- Function counting (via :: signatures)
- Type counting (data/type/newtype)
- Module detection

### Special Notes
- No partial functions allowed
- All functions are total
- Monadic error handling via Maybe/Either
- Type-safe JSON parsing with Aeson

---

## Floor 22 - Elixir (Distributed Systems)

**Location**: `floors/elixir/`  
**Language**: Elixir 1.14+  
**Paradigm**: OTP-based concurrent functional

### Quick Start
```bash
cd floors/elixir
mix deps.get
mix escript.build
echo '{"method": "get_info"}' | ./department_floor
```

### Key Commands
```bash
# Get dependencies
mix deps.get

# Compile
mix compile

# Build escript
mix escript.build

# Run
./department_floor

# Test
mix test

# Format
mix format

# Interactive shell
iex -S mix
```

### OTP Architecture
```
Supervisor (one_for_one)
├── AgentService (GenServer)
├── TaskManager (GenServer)
└── CodeAnalyzer (GenServer)
```

### Code Analysis Features
- Concurrency score (GenServers + processes)
- Function counting (def statements)
- Module detection
- GenServer detection
- Process spawning analysis

### Special Notes
- Let it crash philosophy
- All state in GenServers
- Automatic restart on failure
- Message passing only (no shared state)

---

## Floor 23 - Erlang (Telecom-Grade Reliability)

**Location**: `floors/erlang/`  
**Language**: Erlang/OTP 25+  
**Paradigm**: Process-oriented functional

### Quick Start
```bash
cd floors/erlang
rebar3 compile
rebar3 escriptize
echo '{"method": "get_info"}' | ./_build/default/bin/department_floor
```

### Key Commands
```bash
# Get dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Build escript
rebar3 escriptize

# Run
./_build/default/bin/department_floor

# Test
rebar3 eunit
rebar3 ct

# Dialyzer (static analysis)
rebar3 dialyzer

# Interactive shell
rebar3 shell
```

### OTP Architecture
```
Supervisor (one_for_one, 10/60s)
├── agent_service (gen_server, permanent)
├── task_manager (gen_server, permanent)
└── code_analyzer (gen_server, permanent)
```

### Code Analysis Features
- Reliability score (gen_servers + supervisors)
- Function counting (-export directives)
- Module detection
- gen_server detection
- Supervisor detection
- Process spawning analysis

### Special Notes
- Nine-nines reliability (99.9999999%)
- 10 restarts per 60 seconds intensity
- Hot code loading support
- Telecom-grade patterns
- Binary strings for JSON compatibility

---

## Common API (All Floors)

### Get Floor Info
```bash
echo '{"method": "get_info"}' | <floor-executable>
```

Returns:
- floor_number
- language
- domain
- architectural_law
- security_doctrine
- offices (array of 6)
- agent_count
- task_count
- agents (array)
- tasks (array)

### Add Agent
```bash
echo '{"method": "add_agent", "params": {"agent_id": "a1", "name": "Alice", "role": "Architect", "capabilities": ["design"]}}' | <floor-executable>
```

### Create Task
```bash
echo '{"method": "create_task", "params": {"task_id": "t1", "title": "Design system", "assigned_to": "a1"}}' | <floor-executable>
```

### Process Code
```bash
# Analyze
echo '{"method": "process_code", "params": {"code": "<code>", "operation": "analyze"}}' | <floor-executable>

# Format
echo '{"method": "process_code", "params": {"code": "<code>", "operation": "format"}}' | <floor-executable>

# Lint
echo '{"method": "process_code", "params": {"code": "<code>", "operation": "lint"}}' | <floor-executable>
```

---

## Troubleshooting

### Haskell
- **Issue**: Dependency conflicts
- **Solution**: Use `cabal build --allow-newer`

- **Issue**: Module not found
- **Solution**: Check all modules listed in .cabal file

### Elixir
- **Issue**: Mix not found
- **Solution**: Install Elixir 1.14+ via package manager or asdf

- **Issue**: GenServer not starting
- **Solution**: Check Application.start/2 is called in CLI

### Erlang
- **Issue**: JSX dependency error
- **Solution**: Run `rebar3 get-deps` before compile

- **Issue**: gen_server crashes
- **Solution**: Check all gen_server callbacks are implemented

---

## Performance Notes

### Haskell
- Lazy evaluation (be aware of space leaks)
- Compiled binary is ~50-100MB
- Fast execution after compilation

### Elixir
- BEAM VM overhead
- Excellent concurrency performance
- Hot code loading in production

### Erlang
- Minimal BEAM overhead
- Designed for millions of processes
- Proven in telecom systems (99.9999999% uptime)

---

## Integration

All three floors integrate with the main system via:
1. **stdin/stdout** JSON-RPC protocol
2. **Standard error** for logging
3. **Exit codes** for status

Example integration:
```python
import subprocess
import json

# Start floor
proc = subprocess.Popen(
    ['./floors/haskell/dist-newstyle/.../department-floor'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
)

# Send request
request = {"method": "get_info"}
proc.stdin.write(json.dumps(request).encode() + b'\n')
proc.stdin.flush()

# Read response
response = json.loads(proc.stdout.readline())
print(response)
```
