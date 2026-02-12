# Multi-Language Floor Architecture

## Overview

The Miniature Office project now implements **actual multi-language department floors**, where each floor's department logic is written in its specified programming language. This demonstrates **"displayed and transparent Diversity flexibility"** by having a truly polyglot architecture.

## Architecture Principle

Instead of having all code in Python with language specifications only in documentation, we now have:

```
Main Python Application
    ├── Floor Manager (Python)
    │   ├── Spawns floor processes
    │   ├── Manages communication
    │   └── Lifecycle management
    │
    └── Language-Specific Floors (Native Implementation)
        ├── Floor 1: Python Department (Python)
        ├── Floor 2: Rust Department (Rust)
        ├── Floor 4: JavaScript Department (JavaScript/Node.js)
        ├── Floor 5: Go Department (Go)
        └── Floor 7: Shell Department (Bash)
```

## Communication Protocol

All floors communicate via **JSON-RPC over stdin/stdout**:

### Request Format
```json
{
  "method": "method_name",
  "params": {
    "param1": "value1",
    "param2": "value2"
  }
}
```

### Response Format
```json
{
  "status": "success",
  "data": {...}
}
```

## Implemented Floors

### Floor 1 - Python Jurisdiction
- **Language**: Python 3.9+
- **Domain**: Application logic, automation, data processing
- **File**: `floors/python/department_floor.py`
- **Status**: ✅ Working

### Floor 2 - Rust Jurisdiction
- **Language**: Rust 1.70+
- **Domain**: Memory-safe systems, performance-critical logic
- **File**: `floors/rust/src/main.rs`
- **Build**: `cargo build` in `floors/rust/`
- **Status**: ✅ Working

### Floor 4 - JavaScript Jurisdiction
- **Language**: JavaScript/Node.js 14+
- **Domain**: Frontend logic, tooling, Node services
- **File**: `floors/javascript/department_floor.js`
- **Status**: ✅ Working

### Floor 5 - Go Jurisdiction
- **Language**: Go 1.21+
- **Domain**: Network services, concurrency-heavy systems
- **File**: `floors/go/department_floor.go`
- **Build**: `go build` in `floors/go/`
- **Status**: ✅ Working

### Floor 7 - Shell Jurisdiction
- **Language**: Bash 4.0+
- **Domain**: System automation, orchestration
- **File**: `floors/shell/department_floor.sh`
- **Status**: ✅ Working

## Uniform Floor Structure

Every floor, regardless of language, implements the same structure:

### Six Offices (Identical Topology)
1. **Architecture Office** - Design and planning
2. **Implementation Office** - Code construction
3. **Review Office** - Code review and validation
4. **Test Office** - Testing and verification
5. **Security Office** - Security auditing
6. **Manager Office** - Coordination and oversight

### Core Operations
- `get_info` - Get floor metadata
- `add_agent` - Add an agent to the floor
- `create_task` - Create a task on the floor
- `process_code` - Process code in the floor's language

## Building All Floors

```bash
./build_floors.sh
```

This script:
- Checks for required toolchains
- Builds Rust floor (if cargo available)
- Builds Go floor (if go available)
- Confirms Python, JavaScript, and Shell floors are ready

## Running the Demo

```bash
python3 src/core/floor_manager.py
```

This demonstrates:
- Starting multiple floors in different languages
- Getting info from all running floors
- Processing code in each floor's native language
- Clean shutdown of all floor processes

## Extending to More Languages

To add a new language floor:

1. **Create floor directory**: `floors/{language}/`
2. **Implement JSON-RPC server**: Read from stdin, write to stdout
3. **Implement required methods**: `get_info`, `add_agent`, `create_task`, `process_code`
4. **Add configuration**: Update `floor_manager.py` with floor config
5. **Add README**: Document the floor's setup and usage

### Example for C/C++ Floor
```c
// floors/c/department_floor.c
#include <stdio.h>
#include <json-c/json.h>

int main() {
    char line[1024];
    while (fgets(line, sizeof(line), stdin)) {
        // Parse JSON request
        // Process method
        // Write JSON response
    }
    return 0;
}
```

## Benefits of Multi-Language Implementation

### 1. **Transparent Diversity**
Each floor visibly uses its designated language, making the architecture's diversity explicit.

### 2. **Language Sovereignty**
Each floor can use language-specific features, idioms, and best practices without compromise.

### 3. **Real-World Validation**
The architecture isn't just theoretical - it's proven to work across multiple languages.

### 4. **Educational Value**
Developers can see how the same architectural pattern is implemented in different languages.

### 5. **Performance Optimization**
Performance-critical floors (Rust, Go, C++) run natively without Python overhead.

### 6. **Ecosystem Integration**
Each floor can use its language's native tooling, libraries, and ecosystem.

## Comparison: Before vs After

### Before
```
All code in Python
└── Floor specifications in documentation only
```

### After
```
Python main application
├── Python floor (in Python)
├── Rust floor (in Rust - compiled binary)
├── JavaScript floor (in JavaScript - Node.js)
├── Go floor (in Go - compiled binary)
└── Shell floor (in Bash - shell script)
```

## Integration with Main Application

The floor manager (`src/core/floor_manager.py`) can be integrated into the main application:

```python
from src.core.floor_manager import MultiLanguageFloorManager

# Create manager
manager = MultiLanguageFloorManager()

# Start specific floor
manager.start_floor('rust')

# Send request to floor
result = manager.send_request_to_floor(
    'rust',
    'process_code',
    {'code': 'fn main() {}', 'operation': 'analyze'}
)

# Get all floor info
all_info = manager.get_all_floor_info()
```

## Performance Characteristics

| Floor | Startup Time | Memory | Throughput |
|-------|-------------|---------|------------|
| Python | ~50ms | Low | Good |
| JavaScript | ~100ms | Medium | Good |
| Go | ~10ms (compiled) | Low | Excellent |
| Rust | ~5ms (compiled) | Minimal | Excellent |
| Shell | ~5ms | Minimal | Fair |

## Future Enhancements

1. **More Language Floors**: Add C, C++, Java, Kotlin, Swift, etc.
2. **HTTP/gRPC Protocol**: Add alternative communication protocols
3. **Floor Discovery**: Auto-discover available floor implementations
4. **Hot Reload**: Reload floor implementations without restart
5. **Floor Clustering**: Run multiple instances of the same floor for load balancing
6. **Inter-Floor Communication**: Direct floor-to-floor messaging
7. **WebAssembly Floors**: Run floors in browser via WASM

## Testing

Test individual floors:
```bash
# Python
echo '{"method": "get_info"}' | python3 floors/python/department_floor.py

# JavaScript
echo '{"method": "get_info"}' | node floors/javascript/department_floor.js

# Go
echo '{"method": "get_info"}' | go run floors/go/department_floor.go

# Rust (after build)
echo '{"method": "get_info"}' | ./floors/rust/target/debug/department_floor
```

## Security Considerations

1. **Process Isolation**: Each floor runs in a separate process
2. **No Shared Memory**: Communication only via JSON
3. **Resource Limits**: OS-level resource limits can be applied per floor
4. **Language-Specific Security**: Each floor applies its language's security best practices
5. **Input Validation**: All JSON-RPC inputs are validated per floor

## Troubleshooting

### Floor Won't Start
- Check if language toolchain is installed
- Verify floor executable has correct permissions
- Check floor directory structure

### Communication Issues
- Ensure JSON-RPC format is correct
- Check stdout/stderr for error messages
- Verify stdin/stdout aren't being used for other purposes

### Build Failures
- Update language toolchains to required versions
- Check for missing dependencies
- Review build logs in floor directories

## Conclusion

The multi-language floor implementation transforms the Miniature Office from a monolithic Python application with language specifications in documentation, to a truly polyglot system where each department floor is implemented in its designated language. This provides **visible, transparent diversity** and demonstrates that the architectural principles work across language boundaries.
