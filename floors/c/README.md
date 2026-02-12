# Floor 3 - C Jurisdiction

**Language:** C  
**Floor Number:** 3  
**Domain:** Low-level systems, Embedded systems, Device drivers, Performance-critical code

## Domain and Jurisdiction

Floor 3 is the C jurisdiction, responsible for all low-level systems programming within the Miniature Office. This floor handles:

- **Embedded Systems**: Firmware, microcontroller code, bare-metal programming
- **Device Drivers**: Kernel modules, hardware interfaces
- **Performance-Critical Code**: Real-time systems, high-frequency processing
- **Systems Infrastructure**: Core utilities, system libraries
- **Memory-Constrained Environments**: Optimization for minimal resource usage

## Architectural Laws

The C jurisdiction operates under strict architectural laws that reflect the language's philosophy:

### 1. Explicit Over Implicit
All operations must be explicit. No hidden allocations, no implicit conversions that lose precision, no automatic memory management.

### 2. Manual Memory Management
Every allocation must have a corresponding deallocation. Memory leaks are considered critical failures. RAII is not available; discipline is mandatory.

### 3. Buffer Safety Paramount
All buffer operations must include bounds checking. Use `strncpy`, `snprintf`, and manual size validation. Buffer overruns are the primary security concern.

### 4. Minimal Dependencies
Standard library only. No external dependencies unless absolutely necessary for the domain. Keep the footprint small.

### 5. Portable C11
Code must conform to C11 standard. Use of compiler extensions must be documented and justified. Cross-platform compatibility is essential.

## Security Doctrine

Security in C is about defense in depth:

### Buffer Overflow Prevention
- Always use bounded string functions (`strncpy`, `snprintf`, `strncat`)
- Validate all array indices before access
- Null-terminate all strings explicitly
- Use `sizeof` for buffer sizes, never hardcode

### Memory Safety
- Initialize all variables before use
- Zero sensitive memory before deallocation
- Check all allocation return values
- Avoid use-after-free by nulling freed pointers

### Input Validation
- Validate all input lengths before copying
- Sanitize data from untrusted sources
- Bounds-check all array accesses
- Validate format strings

### Integer Safety
- Check for overflow in arithmetic operations
- Use appropriate integer types for size calculations
- Validate conversions between signed and unsigned

## Office Structure

All six offices are staffed and operational:

### 1. Architecture Office
**Role:** System design, memory layout planning, API design  
**Minimum Staff:** 3 agents  
**Agents:**
- System Architect: Overall architecture design
- Memory Architect: Memory layout and allocation strategies
- API Designer: Interface specifications

**Capabilities:**
- Structure design and layout optimization
- Memory allocation strategies
- API contract definition
- Performance requirement specification

### 2. Implementation Office
**Role:** Code implementation, optimization  
**Minimum Staff:** 3 agents  
**Agents:**
- Implementation Engineer: Core logic implementation
- Optimization Specialist: Performance optimization
- Assembly Expert: Critical path optimization

**Capabilities:**
- Low-level coding
- Algorithm implementation
- Performance tuning
- Inline assembly when needed

### 3. Review Office
**Role:** Code review, static analysis  
**Minimum Staff:** 3 agents  
**Agents:**
- Code Reviewer: General code quality
- Buffer Safety Specialist: Buffer overflow detection
- Static Analysis Expert: Automated analysis tools

**Capabilities:**
- Manual code review
- Static analysis (cppcheck, splint)
- Buffer safety verification
- Style compliance checking

### 4. Test Office
**Role:** Testing, validation  
**Minimum Staff:** 3 agents  
**Agents:**
- Unit Test Engineer: Function-level testing
- Integration Test Engineer: System-level testing
- Memory Test Specialist: Valgrind, leak detection

**Capabilities:**
- Unit testing frameworks
- Integration testing
- Memory leak detection (Valgrind)
- Coverage analysis (gcov)

### 5. Security Office
**Role:** Security analysis, vulnerability detection  
**Minimum Staff:** 3 agents  
**Agents:**
- Security Analyst: Vulnerability assessment
- Fuzzing Engineer: Input fuzzing
- Exploit Mitigation Specialist: Defense mechanisms

**Capabilities:**
- Vulnerability scanning
- Fuzzing (AFL, LibFuzzer)
- Static security analysis
- Exploit mitigation verification

### 6. Manager Office
**Role:** Task coordination, resource management  
**Minimum Staff:** 3 agents  
**Agents:**
- Floor Manager: Overall coordination
- Resource Allocator: Memory and CPU budgets
- Escalation Manager: Failure handling

**Capabilities:**
- Task prioritization
- Resource allocation
- Cross-office coordination
- Escalation to executive

## API Documentation

The C Department Floor implements the standard JSON-RPC protocol over stdin/stdout.

### Get Floor Information

Request:
```json
{"method": "get_info"}
```

Response:
```json
{
  "status": "success",
  "floor_number": 3,
  "language": "c",
  "domain": "Low-level systems, Embedded, Device drivers",
  "offices": [
    "Architecture Office",
    "Implementation Office",
    "Review Office",
    "Test Office",
    "Security Office",
    "Manager Office"
  ],
  "agent_count": 6,
  "task_count": 0,
  "agents": [...],
  "tasks": [...]
}
```

### Add Agent

Request:
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_7",
    "name": "Hardware Driver Specialist",
    "role": "Implementation Office"
  }
}
```

Response:
```json
{
  "status": "success",
  "message": "Agent Hardware Driver Specialist added"
}
```

### Create Task

Request:
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_001",
    "title": "Implement USB driver interface",
    "assigned_to": "agent_7"
  }
}
```

Response:
```json
{
  "status": "success",
  "message": "Task created",
  "task_id": "task_001"
}
```

### Process Code

Request:
```json
{
  "method": "process_code",
  "params": {
    "code": "#include <stdio.h>\nint main() {\n    printf(\"Hello\\n\");\n    return 0;\n}",
    "operation": "analyze"
  }
}
```

Response:
```json
{
  "status": "success",
  "analysis": {
    "lines": 5,
    "functions": 1,
    "includes": 1,
    "structs": 0,
    "language": "c"
  }
}
```

## Building Instructions

### Requirements
- GCC 7.0 or later (C11 support)
- Make
- Optional: Valgrind (for memory testing)
- Optional: cppcheck (for static analysis)

### Build Commands

**Production build:**
```bash
make
```

**Debug build with sanitizers:**
```bash
make debug
```

**Run tests:**
```bash
make test
```

**Clean build artifacts:**
```bash
make clean
```

**Install system-wide:**
```bash
sudo make install
```

### Compiler Flags

Production build uses:
- `-std=c11`: C11 standard compliance
- `-Wall -Wextra -Werror`: All warnings, treat as errors
- `-pedantic`: Strict ISO C compliance
- `-O2`: Optimization level 2

Debug build adds:
- `-g`: Debug symbols
- `-fsanitize=address`: Address sanitizer
- `-fsanitize=undefined`: Undefined behavior sanitizer

## Running the Floor

### Interactive Mode
```bash
./department_floor
```

Then send JSON-RPC commands via stdin:
```bash
{"method": "get_info"}
```

### Pipe Mode
```bash
echo '{"method": "get_info"}' | ./department_floor
```

### Script Mode
```bash
./department_floor < commands.json
```

## Memory Management

This implementation follows strict memory management practices:

### Static Allocation
All major data structures use static allocation where possible:
- Agent array: 100 agents maximum
- Task array: 100 tasks maximum
- Input buffer: 8KB line buffer

### Buffer Safety
All string operations use bounded functions:
- `strncpy` instead of `strcpy`
- `snprintf` instead of `sprintf`
- Manual bounds checking for all buffers

### No Memory Leaks
The implementation avoids dynamic allocation in the main processing loop. All allocations (if any) are cleaned up properly.

## Error Handling

Errors are handled explicitly:
- All pointer operations check for NULL
- Buffer operations verify sizes
- JSON parsing validates format
- All errors return proper JSON error responses

Example error response:
```json
{
  "status": "error",
  "message": "Buffer overflow prevented in input parsing"
}
```

## Testing

### Unit Testing
```bash
# Test basic operations
echo '{"method": "get_info"}' | ./department_floor | head -20

# Test agent addition
echo '{"method": "add_agent", "params": {"agent_id": "test_1", "name": "Test", "role": "Tester"}}' | ./department_floor

# Test code analysis
echo '{"method": "process_code", "params": {"code": "int main() {}", "operation": "analyze"}}' | ./department_floor
```

### Memory Testing
```bash
# Build debug version
make debug

# Run with valgrind
echo '{"method": "get_info"}' | valgrind --leak-check=full ./department_floor_debug
```

### Static Analysis
```bash
# Run cppcheck
cppcheck --enable=all --std=c11 department_floor.c
```

## Performance Characteristics

- **Memory footprint**: ~150KB for static data structures
- **Startup time**: < 1ms
- **Request latency**: < 1ms per JSON-RPC request
- **Zero dynamic allocations**: Main processing loop
- **Safe for embedded**: Bounded memory usage

## Integration

The C floor integrates with the executive layer through:
- Standard JSON-RPC protocol
- Stdin/stdout communication
- Non-blocking I/O ready
- Stateful session management

## Compliance

This implementation complies with:
- **Language Sovereignty**: 100% C code
- **Identical Topology**: All 6 offices implemented
- **Contract-Bound Operation**: JSON-RPC protocol
- **Non-Creative Mandate**: Strict request fulfillment
- **Failure Escalation**: Explicit error reporting

## Known Limitations

1. JSON parsing is simplified for production use (full parser would require library)
2. Maximum 100 agents and 100 tasks (configurable via defines)
3. Input line length limited to 8KB
4. Code analysis is pattern-based, not a full parser

## Future Enhancements

- Complete JSON parser implementation
- More sophisticated code analysis
- Support for concurrent requests
- Extended security analysis features
- Integration with static analysis tools

## License

Part of the Miniature Office project. See root LICENSE file.
