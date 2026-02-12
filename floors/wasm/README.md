# Floor 27 - WebAssembly Jurisdiction

**Language:** WebAssembly (Rust compiled to WASM)  
**Floor Number:** 27  
**Domain:** Portable bytecode, browser/embedded execution, sandboxed computation

## Domain and Jurisdiction

Floor 27 governs the realm of WebAssembly - portable, size-optimized bytecode that executes deterministically across diverse environments. This jurisdiction specializes in:

- **Portable Bytecode Execution**: Code that runs identically in browsers, servers, and embedded systems
- **Sandboxed Computation**: Capability-based security with zero host access by default
- **Size-Optimized Binaries**: Aggressive optimization for minimal binary footprint
- **Linear Memory Management**: Efficient memory layouts within WASM's linear memory model
- **Host Function Bindings**: Safe interoperability with JavaScript and native hosts

## Architectural Laws

The WebAssembly jurisdiction operates under strict architectural principles:

1. **Memory Safety Absolute**: No unsafe operations; all memory access bounds-checked
2. **Size Optimization Enforced**: Binary size minimization through dead code elimination and LTO
3. **Deterministic Execution Required**: All operations produce identical results across platforms
4. **Sandbox Integrity Maintained**: Capability-based security model strictly enforced
5. **Format Compliance**: Strict adherence to WebAssembly binary format specification

## Security Doctrine

Security in the WebAssembly jurisdiction follows defense-in-depth principles:

- **Sandboxed by Design**: Code executes in isolated environment with no default host access
- **Capability-Based Security**: Explicit import declarations for all external interactions
- **Linear Memory Bounds**: Hardware-enforced memory bounds checking
- **No Undefined Behavior**: Rust's safety guarantees prevent UB in compilation
- **Side-Channel Mitigation**: Constant-time operations for security-critical code

## Office Structure

### Architecture Office
**Staffing:** 3+ agents  
**Responsibilities:**
- WASM module design and structure planning
- Memory layout optimization for linear memory model
- Interface definitions and import/export design
- Binary format standards and section layout

**Key Agents:**
- Module Architect - Overall module structure and design
- Memory Architect - Linear memory layout and heap management  
- Binary Architect - Binary format and section optimization

### Implementation Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Rust to WASM compilation using rustc + wasm32 target
- wasm-bindgen integration for JavaScript interop
- Host function bindings and import implementation
- Linear memory management and allocator implementation

**Key Agents:**
- Rust-WASM Compiler - Compilation pipeline management
- Binding Engineer - Host function bindings and JS interop
- Memory Engineer - Allocator and linear memory implementation

### Review Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Bytecode validation and format verification
- Import/export interface verification
- Size optimization review and dead code detection
- Binary format compliance checking

**Key Agents:**
- Bytecode Validator - WASM binary validation
- Size Optimizer - Binary size reduction specialist
- Export Auditor - Interface surface area review

### Test Office
**Staffing:** 3+ agents  
**Responsibilities:**
- WASM runtime testing (wasmtime, wasmer, browser engines)
- Cross-platform validation across different hosts
- Performance benchmarking and profiling
- Memory safety verification and bounds checking tests

**Key Agents:**
- Runtime Tester - Testing across WASM runtimes
- Browser Tester - Cross-browser validation
- Performance Tester - Benchmarking and optimization validation

### Security Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Sandbox integrity verification
- Capability enforcement and permission auditing
- Memory bounds checking validation
- Side-channel analysis and timing attack prevention

**Key Agents:**
- Sandbox Guardian - Isolation and sandbox integrity
- Bounds Checker - Memory safety verification
- Capability Auditor - Permission and capability analysis

### Manager Office
**Staffing:** 3+ agents  
**Responsibilities:**
- Build orchestration (cargo build --target wasm32-unknown-unknown)
- Deployment coordination to npm/CDN/embedded targets
- Performance monitoring and binary size tracking
- CI/CD pipeline management

**Key Agents:**
- Build Manager - Build pipeline orchestration
- Deployment Manager - Multi-target deployment
- Metrics Manager - Size and performance monitoring

## Specialized Agent Modules

### Service Agent
**Role:** External communications and I/O orchestration  
**Capabilities:**
- JSON-RPC request handling over stdin/stdout
- I/O multiplexing and message routing
- Protocol adaptation for different hosts
- Connection lifecycle management

### Data Agent
**Role:** State management and memory optimization  
**Capabilities:**
- Linear memory layout optimization
- Memory allocation and deallocation tracking
- Serialization/deserialization for host communication
- Cache management and state persistence

### Operations Agent
**Role:** Computational execution and code analysis  
**Capabilities:**
- WASM module analysis and validation
- Bytecode verification and structure analysis
- Performance profiling and optimization
- Size estimation and optimization recommendations

## API Documentation

The floor communicates via JSON-RPC over stdin/stdout.

### get_info
Returns comprehensive floor information including all offices, agents, and specialized modules.

```json
{"method": "get_info"}
```

Response includes:
- Floor metadata (number, language, domain)
- All 6 offices with agent counts and responsibilities
- Specialized agent status (Service, Data, Operations)
- Architectural laws and security doctrine
- Complete agent and task listings

### add_agent
Add a new agent to a specific office.

```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "custom-001",
    "name": "Custom Agent",
    "role": "Specialist",
    "capabilities": ["WASM optimization", "Binary analysis"],
    "office": "Implementation"
  }
}
```

### create_task
Create a task assigned to a specific agent and office.

```json
{
  "method": "create_task",
  "params": {
    "task_id": "task-001",
    "title": "Optimize WASM binary size",
    "assigned_to": "impl-001",
    "office": "Implementation"
  }
}
```

### process_code
Process WASM or Rust code with various operations.

**Operations:**
- `analyze` - Analyze WASM module structure
- `validate` - Validate WASM binary format
- `optimize` - Optimize for size and performance
- `compile` - Compile Rust to WASM

```json
{
  "method": "process_code",
  "params": {
    "code": "export fn calculate() { ... }",
    "operation": "analyze"
  }
}
```

Analysis returns:
- Line and function counts
- Import/export counts
- Memory reference analysis
- Size estimates
- Validation status

## Building and Running

### Prerequisites

- Rust 1.70+ with cargo
- wasm32-unknown-unknown target (for WASM compilation)
- wasm-bindgen-cli (optional, for JS bindings)

Install WASM target:
```bash
rustup target add wasm32-unknown-unknown
```

### Building (Native Binary)

Build as standard Rust application for native execution:

```bash
cd floors/wasm
cargo build --release
```

Run the native binary:
```bash
./target/release/department_floor
```

### Building (WebAssembly)

Build as WASM module:

```bash
cargo build --release --target wasm32-unknown-unknown
```

Output: `target/wasm32-unknown-unknown/release/department_floor.wasm`

Optimize with wasm-opt (from binaryen):
```bash
wasm-opt -Oz -o optimized.wasm target/wasm32-unknown-unknown/release/department_floor.wasm
```

### Running

**Native mode** (JSON-RPC via stdin/stdout):
```bash
echo '{"method": "get_info"}' | cargo run --release
```

**Test compilation targets:**
```bash
# Native
cargo build --release

# WASM
cargo build --release --target wasm32-unknown-unknown

# Check both work
echo '{"method": "get_info"}' | ./target/release/department_floor
```

### Interactive Testing

```bash
cargo run --release
```

Then type JSON-RPC commands:
```json
{"method": "get_info"}
{"method": "process_code", "params": {"code": "export fn test() {}", "operation": "analyze"}}
```

## Testing

Run unit tests:
```bash
cargo test
```

Run with verbose output:
```bash
cargo test -- --nocapture
```

Test WASM compilation:
```bash
cargo test --target wasm32-unknown-unknown
```

## Linting and Code Quality

```bash
# Check formatting
cargo fmt --check

# Run clippy
cargo clippy -- -D warnings

# Check for both targets
cargo clippy --target wasm32-unknown-unknown
```

## Size Optimization

For minimal WASM binaries:

1. Use release profile with size optimization:
```toml
[profile.release]
opt-level = "z"
lto = true
codegen-units = 1
panic = "abort"
```

2. Strip symbols:
```bash
wasm-strip target/wasm32-unknown-unknown/release/department_floor.wasm
```

3. Use wasm-opt:
```bash
wasm-opt -Oz target/wasm32-unknown-unknown/release/department_floor.wasm -o optimized.wasm
```

4. Analyze size:
```bash
twiggy top optimized.wasm
```

## Performance Considerations

- **Linear Memory**: All allocations use WASM's linear memory model
- **Zero-Copy Serialization**: Minimize data copying across host boundaries
- **Inline Small Functions**: Aggressive inlining for reduced call overhead
- **Dead Code Elimination**: LTO and tree-shaking remove unused code
- **Size vs Speed**: Profile configured for size (`opt-level = "z"`)

## Deployment Targets

- **Browsers**: Chrome, Firefox, Safari, Edge with wasm-bindgen
- **Server-side**: wasmtime, wasmer, WAMR
- **Embedded**: wasm3, wasm-micro-runtime
- **CDN**: Deploy to npm, unpkg, jsdelivr
- **Edge Computing**: Cloudflare Workers, Fastly Compute@Edge

## Security Best Practices

1. **Audit Imports**: Review all imported host functions
2. **Minimize Surface**: Export only necessary functions
3. **Validate Inputs**: Check all data from host environment
4. **Bounds Checking**: Rely on WASM's automatic bounds checks
5. **Constant Time**: Use constant-time operations for crypto
6. **Memory Limits**: Set appropriate linear memory size limits

## Troubleshooting

**Build fails with wasm32 target:**
```bash
rustup target add wasm32-unknown-unknown
```

**Large binary size:**
- Use `opt-level = "z"` in Cargo.toml
- Enable LTO: `lto = true`
- Run wasm-opt: `wasm-opt -Oz`
- Check for bloat: `twiggy top module.wasm`

**Runtime errors:**
- Check imported functions are provided by host
- Verify linear memory size is sufficient
- Validate exports match host expectations

## Integration Examples

### JavaScript (Browser)
```javascript
const wasmModule = await WebAssembly.instantiateStreaming(
  fetch('department_floor.wasm')
);
// Use exported functions
```

### Node.js
```javascript
const fs = require('fs');
const wasmBuffer = fs.readFileSync('department_floor.wasm');
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
```

### Rust (wasmtime)
```rust
use wasmtime::*;
let engine = Engine::default();
let module = Module::from_file(&engine, "department_floor.wasm")?;
```

## Contributing

When extending Floor 27:
- Maintain memory safety (no `unsafe` without justification)
- Optimize for binary size
- Document all imports/exports
- Test on multiple WASM runtimes
- Profile and benchmark changes
- Update agent staffing as needed

## License

See repository LICENSE file.
