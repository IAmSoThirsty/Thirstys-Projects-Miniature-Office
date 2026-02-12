# Floor 2 - Rust Jurisdiction

**Language:** Rust  
**Floor Number:** 2  
**Domain:** Memory-safe systems, Performance-critical logic, FFI boundaries

## Architecture

This floor implements department logic in Rust, demonstrating:
- **Language Sovereignty**: All code written in Rust
- **Identical Topology**: 6 offices (Architecture, Implementation, Review, Test, Security, Manager)
- **Ownership Correctness**: Strict Rust ownership rules
- **Zero Unsafe**: No unsafe blocks (unless justified)
- **Clippy Compliance**: Rust best practices enforced

## Prerequisites

- Rust 1.70+ (with cargo)

## Building

```bash
cargo build
```

For release build:
```bash
cargo build --release
```

## Running

After building:
```bash
./target/debug/department_floor
```

Or for release:
```bash
./target/release/department_floor
```

Or run directly with cargo:
```bash
cargo run
```

## API

Same JSON-RPC API as other floors. See Python floor README for examples.

## Testing

Test the floor interactively:
```bash
echo '{"method": "get_info"}' | cargo run
```

## Linting

```bash
cargo clippy
cargo fmt --check
```
