# Multi-Language Floor Implementations

## Overview

This directory contains the actual implementations of each department floor in their respective programming languages, demonstrating "displayed and transparent Diversity flexibility."

## Structure

Each floor has its own subdirectory containing:
- Department logic implementation in the floor's language
- API/Interface for communication with the main system
- Build scripts (if needed)
- Floor-specific configuration

## Floors

### Floor 1 - Python (`python/`)
**Language**: Python
**Domain**: Application logic, automation, data processing

### Floor 2 - Rust (`rust/`)
**Language**: Rust
**Domain**: Memory-safe systems, performance-critical logic

### Floor 4 - JavaScript/TypeScript (`javascript/`)
**Language**: JavaScript/TypeScript
**Domain**: Frontend logic, Node services

### Floor 5 - Go (`go/`)
**Language**: Go
**Domain**: Network services, concurrency

### Floor 3 - C (`c/`)
**Language**: C
**Domain**: Low-level systems, embedded

### Floor 4 - C++ (`cpp/`)
**Language**: C++
**Domain**: High-performance systems

### Floor 7 - Shell (`shell/`)
**Language**: Bash
**Domain**: System automation, orchestration

## Communication Protocol

All floors communicate via:
1. **JSON-RPC over stdin/stdout** - For synchronous operations
2. **HTTP REST API** - For async operations (optional)
3. **Message Queue** - For event-driven operations (optional)

## Building

See individual floor READMEs for build instructions.

Quick build all:
```bash
./build_all_floors.sh
```

## Running

The main Python application automatically spawns and manages floor processes.
