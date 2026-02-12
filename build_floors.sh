#!/bin/bash
# Build script for all multi-language floors

set -e

echo "=================================="
echo "Building Multi-Language Floors"
echo "=================================="
echo ""

# Python floor (no build needed)
echo "✓ Python floor ready (no build required)"

# JavaScript floor (no build needed)
echo "✓ JavaScript floor ready (no build required)"

# Rust floor
echo ""
echo "Building Rust floor..."
cd floors/rust
if command -v cargo &> /dev/null; then
    cargo build
    echo "✓ Rust floor built successfully"
else
    echo "⚠ Cargo not found. Skipping Rust floor build."
fi
cd ../..

# Go floor (build optional, can run with 'go run')
echo ""
echo "Building Go floor..."
cd floors/go
if command -v go &> /dev/null; then
    go build -o department_floor department_floor.go
    echo "✓ Go floor built successfully"
else
    echo "⚠ Go not found. Skipping Go floor build."
fi
cd ../..

# Shell floor (no build needed)
echo "✓ Shell floor ready (no build required)"

echo ""
echo "=================================="
echo "All available floors built!"
echo "=================================="
