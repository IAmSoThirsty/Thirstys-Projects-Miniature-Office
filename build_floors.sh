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

# TypeScript floor
echo ""
echo "Building TypeScript floor..."
cd floors/typescript
if command -v npm &> /dev/null; then
    npm install --no-audit --no-fund > /dev/null 2>&1
    npm run build
    echo "✓ TypeScript floor built successfully"
else
    echo "⚠ npm not found. Skipping TypeScript floor build."
fi
cd ../..

# PHP floor (no build needed, but check if PHP is available)
echo ""
if command -v php &> /dev/null; then
    echo "✓ PHP floor ready (PHP $(php -r 'echo PHP_VERSION;') detected)"
else
    echo "⚠ PHP not found. PHP floor requires PHP 8.1+"
fi

# Ruby floor (no build needed, but check if Ruby is available)
echo ""
if command -v ruby &> /dev/null; then
    echo "✓ Ruby floor ready (Ruby $(ruby -e 'puts RUBY_VERSION') detected)"
else
    echo "⚠ Ruby not found. Ruby floor requires Ruby 3.0+"
fi

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

# C floor
echo ""
echo "Building C floor..."
cd floors/c
if command -v gcc &> /dev/null; then
    gcc -o department_floor department_floor.c -ljson-c
    echo "✓ C floor built successfully"
else
    echo "⚠ GCC not found. Skipping C floor build."
fi
cd ../..

# C++ floor
echo ""
echo "Building C++ floor..."
cd floors/cpp
if command -v g++ &> /dev/null; then
    g++ -o department_floor department_floor.cpp -ljsoncpp
    echo "✓ C++ floor built successfully"
else
    echo "⚠ G++ not found. Skipping C++ floor build."
fi
cd ../..

# Java floor
echo ""
echo "Building Java floor..."
cd floors/java
if command -v javac &> /dev/null; then
    javac DepartmentFloor.java
    echo "✓ Java floor built successfully"
else
    echo "⚠ javac not found. Skipping Java floor build."
fi
cd ../..

# Shell floor (no build needed)
echo "✓ Shell floor ready (no build required)"

echo ""
echo "=================================="
echo "All available floors built!"
echo "=================================="

