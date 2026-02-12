#!/bin/bash
# Swift Floor 13 Build Script

set -e

echo "Building Swift Floor 13..."

# Build with Swift Package Manager
swift build -c release

echo "Swift Floor 13 built successfully!"
echo "Binary location: .build/release/department_floor"
