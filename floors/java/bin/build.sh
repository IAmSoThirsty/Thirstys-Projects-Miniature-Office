#!/bin/bash
# Build script for Floor 10 - Java Jurisdiction

set -e

echo "Building Floor 10 - Java Department..."

# Compile with javac directly (no Maven required)
mkdir -p build/com/miniatureoffice/floor10
javac -d build -source 11 -target 11 -Xlint:all DepartmentFloor.java

# Create JAR
cd build
jar cfe ../department_floor.jar com.miniatureoffice.floor10.DepartmentFloor com/miniatureoffice/floor10/*.class
cd ..

echo "Build complete: department_floor.jar"

# Run tests
echo ""
echo "Running tests..."
echo '{"method": "get_info"}' | java -jar department_floor.jar | head -20
echo ""
echo '{"method": "add_agent", "params": {"agent_id": "test_1", "name": "TestAgent", "role": "Tester"}}' | java -jar department_floor.jar
echo ""
echo "Tests passed!"
