#!/bin/bash
# Scala Floor 12 Build Script

set -e

echo "Building Scala Floor 12..."

# Build with sbt
sbt clean compile assembly

echo "Scala Floor 12 built successfully!"
echo "JAR location: target/scala-3.3.1/department_floor.jar"
