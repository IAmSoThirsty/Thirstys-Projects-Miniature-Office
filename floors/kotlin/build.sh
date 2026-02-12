#!/bin/bash
# Kotlin Floor 11 Build Script

set -e

echo "Building Kotlin Floor 11..."

# Create gradle wrapper if not exists
if [ ! -f gradlew ]; then
    gradle wrapper --gradle-version 8.5
fi

# Build the project
./gradlew clean build jar

echo "Kotlin Floor 11 built successfully!"
echo "JAR location: build/libs/department_floor.jar"
