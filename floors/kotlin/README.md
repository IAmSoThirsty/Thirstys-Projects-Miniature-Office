# Floor 11 - Kotlin Jurisdiction

**Language:** Kotlin  
**Floor Number:** 11  
**Domain:** JVM modernization, Android development, Backend services, Multiplatform

## Domain and Jurisdiction

Floor 11 is the Kotlin jurisdiction, responsible for modern JVM applications. This floor handles:

- **JVM Modernization**: Next-generation Java replacements with modern language features
- **Android Development**: Native Android apps with Jetpack libraries and Compose
- **Backend Services**: Ktor servers, Spring Boot with Kotlin, microservices
- **Multiplatform**: Shared code across JVM, Android, iOS, JS, Native
- **DSL Creation**: Type-safe builders, domain-specific languages

## Architectural Laws

The Kotlin jurisdiction operates under modern JVM principles with enhanced safety:

### 1. Null Safety First
Kotlin's type system eliminates null pointer exceptions at compile-time:
- All types are non-nullable by default
- Nullable types explicitly marked with `?`
- Safe call operator `?.` for null-safe access
- Elvis operator `?:` for default values
- Smart casts eliminate redundant checks

```kotlin
// Compile error - can't assign null
val name: String = null

// Correct - explicit nullable type
val name: String? = null
val length = name?.length ?: 0
```

### 2. Coroutines for Concurrency
Replace threads and callbacks with structured concurrency:
- Suspend functions for async operations
- Coroutine scopes for lifecycle management
- Flow for reactive streams
- Structured concurrency prevents leaks

```kotlin
suspend fun fetchData(): Data {
    return withContext(Dispatchers.IO) {
        // Async operation
    }
}
```

### 3. Immutability and Data Classes
Prefer immutable data structures:
- Use `val` over `var` whenever possible
- Data classes for value objects
- Copy methods for modifications
- Collection builders return immutable results

```kotlin
data class User(val id: String, val name: String)
val updated = user.copy(name = "New Name")
```

### 4. Functional + Object-Oriented Hybrid
Combine the best of both paradigms:
- Higher-order functions and lambdas
- Extension functions add functionality
- Sealed classes for exhaustive when expressions
- Object expressions for single instances

### 5. Type Safety and Smart Casts
Leverage advanced type system features:
- Reified type parameters
- Inline functions for zero overhead
- Smart casts after type checks
- Contracts for compiler hints

## Security Doctrine

Security in Kotlin builds on JVM security with additional guarantees:

### Compile-Time Null Safety
- No NullPointerExceptions from Kotlin code
- Explicit handling of nullable types
- Platform types from Java require careful handling
- Null checks enforced by compiler

### Coroutine Safety
- Structured concurrency prevents resource leaks
- Cancellation propagates correctly
- Exception handling in coroutine scopes
- Thread-safe by default with proper dispatchers

### Immutability by Design
- Data classes encourage immutable patterns
- Collection methods return new instances
- Copy-on-write semantics
- Const vals for compile-time constants

### Type System Protection
- Sealed classes prevent unauthorized subtyping
- Visibility modifiers (private, internal, protected)
- Inline classes for type-safe wrappers
- Smart casts reduce casting errors

### Android-Specific Security
- Scoped storage APIs
- Biometric authentication
- Encrypted SharedPreferences
- SafetyNet attestation

## Agent Staffing

Floor 11 employs specialized Kotlin agents across 6 offices:

### Architecture Office
- **ServiceAgent**: Designs service layers, API contracts, repository patterns
- **ArchitecturePatternAgent**: MVVM, MVI, Clean Architecture for Android
- **DSLDesignAgent**: Creates type-safe builders and domain-specific languages

### Implementation Office
- **DataModelAgent**: Implements data classes, sealed hierarchies, type-safe models
- **CoroutineImplementer**: Async operations, flows, channel-based communication
- **AndroidUIAgent**: Jetpack Compose, View binding, Navigation component

### Review Office
- **KotlinStyleReviewer**: ktlint compliance, idiom checking
- **NullSafetyReviewer**: Validates null-safety patterns, flag unsafe operations
- **CoroutineReviewer**: Checks structured concurrency, proper scope usage

### Test Office
- **UnitTestAgent**: JUnit 5, MockK, Kotest
- **CoroutineTestAgent**: Tests with TestCoroutineDispatcher, runTest
- **AndroidTestAgent**: Espresso, Compose testing, instrumented tests

### Security Office
- **NullSafetyAuditor**: Scans for unsafe null operations, platform type exposure
- **ConcurrencySafetyAgent**: Race condition detection, thread safety validation
- **AndroidSecurityAgent**: Permission checks, secure storage, obfuscation

### Manager Office
- **ProjectCoordinator**: Gradle build management, dependency coordination
- **MultiplatformManager**: Common/platform-specific code organization
- **ReleaseManager**: Version management, signing, deployment

## API Documentation

### Get Floor Info
```json
{"method": "get_info"}
```

Returns floor metadata, offices, agents, and capabilities.

### Add Agent
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_kt_001",
    "name": "CoroutineExpert",
    "role": "Async Specialist",
    "capabilities": ["coroutines", "flows", "channels"]
  }
}
```

### Create Task
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_001",
    "title": "Implement repository layer",
    "assigned_to": "service_001"
  }
}
```

Tasks are automatically executed by assigned agents using coroutines.

### Process Code
```json
{
  "method": "process_code",
  "params": {
    "code": "suspend fun fetchUser(): User { ... }",
    "operation": "analyze"
  }
}
```

Operations: `analyze`, `format`, `lint`, `compile`

#### Code Analysis
Returns:
- Line count, function count, class count
- Null-safety score (0.0-1.0)
- Coroutine usage metrics
- Language-specific patterns

## Building and Running

### Prerequisites
- JDK 17 or higher
- Gradle 8.x (wrapper included)

### Build
```bash
cd floors/kotlin
./gradlew build
```

### Run
```bash
./gradlew run
```

Or run the compiled JAR:
```bash
java -jar build/libs/department_floor.jar
```

### Create Standalone JAR
```bash
./gradlew jar
# Creates build/libs/department_floor.jar
```

### Test
```bash
./gradlew test
```

## Interactive Testing

Test floor communication:
```bash
# Get floor info
echo '{"method":"get_info"}' | java -jar build/libs/department_floor.jar

# Analyze Kotlin code
echo '{"method":"process_code","params":{"code":"suspend fun main() { }","operation":"analyze"}}' | java -jar build/libs/department_floor.jar

# Create task
echo '{"method":"create_task","params":{"task_id":"t1","title":"Build API","assigned_to":"service_001"}}' | java -jar build/libs/department_floor.jar
```

## Kotlin Features Demonstrated

1. **Null Safety**: Complete elimination of NPEs through type system
2. **Coroutines**: Structured concurrency for async operations
3. **Data Classes**: Immutable value objects with zero boilerplate
4. **Sealed Classes**: Type-safe office hierarchy
5. **Extension Functions**: Enhanced functionality without inheritance
6. **Smart Casts**: Automatic type casting after checks
7. **When Expressions**: Exhaustive pattern matching
8. **Higher-Order Functions**: Functions as first-class citizens
9. **Delegation**: Property delegation, class delegation
10. **Inline Functions**: Zero-overhead abstractions

## Integration with Building

This floor integrates with the main office building through:
- Standard JSON-RPC protocol over stdin/stdout
- Floor registry in `floors/README.md`
- Build script integration in `build_floors.sh`
- Demo pipeline in `demo_pipeline.py`

Floor 11 represents modern JVM development with Kotlin's powerful features, combining null-safety, coroutines, and multiplatform capabilities.
