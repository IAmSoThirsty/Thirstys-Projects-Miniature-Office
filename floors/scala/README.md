# Floor 12 - Scala Jurisdiction

**Language:** Scala  
**Floor Number:** 12  
**Domain:** Functional JVM systems, Big data processing, Type-level programming, Distributed systems

## Domain and Jurisdiction

Floor 12 is the Scala jurisdiction, responsible for functional programming on the JVM. This floor handles:

- **Functional JVM Systems**: Pure functional architectures, effect systems (Cats, ZIO)
- **Big Data Processing**: Apache Spark, Flink, Kafka Streams, distributed computing
- **Type-Level Programming**: Advanced types, implicits/givens, macros, metaprogramming
- **Distributed Systems**: Akka actors, cluster computing, reactive streams
- **Web Services**: Play Framework, http4s, Scalatra
- **Scientific Computing**: Numerical processing, machine learning (Breeze, Spark MLlib)

## Architectural Laws

The Scala jurisdiction operates under functional programming principles:

### 1. Immutability by Default
All data structures are immutable unless explicitly mutable:
- Use `val` for immutable values
- Case classes for immutable data
- Immutable collections (List, Vector, Map)
- Persistent data structures with structural sharing

```scala
case class User(id: String, name: String)
val updated = user.copy(name = "New Name")
val list = List(1, 2, 3)
val newList = 0 :: list  // Prepend, original unchanged
```

### 2. Pure Functions
Functions should be pure - no side effects:
- Same input always produces same output
- No mutation of state
- No I/O in pure functions
- Side effects isolated in effect types (IO, Future, Task)

```scala
// Pure function
def add(a: Int, b: Int): Int = a + b

// Impure operation wrapped in effect
def fetchUser(id: String): IO[User] = IO { /* fetch from DB */ }
```

### 3. Type-Driven Design
Let types guide implementation:
- Algebraic Data Types (ADTs) for domain modeling
- Sealed traits for exhaustive pattern matching
- Type classes for polymorphism
- Higher-kinded types for abstraction

```scala
sealed trait Result[+A]
case class Success[A](value: A) extends Result[A]
case class Failure(error: String) extends Result[Nothing]

// Compiler ensures exhaustive matching
def handle[A](result: Result[A]): String = result match {
  case Success(v) => s"Got: $v"
  case Failure(e) => s"Error: $e"
}
```

### 4. Expression-Oriented Programming
Everything is an expression that returns a value:
- If/else returns values
- Pattern matching returns values
- Try/catch returns values
- For-comprehensions chain operations

```scala
val result = if (condition) "yes" else "no"
val value = try { compute() } catch { case _: Exception => default }
```

### 5. Composition Over Inheritance
Compose behavior rather than inherit:
- Traits for mixins
- Type classes for ad-hoc polymorphism
- Function composition
- Monad/Functor patterns for sequencing

## Security Doctrine

Security in Scala leverages functional principles and type safety:

### Type Safety
- Strong static typing catches errors at compile time
- Pattern matching ensures exhaustive case handling
- Sealed traits prevent unauthorized extensions
- Type classes provide safe polymorphism

### Immutability
- Immutable data prevents accidental mutation
- Thread-safe by default
- No defensive copying needed
- Easier to reason about correctness

### Effect Tracking
- Side effects encoded in types (IO, Future, Task)
- Pure core, imperative shell pattern
- Effects composed safely
- Resource safety with bracket/managed

### Null Safety
- Prefer Option over null
- Compiler can't enforce null checks (from Java)
- Try for exception handling
- Either for error handling

### Concurrency Safety
- Immutable data prevents race conditions
- Futures for async operations
- Akka actors for isolated state
- STM (Software Transactional Memory) options

## Agent Staffing

Floor 12 employs functional Scala agents across 6 offices:

### Architecture Office
- **FunctionalArchitect**: Designs pure functional architectures, effect systems
- **TypeSystemDesigner**: Advanced types, type classes, higher-kinded types
- **DistributedSystemsAgent**: Akka, cluster design, reactive patterns

### Implementation Office
- **DataModelAgent**: Case classes, ADTs, immutable models
- **PureFunctionAgent**: Pure function implementation, referential transparency
- **EffectSystemAgent**: IO, ZIO, Cats Effect integration

### Review Office
- **FunctionalReviewer**: Checks purity, immutability, functional patterns
- **TypeSafetyReviewer**: Validates type usage, pattern matching exhaustiveness
- **ScalafmtAgent**: Code formatting, style guide compliance

### Test Office
- **PropertyBasedTester**: ScalaCheck property-based tests
- **UnitTestAgent**: ScalaTest, Specs2, MUnit
- **IntegrationTestAgent**: End-to-end tests, Akka TestKit

### Security Office
- **TypeSafetyAuditor**: Validates type safety, prevents unsafe operations
- **EffectTrackingAgent**: Ensures side effects are properly tracked
- **ImmutabilityChecker**: Scans for mutable state, thread safety issues

### Manager Office
- **BuildManager**: sbt builds, multi-project coordination
- **DependencyCoordinator**: Library management, version resolution
- **ReleaseManager**: Publishing, versioning, artifact management

## API Documentation

### Get Floor Info
```json
{"method": "get_info"}
```

Returns floor metadata, offices, agents, and functional capabilities.

### Add Agent
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_scala_001",
    "name": "FunctionalExpert",
    "role": "Pure Function Specialist",
    "capabilities": ["pure_functions", "immutability", "type_classes"]
  }
}
```

### Create Task
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_001",
    "title": "Implement repository with IO",
    "assigned_to": "service_001"
  }
}
```

Tasks executed functionally with proper effect handling.

### Process Code
```json
{
  "method": "process_code",
  "params": {
    "code": "def pure(x: Int): Int = x * 2",
    "operation": "analyze"
  }
}
```

Operations: `analyze`, `format`, `lint`, `compile`

#### Code Analysis
Returns:
- Line count, function/class/trait/object counts
- Functional score (usage of map, flatMap, etc.)
- Immutability score (val vs var ratio)
- Language-specific patterns

## Building and Running

### Prerequisites
- JDK 11 or higher
- sbt 1.9.x (will be downloaded automatically)

### Build
```bash
cd floors/scala
sbt compile
```

### Create JAR
```bash
sbt assembly
# Creates target/scala-3.3.1/department_floor.jar
```

### Run
```bash
sbt run
```

Or run the JAR:
```bash
java -jar target/scala-3.3.1/department_floor.jar
```

### Test
```bash
sbt test
```

### Build Script
```bash
./build.sh
```

## Interactive Testing

Test floor communication:
```bash
# Get floor info
echo '{"method":"get_info"}' | sbt run

# Analyze Scala code
echo '{"method":"process_code","params":{"code":"val x = 42","operation":"analyze"}}' | sbt run

# Create task
echo '{"method":"create_task","params":{"task_id":"t1","title":"Build API","assigned_to":"service_001"}}' | sbt run
```

With JAR:
```bash
echo '{"method":"get_info"}' | java -jar target/scala-3.3.1/department_floor.jar
```

## Scala Features Demonstrated

1. **Case Classes**: Immutable data with structural equality
2. **Pattern Matching**: Exhaustive, type-safe matching
3. **Sealed Traits**: Closed type hierarchies
4. **For-Comprehensions**: Monadic composition
5. **Higher-Order Functions**: Functions as first-class values
6. **Implicits/Givens**: Context passing, type classes
7. **Options**: Null-safe value handling
8. **Try/Either**: Functional error handling
9. **Futures**: Async computation
10. **Collections**: Rich, immutable collection library

## Functional Programming Patterns

### Option for Null Safety
```scala
def findUser(id: String): Option[User] = ???
val name = findUser("123").map(_.name).getOrElse("Unknown")
```

### Either for Error Handling
```scala
def divide(a: Int, b: Int): Either[String, Int] =
  if (b == 0) Left("Division by zero")
  else Right(a / b)
```

### For-Comprehensions
```scala
for {
  user <- getUser(id)
  posts <- getPosts(user)
  comments <- getComments(posts)
} yield comments
```

### Type Classes
```scala
trait Show[A] {
  def show(a: A): String
}

given Show[Int] = (a: Int) => a.toString
```

## Integration with Building

This floor integrates with the main office building through:
- Standard JSON-RPC protocol over stdin/stdout
- Floor registry in `floors/README.md`
- Build script integration in `build_floors.sh`
- Demo pipeline in `demo_pipeline.py`

Floor 12 represents functional programming excellence on the JVM, combining immutability, type safety, and pure functional patterns for robust distributed systems.
