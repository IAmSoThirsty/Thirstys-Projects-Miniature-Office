# Floor 10 - Java Jurisdiction

**Language:** Java  
**Floor Number:** 10  
**Domain:** Enterprise systems, Web services, Business applications, Cloud services

## Domain and Jurisdiction

Floor 10 is the Java jurisdiction, responsible for enterprise-grade systems. This floor handles:

- **Enterprise Systems**: Large-scale business applications, ERP, CRM
- **Web Services**: REST APIs, SOAP services, microservices
- **Business Applications**: Workflow management, transaction processing
- **Cloud Services**: AWS, Azure, GCP integrations, serverless
- **Middleware**: Application servers, message queues, integration platforms

## Architectural Laws

The Java jurisdiction operates under enterprise patterns and SOLID principles:

### 1. Object-Oriented Design
Everything is an object. Use proper encapsulation, inheritance, and polymorphism. Favor composition over inheritance.

### 2. SOLID Principles
- **S**ingle Responsibility: Each class has one reason to change
- **O**pen/Closed: Open for extension, closed for modification
- **L**iskov Substitution: Subtypes must be substitutable for base types
- **I**nterface Segregation: Many specific interfaces > one general interface
- **D**ependency Inversion: Depend on abstractions, not concretions

### 3. Enterprise Patterns
Leverage proven patterns:
- Factory for object creation
- Singleton for shared resources
- Observer for event handling
- Strategy for algorithms
- Decorator for extending functionality

### 4. Type Safety and Immutability
- Use generics for type safety
- Prefer immutable objects where possible
- Use `final` for constants and immutable references
- Defensive copying for mutable data

### 5. Exception Handling
- Use checked exceptions for recoverable conditions
- Use unchecked exceptions for programming errors
- Always clean up resources (try-with-resources)
- Never swallow exceptions silently

## Security Doctrine

Security in Java leverages the platform's built-in capabilities:

### Type Safety
- Strong static typing prevents type confusion
- Generics provide compile-time type checking
- No pointer arithmetic or buffer overflows
- Array bounds checking at runtime

### Memory Management
- Automatic garbage collection
- No manual memory management
- No dangling pointers
- No memory leaks from proper resource handling

### Exception Safety
- Try-with-resources for automatic resource cleanup
- Finally blocks for cleanup code
- Exception hierarchies for proper handling
- Stack traces for debugging

### Security Manager
- Sandboxing capabilities
- Permission-based access control
- Secure classloading
- Cryptography APIs (JCE)

### Input Validation
- Always validate external input
- Use prepared statements for SQL
- Sanitize data for XSS prevention
- Validate file paths and names

## Office Structure

All six offices are staffed with Java specialists:

### 1. Architecture Office
**Role:** Enterprise architecture, design patterns, system design  
**Minimum Staff:** 3 agents  
**Agents:**
- Enterprise Architect: Overall system architecture
- Pattern Specialist: Design patterns and best practices
- Integration Architect: System integration design

**Capabilities:**
- UML and system modeling
- Design patterns (GoF, Enterprise)
- Microservices architecture
- Domain-driven design (DDD)
- Service-oriented architecture (SOA)

### 2. Implementation Office
**Role:** Java development using enterprise frameworks  
**Minimum Staff:** 3 agents  
**Agents:**
- Spring Developer: Spring Framework expertise
- Persistence Expert: JPA, Hibernate, database access
- Microservices Developer: Spring Boot, Spring Cloud

**Capabilities:**
- Spring Framework (Core, MVC, Boot)
- Java EE / Jakarta EE
- Hibernate / JPA
- RESTful web services
- Dependency injection

### 3. Review Office
**Role:** Code quality and standards compliance  
**Minimum Staff:** 3 agents  
**Agents:**
- Code Quality Specialist: SonarQube, code metrics
- Standards Enforcer: Checkstyle, PMD, SpotBugs
- Best Practices Reviewer: Java best practices

**Capabilities:**
- SonarQube analysis
- Checkstyle enforcement
- PMD and SpotBugs
- Code coverage (JaCoCo)
- Effective Java guidelines

### 4. Test Office
**Role:** Comprehensive testing strategy  
**Minimum Staff:** 3 agents  
**Agents:**
- Unit Test Engineer: JUnit, TestNG
- Integration Test Engineer: Spring Test, Testcontainers
- Performance Test Engineer: JMeter, Gatling

**Capabilities:**
- JUnit 5 testing
- Mockito for mocking
- Spring Test framework
- Integration testing
- Performance testing

### 5. Security Office
**Role:** Application security and vulnerability management  
**Minimum Staff:** 3 agents  
**Agents:**
- OWASP Specialist: OWASP Top 10 mitigation
- Security Auditor: Dependency scanning, SAST
- Penetration Tester: Security testing

**Capabilities:**
- OWASP dependency check
- Static analysis (FindSecBugs)
- Security scanning
- Authentication/Authorization (Spring Security)
- JWT and OAuth2

### 6. Manager Office
**Role:** Agile project management and coordination  
**Minimum Staff:** 3 agents  
**Agents:**
- Scrum Master: Agile ceremonies and process
- Release Manager: Build and deployment pipeline
- Team Coordinator: Cross-office communication

**Capabilities:**
- Agile/Scrum methodology
- CI/CD pipeline (Jenkins, GitLab)
- Maven/Gradle build tools
- Release management
- Team coordination

## API Documentation

The Java Department Floor implements JSON-RPC protocol with enterprise reliability.

### Get Floor Information

Request:
```json
{"method": "get_info"}
```

Response:
```json
{
  "status": "success",
  "floor_number": 10,
  "language": "java",
  "domain": "Enterprise systems, Web services, Business applications",
  "offices": [
    "Architecture Office",
    "Implementation Office",
    "Review Office",
    "Test Office",
    "Security Office",
    "Manager Office"
  ],
  "agent_count": 6,
  "task_count": 0,
  "agents": [...],
  "tasks": [...]
}
```

### Add Agent

Request:
```json
{
  "method": "add_agent",
  "params": {
    "agent_id": "agent_7",
    "name": "Spring Boot Specialist",
    "role": "Implementation Office"
  }
}
```

Response:
```json
{
  "status": "success",
  "message": "Agent added"
}
```

### Create Task

Request:
```json
{
  "method": "create_task",
  "params": {
    "task_id": "task_001",
    "title": "Implement REST API endpoints",
    "assigned_to": "agent_7"
  }
}
```

Response:
```json
{
  "status": "success",
  "message": "Task created",
  "task_id": "task_001"
}
```

### Process Code

Request:
```json
{
  "method": "process_code",
  "params": {
    "code": "public class Foo {\n    public void bar() {}\n}",
    "operation": "analyze"
  }
}
```

Response:
```json
{
  "status": "success",
  "analysis": {
    "lines": 3,
    "classes": 1,
    "methods": 1,
    "packages": 0,
    "interfaces": 0,
    "language": "java"
  }
}
```

## Building Instructions

### Requirements
- Java JDK 11 or later
- Optional: Maven 3.6+ (for Maven build)
- Optional: Gradle 7+ (for Gradle build)

### Build Methods

#### Method 1: Using Shell Script (Simplest)
```bash
./build.sh
```

This creates `department_floor.jar` directly using javac and jar.

#### Method 2: Using Maven
```bash
mvn clean package
```

This creates:
- `target/floor10-java-1.0.0.jar` - Regular JAR
- `target/floor10-java-1.0.0-jar-with-dependencies.jar` - Fat JAR

#### Method 3: Direct Compilation
```bash
# Compile
mkdir -p build/com/miniatureoffice/floor10
javac -d build -source 11 -target 11 DepartmentFloor.java

# Create JAR
cd build
jar cfe ../department_floor.jar com.miniatureoffice.floor10.DepartmentFloor \
    com/miniatureoffice/floor10/*.class
cd ..
```

### Running Tests

With Maven:
```bash
mvn test
```

With shell script:
```bash
./build.sh
```

Manual testing:
```bash
echo '{"method": "get_info"}' | java -jar department_floor.jar
```

### Code Quality Checks

With Maven:
```bash
# Run Checkstyle
mvn checkstyle:check

# Run all quality checks
mvn verify
```

## Running the Floor

### Using JAR file
```bash
java -jar department_floor.jar
```

### Using class files directly
```bash
java -cp build com.miniatureoffice.floor10.DepartmentFloor
```

### Interactive Mode
```bash
java -jar department_floor.jar
```

Then enter JSON-RPC commands.

### Pipe Mode
```bash
echo '{"method": "get_info"}' | java -jar department_floor.jar
```

### Script Mode
```bash
java -jar department_floor.jar < commands.json
```

## Java Design Patterns Used

### Immutability Pattern
```java
public class Agent {
    private final String agentId;  // Immutable field
    private final String name;
    
    public Agent(String id, String name) {
        this.agentId = Objects.requireNonNull(id);
        this.name = Objects.requireNonNull(name);
    }
}
```

### Builder Pattern (if extended)
```java
Agent agent = Agent.builder()
    .id("agent_1")
    .name("John Doe")
    .role("Developer")
    .build();
```

### Factory Pattern
```java
public class AgentFactory {
    public static Agent createAgent(String type, String id, String name) {
        // Factory logic
    }
}
```

### Thread Safety
```java
// ConcurrentHashMap for thread-safe collections
private final Map<String, Agent> agents = new ConcurrentHashMap<>();

// Volatile for visibility
private volatile boolean active;
```

## Memory Management

### Automatic Garbage Collection
- No manual memory management required
- Objects collected when no longer referenced
- Weak/Soft/Phantom references for caching

### Resource Management
```java
// Try-with-resources for automatic cleanup
try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
    // Use reader
} // Automatically closed
```

### Best Practices
- Avoid memory leaks from listeners and callbacks
- Close streams and resources
- Be careful with static collections
- Use weak references for caches

## Error Handling

### Exception Hierarchy
```java
try {
    // Code that may throw
} catch (SpecificException e) {
    // Handle specific case
} catch (Exception e) {
    // Handle general case
} finally {
    // Always executed
}
```

### Custom Exceptions
```java
public class FloorException extends RuntimeException {
    public FloorException(String message) {
        super(message);
    }
}
```

## Testing

### Unit Testing with JUnit 5
```bash
# With Maven
mvn test

# Manual test commands
echo '{"method": "get_info"}' | java -jar department_floor.jar | head -20
echo '{"method": "add_agent", "params": {"agent_id": "test_1", "name": "Test", "role": "Tester"}}' | java -jar department_floor.jar
echo '{"method": "process_code", "params": {"code": "public class Test {}", "operation": "analyze"}}' | java -jar department_floor.jar
```

### Integration Testing
```java
@Test
public void testAgentManagement() {
    DepartmentFloor floor = new DepartmentFloor();
    floor.addAgent("test_1", "Test Agent", "Tester", 
                   Arrays.asList("testing"));
    // Assertions
}
```

### Performance Testing
```bash
# Benchmark with time
time bash -c 'for i in {1..100}; do echo "{\"method\": \"get_info\"}" | java -jar department_floor.jar > /dev/null; done'
```

## Performance Characteristics

- **Startup time**: ~100-500ms (JVM startup)
- **Request latency**: ~1-5ms per JSON-RPC request (after warmup)
- **Memory footprint**: ~50MB JVM heap
- **Garbage collection**: G1GC (default Java 11+)
- **Concurrent requests**: Thread-safe with ConcurrentHashMap

## JVM Tuning

### Production Settings
```bash
java -Xms256m -Xmx512m \
     -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=200 \
     -jar department_floor.jar
```

### Development Settings
```bash
java -Xms128m -Xmx256m \
     -XX:+UseSerialGC \
     -jar department_floor.jar
```

## Java Features Utilized

### Java 11 Features
- Local variable type inference (`var`)
- HTTP Client API
- String methods (`isBlank()`, `lines()`)
- Optional improvements

### Modern Java Patterns
- Immutable objects with `final`
- Try-with-resources
- Diamond operator
- Lambda expressions
- Stream API (for extensions)
- Optional for null safety

## Integration

### Spring Boot Integration
This can be extended to run as a Spring Boot application:
```java
@SpringBootApplication
public class FloorApplication {
    public static void main(String[] args) {
        SpringApplication.run(FloorApplication.class, args);
    }
}
```

### RESTful API Wrapper
Wrap the JSON-RPC interface in REST endpoints:
```java
@RestController
@RequestMapping("/api/floor")
public class FloorController {
    @PostMapping("/request")
    public ResponseEntity<String> handleRequest(@RequestBody String request) {
        return ResponseEntity.ok(floor.handleRequest(request));
    }
}
```

## Compliance

This implementation complies with:
- **Language Sovereignty**: 100% Java code
- **Identical Topology**: All 6 offices implemented
- **Contract-Bound Operation**: JSON-RPC protocol
- **Non-Creative Mandate**: Strict request fulfillment
- **Failure Escalation**: Proper exception handling
- **Java Best Practices**: Effective Java guidelines

## Known Limitations

1. JSON parsing is simplified (production would use Jackson or Gson)
2. No persistence layer (in-memory only)
3. Single-threaded request processing
4. No logging framework (uses System.err)

## Future Enhancements

- Jackson or Gson for JSON parsing
- Spring Framework integration
- RESTful API wrapper
- Persistence with JPA/Hibernate
- Logging with SLF4J/Logback
- Metrics with Micrometer
- Distributed tracing with Zipkin
- Container deployment with Docker
- Kubernetes orchestration

## Dependencies

Core implementation has zero dependencies (standard library only).

Optional dependencies (via Maven):
- JUnit 5 for testing
- Jackson for JSON (if added)
- Spring Boot for enterprise features (if added)

## License

Part of the Miniature Office project. See root LICENSE file.
