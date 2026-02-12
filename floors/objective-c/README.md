# Floor 14 - Objective-C Jurisdiction

**Language:** Objective-C  
**Floor Number:** 14  
**Domain:** Legacy Apple systems, macOS/iOS development, Darwin frameworks

## Architectural Law

**Objective-C Best Practices**
- Proper memory management (ARC or manual retain/release)
- Message passing paradigm (square bracket syntax)
- Protocol-oriented design
- Delegate pattern for callbacks
- Category extensions for code organization
- Nullability annotations (nullable/nonnull)
- Lightweight generics for type safety
- Property attributes (nonatomic/atomic, strong/weak/copy/assign)
- Header file organization with include guards

**Code Quality Standards**
- Clear separation of .h (interface) and .m (implementation)
- Documentation comments for public APIs
- Designated initializer pattern
- Proper use of NS_DESIGNATED_INITIALIZER macro
- Immutability where appropriate (copy properties for NSString, NSArray, etc.)
- Prefix class names to avoid namespace collisions
- Use of modern Objective-C syntax (literals, subscripting)
- Error handling with NSError**

## Security Doctrine

**Critical Security Requirements**
- **Buffer Overflow Prevention**: Never use unsafe C functions (strcpy, strcat, sprintf); use safe alternatives (strlcpy, strlcat, snprintf)
- **Memory Safety**: Avoid retain cycles in blocks; use __weak self references
- **SQL Injection**: Use parameterized queries, never string concatenation
- **Random Numbers**: Use SecRandomCopyBytes for cryptographic randomness, not rand()
- **Credentials**: Never hardcode passwords; use Keychain Services
- **Input Validation**: Validate all external data and user input
- **Format String Attacks**: Never use user input directly in format strings
- **Integer Overflow**: Check arithmetic operations on untrusted input

**Memory Management**
- Use ARC (Automatic Reference Counting) for modern code
- Avoid strong reference cycles between objects
- Use __weak or __unsafe_unretained to break retain cycles in blocks
- Properly manage Core Foundation objects (CFRetain/CFRelease or CFBridgingRetain/Release)
- Implement dealloc for cleanup in pre-ARC code
- Use @autoreleasepool for tight loops creating many objects
- Be careful with assign properties for object types (use weak)

**iOS/macOS Security**
- Use App Transport Security (HTTPS only)
- Validate SSL certificates
- Use Keychain for credential storage
- Implement proper sandboxing
- Use secure coding practices for file I/O
- Validate data before deserialization (NSCoding, NSKeyedArchiver)

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, Core Data Agent, Schema Design Agent  
**Responsibilities:**
- Object-oriented model design
- Core Data entity and relationship modeling
- Property attribute selection (strong/weak/copy)
- Protocol definition and adoption
- Type encoding and serialization

**Key Agent: Data Model Agent**
- Validates Objective-C data structures (NSDictionary, NSArray, NSSet)
- Serializes objects to JSON and property lists
- Infers types from runtime objects (introspection)
- Manages type conversions between Foundation types
- Handles Core Data model validation

### Implementation Office
**Staff:** Service Agent, Cocoa Frameworks Agent, API Integration Agent  
**Responsibilities:**
- Cocoa/CocoaTouch framework integration
- Delegate pattern implementation
- Block (closure) usage and memory management
- NSNotificationCenter messaging
- Grand Central Dispatch (GCD) concurrency
- URLSession networking

**Key Agent: Service Agent**
- Validates Objective-C style conventions (naming, structure)
- Checks design patterns (delegate, observer, singleton)
- Analyzes memory management correctness
- Detects retain cycles in blocks
- Framework best practices enforcement
- Message passing validation

### Review Office
**Staff:** Operations Agent, Code Quality Agent, Pattern Detection Agent  
**Responsibilities:**
- Code structure analysis (interfaces, implementations, methods)
- Property declaration validation
- Quality metrics calculation
- Design pattern detection
- Refactoring suggestions
- API usage review

**Key Agent: Operations Agent**
- Analyzes code structure (@interface/@implementation/@property/@method)
- Counts interfaces, implementations, methods, properties
- Quality checks (documentation, nullability, header guards)
- Identifies code smells and anti-patterns
- Suggests Objective-C idioms
- Property attribute validation

### Test Office
**Staff:** Test Agent, XCTest Agent, UI Testing Agent  
**Responsibilities:**
- XCTest framework management
- Unit test execution and validation
- UI testing with XCUITest
- Test coverage analysis
- Mock object creation
- setUp/tearDown validation

**Key Agent: Test Agent**
- Detects XCTest structure (test cases, test methods)
- Counts XCTAssert assertions
- Validates setUp and tearDown methods
- Analyzes test organization
- Reports test metrics
- Integration with Xcode test navigator

### Security Office
**Staff:** Security Agent, Memory Safety Agent, Vulnerability Scanner Agent  
**Responsibilities:**
- Buffer overflow detection
- Retain cycle identification
- Memory leak detection
- SQL injection vulnerability scanning
- Weak random number generator detection
- Hardcoded credential detection
- Unsafe C function usage

**Key Agent: Security Agent**
- Scans for unsafe C functions (strcpy, strcat, sprintf)
- Detects potential retain cycles (self captured in blocks without __weak)
- Identifies memory leaks (alloc without proper release/autorelease)
- Checks SQL injection risks (string concatenation in queries)
- Validates random number generation (SecRandomCopyBytes vs rand)
- Detects hardcoded credentials
- Reports vulnerability severity

### Manager Office
**Staff:** Manager Agent, Workflow Coordination Agent, Resource Management Agent  
**Responsibilities:**
- Task coordination across agents
- Resource allocation and management
- Workflow orchestration
- Priority scheduling
- Agent communication coordination

**Key Agent: Manager Agent**
- Delegates tasks to appropriate agents
- Manages workflow execution
- Monitors resource utilization
- Coordinates cross-office operations
- Reports workflow status

## API Documentation

### JSON-RPC Methods

#### `get_info`
Get floor information including agents, tasks, and offices.

**Request:**
```json
{"method": "get_info", "params": {}}
```

**Response:**
```json
{
  "floorNumber": 14,
  "language": "objective-c",
  "domain": "Legacy Apple systems, macOS/iOS development, Darwin frameworks",
  "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
  "agentCount": 6,
  "taskCount": 0,
  "agents": [...],
  "tasks": [...]
}
```

#### `process_code`
Process Objective-C code with various operations.

**Operations:**
- `analyze`: Analyze code structure (interfaces, implementations, methods, properties)
- `quality`: Check code quality and best practices
- `security`: Scan for security vulnerabilities (buffer overflows, retain cycles)
- `test_analysis`: Analyze XCTest structure

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "@interface MyClass : NSObject\n@property (nonatomic, strong) NSString *name;\n@end",
    "operation": "analyze"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "lines": 3,
    "interfaces": 1,
    "implementations": 0,
    "methods": 0,
    "properties": 1,
    "language": "objective-c"
  }
}
```

#### `execute_service`
Execute service agent operations.

**Services:**
- `validate_style`: Validate Objective-C style conventions
- `check_patterns`: Analyze design patterns (delegate, protocol, blocks)
- `analyze_memory`: Check memory management issues

**Request:**
```json
{
  "method": "execute_service",
  "params": {
    "serviceName": "validate_style",
    "code": "@interface MyClass : NSObject\n@end"
  }
}
```

#### `process_data`
Process data with data model agent.

**Operations:**
- `validate`: Validate data structure
- `serialize`: Serialize data to JSON
- `infer_type`: Infer Objective-C type information

**Request:**
```json
{
  "method": "process_data",
  "params": {
    "operation": "validate",
    "data": {"key": "value"}
  }
}
```

#### `add_agent`
Add a new agent to the floor.

**Request:**
```json
{
  "method": "add_agent",
  "params": {
    "agentId": "custom-001",
    "name": "CustomAgent",
    "role": "Custom Role",
    "office": "Implementation Office",
    "capabilities": ["capability1", "capability2"]
  }
}
```

#### `create_task`
Create a new task on the floor.

**Request:**
```json
{
  "method": "create_task",
  "params": {
    "taskId": "task-001",
    "title": "Implement feature X",
    "assignedTo": "service-001"
  }
}
```

## Installation and Running

### Requirements
- **macOS**: Xcode Command Line Tools or full Xcode
- **Linux**: GNUstep (for Objective-C runtime)
- **clang**: Modern C compiler with Objective-C support

### Installation

**macOS:**
```bash
# Install Xcode Command Line Tools
xcode-select --install

# Verify installation
clang --version
```

**Linux (Ubuntu/Debian):**
```bash
# Install GNUstep and Objective-C runtime
sudo apt-get update
sudo apt-get install -y gnustep-devel gobjc libobjc-4-dev clang

# Install Foundation framework
sudo apt-get install -y libgnustep-base-dev
```

### Building

**Using Makefile:**
```bash
cd floors/objective-c
make

# Run the compiled binary
./department_floor
```

**Manual Compilation (macOS):**
```bash
clang -framework Foundation -fobjc-arc \
  -o department_floor \
  floors/objective-c/department_floor.m
```

**Manual Compilation (Linux with GNUstep):**
```bash
clang -fobjc-arc -fblocks \
  $(gnustep-config --objc-flags) \
  -o department_floor \
  floors/objective-c/department_floor.m \
  $(gnustep-config --base-libs) -lobjc -ldispatch
```

### Running the Floor

**Interactive Mode:**
```bash
./department_floor
```

**JSON-RPC Communication:**
```bash
echo '{"method":"get_info","params":{}}' | ./department_floor
```

**From Pipeline:**
```bash
cat request.json | ./department_floor
```

### Testing

```bash
# Test basic functionality
echo '{"method":"get_info","params":{}}' | ./department_floor

# Test code analysis
echo '{"method":"process_code","params":{"code":"@interface Test : NSObject @end","operation":"analyze"}}' | ./department_floor
```

## Security Considerations

### Memory Management

**ARC (Recommended):**
```objective-c
// Compile with -fobjc-arc
@interface MyClass : NSObject
@property (nonatomic, strong) NSString *name;  // Automatically managed
@end
```

**Manual Retain/Release (Legacy):**
```objective-c
// Pre-ARC code
- (void)dealloc {
    [_name release];
    [super dealloc];
}
```

### Avoiding Retain Cycles

**GOOD: Using __weak:**
```objective-c
__weak typeof(self) weakSelf = self;
[self.service fetchDataWithCompletion:^(NSData *data) {
    __strong typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
        [strongSelf processData:data];
    }
}];
```

**BAD: Strong reference cycle:**
```objective-c
// Creates retain cycle!
[self.service fetchDataWithCompletion:^(NSData *data) {
    [self processData:data];  // self captured strongly
}];
```

### Buffer Overflow Prevention

**GOOD: Safe string functions:**
```objective-c
char dest[100];
strlcpy(dest, source, sizeof(dest));  // Safe
snprintf(dest, sizeof(dest), "Value: %d", value);  // Safe
```

**BAD: Unsafe functions:**
```objective-c
// NEVER DO THIS
strcpy(dest, source);  // Buffer overflow risk!
strcat(dest, source);  // Buffer overflow risk!
sprintf(dest, "Value: %d", value);  // Buffer overflow risk!
```

### SQL Injection Prevention

**GOOD: Parameterized queries:**
```objective-c
NSString *sql = @"SELECT * FROM users WHERE id = ?";
sqlite3_stmt *statement;
sqlite3_prepare_v2(db, [sql UTF8String], -1, &statement, NULL);
sqlite3_bind_int(statement, 1, userId);
```

**BAD: String concatenation:**
```objective-c
// NEVER DO THIS - SQL injection!
NSString *sql = [NSString stringWithFormat:@"SELECT * FROM users WHERE id = %d", userId];
```

## Design Patterns

### Delegate Pattern
```objective-c
@protocol DataSourceDelegate <NSObject>
- (void)dataSource:(DataSource *)source didReceiveData:(NSData *)data;
@end

@interface DataSource : NSObject
@property (nonatomic, weak) id<DataSourceDelegate> delegate;
@end
```

### Singleton Pattern
```objective-c
+ (instancetype)sharedInstance {
    static MyClass *instance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        instance = [[self alloc] init];
    });
    return instance;
}
```

### Block Callbacks
```objective-c
typedef void (^CompletionBlock)(NSData *data, NSError *error);

- (void)fetchDataWithCompletion:(CompletionBlock)completion {
    // Async operation
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        NSData *data = [self loadData];
        dispatch_async(dispatch_get_main_queue(), ^{
            if (completion) {
                completion(data, nil);
            }
        });
    });
}
```

## Performance Considerations

- Use @autoreleasepool for tight loops creating many objects
- Prefer immutable collections (NSArray vs NSMutableArray) when possible
- Use lazy initialization for expensive objects
- Cache frequently accessed data
- Use Grand Central Dispatch for concurrency
- Profile with Instruments (Time Profiler, Allocations, Leaks)
- Optimize drawing code (avoid unnecessary view redraws)
- Use NSCache for memory-sensitive caching

## Integration Examples

### Xcode Project
```bash
# Add to Xcode project
# File -> Add Files to Project -> department_floor.h, department_floor.m
```

### CI/CD Integration
```yaml
# GitHub Actions
steps:
  - name: Build Objective-C Floor
    run: |
      cd floors/objective-c
      make
      ./department_floor < test_request.json
```

### Docker Support
```dockerfile
FROM swift:latest
WORKDIR /app
COPY floors/objective-c/ .
RUN clang -framework Foundation -fobjc-arc -o department_floor department_floor.m
CMD ["./department_floor"]
```

## Troubleshooting

### Common Issues

**Issue: Foundation framework not found**
```
Solution (macOS): Install Xcode Command Line Tools
Solution (Linux): Install libgnustep-base-dev
```

**Issue: ARC errors**
```
Solution: Compile with -fobjc-arc flag
```

**Issue: Undefined symbols**
```
Solution: Link with -framework Foundation (macOS) or $(gnustep-config --base-libs) (Linux)
```

**Issue: Block support errors**
```
Solution: Add -fblocks flag and link with -ldispatch
```

## References

- [Objective-C Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/)
- [Memory Management Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/)
- [Secure Coding Guide](https://developer.apple.com/library/archive/documentation/Security/Conceptual/SecureCodingGuide/)
- [XCTest Framework](https://developer.apple.com/documentation/xctest)
- [GNUstep Documentation](http://www.gnustep.org/resources/documentation.html)
