# Floor 16 - Ruby Jurisdiction

**Language:** Ruby  
**Floor Number:** 16  
**Domain:** Scripting, Web frameworks (Rails/Sinatra), Developer tools

## Architectural Law

**Idiomatic Ruby**
- Follow Ruby style guide conventions
- Use 2-space indentation
- Prefer snake_case for methods and variables
- Use CamelCase for classes and modules
- Embrace duck typing and metaprogramming wisely
- Blocks, iterators, and symbols are first-class citizens
- SOLID principles

**Security Doctrine**
- Input sanitization for all user data
- SQL injection prevention (ActiveRecord, parameterized queries)
- Mass assignment protection (strong parameters in Rails)
- XSS protection (never use html_safe without sanitization)
- Secure session management
- Avoid eval(), system() with user input

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, ActiveRecord Agent, Schema Agent  
**Responsibilities:**
- ActiveRecord model design
- Database schema design
- Data validation and callbacks
- Serialization (JSON, XML, MessagePack)

**Key Agent: Data Model Agent**
- Validates data structures (Hash, Array, Objects)
- Transforms data formats (snake_case ↔ camelCase)
- Infers data structures from runtime values
- Type checking and coercion

### Implementation Office
**Staff:** Service Agent, Rack Agent, HTTP Agent  
**Responsibilities:**
- HTTP/Rack middleware
- RESTful API implementation
- External service integration
- Gem management

**Key Agent: Service Agent**
- Validates Ruby style conventions
- Checks idiomatic Ruby patterns
- Analyzes gem dependencies
- Service orchestration

### Review Office
**Staff:** Operations Agent, Rubocop Agent, Quality Agent  
**Responsibilities:**
- Code analysis and metrics
- RuboCop integration
- Quality scoring
- Refactoring suggestions

**Key Agent: Operations Agent**
- Analyzes Ruby code structure (classes, modules, methods)
- Quality checks (frozen_string_literal, documentation, rescue blocks)
- Finds anti-patterns and issues
- Suggests Ruby idioms

### Test Office
**Staff:** Test Agent, RSpec Agent, Coverage Agent  
**Responsibilities:**
- RSpec test management
- Minitest support
- Test coverage analysis
- Factory and fixture management

**Key Agent: Test Agent**
- Detects test frameworks (RSpec, Minitest, Test::Unit)
- Analyzes test coverage
- Counts expectations and assertions
- Validates test structure

### Security Office
**Staff:** Security Agent, Brakeman Agent, Audit Agent  
**Responsibilities:**
- Security vulnerability scanning
- Rails-specific security checks
- Command injection detection
- Mass assignment vulnerability detection

**Key Agent: Security Agent**
- Scans for eval() and system() usage
- Detects command injection risks
- Finds XSS vulnerabilities (html_safe)
- Identifies mass assignment issues (permit!)
- Checks for hardcoded credentials
- Validates file operation security
- Detects arbitrary method invocation

### Manager Office
**Staff:** Manager Agent, Coordinator Agent, Workflow Agent  
**Responsibilities:**
- Task creation and tracking
- Workflow coordination
- Status management
- Resource allocation

**Key Agent: Manager Agent**
- Creates and tracks tasks
- Updates task status
- Manages task lifecycle
- Progress reporting

## Installation

```bash
cd floors/ruby
bundle install
```

## Running

```bash
ruby department_floor.rb
```

Or make it executable:
```bash
chmod +x department_floor.rb
./department_floor.rb
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## Requirements

- Ruby 3.0 or higher (for modern Ruby features)
- JSON gem (standard library)

## API Documentation

All communication uses JSON-RPC 2.0 protocol over stdin/stdout.

### Get Floor Info
Returns complete floor information including all agents and their capabilities.

**Request:**
```json
{
  "method": "get_info",
  "id": 1
}
```

**Response:**
```json
{
  "result": {
    "floorNumber": 16,
    "language": "ruby",
    "domain": "Scripting, Web frameworks (Rails/Sinatra), Developer tools",
    "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
    "agentCount": 6,
    "agents": [...],
    "architecturalLaw": "Idiomatic Ruby, SOLID principles, Duck typing",
    "securityDoctrine": "Input sanitization, SQL injection prevention, Mass assignment protection"
  },
  "id": 1
}
```

### Process Code - Analyze
Analyzes Ruby code structure and metrics.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "class User\n  def initialize(name)\n    @name = name\n  end\nend",
    "operation": "analyze"
  },
  "id": 2
}
```

**Response:**
```json
{
  "result": {
    "status": "success",
    "analysis": {
      "lines": 5,
      "classes": 1,
      "modules": 0,
      "methods": 1,
      "symbols": 0,
      "language": "ruby"
    }
  },
  "id": 2
}
```

### Process Code - Security Audit
Performs comprehensive security analysis.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "system(\"rm #{params[:file]}\")",
    "operation": "security_audit"
  },
  "id": 3
}
```

**Response:**
```json
{
  "result": {
    "secure": false,
    "vulnerabilities": [
      {
        "type": "command_injection",
        "severity": "critical",
        "description": "Shell command execution detected"
      }
    ],
    "criticalCount": 1,
    "highCount": 0
  },
  "id": 3
}
```

### Process Code - Quality Check
Checks code quality and Ruby best practices.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "# frozen_string_literal: true\n\nmodule MyModule\n  def greet(name)\n    \"Hello, #{name}\"\n  end\nend",
    "operation": "check_quality"
  },
  "id": 4
}
```

**Response:**
```json
{
  "result": {
    "score": 95,
    "issues": ["Class lacks documentation"],
    "passed": true
  },
  "id": 4
}
```

### Process Code - Find Issues
Finds specific code issues and anti-patterns.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "def process\n  eval(params[:code])\n  user_input.html_safe\nend",
    "operation": "find_issues"
  },
  "id": 5
}
```

**Response:**
```json
{
  "result": {
    "totalIssues": 2,
    "issues": [
      {
        "severity": "critical",
        "message": "eval() usage is dangerous",
        "line": 2
      },
      {
        "severity": "high",
        "message": "html_safe bypasses XSS protection",
        "line": 3
      }
    ],
    "critical": 1
  },
  "id": 5
}
```

### Process Code - Analyze Tests
Analyzes test code and framework usage.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "RSpec.describe User do\n  it 'has a name' do\n    expect(user.name).to eq('John')\n  end\nend",
    "operation": "analyze_tests"
  },
  "id": 6
}
```

**Response:**
```json
{
  "result": {
    "hasTests": true,
    "frameworks": ["rspec"],
    "assertionCount": 1,
    "coverage": "unknown"
  },
  "id": 6
}
```

### Create Task
Creates a new task managed by the Manager Agent.

**Request:**
```json
{
  "method": "create_task",
  "params": {
    "taskId": "task-001",
    "title": "Implement user authentication",
    "assignedTo": "service-001"
  },
  "id": 7
}
```

**Response:**
```json
{
  "result": {
    "taskId": "task-001",
    "title": "Implement user authentication",
    "status": "pending",
    "assignedTo": "service-001",
    "createdAt": "2024-01-15T10:30:00Z",
    "completedAt": null
  },
  "id": 7
}
```

### Update Task Status
Updates the status of an existing task.

**Request:**
```json
{
  "method": "update_task",
  "params": {
    "taskId": "task-001",
    "status": "completed"
  },
  "id": 8
}
```

**Response:**
```json
{
  "result": {
    "taskId": "task-001",
    "title": "Implement user authentication",
    "status": "completed",
    "assignedTo": "service-001",
    "createdAt": "2024-01-15T10:30:00Z",
    "completedAt": "2024-01-15T11:45:00Z"
  },
  "id": 8
}
```

### Execute Service
Executes a specific service operation.

**Request:**
```json
{
  "method": "execute_service",
  "params": {
    "serviceName": "validate_style",
    "params": {
      "code": "def myMethod()\n  puts \"hello\"\nend"
    }
  },
  "id": 9
}
```

**Response:**
```json
{
  "result": {
    "valid": false,
    "issues": ["Use snake_case for methods"],
    "issueCount": 1
  },
  "id": 9
}
```

### Process Data
Processes data through the Data Model Agent.

**Request:**
```json
{
  "method": "process_data",
  "params": {
    "operation": "infer_structure",
    "data": {
      "name": "John",
      "age": 30,
      "active": true
    }
  },
  "id": 10
}
```

**Response:**
```json
{
  "result": {
    "type": "Hash",
    "schema": {
      "name": "String",
      "age": "Integer",
      "active": "TrueClass"
    }
  },
  "id": 10
}
```

## Testing the Floor

### Interactive Testing
```bash
echo '{"method": "get_info", "id": 1}' | ruby department_floor.rb
```

### Multiple Commands
```bash
ruby department_floor.rb << EOF
{"method": "get_info", "id": 1}
{"method": "process_code", "params": {"code": "class User; end", "operation": "analyze"}, "id": 2}
{"method": "create_task", "params": {"taskId": "t1", "title": "Test task", "assignedTo": "ops-001"}, "id": 3}
EOF
```

## Development Tools

### Run RuboCop
```bash
bundle exec rubocop
```

### Auto-fix Style Issues
```bash
bundle exec rubocop -a
```

### Run RSpec Tests
```bash
bundle exec rspec
```

## Compliance

This floor strictly adheres to:
- **Language Sovereignty**: Pure Ruby implementation
- **Identical Topology**: 6 offices with specialized agents
- **Contract-Bound**: All operations via JSON-RPC
- **Non-Creative Mandate**: Executes requests without interpretation
- **Failure Escalation**: All errors explicitly reported

## Ruby Idioms

✅ **Use blocks and iterators**: `.each`, `.map`, `.select`, `.reject`  
✅ **Symbols for keys**: `{ name: 'John' }` instead of `{ 'name' => 'John' }`  
✅ **String interpolation**: `"Hello #{name}"` instead of `"Hello " + name`  
✅ **Implicit returns**: Last expression is automatically returned  
✅ **Duck typing**: Focus on behavior, not type  
✅ **Method chaining**: `users.select(&:active).map(&:name).sort`  
✅ **Safe navigation**: `user&.profile&.email`  
✅ **Frozen string literal**: Add `# frozen_string_literal: true` to all files  

## Ruby Anti-Patterns to Avoid

❌ **C-style for loops**: Use `.each` or `.times` instead  
❌ **Explicit return at end**: Ruby returns last expression automatically  
❌ **Empty rescue blocks**: Always handle exceptions properly  
❌ **Global variables**: Use instance variables or constants  
❌ **Monkey patching core classes**: Prefer refinements or composition  
❌ **eval() with user input**: Major security risk  
❌ **Mixing tabs and spaces**: Use 2-space indentation consistently  

## Rails-Specific Security

✅ **Strong Parameters**: Always use `.permit()` to whitelist attributes  
✅ **SQL Injection**: Use ActiveRecord queries or parameterized SQL  
✅ **XSS Protection**: Use `<%= %>` (auto-escaped), avoid `html_safe`  
✅ **CSRF Protection**: Enable `protect_from_forgery` in controllers  
✅ **Mass Assignment**: Use `attr_accessible` or strong parameters  
✅ **Secure Cookies**: Set `secure: true, httponly: true`  
✅ **Authentication**: Use Devise or bcrypt for password hashing  

## Common Vulnerabilities Detected

1. **Code Injection** - eval() usage
2. **Command Injection** - system(), exec(), backticks with user input
3. **XSS** - html_safe without sanitization
4. **Mass Assignment** - permit! on parameters
5. **Arbitrary Method Invocation** - send() with user input
6. **Path Traversal** - File operations with user input
7. **Hardcoded Secrets** - Passwords/keys in source code

## Ruby 3.0+ Features

- **Rightward assignment**: `expression => variable`
- **Pattern matching**: `case/in` syntax
- **Numbered parameters**: `_1`, `_2` in blocks
- **Endless methods**: `def square(x) = x * x`
- **Ractor** for true parallelism
- **Fiber Scheduler** for async I/O
