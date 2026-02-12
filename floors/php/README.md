# Floor 15 - PHP Jurisdiction

**Language:** PHP  
**Floor Number:** 15  
**Domain:** Web backend systems, CMS platforms, Server-side applications

## Architectural Law

**PSR Compliance Mandatory**
- PSR-12 coding style standard
- Type declarations on all functions (strict_types=1)
- Property type hints (PHP 7.4+)
- Return type declarations
- Secure by default approach

**Security Doctrine**
- Input validation and sanitization
- SQL injection prevention (prepared statements)
- XSS protection (output escaping)
- CSRF token validation
- Secure session management

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, Schema Agent, ORM Agent  
**Responsibilities:**
- Database model design
- ORM integration (Eloquent, Doctrine)
- Data validation and transformation
- Schema generation and migrations

**Key Agent: Data Model Agent**
- Validates data structures
- Transforms between formats (snake_case ↔ camelCase)
- Infers database schemas from data
- Type validation and casting

### Implementation Office
**Staff:** Service Agent, HTTP Agent, API Agent  
**Responsibilities:**
- HTTP request/response handling
- RESTful API implementation
- External service integration
- Session and cookie management

**Key Agent: Service Agent**
- Validates type declarations
- Checks security best practices
- Analyzes dependencies (use statements, requires)
- Service orchestration

### Review Office
**Staff:** Operations Agent, Quality Agent, PSR Agent  
**Responsibilities:**
- Code analysis and metrics
- PSR compliance checking
- Quality scoring
- Performance optimization

**Key Agent: Operations Agent**
- Analyzes PHP code structure (classes, functions, traits)
- Quality checks (strict_types, PSR compliance)
- Finds security issues and bad practices
- Suggests improvements

### Test Office
**Staff:** Test Agent, PHPUnit Agent, Coverage Agent  
**Responsibilities:**
- PHPUnit test management
- Test coverage analysis
- Integration testing
- Mock and stub generation

**Key Agent: Test Agent**
- Detects test frameworks (PHPUnit, Pest, Codeception)
- Analyzes test coverage
- Counts assertions
- Validates test structure

### Security Office
**Staff:** Security Agent, Vulnerability Agent, Audit Agent  
**Responsibilities:**
- Security vulnerability scanning
- SQL injection detection
- XSS vulnerability detection
- Credential leak detection

**Key Agent: Security Agent**
- Scans for eval() usage
- Detects SQL injection risks
- Finds XSS vulnerabilities
- Identifies hardcoded credentials
- Checks for unsafe deserialization
- Validates file operation security

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
cd floors/php
composer install
```

## Running

```bash
php department_floor.php
```

Or make it executable:
```bash
chmod +x department_floor.php
./department_floor.php
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## Requirements

- PHP 8.1 or higher (for readonly properties and enums)
- JSON extension (standard with PHP)

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
    "floorNumber": 15,
    "language": "php",
    "domain": "Web backend systems, CMS platforms, Server-side applications",
    "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
    "agentCount": 6,
    "agents": [...],
    "architecturalLaw": "PSR compliance, type declarations, secure by default",
    "securityDoctrine": "Input validation, SQL injection prevention, XSS protection"
  },
  "id": 1
}
```

### Process Code - Analyze
Analyzes PHP code structure and metrics.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "<?php\nclass User { public function getName(): string { return ''; } }",
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
      "lines": 2,
      "classes": 1,
      "functions": 1,
      "namespaces": 0,
      "traits": 0,
      "language": "php"
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
    "code": "<?php\necho $_GET['name'];",
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
        "type": "xss",
        "severity": "high",
        "description": "Unsanitized output - XSS vulnerability"
      }
    ],
    "criticalCount": 0,
    "highCount": 1
  },
  "id": 3
}
```

### Process Code - Quality Check
Checks code quality and PSR compliance.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "<?php\ndeclare(strict_types=1);\nnamespace App;\nfunction add(int $a, int $b): int { return $a + $b; }",
    "operation": "check_quality"
  },
  "id": 4
}
```

**Response:**
```json
{
  "result": {
    "score": 100,
    "issues": [],
    "passed": true
  },
  "id": 4
}
```

### Process Code - Find Issues
Finds specific code issues and security vulnerabilities.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "<?php\n$query = \"SELECT * FROM users WHERE id = \" . $_GET['id'];\nmysqli_query($conn, $query);",
    "operation": "find_issues"
  },
  "id": 5
}
```

**Response:**
```json
{
  "result": {
    "totalIssues": 1,
    "issues": [
      {
        "severity": "critical",
        "message": "Potential SQL injection",
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
    "code": "<?php\nclass UserTest extends TestCase { public function testName() { $this->assertTrue(true); } }",
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
    "frameworks": ["phpunit"],
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
    "createdAt": "2024-01-15T10:30:00+00:00",
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
    "createdAt": "2024-01-15T10:30:00+00:00",
    "completedAt": "2024-01-15T11:45:00+00:00"
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
    "serviceName": "validate_types",
    "params": {
      "code": "<?php\nfunction test($x) { return $x; }"
    }
  },
  "id": 9
}
```

**Response:**
```json
{
  "result": {
    "hasStrictTypes": false,
    "typeHintCount": 0,
    "returnTypeCount": 0,
    "recommendation": "Add declare(strict_types=1)"
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
    "operation": "infer_schema",
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
    "type": "array",
    "elementTypes": ["string", "integer", "boolean"],
    "length": 3
  },
  "id": 10
}
```

## Testing the Floor

### Interactive Testing
```bash
echo '{"method": "get_info", "id": 1}' | php department_floor.php
```

### Multiple Commands
```bash
php department_floor.php << EOF
{"method": "get_info", "id": 1}
{"method": "process_code", "params": {"code": "<?php declare(strict_types=1);", "operation": "analyze"}, "id": 2}
{"method": "create_task", "params": {"taskId": "t1", "title": "Test task", "assignedTo": "ops-001"}, "id": 3}
EOF
```

## Development Tools

### Linting (PSR-12)
```bash
composer lint
```

### Static Analysis
```bash
composer analyze
```

### Auto-fix Code Style
```bash
composer fix
```

### Run Tests
```bash
composer test
```

## Compliance

This floor strictly adheres to:
- **Language Sovereignty**: Pure PHP implementation
- **Identical Topology**: 6 offices with specialized agents
- **Contract-Bound**: All operations via JSON-RPC
- **Non-Creative Mandate**: Executes requests without interpretation
- **Failure Escalation**: All errors explicitly reported

## PHP 8.1+ Features Used

- **Readonly properties** for immutable data
- **Constructor property promotion**
- **Match expressions** for cleaner control flow
- **Named arguments** for clarity
- **Union and intersection types**
- **Strict types** throughout

## Security Best Practices

✅ **Input Validation**: Never trust user input  
✅ **Prepared Statements**: Use PDO/mysqli with placeholders  
✅ **Output Escaping**: Use htmlspecialchars() for HTML output  
✅ **CSRF Protection**: Implement token validation  
✅ **Password Hashing**: Use password_hash() and password_verify()  
✅ **Session Security**: Regenerate session IDs, use secure cookies  
✅ **Error Handling**: Don't expose sensitive information in errors  

## Common Vulnerabilities Detected

1. **SQL Injection** - User input in queries without prepared statements
2. **XSS** - Unescaped output to HTML
3. **Code Injection** - eval() usage
4. **Path Traversal** - File operations with user input
5. **Deserialization** - unserialize() on user input
6. **Hardcoded Secrets** - Passwords/keys in source code
