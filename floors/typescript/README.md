# Floor 6 - TypeScript Jurisdiction

**Language:** TypeScript  
**Floor Number:** 6  
**Domain:** Typed frontend systems, Node.js backends, Type-safe applications

## Architectural Law

**Strict Typing Mandatory**
- No `any` types permitted
- All function signatures must be fully typed
- Compile-time safety is non-negotiable
- Use `unknown` for truly unknown types, then narrow with type guards

**Security Doctrine**
- Type guards for runtime validation
- Null safety through strict null checks
- No type assertions without validation
- Sanitize all external inputs

## Offices and Agent Staffing

### Architecture Office
**Staff:** Data Model Agent, Schema Agent, Design Agent  
**Responsibilities:**
- Define interfaces and types
- Data structure validation
- Schema generation and inference
- Type transformation pipelines

**Key Agent: Data Model Agent**
- Validates data structures
- Transforms between formats (snake_case ↔ camelCase)
- Infers TypeScript types from runtime data
- Ensures type safety across boundaries

### Implementation Office
**Staff:** Service Agent, Integration Agent, I/O Agent  
**Responsibilities:**
- External API integration
- File and network operations
- Event handling
- Module imports and dependencies

**Key Agent: Service Agent**
- Validates strict type usage
- Analyzes import patterns (ES modules vs CommonJS)
- Dependency analysis
- Service orchestration

### Review Office
**Staff:** Operations Agent, Quality Agent, Refactor Agent  
**Responsibilities:**
- Code analysis and metrics
- Quality scoring
- Issue detection
- Code optimization

**Key Agent: Operations Agent**
- Analyzes TypeScript code structure
- Quality checks (no `any`, no console logs in production)
- Finds violations of architectural laws
- Suggests improvements

### Test Office
**Staff:** Test Agent, Coverage Agent, Mock Agent  
**Responsibilities:**
- Test framework detection
- Coverage analysis
- Assertion counting
- Mock generation

**Key Agent: Test Agent**
- Detects test frameworks (Jest, Mocha, Jasmine)
- Analyzes test coverage
- Validates test structure
- Generates test templates

### Security Office
**Staff:** Security Agent, Audit Agent, Vulnerability Agent  
**Responsibilities:**
- Security vulnerability scanning
- Dependency auditing
- Injection detection
- Secure coding validation

**Key Agent: Security Agent**
- Scans for eval() usage (code injection)
- Detects XSS vulnerabilities (innerHTML, dangerouslySetInnerHTML)
- Finds hardcoded secrets
- Validates secure protocols (HTTPS)

### Manager Office
**Staff:** Manager Agent, Coordinator Agent, Resource Agent  
**Responsibilities:**
- Task creation and tracking
- Workflow coordination
- Status management
- Progress reporting

**Key Agent: Manager Agent**
- Creates and tracks tasks
- Updates task status
- Resource allocation
- Provides workflow oversight

## Installation

```bash
cd floors/typescript
npm install
```

## Building

```bash
npm run build
```

This compiles TypeScript to JavaScript in the `dist/` directory with full type checking.

## Running

### Production Mode
```bash
npm start
```

### Development Mode (with ts-node)
```bash
npm run dev
```

### Type Checking Only
```bash
npm run type-check
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

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
    "floorNumber": 6,
    "language": "typescript",
    "domain": "Typed frontend systems, Node.js backends, Type-safe applications",
    "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
    "agentCount": 6,
    "agents": [...],
    "architecturalLaw": "Strict typing mandatory, no any types, compile-time safety",
    "securityDoctrine": "Type guards, runtime validation, null safety"
  },
  "id": 1
}
```

### Process Code - Analyze
Analyzes TypeScript code structure and metrics.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "interface User { name: string; age: number; }",
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
      "lines": 1,
      "interfaces": 1,
      "types": 0,
      "classes": 0,
      "functions": 0,
      "anyUsage": 0,
      "language": "typescript"
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
    "code": "const html = '<div>' + userInput + '</div>'; element.innerHTML = html;",
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
        "description": "innerHTML assignment - potential XSS vulnerability"
      }
    ],
    "criticalCount": 0,
    "highCount": 1
  },
  "id": 3
}
```

### Process Code - Quality Check
Checks code quality and provides scoring.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "export function add(a: number, b: number): number { return a + b; }",
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
Finds specific code issues and violations.

**Request:**
```json
{
  "method": "process_code",
  "params": {
    "code": "function test(x: any) { if (x == null) return; }",
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
        "severity": "error",
        "message": "Usage of 'any' type violates strict typing law",
        "line": 1
      },
      {
        "severity": "warning",
        "message": "Use === instead of == for comparison",
        "line": 1
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
    "code": "describe('Calculator', () => { test('adds numbers', () => { expect(1+1).toBe(2); }); });",
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
    "frameworks": ["jest"],
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
    "createdAt": "2024-01-15T10:30:00.000Z"
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
    "createdAt": "2024-01-15T10:30:00.000Z",
    "completedAt": "2024-01-15T11:45:00.000Z"
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
      "code": "function test(x: any): void {}"
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
    "anyUsage": 1,
    "unknownUsage": 0,
    "recommendation": "Replace any with specific types"
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
    "operation": "infer_types",
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
    "type": "object",
    "schema": {
      "name": "string",
      "age": "number",
      "active": "boolean"
    }
  },
  "id": 10
}
```

## Testing the Floor

### Interactive Testing
```bash
echo '{"method": "get_info", "id": 1}' | npm start
```

### Multiple Commands
```bash
npm start << EOF
{"method": "get_info", "id": 1}
{"method": "process_code", "params": {"code": "const x: number = 42;", "operation": "analyze"}, "id": 2}
{"method": "create_task", "params": {"taskId": "t1", "title": "Test task", "assignedTo": "ops-001"}, "id": 3}
EOF
```

## Compliance

This floor strictly adheres to:
- **Language Sovereignty**: Pure TypeScript implementation
- **Identical Topology**: 6 offices with specialized agents
- **Contract-Bound**: All operations via JSON-RPC
- **Non-Creative Mandate**: Executes requests without interpretation
- **Failure Escalation**: All errors explicitly reported

## Type Safety Guarantees

- **No `any` types** in production code
- **Strict null checks** enabled
- **No implicit `this`**
- **No unused locals or parameters**
- **No implicit returns**
- **All function types** strictly defined
- **Complete type coverage** for public APIs
