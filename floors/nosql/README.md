# Floor 19 - NoSQL Jurisdiction

**Language:** JavaScript/Node.js  
**Floor Number:** 19  
**Domain:** Document stores, Key-value stores, Schema-less data  
**Architectural Law:** Flexible schemas, Document validation, Eventual consistency  
**Security Doctrine:** Injection prevention, Schema validation, Access control

## Overview

The NoSQL Department Floor specializes in document-oriented and key-value database operations. This floor handles MongoDB-style document stores, Redis-style key-value stores, and other NoSQL paradigms with a focus on schema flexibility, validation, and security.

## Architecture

This floor implements department logic following core principles:

- **Language Sovereignty**: All NoSQL operations in JavaScript/Node.js
- **Identical Topology**: 6 offices with specialized roles
- **Contract-Bound Operation**: JSON-RPC protocol via stdin/stdout
- **Non-Creative Mandate**: Strict adherence to NoSQL best practices
- **Failure Escalation**: Explicit error reporting and vulnerability detection

## Domain & Jurisdiction

The NoSQL floor has exclusive authority over:

1. **Document Operations**
   - Document CRUD operations
   - Embedded document handling
   - Array operations
   - Document validation

2. **Query Operations**
   - Filter queries
   - Aggregation pipelines
   - MapReduce operations
   - Full-text search

3. **Schema Management**
   - Flexible schema design
   - Schema validation rules
   - Schema evolution
   - Document structure analysis

4. **Security & Validation**
   - NoSQL injection detection
   - Operator validation
   - Input sanitization
   - Access control patterns

## Architectural Laws

### 1. Flexible Schemas
NoSQL embraces schema flexibility while maintaining structure through validation:
- Documents can evolve over time
- Schema validation is recommended but not enforced
- Embedded documents and arrays are first-class citizens

### 2. Document Validation
All documents should follow validation rules:
- Define expected field types
- Mark required fields
- Validate nested structures
- Use JSON Schema for complex validation

### 3. Eventual Consistency
Accept the eventual consistency model:
- Reads may not reflect latest writes immediately
- Design for idempotency
- Handle stale data gracefully

## Security Doctrine

### Core Principles
1. **Prevent NoSQL injection** - Validate all query operators
2. **Avoid dangerous operations** - No $where, eval, or mapReduce with user input
3. **Validate input types** - Ensure operators match expected types
4. **Use projection** - Return only necessary fields
5. **Implement access control** - Role-based permissions

### Security Levels
- **SAFE**: No vulnerabilities, proper validation
- **WARNING**: Potential issues (missing limits, unoptimized queries)
- **CRITICAL**: Injection vulnerabilities, dangerous operators

### Common Vulnerabilities
- **NoSQL Injection**: Malicious operators in query objects
- **$where injection**: Arbitrary JavaScript execution
- **ReDoS**: Regular expression denial of service
- **Operator injection**: Unexpected query operators

## Office Structure

### Architecture Office
**Staff:** 3 Agents

1. **Data Model Architect** (Senior Architect)
   - Capabilities: document_design, schema_modeling, denormalization_strategy
   - Specialization: Document Design
   
2. **Scalability Architect** (Architect)
   - Capabilities: sharding_strategy, replication_design, partition_key_selection
   - Specialization: Scalability
   
3. **Index Architect** (Architect)
   - Capabilities: index_strategy, query_optimization, performance_tuning
   - Specialization: Indexing Strategy

**Responsibilities:**
- Document structure design
- Denormalization strategies
- Sharding key selection
- Index planning

### Implementation Office
**Staff:** 3 Agents

1. **Document Engineer** (Senior Engineer)
   - Capabilities: document_operations, aggregation_pipelines, index_creation
   - Specialization: Document Operations
   
2. **Query Engineer** (Engineer)
   - Capabilities: query_writing, optimization, aggregation
   - Specialization: Query Development
   
3. **Migration Engineer** (Engineer)
   - Capabilities: data_migration, schema_evolution, version_management
   - Specialization: Data Migration

**Responsibilities:**
- CRUD operation implementation
- Aggregation pipeline development
- Data migration scripts
- Index creation

### Review Office
**Staff:** 3 Agents

1. **Senior Query Reviewer** (Lead Reviewer)
   - Capabilities: query_review, schema_review, performance_review
   - Specialization: Comprehensive Review
   
2. **Document Reviewer** (Reviewer)
   - Capabilities: document_validation, structure_review
   - Specialization: Document Review
   
3. **Performance Reviewer** (Reviewer)
   - Capabilities: query_optimization, index_review
   - Specialization: Performance Review

**Responsibilities:**
- Query quality review
- Performance analysis
- Security validation
- Best practices enforcement

### Test Office
**Staff:** 3 Agents

1. **Query Tester** (Senior Tester)
   - Capabilities: query_testing, performance_testing, load_testing
   - Specialization: Query Testing
   
2. **Integration Tester** (Tester)
   - Capabilities: integration_testing, consistency_testing
   - Specialization: Integration Testing
   
3. **Data Validator** (Tester)
   - Capabilities: schema_validation, document_validation
   - Specialization: Data Validation

**Responsibilities:**
- Query execution testing
- Performance validation
- Consistency testing
- Schema validation testing

### Security Office
**Staff:** 3 Agents

1. **NoSQL Security Auditor** (Senior Security Engineer)
   - Capabilities: injection_detection, access_control, encryption_audit
   - Specialization: NoSQL Injection Prevention
   
2. **Access Control Specialist** (Security Engineer)
   - Capabilities: rbac_implementation, permission_management
   - Specialization: Access Control
   
3. **Data Security Specialist** (Security Engineer)
   - Capabilities: encryption, data_masking, audit_logging
   - Specialization: Data Security

**Responsibilities:**
- NoSQL injection detection
- Operator validation
- Access control implementation
- Data encryption compliance

### Manager Office
**Staff:** 3 Agents

1. **NoSQL Manager** (Senior Manager)
   - Capabilities: task_coordination, resource_allocation, project_planning
   - Specialization: Project Management
   
2. **Operations Manager** (Manager)
   - Capabilities: performance_monitoring, capacity_planning, scaling
   - Specialization: Operations Management
   
3. **Quality Manager** (Manager)
   - Capabilities: quality_assurance, standards_compliance, best_practices
   - Specialization: Quality Assurance

**Responsibilities:**
- Task coordination
- Resource allocation
- Performance monitoring
- Quality assurance

## Specialist Agents

### Service Agent
Manages NoSQL service operations and connections.

**Capabilities:**
- Connection string validation
- Index strategy analysis
- Query execution coordination

### Document Model Agent
Handles schema validation and document analysis.

**Capabilities:**
- Schema validation
- Document structure analysis
- Schema suggestion from sample documents

### Operations Agent
Analyzes queries for performance and security.

**Capabilities:**
- Query analysis
- Injection detection
- Query optimization

## Installation

### Requirements
- Node.js 12.x or higher

### Setup
```bash
cd floors/nosql
chmod +x department_floor.js
```

No additional npm packages required - uses Node.js standard library only.

## Running

Start the NoSQL Department Floor:

```bash
node department_floor.js
```

Or make executable and run directly:

```bash
./department_floor.js
```

The floor listens on stdin for JSON-RPC requests and writes responses to stdout.

## API Documentation

### Get Floor Information
```json
{"method": "get_info"}
```

**Response:**
```json
{
  "floor_number": 19,
  "language": "nosql",
  "domain": "Document stores, Key-value stores, Schema-less data",
  "architectural_law": "Flexible schemas, Document validation, Eventual consistency",
  "security_doctrine": "Injection prevention, Schema validation, Access control",
  "offices": [...],
  "agent_count": 18,
  "agents": [...],
  "specialist_agents": [...]
}
```

### Analyze Query
Comprehensive query analysis including structure, security, and optimization.

```json
{
  "method": "analyze_query",
  "params": {
    "query": {
      "filter": {"status": "active"},
      "projection": {"name": 1, "email": 1},
      "sort": {"createdAt": -1},
      "limit": 10
    },
    "query_type": "find"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "query": {
      "queryType": "find",
      "hasFilter": true,
      "hasProjection": true,
      "hasSort": true,
      "hasLimit": true,
      "complexity": "moderate",
      "warnings": []
    },
    "security": {
      "securityLevel": "safe",
      "vulnerabilities": [],
      "safe": true
    },
    "optimization": {
      "optimizations": [],
      "optimizationCount": 0,
      "priority": "medium"
    }
  }
}
```

### Validate Schema
Validate a JSON schema for documents.

```json
{
  "method": "validate_schema",
  "params": {
    "schema": {
      "type": "object",
      "properties": {
        "_id": {"type": "string"},
        "name": {"type": "string"},
        "email": {"type": "string"},
        "age": {"type": "number"}
      },
      "required": ["name", "email"]
    }
  }
}
```

**Response:**
```json
{
  "valid": true,
  "issues": [],
  "recommendations": [],
  "schemaType": "object"
}
```

### Analyze Document
Analyze a document's structure.

```json
{
  "method": "analyze_document",
  "params": {
    "document": {
      "_id": "507f1f77bcf86cd799439011",
      "name": "John Doe",
      "email": "john@example.com",
      "profile": {
        "age": 30,
        "location": "NYC"
      },
      "tags": ["developer", "nodejs"]
    }
  }
}
```

**Response:**
```json
{
  "fieldCount": 6,
  "nestedFields": ["profile"],
  "arrayFields": ["tags"],
  "hasId": true,
  "estimatedSize": 145
}
```

### Suggest Schema
Generate schema from sample documents.

```json
{
  "method": "suggest_schema",
  "params": {
    "documents": [
      {"name": "Alice", "age": 25, "email": "alice@example.com"},
      {"name": "Bob", "age": 30, "email": "bob@example.com"}
    ]
  }
}
```

**Response:**
```json
{
  "schema": {
    "type": "object",
    "properties": {
      "name": {"type": "string"},
      "age": {"type": "number"},
      "email": {"type": "string"}
    },
    "required": ["name", "age", "email"]
  },
  "documentCount": 2
}
```

### Review Query
Submit query for review by Review Office.

```json
{
  "method": "review_query",
  "params": {
    "query": {
      "filter": {"$where": "this.age > 18"}
    },
    "context": {"queryType": "find"}
  }
}
```

**Response:**
```json
{
  "status": "reviewed",
  "issues": [
    "Dangerous operation detected: $where",
    "$where operator allows arbitrary JavaScript execution"
  ],
  "recommendations": ["Sanitize input and validate query operators"],
  "approved": false,
  "securityLevel": "critical"
}
```

### Security Audit
Perform security audit on query.

```json
{
  "method": "security_audit",
  "params": {
    "query": {
      "filter": {"username": {"$ne": null}}
    }
  }
}
```

**Response:**
```json
{
  "status": "audited",
  "securityLevel": "safe",
  "findings": [],
  "passedAudit": true,
  "auditDate": "2024-01-15T10:30:00.000Z"
}
```

### Test Query
Test query execution.

```json
{
  "method": "test_query",
  "params": {
    "query": {
      "filter": {"status": "active"},
      "limit": 10
    },
    "test_data": null
  }
}
```

**Response:**
```json
{
  "status": "tested",
  "testResults": {
    "syntaxValid": true,
    "executesSuccessfully": true,
    "returnsExpectedResults": true,
    "performanceAcceptable": true
  },
  "allTestsPassed": true,
  "queryComplexity": "simple"
}
```

### Validate Connection
Validate NoSQL database connection string.

```json
{
  "method": "validate_connection",
  "params": {
    "connection_string": "mongodb://user:pass@localhost:27017/mydb"
  }
}
```

**Response:**
```json
{
  "valid": true,
  "type": "mongodb",
  "hasCredentials": true
}
```

### Analyze Index Strategy
Analyze and recommend indexes based on queries.

```json
{
  "method": "analyze_index_strategy",
  "params": {
    "collection": "users",
    "queries": [
      {"filter": {"email": "test@example.com"}},
      {"filter": {"status": "active"}, "sort": {"createdAt": -1}}
    ]
  }
}
```

**Response:**
```json
{
  "collection": "users",
  "recommendedIndexes": [
    {"field": "email", "type": "single", "reason": "Frequently queried field"},
    {"field": "status", "type": "single", "reason": "Frequently queried field"},
    {"field": "createdAt", "type": "single", "reason": "Frequently queried field"}
  ],
  "indexCount": 3
}
```

### Create Task
Create a task on this floor.

```json
{
  "method": "create_task",
  "params": {
    "task_id": "nosql_task_001",
    "title": "Optimize user queries",
    "task_type": "optimization",
    "assigned_to": "impl_002",
    "metadata": {"priority": "high"}
  }
}
```

## Usage Examples

### Example 1: Analyze MongoDB Query
```bash
echo '{"method": "analyze_query", "params": {"query": {"filter": {"age": {"$gt": 18}}, "projection": {"name": 1, "email": 1}, "limit": 10}, "query_type": "find"}}' | node department_floor.js
```

### Example 2: Detect NoSQL Injection
```bash
echo '{"method": "security_audit", "params": {"query": {"filter": {"$where": "this.password == '\''secret'\''"}}}}'  | node department_floor.js
```

### Example 3: Validate Schema
```bash
echo '{"method": "validate_schema", "params": {"schema": {"type": "object", "properties": {"name": {"type": "string"}}, "required": ["name"]}}}' | node department_floor.js
```

### Example 4: Suggest Schema from Documents
```bash
echo '{"method": "suggest_schema", "params": {"documents": [{"name": "Alice", "age": 25}, {"name": "Bob", "age": 30}]}}' | node department_floor.js
```

## Supported NoSQL Databases

- **MongoDB** - Document store
- **Redis** - Key-value store  
- **CouchDB** - Document store
- **DynamoDB** - Key-value and document store

## Best Practices

### 1. Always Use Filters
```javascript
// Good
db.users.find({status: "active"})

// Bad - scans entire collection
db.users.find({})
```

### 2. Use Projection
```javascript
// Good - returns only needed fields
db.users.find({}, {name: 1, email: 1})

// Bad - returns all fields
db.users.find({})
```

### 3. Add Limits
```javascript
// Good
db.users.find({status: "active"}).limit(10)

// Bad - may return millions of documents
db.users.find({status: "active"})
```

### 4. Avoid Dangerous Operators
```javascript
// Bad - allows JavaScript execution
db.users.find({$where: "this.age > 18"})

// Good - use comparison operators
db.users.find({age: {$gt: 18}})
```

### 5. Validate Query Operators
```javascript
// Validate that user input doesn't inject operators
const safeQuery = {
  filter: {
    email: String(userInput) // Ensures it's a string, not an object
  }
};
```

## Error Handling

All errors are returned with explicit status:

```json
{
  "status": "error",
  "message": "Detailed error description"
}
```

Security violations:
```json
{
  "securityLevel": "critical",
  "vulnerabilities": ["Dangerous operation detected: $where"]
}
```

## Performance Tips

1. **Index frequently queried fields**
2. **Use projection to reduce data transfer**
3. **Add limits to queries**
4. **Avoid $where and mapReduce with user input**
5. **Use aggregation pipelines for complex queries**
6. **Consider denormalization for read-heavy workloads**

## Contributing

When extending NoSQL floor capabilities:

1. Maintain schema flexibility
2. Add validation for new operations
3. Implement security checks
4. Update office staffing appropriately
5. Document all new methods

## License

Part of Thirsty's Projects Miniature Office - See main LICENSE file.
