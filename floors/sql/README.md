# Floor 8 - SQL Jurisdiction

**Language:** SQL (Analyzed via Python)  
**Floor Number:** 8  
**Domain:** Data definition, Query logic, Schema design  
**Architectural Law:** ACID compliance, Parameterization mandatory, SQL injection prevention  
**Security Doctrine:** No dynamic SQL, Always use prepared statements, Input validation

## Overview

The SQL Department Floor specializes in relational database operations, schema design, and query analysis. This floor operates as a meta-SQL environment that analyzes, validates, and secures SQL code across multiple dialects (PostgreSQL, MySQL, SQLite, MS SQL, Oracle).

## Architecture

This floor implements department logic following the core principles:

- **Language Sovereignty**: All SQL operations are analyzed and validated within this jurisdiction
- **Identical Topology**: 6 offices with specialized roles
- **Contract-Bound Operation**: All operations via JSON-RPC protocol (stdin/stdout)
- **Non-Creative Mandate**: Strict adherence to SQL standards and ACID principles
- **Failure Escalation**: Explicit error reporting and security warnings

## Domain & Jurisdiction

The SQL floor has exclusive authority over:

1. **Data Definition Language (DDL)**
   - Table creation and modification
   - Index management
   - Constraint definitions
   - Schema migrations

2. **Data Manipulation Language (DML)**
   - Query analysis and optimization
   - Transaction management
   - CRUD operations validation

3. **Security & Compliance**
   - SQL injection detection and prevention
   - Parameterized query validation
   - Access control patterns

4. **Performance & Optimization**
   - Query complexity analysis
   - Index recommendations
   - Join optimization

## Architectural Laws

### 1. ACID Compliance
All transactions must follow ACID principles:
- **Atomicity**: Transactions are all-or-nothing
- **Consistency**: Database remains in valid state
- **Isolation**: Concurrent transactions don't interfere
- **Durability**: Committed data persists

### 2. Parameterization Mandatory
All queries with user input MUST use parameterized queries or prepared statements. Dynamic SQL construction is prohibited.

### 3. SQL Injection Prevention
Every query undergoes security analysis. Queries with injection vulnerabilities are flagged as CRITICAL.

## Security Doctrine

### Core Principles
1. **Never trust user input** - All inputs must be parameterized
2. **Principle of least privilege** - Grant minimum necessary permissions
3. **Defense in depth** - Multiple layers of security validation
4. **Audit everything** - All security-sensitive operations are logged

### Security Levels
- **SAFE**: No vulnerabilities detected, uses parameterized queries
- **WARNING**: Contains potentially dangerous operations (DROP, DELETE, etc.)
- **CRITICAL**: SQL injection vulnerabilities detected

## Office Structure

### Architecture Office
**Staff:** 3 Agents

1. **Schema Architect** (Senior Architect)
   - Capabilities: database_design, normalization, indexing_strategy
   - Specialization: Schema Design
   
2. **Performance Architect** (Architect)
   - Capabilities: query_optimization, index_design, partitioning
   - Specialization: Performance
   
3. **Data Architect** (Architect)
   - Capabilities: data_modeling, relationship_design, constraint_definition
   - Specialization: Data Modeling

**Responsibilities:**
- Database schema design
- Normalization analysis (1NF through 3NF)
- Index strategy planning
- Relationship modeling

### Implementation Office
**Staff:** 3 Agents

1. **DDL Engineer** (Senior Engineer)
   - Capabilities: table_creation, index_creation, constraint_implementation
   - Specialization: DDL
   
2. **DML Engineer** (Engineer)
   - Capabilities: query_writing, stored_procedures, triggers
   - Specialization: DML
   
3. **Migration Engineer** (Engineer)
   - Capabilities: schema_migration, data_migration, version_control
   - Specialization: Migration

**Responsibilities:**
- DDL statement generation
- Query implementation
- Schema migrations
- Stored procedure development

### Review Office
**Staff:** 3 Agents

1. **Senior Reviewer** (Lead Reviewer)
   - Capabilities: code_review, performance_review, security_review
   - Specialization: Comprehensive Review
   
2. **Query Reviewer** (Reviewer)
   - Capabilities: query_analysis, optimization_review
   - Specialization: Query Review
   
3. **Schema Reviewer** (Reviewer)
   - Capabilities: schema_review, normalization_check
   - Specialization: Schema Review

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
   - Capabilities: integration_testing, transaction_testing
   - Specialization: Integration
   
3. **Data Tester** (Tester)
   - Capabilities: data_validation, constraint_testing
   - Specialization: Data Validation

**Responsibilities:**
- Query execution testing
- Transaction validation
- Performance benchmarking
- Data integrity verification

### Security Office
**Staff:** 3 Agents

1. **Security Auditor** (Senior Security Engineer)
   - Capabilities: injection_detection, privilege_audit, encryption_review
   - Specialization: SQL Injection Prevention
   
2. **Access Control Specialist** (Security Engineer)
   - Capabilities: rbac_implementation, privilege_management
   - Specialization: Access Control
   
3. **Encryption Specialist** (Security Engineer)
   - Capabilities: data_encryption, tls_configuration
   - Specialization: Encryption

**Responsibilities:**
- SQL injection detection
- Vulnerability scanning
- Access control validation
- Encryption compliance

### Manager Office
**Staff:** 3 Agents

1. **Database Manager** (Senior Manager)
   - Capabilities: task_coordination, resource_allocation, project_planning
   - Specialization: Project Management
   
2. **Operations Manager** (Manager)
   - Capabilities: performance_monitoring, capacity_planning
   - Specialization: Operations
   
3. **Quality Manager** (Manager)
   - Capabilities: quality_assurance, standards_compliance
   - Specialization: Quality Assurance

**Responsibilities:**
- Task coordination across offices
- Resource allocation
- Quality assurance
- Project planning

## Specialist Agents

### Service Agent
Handles SQL service operations including connection management, transaction handling, and query execution coordination.

**Capabilities:**
- Connection string validation
- Transaction ACID compliance analysis
- Query execution planning

### Data Model Agent
Analyzes schemas and data structures for normalization, relationships, and integrity.

**Capabilities:**
- DDL analysis
- Normalization checking (1NF-3NF)
- Constraint validation

### Operations Agent
Handles query analysis, optimization, and security scanning.

**Capabilities:**
- Query structure analysis
- Performance optimization recommendations
- SQL injection detection

## Installation

### Requirements
- Python 3.7 or higher

### Setup
```bash
cd floors/sql
chmod +x department_floor.py
```

No additional dependencies required - uses Python standard library only.

## Running

Start the SQL Department Floor:

```bash
python3 department_floor.py
```

Or make executable and run directly:

```bash
./department_floor.py
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
  "floor_number": 8,
  "language": "sql",
  "domain": "Data definition, Query logic, Schema design",
  "architectural_law": "ACID compliance, Parameterization mandatory, SQL injection prevention",
  "security_doctrine": "No dynamic SQL, Always use prepared statements, Input validation",
  "offices": ["Architecture Office", "Implementation Office", "Review Office", "Test Office", "Security Office", "Manager Office"],
  "agent_count": 18,
  "agents": [...],
  "specialist_agents": [...]
}
```

### Analyze SQL
Comprehensive SQL analysis including structure, security, and performance.

```json
{
  "method": "analyze_sql",
  "params": {
    "sql": "SELECT * FROM users WHERE id = ?",
    "analysis_type": "comprehensive"
  }
}
```

**Analysis Types:**
- `comprehensive`: Full analysis (default)
- `structure`: Query structure only
- `security`: Security audit only
- `ddl`: DDL and normalization analysis

**Response:**
```json
{
  "status": "success",
  "analysis": {
    "query_analysis": {
      "query_type": "SELECT",
      "has_where": true,
      "has_join": false,
      "complexity": "simple",
      "performance_warnings": ["SELECT * detected - specify columns explicitly"]
    },
    "security_analysis": {
      "security_level": "safe",
      "has_parameterized_queries": true,
      "vulnerabilities": [],
      "safe": true
    }
  }
}
```

### Review SQL
Submit SQL for review by Review Office.

```json
{
  "method": "review_sql",
  "params": {
    "sql": "UPDATE users SET status = 'active' WHERE id = $1",
    "context": {"purpose": "user activation"}
  }
}
```

**Response:**
```json
{
  "status": "reviewed",
  "issues": [],
  "recommendations": [],
  "approved": true,
  "security_level": "safe"
}
```

### Security Audit
Perform security audit through Security Office.

```json
{
  "method": "security_audit",
  "params": {
    "sql": "DELETE FROM logs WHERE date < '2024-01-01'"
  }
}
```

**Response:**
```json
{
  "status": "audited",
  "security_level": "warning",
  "findings": [
    {
      "severity": "high",
      "finding": "Not using parameterized queries",
      "recommendation": "Always use prepared statements"
    }
  ],
  "passed_audit": false
}
```

### Test SQL
Test SQL query through Test Office.

```json
{
  "method": "test_sql",
  "params": {
    "sql": "SELECT id, name FROM users WHERE status = ?",
    "test_data": {"status": "active"}
  }
}
```

**Response:**
```json
{
  "status": "tested",
  "test_results": {
    "syntax_valid": true,
    "executes_successfully": true,
    "returns_expected_results": true,
    "performance_acceptable": true
  },
  "all_tests_passed": true,
  "query_complexity": "simple"
}
```

### Validate Connection String
Validate database connection strings.

```json
{
  "method": "validate_connection",
  "params": {
    "connection_string": "postgresql://user:pass@localhost/mydb"
  }
}
```

**Response:**
```json
{
  "valid": true,
  "dialect": "postgresql",
  "contains_password": true
}
```

### Analyze Transaction
Analyze transaction for ACID compliance.

```json
{
  "method": "analyze_transaction",
  "params": {
    "statements": [
      "BEGIN",
      "UPDATE accounts SET balance = balance - 100 WHERE id = 1",
      "UPDATE accounts SET balance = balance + 100 WHERE id = 2",
      "COMMIT"
    ]
  }
}
```

**Response:**
```json
{
  "has_proper_boundaries": true,
  "statement_count": 4,
  "issues": []
}
```

### Create Task
Create a new task on this floor.

```json
{
  "method": "create_task",
  "params": {
    "task_id": "sql_task_001",
    "title": "Optimize user query",
    "task_type": "optimization",
    "assigned_to": "arch_002",
    "metadata": {"priority": "high"}
  }
}
```

## Usage Examples

### Example 1: Analyze a Complex Query
```bash
echo '{"method": "analyze_sql", "params": {"sql": "SELECT u.id, u.name, COUNT(o.id) FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE u.status = ? GROUP BY u.id ORDER BY COUNT(o.id) DESC", "analysis_type": "comprehensive"}}' | python3 department_floor.py
```

### Example 2: Security Audit for Injection
```bash
echo '{"method": "security_audit", "params": {"sql": "SELECT * FROM users WHERE username = '\''admin'\'' OR '\''1'\''='\''1'\''"}}' | python3 department_floor.py
```

### Example 3: Review DDL Statement
```bash
echo '{"method": "review_sql", "params": {"sql": "CREATE TABLE users (id SERIAL PRIMARY KEY, username VARCHAR(255) NOT NULL UNIQUE, email VARCHAR(255) NOT NULL, created_at TIMESTAMP DEFAULT NOW())"}}' | python3 department_floor.py
```

### Example 4: Get Floor Information
```bash
echo '{"method": "get_info"}' | python3 department_floor.py
```

## Supported SQL Dialects

- ANSI SQL
- PostgreSQL
- MySQL/MariaDB
- SQLite
- Microsoft SQL Server
- Oracle

## Best Practices

1. **Always use parameterized queries**
   ```sql
   -- Good
   SELECT * FROM users WHERE id = ?
   
   -- Bad
   SELECT * FROM users WHERE id = 123
   ```

2. **Explicit column selection**
   ```sql
   -- Good
   SELECT id, name, email FROM users
   
   -- Bad
   SELECT * FROM users
   ```

3. **Proper transaction boundaries**
   ```sql
   BEGIN;
   -- operations
   COMMIT; -- or ROLLBACK
   ```

4. **Use constraints for data integrity**
   ```sql
   CREATE TABLE users (
     id SERIAL PRIMARY KEY,
     email VARCHAR(255) NOT NULL UNIQUE,
     created_at TIMESTAMP NOT NULL DEFAULT NOW()
   );
   ```

## Error Handling

All errors are returned with explicit status and messages:

```json
{
  "status": "error",
  "message": "Detailed error description"
}
```

Security violations return CRITICAL status:
```json
{
  "security_level": "critical",
  "vulnerabilities": ["SQL injection detected"]
}
```

## Contributing

When extending SQL floor capabilities:

1. Maintain ACID compliance principles
2. Enforce parameterization for all dynamic queries
3. Add security validation for new operations
4. Update office staffing appropriately
5. Document all new methods in API section

## License

Part of Thirsty's Projects Miniature Office - See main LICENSE file.
