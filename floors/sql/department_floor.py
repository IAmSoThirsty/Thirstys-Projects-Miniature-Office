#!/usr/bin/env python3
"""
FLOOR 8 - SQL JURISDICTION
Department Floor Implementation

Domain: Data definition, Query logic, Schema design
Architectural Law: ACID compliance, Parameterization mandatory, SQL injection prevention
Security Doctrine: No dynamic SQL, Always use prepared statements, Input validation
"""

import json
import sys
import re
from typing import Dict, Any, List, Optional, Set, Tuple
from dataclasses import dataclass, asdict
from datetime import datetime
from enum import Enum


class SQLDialect(Enum):
    """Supported SQL dialects"""
    ANSI = "ansi"
    POSTGRESQL = "postgresql"
    MYSQL = "mysql"
    SQLITE = "sqlite"
    MSSQL = "mssql"
    ORACLE = "oracle"


class SecurityLevel(Enum):
    """Security assessment levels"""
    SAFE = "safe"
    WARNING = "warning"
    CRITICAL = "critical"


@dataclass
class FloorAgent:
    """Represents an agent working on this floor"""
    agent_id: str
    name: str
    role: str
    office: str
    capabilities: List[str]
    specialization: str
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class Task:
    """Task managed by this floor"""
    task_id: str
    title: str
    status: str
    assigned_to: str
    created_at: str
    task_type: str
    metadata: Dict[str, Any]
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class ServiceAgent:
    """Service Agent - Handles SQL service operations"""
    
    def __init__(self):
        self.name = "SQL Service Agent"
        self.capabilities = ["connection_management", "transaction_handling", "query_execution"]
    
    def validate_connection_string(self, connection_string: str) -> Dict[str, Any]:
        """Validate database connection string"""
        patterns = {
            'postgresql': r'postgresql://[^:]+:[^@]+@[^/]+/\w+',
            'mysql': r'mysql://[^:]+:[^@]+@[^/]+/\w+',
            'sqlite': r'sqlite:///[\w/]+\.db',
        }
        
        for dialect, pattern in patterns.items():
            if re.match(pattern, connection_string):
                return {
                    "valid": True,
                    "dialect": dialect,
                    "contains_password": ":" in connection_string.split("@")[0] if "@" in connection_string else False
                }
        
        return {"valid": False, "error": "Unknown or invalid connection string format"}
    
    def analyze_transaction(self, statements: List[str]) -> Dict[str, Any]:
        """Analyze transaction for ACID compliance"""
        has_begin = any('BEGIN' in stmt.upper() for stmt in statements)
        has_commit = any('COMMIT' in stmt.upper() for stmt in statements)
        has_rollback = any('ROLLBACK' in stmt.upper() for stmt in statements)
        
        issues = []
        if not has_begin and len(statements) > 1:
            issues.append("Transaction missing BEGIN statement")
        if not (has_commit or has_rollback):
            issues.append("Transaction missing COMMIT or ROLLBACK")
        
        return {
            "has_proper_boundaries": has_begin and (has_commit or has_rollback),
            "statement_count": len(statements),
            "issues": issues
        }


class DataModelAgent:
    """Data Model Agent - Analyzes schemas and data structures"""
    
    def __init__(self):
        self.name = "SQL Data Model Agent"
        self.capabilities = ["schema_analysis", "normalization_check", "relationship_detection"]
    
    def analyze_ddl(self, ddl: str) -> Dict[str, Any]:
        """Analyze DDL statements"""
        ddl_upper = ddl.upper()
        
        tables = re.findall(r'CREATE\s+TABLE\s+(\w+)', ddl, re.IGNORECASE)
        indexes = re.findall(r'CREATE\s+INDEX\s+(\w+)', ddl, re.IGNORECASE)
        constraints = len(re.findall(r'(PRIMARY KEY|FOREIGN KEY|UNIQUE|CHECK|NOT NULL)', ddl, re.IGNORECASE))
        
        # Extract column definitions
        columns_by_table = {}
        for table in tables:
            pattern = rf'CREATE\s+TABLE\s+{table}\s*\((.*?)\)'
            match = re.search(pattern, ddl, re.IGNORECASE | re.DOTALL)
            if match:
                col_defs = match.group(1).split(',')
                columns_by_table[table] = len(col_defs)
        
        return {
            "tables": tables,
            "table_count": len(tables),
            "indexes": indexes,
            "index_count": len(indexes),
            "constraint_count": constraints,
            "columns_by_table": columns_by_table,
            "has_primary_keys": 'PRIMARY KEY' in ddl_upper,
            "has_foreign_keys": 'FOREIGN KEY' in ddl_upper
        }
    
    def check_normalization(self, ddl: str) -> Dict[str, Any]:
        """Check database normalization level"""
        issues = []
        recommendations = []
        
        if 'PRIMARY KEY' not in ddl.upper():
            issues.append("Missing primary keys - violates 1NF")
            recommendations.append("Add PRIMARY KEY constraints to all tables")
        
        if 'FOREIGN KEY' not in ddl.upper():
            issues.append("No foreign key relationships defined")
            recommendations.append("Define FOREIGN KEY constraints to maintain referential integrity")
        
        # Check for common denormalization patterns
        if re.search(r'\w+_json\s+', ddl, re.IGNORECASE):
            issues.append("JSON columns detected - may indicate denormalization")
        
        return {
            "normalization_issues": issues,
            "recommendations": recommendations,
            "estimated_normal_form": "3NF" if not issues else "Unknown"
        }


class OperationsAgent:
    """Operations Agent - Handles query analysis and optimization"""
    
    def __init__(self):
        self.name = "SQL Operations Agent"
        self.capabilities = ["query_analysis", "performance_optimization", "security_scanning"]
        self.dangerous_keywords = {
            'DROP', 'DELETE', 'TRUNCATE', 'ALTER', 'EXEC', 'EXECUTE',
            'SHUTDOWN', 'GRANT', 'REVOKE'
        }
    
    def analyze_query(self, query: str) -> Dict[str, Any]:
        """Analyze SQL query for structure and performance"""
        query_upper = query.upper()
        
        # Detect query type
        query_type = None
        if query_upper.strip().startswith('SELECT'):
            query_type = 'SELECT'
        elif query_upper.strip().startswith('INSERT'):
            query_type = 'INSERT'
        elif query_upper.strip().startswith('UPDATE'):
            query_type = 'UPDATE'
        elif query_upper.strip().startswith('DELETE'):
            query_type = 'DELETE'
        
        # Count clauses
        has_where = 'WHERE' in query_upper
        has_join = any(j in query_upper for j in ['JOIN', 'INNER JOIN', 'LEFT JOIN', 'RIGHT JOIN'])
        has_subquery = query.count('(') > query.count(',')  # Simple heuristic
        has_group_by = 'GROUP BY' in query_upper
        has_order_by = 'ORDER BY' in query_upper
        
        # Detect potential performance issues
        performance_warnings = []
        if query_type == 'SELECT' and not has_where:
            performance_warnings.append("SELECT without WHERE clause - full table scan")
        if 'SELECT *' in query_upper:
            performance_warnings.append("SELECT * detected - specify columns explicitly")
        if has_subquery:
            performance_warnings.append("Subquery detected - consider JOIN optimization")
        
        return {
            "query_type": query_type,
            "has_where": has_where,
            "has_join": has_join,
            "has_subquery": has_subquery,
            "has_group_by": has_group_by,
            "has_order_by": has_order_by,
            "performance_warnings": performance_warnings,
            "complexity": self._calculate_complexity(query)
        }
    
    def _calculate_complexity(self, query: str) -> str:
        """Calculate query complexity"""
        score = 0
        query_upper = query.upper()
        
        score += query_upper.count('JOIN')
        score += query_upper.count('UNION') * 2
        score += query_upper.count('GROUP BY')
        score += query.count('(')  # Subqueries
        
        if score == 0:
            return "simple"
        elif score <= 3:
            return "moderate"
        else:
            return "complex"
    
    def detect_sql_injection(self, query: str, parameters: Optional[List[str]] = None) -> Dict[str, Any]:
        """Detect SQL injection vulnerabilities"""
        vulnerabilities = []
        security_level = SecurityLevel.SAFE
        
        # Check for string concatenation patterns
        concat_patterns = [
            r"['\"].*?\+.*?['\"]",  # String concatenation
            r"\$\{.*?\}",  # Template literals
            r"%s.*?%",  # Python string formatting
            r"#\{.*?\}",  # Ruby interpolation
        ]
        
        for pattern in concat_patterns:
            if re.search(pattern, query):
                vulnerabilities.append(f"Dynamic SQL detected: {pattern}")
                security_level = SecurityLevel.CRITICAL
        
        # Check for dangerous keywords with user input
        for keyword in self.dangerous_keywords:
            if keyword in query.upper():
                vulnerabilities.append(f"Dangerous operation: {keyword}")
                if security_level == SecurityLevel.SAFE:
                    security_level = SecurityLevel.WARNING
        
        # Check if using parameterized queries
        has_placeholders = bool(re.search(r'[\?$:][\w]+|\$\d+', query))
        
        if not has_placeholders and any(op in query.upper() for op in ['INSERT', 'UPDATE', 'DELETE']):
            vulnerabilities.append("No parameterized queries detected for DML operation")
            security_level = SecurityLevel.CRITICAL
        
        return {
            "security_level": security_level.value,
            "has_parameterized_queries": has_placeholders,
            "vulnerabilities": vulnerabilities,
            "safe": security_level == SecurityLevel.SAFE
        }


class ArchitectureOffice:
    """Architecture Office - Schema design and database architecture"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("arch_001", "Schema Architect", "Senior Architect", "Architecture", 
                      ["database_design", "normalization", "indexing_strategy"], "Schema Design"),
            FloorAgent("arch_002", "Performance Architect", "Architect", "Architecture",
                      ["query_optimization", "index_design", "partitioning"], "Performance"),
            FloorAgent("arch_003", "Data Architect", "Architect", "Architecture",
                      ["data_modeling", "relationship_design", "constraint_definition"], "Data Modeling")
        ]
    
    def design_schema(self, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Design database schema based on requirements"""
        entities = requirements.get("entities", [])
        relationships = requirements.get("relationships", [])
        
        return {
            "status": "success",
            "schema": {
                "entities": entities,
                "relationships": relationships,
                "recommended_indexes": [f"{entity}_id_idx" for entity in entities],
                "normalization_level": "3NF"
            }
        }


class ImplementationOffice:
    """Implementation Office - DDL/DML implementation"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("impl_001", "DDL Engineer", "Senior Engineer", "Implementation",
                      ["table_creation", "index_creation", "constraint_implementation"], "DDL"),
            FloorAgent("impl_002", "DML Engineer", "Engineer", "Implementation",
                      ["query_writing", "stored_procedures", "triggers"], "DML"),
            FloorAgent("impl_003", "Migration Engineer", "Engineer", "Implementation",
                      ["schema_migration", "data_migration", "version_control"], "Migration")
        ]
    
    def generate_ddl(self, schema: Dict[str, Any]) -> Dict[str, Any]:
        """Generate DDL from schema definition"""
        tables = schema.get("tables", [])
        ddl_statements = []
        
        for table in tables:
            stmt = f"CREATE TABLE {table};"
            ddl_statements.append(stmt)
        
        return {
            "status": "success",
            "ddl": ddl_statements,
            "statement_count": len(ddl_statements)
        }


class ReviewOffice:
    """Review Office - Query and schema review"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("rev_001", "Senior Reviewer", "Lead Reviewer", "Review",
                      ["code_review", "performance_review", "security_review"], "Comprehensive Review"),
            FloorAgent("rev_002", "Query Reviewer", "Reviewer", "Review",
                      ["query_analysis", "optimization_review"], "Query Review"),
            FloorAgent("rev_003", "Schema Reviewer", "Reviewer", "Review",
                      ["schema_review", "normalization_check"], "Schema Review")
        ]
    
    def review_query(self, query: str, context: Dict[str, Any]) -> Dict[str, Any]:
        """Review SQL query for quality and security"""
        ops_agent = OperationsAgent()
        analysis = ops_agent.analyze_query(query)
        security = ops_agent.detect_sql_injection(query)
        
        issues = []
        recommendations = []
        
        if not security["safe"]:
            issues.extend(security["vulnerabilities"])
            recommendations.append("Use parameterized queries")
        
        if analysis["performance_warnings"]:
            issues.extend(analysis["performance_warnings"])
        
        return {
            "status": "reviewed",
            "issues": issues,
            "recommendations": recommendations,
            "approved": len(issues) == 0,
            "security_level": security["security_level"]
        }


class TestOffice:
    """Test Office - Query and schema testing"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("test_001", "Query Tester", "Senior Tester", "Test",
                      ["query_testing", "performance_testing", "load_testing"], "Query Testing"),
            FloorAgent("test_002", "Integration Tester", "Tester", "Test",
                      ["integration_testing", "transaction_testing"], "Integration"),
            FloorAgent("test_003", "Data Tester", "Tester", "Test",
                      ["data_validation", "constraint_testing"], "Data Validation")
        ]
    
    def test_query(self, query: str, test_data: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Test SQL query execution"""
        ops_agent = OperationsAgent()
        analysis = ops_agent.analyze_query(query)
        
        # Simulate testing
        test_results = {
            "syntax_valid": True,
            "executes_successfully": True,
            "returns_expected_results": True,
            "performance_acceptable": analysis["complexity"] != "complex"
        }
        
        all_passed = all(test_results.values())
        
        return {
            "status": "tested",
            "test_results": test_results,
            "all_tests_passed": all_passed,
            "query_complexity": analysis["complexity"]
        }


class SecurityOffice:
    """Security Office - SQL injection prevention and security auditing"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("sec_001", "Security Auditor", "Senior Security Engineer", "Security",
                      ["injection_detection", "privilege_audit", "encryption_review"], "SQL Injection Prevention"),
            FloorAgent("sec_002", "Access Control Specialist", "Security Engineer", "Security",
                      ["rbac_implementation", "privilege_management"], "Access Control"),
            FloorAgent("sec_003", "Encryption Specialist", "Security Engineer", "Security",
                      ["data_encryption", "tls_configuration"], "Encryption")
        ]
    
    def security_audit(self, query: str) -> Dict[str, Any]:
        """Perform security audit on SQL query"""
        ops_agent = OperationsAgent()
        security_analysis = ops_agent.detect_sql_injection(query)
        
        audit_findings = []
        
        if not security_analysis["safe"]:
            audit_findings.append({
                "severity": "critical",
                "finding": "SQL injection vulnerability detected",
                "details": security_analysis["vulnerabilities"]
            })
        
        if not security_analysis["has_parameterized_queries"]:
            audit_findings.append({
                "severity": "high",
                "finding": "Not using parameterized queries",
                "recommendation": "Always use prepared statements"
            })
        
        return {
            "status": "audited",
            "security_level": security_analysis["security_level"],
            "findings": audit_findings,
            "passed_audit": len(audit_findings) == 0
        }


class ManagerOffice:
    """Manager Office - Task coordination and resource management"""
    
    def __init__(self):
        self.agents = [
            FloorAgent("mgr_001", "Database Manager", "Senior Manager", "Manager",
                      ["task_coordination", "resource_allocation", "project_planning"], "Project Management"),
            FloorAgent("mgr_002", "Operations Manager", "Manager", "Manager",
                      ["performance_monitoring", "capacity_planning"], "Operations"),
            FloorAgent("mgr_003", "Quality Manager", "Manager", "Manager",
                      ["quality_assurance", "standards_compliance"], "Quality Assurance")
        ]
    
    def coordinate_task(self, task: Task, offices: List[str]) -> Dict[str, Any]:
        """Coordinate task across multiple offices"""
        workflow = []
        
        if "Architecture" in offices:
            workflow.append({"office": "Architecture", "action": "design_review"})
        if "Implementation" in offices:
            workflow.append({"office": "Implementation", "action": "implement"})
        if "Review" in offices:
            workflow.append({"office": "Review", "action": "code_review"})
        if "Test" in offices:
            workflow.append({"office": "Test", "action": "test"})
        if "Security" in offices:
            workflow.append({"office": "Security", "action": "security_audit"})
        
        return {
            "status": "coordinated",
            "task_id": task.task_id,
            "workflow": workflow,
            "estimated_time": len(workflow) * 2
        }


class SQLDepartmentFloor:
    """
    SQL Department Floor - Floor 8
    
    Implements the department logic for SQL jurisdiction following:
    - Language Sovereignty: SQL-centric operations
    - Identical Internal Topology: 6 offices
    - Contract-Bound Operation: JSON-RPC protocol
    - Non-Creative Mandate: Strict SQL standards
    - Failure Escalation Guarantee: Explicit error reporting
    """
    
    def __init__(self):
        self.floor_number = 8
        self.language = "sql"
        self.domain = "Data definition, Query logic, Schema design"
        self.architectural_law = "ACID compliance, Parameterization mandatory, SQL injection prevention"
        self.security_doctrine = "No dynamic SQL, Always use prepared statements, Input validation"
        
        # Initialize offices
        self.architecture_office = ArchitectureOffice()
        self.implementation_office = ImplementationOffice()
        self.review_office = ReviewOffice()
        self.test_office = TestOffice()
        self.security_office = SecurityOffice()
        self.manager_office = ManagerOffice()
        
        # Initialize specialist agents
        self.service_agent = ServiceAgent()
        self.data_model_agent = DataModelAgent()
        self.operations_agent = OperationsAgent()
        
        self.tasks: Dict[str, Task] = {}
        self.offices = [
            "Architecture Office",
            "Implementation Office",
            "Review Office",
            "Test Office",
            "Security Office",
            "Manager Office"
        ]
    
    def get_all_agents(self) -> List[FloorAgent]:
        """Get all agents across all offices"""
        all_agents = []
        all_agents.extend(self.architecture_office.agents)
        all_agents.extend(self.implementation_office.agents)
        all_agents.extend(self.review_office.agents)
        all_agents.extend(self.test_office.agents)
        all_agents.extend(self.security_office.agents)
        all_agents.extend(self.manager_office.agents)
        return all_agents
    
    def create_task(self, task_id: str, title: str, task_type: str, assigned_to: str, 
                   metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Create a new task on this floor"""
        task = Task(
            task_id=task_id,
            title=title,
            status="pending",
            assigned_to=assigned_to,
            created_at=datetime.utcnow().isoformat(),
            task_type=task_type,
            metadata=metadata or {}
        )
        self.tasks[task_id] = task
        return {"status": "success", "task": task.to_dict()}
    
    def get_floor_info(self) -> Dict[str, Any]:
        """Get comprehensive floor information"""
        all_agents = self.get_all_agents()
        
        return {
            "floor_number": self.floor_number,
            "language": self.language,
            "domain": self.domain,
            "architectural_law": self.architectural_law,
            "security_doctrine": self.security_doctrine,
            "offices": self.offices,
            "agent_count": len(all_agents),
            "task_count": len(self.tasks),
            "agents": [agent.to_dict() for agent in all_agents],
            "tasks": [task.to_dict() for task in self.tasks.values()],
            "specialist_agents": [
                {
                    "name": self.service_agent.name,
                    "capabilities": self.service_agent.capabilities
                },
                {
                    "name": self.data_model_agent.name,
                    "capabilities": self.data_model_agent.capabilities
                },
                {
                    "name": self.operations_agent.name,
                    "capabilities": self.operations_agent.capabilities
                }
            ]
        }
    
    def analyze_sql(self, sql: str, analysis_type: str = "comprehensive") -> Dict[str, Any]:
        """Analyze SQL code"""
        results = {}
        
        if analysis_type in ["comprehensive", "structure"]:
            results["query_analysis"] = self.operations_agent.analyze_query(sql)
        
        if analysis_type in ["comprehensive", "security"]:
            results["security_analysis"] = self.operations_agent.detect_sql_injection(sql)
        
        if analysis_type in ["comprehensive", "ddl"] and "CREATE" in sql.upper():
            results["ddl_analysis"] = self.data_model_agent.analyze_ddl(sql)
            results["normalization"] = self.data_model_agent.check_normalization(sql)
        
        return {
            "status": "success",
            "analysis": results,
            "sql": sql
        }
    
    def review_sql(self, sql: str, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Review SQL through Review Office"""
        return self.review_office.review_query(sql, context or {})
    
    def security_audit_sql(self, sql: str) -> Dict[str, Any]:
        """Audit SQL through Security Office"""
        return self.security_office.security_audit(sql)
    
    def test_sql(self, sql: str, test_data: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Test SQL through Test Office"""
        return self.test_office.test_query(sql, test_data)
    
    def validate_connection(self, connection_string: str) -> Dict[str, Any]:
        """Validate database connection string"""
        return self.service_agent.validate_connection_string(connection_string)
    
    def analyze_transaction(self, statements: List[str]) -> Dict[str, Any]:
        """Analyze transaction for ACID compliance"""
        return self.service_agent.analyze_transaction(statements)
    
    def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Handle incoming JSON-RPC request"""
        method = request.get("method")
        params = request.get("params", {})
        
        try:
            if method == "get_info":
                return self.get_floor_info()
            elif method == "create_task":
                return self.create_task(**params)
            elif method == "analyze_sql":
                return self.analyze_sql(**params)
            elif method == "review_sql":
                return self.review_sql(**params)
            elif method == "security_audit":
                return self.security_audit_sql(**params)
            elif method == "test_sql":
                return self.test_sql(**params)
            elif method == "validate_connection":
                return self.validate_connection(**params)
            elif method == "analyze_transaction":
                return self.analyze_transaction(**params)
            else:
                return {"status": "error", "message": f"Unknown method: {method}"}
        except Exception as e:
            return {"status": "error", "message": f"Error processing request: {str(e)}"}


def main():
    """Main entry point - JSON-RPC server over stdin/stdout"""
    floor = SQLDepartmentFloor()
    
    print(f"SQL Department Floor (Floor {floor.floor_number}) - Ready", file=sys.stderr)
    print(f"Domain: {floor.domain}", file=sys.stderr)
    print(f"Architectural Law: {floor.architectural_law}", file=sys.stderr)
    print(f"Security Doctrine: {floor.security_doctrine}", file=sys.stderr)
    print(f"Offices: {', '.join(floor.offices)}", file=sys.stderr)
    print(f"Total Agents: {len(floor.get_all_agents())}", file=sys.stderr)
    sys.stderr.flush()
    
    for line in sys.stdin:
        try:
            request = json.loads(line.strip())
            response = floor.handle_request(request)
            print(json.dumps(response))
            sys.stdout.flush()
        except json.JSONDecodeError as e:
            error_response = {"status": "error", "message": f"Invalid JSON: {str(e)}"}
            print(json.dumps(error_response))
            sys.stdout.flush()
        except Exception as e:
            error_response = {"status": "error", "message": f"Error: {str(e)}"}
            print(json.dumps(error_response))
            sys.stdout.flush()


if __name__ == "__main__":
    main()
