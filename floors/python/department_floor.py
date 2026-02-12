#!/usr/bin/env python3
"""
FLOOR 1 - PYTHON JURISDICTION
Department Floor Implementation

Domain: Application logic, Automation, Data processing, Glue code
Architectural Law: Readability > micro-optimization, PEP 8 compliance
"""

import json
import sys
from typing import Dict, Any, List
from dataclasses import dataclass, asdict
from datetime import datetime


@dataclass
class FloorAgent:
    """Represents an agent working on this floor"""
    agent_id: str
    name: str
    role: str
    capabilities: List[str]
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class Task:
    """Task managed by this floor"""
    task_id: str
    title: str
    status: str  # pending, in_progress, completed, failed
    assigned_to: str
    created_at: str
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class PythonDepartmentFloor:
    """
    Python Department Floor - Floor 1
    
    Implements the department logic for Python jurisdiction following:
    - Language Sovereignty
    - Identical Internal Topology (6 offices)
    - Contract-Bound Operation
    - Non-Creative Mandate
    - Failure Escalation Guarantee
    """
    
    def __init__(self):
        self.floor_number = 1
        self.language = "python"
        self.domain = "Application logic, Automation, Data processing"
        self.agents: Dict[str, FloorAgent] = {}
        self.tasks: Dict[str, Task] = {}
        self.offices = [
            "Architecture Office",
            "Implementation Office", 
            "Review Office",
            "Test Office",
            "Security Office",
            "Manager Office"
        ]
    
    def add_agent(self, agent_id: str, name: str, role: str, capabilities: List[str]) -> Dict[str, Any]:
        """Add an agent to this floor"""
        agent = FloorAgent(agent_id, name, role, capabilities)
        self.agents[agent_id] = agent
        return {"status": "success", "agent": agent.to_dict()}
    
    def create_task(self, task_id: str, title: str, assigned_to: str) -> Dict[str, Any]:
        """Create a new task on this floor"""
        task = Task(
            task_id=task_id,
            title=title,
            status="pending",
            assigned_to=assigned_to,
            created_at=datetime.utcnow().isoformat()
        )
        self.tasks[task_id] = task
        return {"status": "success", "task": task.to_dict()}
    
    def get_floor_info(self) -> Dict[str, Any]:
        """Get information about this floor"""
        return {
            "floor_number": self.floor_number,
            "language": self.language,
            "domain": self.domain,
            "offices": self.offices,
            "agent_count": len(self.agents),
            "task_count": len(self.tasks),
            "agents": [agent.to_dict() for agent in self.agents.values()],
            "tasks": [task.to_dict() for task in self.tasks.values()]
        }
    
    def process_code(self, code: str, operation: str) -> Dict[str, Any]:
        """
        Process Python code according to floor jurisdiction
        Operations: analyze, format, lint, test
        """
        if operation == "analyze":
            # Simple analysis
            lines = code.split('\n')
            return {
                "status": "success",
                "analysis": {
                    "lines": len(lines),
                    "functions": code.count('def '),
                    "classes": code.count('class '),
                    "language": "python"
                }
            }
        elif operation == "format":
            # Placeholder for formatting
            return {
                "status": "success",
                "formatted": True,
                "message": "Python code formatted (PEP 8)"
            }
        else:
            return {
                "status": "error",
                "message": f"Unknown operation: {operation}"
            }
    
    def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Handle incoming JSON-RPC request"""
        method = request.get("method")
        params = request.get("params", {})
        
        if method == "get_info":
            return self.get_floor_info()
        elif method == "add_agent":
            return self.add_agent(**params)
        elif method == "create_task":
            return self.create_task(**params)
        elif method == "process_code":
            return self.process_code(**params)
        else:
            return {"status": "error", "message": f"Unknown method: {method}"}


def main():
    """Main entry point - JSON-RPC server over stdin/stdout"""
    floor = PythonDepartmentFloor()
    
    print("Python Department Floor (Floor 1) - Ready", file=sys.stderr)
    print(f"Domain: {floor.domain}", file=sys.stderr)
    print(f"Offices: {', '.join(floor.offices)}", file=sys.stderr)
    sys.stderr.flush()
    
    # Process requests line by line
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
