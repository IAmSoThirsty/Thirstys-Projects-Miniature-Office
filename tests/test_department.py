"""
Comprehensive tests for departments/department.py to achieve 100% coverage
"""
import pytest

from src.departments.department import (
    Department,
    DepartmentRegistry,
    get_department_registry
)
from src.agents.agent import Agent, AgentRole, CapabilityProfile
from src.core.entity import EntityType, RelationType, get_registry
from src.core.audit import get_audit_log, EventType


class TestDepartment:
    """Test Department class functionality"""
    
    def test_department_creation(self):
        """Test basic department creation"""
        dept = Department(
            department_id="dept1",
            name="Python Department",
            domain="Python",
            toolchain=["pytest", "mypy"]
        )
        
        assert dept.entity_id == "dept1"
        assert dept.name == "Python Department"
        assert dept.domain == "Python"
        assert dept.toolchain == ["pytest", "mypy"]
        assert len(dept.agents) == 0
        assert len(dept.contracts) == 0
        
        # Check registration
        registry = get_registry()
        assert registry.get("dept1") == dept
    
    def test_department_creation_no_toolchain(self):
        """Test department creation without toolchain"""
        dept = Department(
            department_id="dept2",
            name="Rust Department",
            domain="Rust"
        )
        
        assert dept.toolchain == []
    
    def test_add_agent(self):
        """Test adding an agent to department"""
        dept = Department(
            department_id="dept3",
            name="JavaScript Department",
            domain="JavaScript"
        )
        
        agent = Agent(
            agent_id="agent1",
            name="JS Builder",
            role=AgentRole.BUILDER
        )
        
        dept.add_agent(agent)
        
        assert AgentRole.BUILDER in dept.agents
        assert "agent1" in dept.agents[AgentRole.BUILDER]
        assert agent.department_id == "dept3"
        
        # Check relationship
        relationships = agent.get_relationships(RelationType.WORKS_IN)
        assert len(relationships) > 0
        assert any(r.target_id == "dept3" for r in relationships)
    
    def test_add_multiple_agents_same_role(self):
        """Test adding multiple agents with same role"""
        dept = Department(
            department_id="dept4",
            name="Multi Agent Dept",
            domain="Python"
        )
        
        agent1 = Agent(
            agent_id="agent2",
            name="Builder 1",
            role=AgentRole.BUILDER
        )
        agent2 = Agent(
            agent_id="agent3",
            name="Builder 2",
            role=AgentRole.BUILDER
        )
        
        dept.add_agent(agent1)
        dept.add_agent(agent2)
        
        assert len(dept.agents[AgentRole.BUILDER]) == 2
        assert "agent2" in dept.agents[AgentRole.BUILDER]
        assert "agent3" in dept.agents[AgentRole.BUILDER]
    
    def test_get_missing_roles_all_missing(self):
        """Test getting missing roles when none are filled"""
        dept = Department(
            department_id="dept5",
            name="Empty Dept",
            domain="Go"
        )
        
        missing = dept.get_missing_roles()
        
        assert AgentRole.ARCHITECT in missing
        assert AgentRole.BUILDER in missing
        assert AgentRole.VERIFIER in missing
        assert AgentRole.SECURITY in missing
        assert AgentRole.DOC_AGENT in missing
        assert len(missing) == 5
    
    def test_get_missing_roles_partially_filled(self):
        """Test getting missing roles when some are filled"""
        dept = Department(
            department_id="dept6",
            name="Partial Dept",
            domain="Python"
        )
        
        builder = Agent(
            agent_id="agent4",
            name="Builder",
            role=AgentRole.BUILDER
        )
        architect = Agent(
            agent_id="agent5",
            name="Architect",
            role=AgentRole.ARCHITECT
        )
        
        dept.add_agent(builder)
        dept.add_agent(architect)
        
        missing = dept.get_missing_roles()
        
        assert AgentRole.BUILDER not in missing
        assert AgentRole.ARCHITECT not in missing
        assert AgentRole.VERIFIER in missing
        assert AgentRole.SECURITY in missing
        assert AgentRole.DOC_AGENT in missing
        assert len(missing) == 3
    
    def test_auto_spawn_assistants(self):
        """Test auto-spawning assistants for missing roles"""
        dept = Department(
            department_id="dept7",
            name="Auto Spawn Dept",
            domain="Python"
        )
        
        # Add one agent
        builder = Agent(
            agent_id="agent6",
            name="Manual Builder",
            role=AgentRole.BUILDER
        )
        dept.add_agent(builder)
        
        # Auto-spawn assistants for missing roles
        spawned = dept.auto_spawn_assistants()
        
        assert len(spawned) == 4  # Missing 4 roles
        assert dept.is_fully_staffed()
        
        # Check each required role is filled
        assert dept.has_role_filled(AgentRole.ARCHITECT)
        assert dept.has_role_filled(AgentRole.BUILDER)
        assert dept.has_role_filled(AgentRole.VERIFIER)
        assert dept.has_role_filled(AgentRole.SECURITY)
        assert dept.has_role_filled(AgentRole.DOC_AGENT)
    
    def test_default_capabilities_for_architect(self):
        """Test default capabilities for architect role"""
        dept = Department(
            department_id="dept8",
            name="Test Dept",
            domain="Python"
        )
        
        capabilities = dept._default_capabilities_for_role(AgentRole.ARCHITECT)
        
        assert 'python' in capabilities.languages
        assert 'design' in capabilities.skills
        assert 'architecture' in capabilities.skills
        assert 'planning' in capabilities.skills
        assert capabilities.security_clearance == 3
    
    def test_default_capabilities_for_builder(self):
        """Test default capabilities for builder role"""
        dept = Department(
            department_id="dept9",
            name="Test Dept",
            domain="Rust"
        )
        
        capabilities = dept._default_capabilities_for_role(AgentRole.BUILDER)
        
        assert 'rust' in capabilities.languages
        assert 'coding' in capabilities.skills
        assert 'implementation' in capabilities.skills
        assert 'compiler' in capabilities.tools
        assert 'interpreter' in capabilities.tools
        assert capabilities.security_clearance == 2
    
    def test_default_capabilities_for_verifier(self):
        """Test default capabilities for verifier role"""
        dept = Department(
            department_id="dept10",
            name="Test Dept",
            domain="JavaScript"
        )
        
        capabilities = dept._default_capabilities_for_role(AgentRole.VERIFIER)
        
        assert 'javascript' in capabilities.languages
        assert 'testing' in capabilities.skills
        assert 'validation' in capabilities.skills
        assert 'debugging' in capabilities.skills
        assert 'test_framework' in capabilities.tools
        assert 'linter' in capabilities.tools
        assert capabilities.security_clearance == 2
    
    def test_default_capabilities_for_security(self):
        """Test default capabilities for security role"""
        dept = Department(
            department_id="dept11",
            name="Test Dept",
            domain="Go"
        )
        
        capabilities = dept._default_capabilities_for_role(AgentRole.SECURITY)
        
        assert 'go' in capabilities.languages
        assert 'security' in capabilities.skills
        assert 'threat_modeling' in capabilities.skills
        assert 'auditing' in capabilities.skills
        assert capabilities.security_clearance == 4
    
    def test_default_capabilities_for_doc_agent(self):
        """Test default capabilities for doc_agent role"""
        dept = Department(
            department_id="dept12",
            name="Test Dept",
            domain="TypeScript"
        )
        
        capabilities = dept._default_capabilities_for_role(AgentRole.DOC_AGENT)
        
        assert 'typescript' in capabilities.languages
        assert 'documentation' in capabilities.skills
        assert 'communication' in capabilities.skills
        assert 'writing' in capabilities.skills
        assert capabilities.security_clearance == 2
    
    def test_add_tool_to_toolchain_new(self):
        """Test adding new tool to toolchain"""
        dept = Department(
            department_id="dept13",
            name="Tool Dept",
            domain="Python"
        )
        
        # Create a mock tool entity
        from src.core.entity import Entity
        tool = Entity(
            entity_id="tool1",
            entity_type=EntityType.TOOL,
            name="Test Tool"
        )
        get_registry().register(tool)
        
        dept.add_tool_to_toolchain("tool1")
        
        assert "tool1" in dept.toolchain
        
        # Check relationship
        relationships = dept.get_relationships(RelationType.USES)
        assert len(relationships) > 0
        assert any(r.target_id == "tool1" for r in relationships)
    
    def test_add_tool_to_toolchain_existing(self):
        """Test adding existing tool doesn't duplicate"""
        dept = Department(
            department_id="dept14",
            name="Tool Dept",
            domain="Python",
            toolchain=["tool2"]
        )
        
        dept.add_tool_to_toolchain("tool2")
        
        assert dept.toolchain.count("tool2") == 1
    
    def test_add_tool_to_toolchain_not_in_registry(self):
        """Test adding tool that's not in registry"""
        dept = Department(
            department_id="dept15",
            name="Tool Dept",
            domain="Python"
        )
        
        # Should not raise error even if tool not found
        dept.add_tool_to_toolchain("nonexistent_tool")
        
        assert "nonexistent_tool" in dept.toolchain
    
    def test_has_role_filled_true(self):
        """Test has_role_filled when role exists"""
        dept = Department(
            department_id="dept16",
            name="Filled Dept",
            domain="Python"
        )
        
        agent = Agent(
            agent_id="agent7",
            name="Verifier",
            role=AgentRole.VERIFIER
        )
        dept.add_agent(agent)
        
        assert dept.has_role_filled(AgentRole.VERIFIER) is True
    
    def test_has_role_filled_false(self):
        """Test has_role_filled when role doesn't exist"""
        dept = Department(
            department_id="dept17",
            name="Empty Dept",
            domain="Python"
        )
        
        assert dept.has_role_filled(AgentRole.ARCHITECT) is False
    
    def test_is_fully_staffed_true(self):
        """Test is_fully_staffed when all roles filled"""
        dept = Department(
            department_id="dept18",
            name="Full Dept",
            domain="Python"
        )
        
        # Add all required roles
        roles = [
            AgentRole.ARCHITECT,
            AgentRole.BUILDER,
            AgentRole.VERIFIER,
            AgentRole.SECURITY,
            AgentRole.DOC_AGENT
        ]
        
        for i, role in enumerate(roles):
            agent = Agent(
                agent_id=f"agent_{i}",
                name=f"Agent {i}",
                role=role
            )
            dept.add_agent(agent)
        
        assert dept.is_fully_staffed() is True
    
    def test_is_fully_staffed_false(self):
        """Test is_fully_staffed when roles missing"""
        dept = Department(
            department_id="dept19",
            name="Partial Dept",
            domain="Python"
        )
        
        agent = Agent(
            agent_id="agent8",
            name="Only Builder",
            role=AgentRole.BUILDER
        )
        dept.add_agent(agent)
        
        assert dept.is_fully_staffed() is False
    
    def test_get_agents_by_role(self):
        """Test getting agents by role"""
        dept = Department(
            department_id="dept20",
            name="Multi Builder Dept",
            domain="Python"
        )
        
        builder1 = Agent(
            agent_id="agent9",
            name="Builder 1",
            role=AgentRole.BUILDER
        )
        builder2 = Agent(
            agent_id="agent10",
            name="Builder 2",
            role=AgentRole.BUILDER
        )
        architect = Agent(
            agent_id="agent11",
            name="Architect",
            role=AgentRole.ARCHITECT
        )
        
        dept.add_agent(builder1)
        dept.add_agent(builder2)
        dept.add_agent(architect)
        
        builders = dept.get_agents_by_role(AgentRole.BUILDER)
        
        assert len(builders) == 2
        assert builder1 in builders
        assert builder2 in builders
        
        architects = dept.get_agents_by_role(AgentRole.ARCHITECT)
        assert len(architects) == 1
        assert architect in architects
    
    def test_get_agents_by_role_empty(self):
        """Test getting agents by role when none exist"""
        dept = Department(
            department_id="dept21",
            name="Empty Role Dept",
            domain="Python"
        )
        
        agents = dept.get_agents_by_role(AgentRole.SECURITY)
        
        assert len(agents) == 0
    
    def test_implement_contract(self):
        """Test implementing a contract"""
        dept = Department(
            department_id="dept22",
            name="Contract Dept",
            domain="Python"
        )
        
        # Create a mock contract entity
        from src.core.entity import Entity
        contract = Entity(
            entity_id="contract1",
            entity_type=EntityType.CONTRACT,
            name="Test Contract"
        )
        get_registry().register(contract)
        
        dept.implement_contract("contract1")
        
        assert "contract1" in dept.contracts
        
        # Check relationship
        relationships = dept.get_relationships(RelationType.IMPLEMENTS)
        assert len(relationships) > 0
        assert any(r.target_id == "contract1" for r in relationships)
    
    def test_implement_contract_duplicate(self):
        """Test implementing same contract twice"""
        dept = Department(
            department_id="dept23",
            name="Contract Dept",
            domain="Python"
        )
        
        dept.implement_contract("contract2")
        dept.implement_contract("contract2")
        
        assert dept.contracts.count("contract2") == 1
    
    def test_implement_contract_not_in_registry(self):
        """Test implementing contract not in registry"""
        dept = Department(
            department_id="dept24",
            name="Contract Dept",
            domain="Python"
        )
        
        # Should not raise error
        dept.implement_contract("nonexistent_contract")
        
        assert "nonexistent_contract" in dept.contracts


class TestDepartmentRegistry:
    """Test DepartmentRegistry functionality"""
    
    def test_register_department(self):
        """Test registering a department"""
        registry = DepartmentRegistry()
        
        dept = Department(
            department_id="dept25",
            name="Registered Dept",
            domain="Python"
        )
        
        registry.register_department(dept)
        
        assert "dept25" in registry.departments
        assert registry.departments["dept25"] == dept
        
        # Should auto-spawn assistants
        assert dept.is_fully_staffed()
    
    def test_get_department_exists(self):
        """Test getting existing department"""
        registry = DepartmentRegistry()
        
        dept = Department(
            department_id="dept26",
            name="Get Dept",
            domain="Rust"
        )
        registry.register_department(dept)
        
        result = registry.get_department("dept26")
        
        assert result == dept
    
    def test_get_department_not_exists(self):
        """Test getting non-existent department"""
        registry = DepartmentRegistry()
        
        result = registry.get_department("nonexistent")
        
        assert result is None
    
    def test_get_departments_by_domain(self):
        """Test getting departments by domain"""
        registry = DepartmentRegistry()
        
        dept1 = Department(
            department_id="dept27",
            name="Python Dept 1",
            domain="Python"
        )
        dept2 = Department(
            department_id="dept28",
            name="Python Dept 2",
            domain="Python"
        )
        dept3 = Department(
            department_id="dept29",
            name="Rust Dept",
            domain="Rust"
        )
        
        registry.register_department(dept1)
        registry.register_department(dept2)
        registry.register_department(dept3)
        
        python_depts = registry.get_departments_by_domain("Python")
        
        assert len(python_depts) == 2
        assert dept1 in python_depts
        assert dept2 in python_depts
        assert dept3 not in python_depts
    
    def test_get_departments_by_domain_case_insensitive(self):
        """Test getting departments by domain is case-insensitive"""
        registry = DepartmentRegistry()
        
        dept = Department(
            department_id="dept30",
            name="JS Dept",
            domain="JavaScript"
        )
        registry.register_department(dept)
        
        result1 = registry.get_departments_by_domain("javascript")
        result2 = registry.get_departments_by_domain("JavaScript")
        result3 = registry.get_departments_by_domain("JAVASCRIPT")
        
        assert len(result1) == 1
        assert len(result2) == 1
        assert len(result3) == 1
        assert dept in result1
    
    def test_get_departments_by_domain_empty(self):
        """Test getting departments by domain when none exist"""
        registry = DepartmentRegistry()
        
        result = registry.get_departments_by_domain("Haskell")
        
        assert len(result) == 0
    
    def test_ensure_all_staffed(self):
        """Test ensuring all departments are fully staffed"""
        registry = DepartmentRegistry()
        
        # Create department with partial staff
        dept1 = Department(
            department_id="dept31",
            name="Partial Dept",
            domain="Python"
        )
        builder = Agent(
            agent_id="agent12",
            name="Builder",
            role=AgentRole.BUILDER
        )
        dept1.add_agent(builder)
        
        # Create empty department
        dept2 = Department(
            department_id="dept32",
            name="Empty Dept",
            domain="Go"
        )
        
        registry.departments["dept31"] = dept1
        registry.departments["dept32"] = dept2
        
        # Initially not fully staffed
        assert not dept1.is_fully_staffed()
        assert not dept2.is_fully_staffed()
        
        registry.ensure_all_staffed()
        
        # Now both should be fully staffed
        assert dept1.is_fully_staffed()
        assert dept2.is_fully_staffed()


class TestGlobalDepartmentRegistry:
    """Test global department registry singleton"""
    
    def test_get_department_registry(self):
        """Test getting global department registry"""
        registry1 = get_department_registry()
        registry2 = get_department_registry()
        
        assert registry1 is registry2  # Same instance
        assert isinstance(registry1, DepartmentRegistry)
