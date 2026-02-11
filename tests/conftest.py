"""Test fixtures and utilities for the Miniature Office test suite."""
import pytest
from src.core.entity import EntityRegistry, EntityType, Entity, _registry
from src.core.world import World
from src.core.audit import AuditLog


@pytest.fixture(autouse=True)
def reset_global_state():
    """Reset global state before each test."""
    # Clear entity registry
    from src.core import entity
    entity._registry = EntityRegistry()
    
    # Clear audit log
    from src.core import audit
    audit._audit_log = AuditLog()
    
    # Clear world
    from src.core import world
    world._world = None
    
    # Clear supply store
    from src.tools import supply_store
    supply_store._supply_store = supply_store.SupplyStore()
    
    # Clear meeting system
    from src.core import mission
    mission._meeting_system = mission.MeetingSystem()
    
    # Clear contract registry
    from src.core import cognitive_contract
    cognitive_contract._contract_registry = cognitive_contract.ContractRegistry()
    
    # Clear expanded autonomy model
    from src.core import expanded_autonomy
    expanded_autonomy._expanded_autonomy_model = None
    
    # Clear creative autonomy model
    from src.core import creative_autonomy
    creative_autonomy._bounded_creative_autonomy = None
    
    # Clear off-duty city
    from src.core import off_duty_city
    off_duty_city._off_duty_city_instance = None
    
    yield


@pytest.fixture
def entity_registry():
    """Create a fresh entity registry for testing."""
    registry = EntityRegistry()
    return registry


@pytest.fixture
def audit_log():
    """Create a fresh audit log for testing."""
    log = AuditLog()
    return log


@pytest.fixture
def world():
    """Create a basic world for testing."""
    from src.core.world import World, Floor
    w = World("world-test", "Test World")
    return w


@pytest.fixture
def sample_floor():
    """Create a sample floor for testing."""
    from src.core.world import Floor
    floor = Floor("floor-test", "Python")
    return floor


@pytest.fixture
def sample_agent():
    """Create a sample agent for testing."""
    from src.agents.agent import Agent, AgentRole
    agent = Agent(
        "agent-test-001",
        "Test Agent",
        AgentRole.BUILDER,
        {"python", "testing"}
    )
    return agent


@pytest.fixture
def sample_task():
    """Create a sample task for testing."""
    from src.core.mission import Task
    task = Task(
        "task-test-001",
        "Test Task",
        "A test task for unit testing",
        None,
        None
    )
    return task
