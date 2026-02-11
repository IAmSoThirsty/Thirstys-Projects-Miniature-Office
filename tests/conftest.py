"""Test fixtures and utilities for the Miniature Office test suite."""
import pytest
from src.core.entity import EntityRegistry, EntityType, Entity
from src.core.world import World
from src.core.audit import AuditLog


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
