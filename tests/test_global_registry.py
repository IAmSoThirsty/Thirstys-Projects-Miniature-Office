"""
Comprehensive tests for src/core/global_registry.py to achieve 100% coverage

Tests cover:
- Enum classes (FloorStatus, ServiceType)
- Dataclasses (AgentRegistration, FloorRegistration)
- GlobalRegistry class and all methods
- Thread safety
- Edge cases and error conditions
- Singleton pattern
"""

import threading
import time
from datetime import datetime

import pytest

from src.core.global_registry import (
    AgentRegistration,
    FloorRegistration,
    FloorStatus,
    GlobalRegistry,
    ServiceType,
    _global_registry,
    get_global_registry,
)


class TestFloorStatus:
    """Test FloorStatus enum"""

    def test_floor_status_values(self):
        """Test all FloorStatus enum values"""
        assert FloorStatus.INITIALIZING.value == "initializing"
        assert FloorStatus.READY.value == "ready"
        assert FloorStatus.BUSY.value == "busy"
        assert FloorStatus.ERROR.value == "error"
        assert FloorStatus.STOPPED.value == "stopped"

    def test_floor_status_count(self):
        """Test that all expected statuses are defined"""
        expected_statuses = ["initializing", "ready", "busy", "error", "stopped"]
        actual_statuses = [status.value for status in FloorStatus]
        assert len(actual_statuses) == len(expected_statuses)
        for status in expected_statuses:
            assert status in actual_statuses


class TestServiceType:
    """Test ServiceType enum"""

    def test_service_type_values(self):
        """Test all ServiceType enum values"""
        assert ServiceType.CODE_ANALYSIS.value == "code_analysis"
        assert ServiceType.CODE_GENERATION.value == "code_generation"
        assert ServiceType.CODE_FORMATTING.value == "code_formatting"
        assert ServiceType.CODE_TESTING.value == "code_testing"
        assert ServiceType.CODE_SECURITY.value == "code_security"
        assert ServiceType.CODE_REVIEW.value == "code_review"
        assert ServiceType.BUILD.value == "build"
        assert ServiceType.DEPLOYMENT.value == "deployment"
        assert ServiceType.DATA_PROCESSING.value == "data_processing"
        assert ServiceType.WEB_SERVICE.value == "web_service"

    def test_service_type_count(self):
        """Test that all expected service types are defined"""
        expected_count = 10
        actual_count = len([s for s in ServiceType])
        assert actual_count == expected_count


class TestAgentRegistration:
    """Test AgentRegistration dataclass"""

    def test_agent_registration_creation(self):
        """Test basic agent registration creation"""
        agent_reg = AgentRegistration(
            agent_id="agent-001", name="Test Agent", role="Builder", capabilities=["python", "testing"], floor="floor-1"
        )

        assert agent_reg.agent_id == "agent-001"
        assert agent_reg.name == "Test Agent"
        assert agent_reg.role == "Builder"
        assert agent_reg.capabilities == ["python", "testing"]
        assert agent_reg.floor == "floor-1"
        assert isinstance(agent_reg.registered_at, str)

    def test_agent_registration_timestamp(self):
        """Test that registered_at is automatically set"""
        agent_reg = AgentRegistration(
            agent_id="agent-002", name="Agent", role="Tester", capabilities=[], floor="floor-1"
        )

        # Verify it's a valid ISO format timestamp
        datetime.fromisoformat(agent_reg.registered_at)

    def test_agent_registration_to_dict(self):
        """Test agent registration serialization"""
        agent_reg = AgentRegistration(
            agent_id="agent-003",
            name="Test Agent",
            role="Architect",
            capabilities=["design", "review"],
            floor="floor-2",
        )

        result = agent_reg.to_dict()

        assert result["agent_id"] == "agent-003"
        assert result["name"] == "Test Agent"
        assert result["role"] == "Architect"
        assert result["capabilities"] == ["design", "review"]
        assert result["floor"] == "floor-2"
        assert "registered_at" in result

    def test_agent_registration_empty_capabilities(self):
        """Test agent registration with no capabilities"""
        agent_reg = AgentRegistration(
            agent_id="agent-004", name="Simple Agent", role="Observer", capabilities=[], floor="floor-1"
        )

        assert agent_reg.capabilities == []
        result = agent_reg.to_dict()
        assert result["capabilities"] == []


class TestFloorRegistration:
    """Test FloorRegistration dataclass"""

    def test_floor_registration_creation(self):
        """Test basic floor registration creation"""
        floor_reg = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            status=FloorStatus.INITIALIZING,
            services=[ServiceType.CODE_ANALYSIS, ServiceType.CODE_TESTING],
        )

        assert floor_reg.floor_id == "floor-001"
        assert floor_reg.floor_number == 1
        assert floor_reg.language == "python"
        assert floor_reg.domain == "backend"
        assert floor_reg.status == FloorStatus.INITIALIZING
        assert len(floor_reg.services) == 2
        assert ServiceType.CODE_ANALYSIS in floor_reg.services
        assert len(floor_reg.agents) == 0
        assert floor_reg.endpoint is None
        assert floor_reg.process_id is None

    def test_floor_registration_with_optional_fields(self):
        """Test floor registration with all optional fields"""
        floor_reg = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="systems",
            status=FloorStatus.READY,
            services=[ServiceType.BUILD],
            agents=["agent-1", "agent-2"],
            endpoint="http://localhost:8080",
            process_id=1234,
            metadata={"version": "1.0", "region": "us-east"},
        )

        assert floor_reg.agents == ["agent-1", "agent-2"]
        assert floor_reg.endpoint == "http://localhost:8080"
        assert floor_reg.process_id == 1234
        assert floor_reg.metadata["version"] == "1.0"
        assert floor_reg.metadata["region"] == "us-east"

    def test_floor_registration_timestamps(self):
        """Test that timestamps are automatically set"""
        floor_reg = FloorRegistration(
            floor_id="floor-003",
            floor_number=3,
            language="go",
            domain="web",
            status=FloorStatus.READY,
            services=[ServiceType.WEB_SERVICE],
        )

        # Verify timestamps are valid ISO format
        datetime.fromisoformat(floor_reg.registered_at)
        datetime.fromisoformat(floor_reg.last_heartbeat)

    def test_floor_registration_to_dict(self):
        """Test floor registration serialization"""
        floor_reg = FloorRegistration(
            floor_id="floor-004",
            floor_number=4,
            language="javascript",
            domain="frontend",
            status=FloorStatus.BUSY,
            services=[ServiceType.CODE_FORMATTING, ServiceType.CODE_REVIEW],
            agents=["agent-10"],
            endpoint="http://localhost:9000",
            process_id=5678,
            metadata={"team": "frontend"},
        )

        result = floor_reg.to_dict()

        assert result["floor_id"] == "floor-004"
        assert result["floor_number"] == 4
        assert result["language"] == "javascript"
        assert result["domain"] == "frontend"
        assert result["status"] == "busy"  # Converted to string value
        assert len(result["services"]) == 2
        assert "code_formatting" in result["services"]
        assert "code_review" in result["services"]
        assert result["agents"] == ["agent-10"]
        assert result["endpoint"] == "http://localhost:9000"
        assert result["process_id"] == 5678
        assert result["metadata"]["team"] == "frontend"
        assert "registered_at" in result
        assert "last_heartbeat" in result


class TestGlobalRegistry:
    """Test GlobalRegistry class"""

    @pytest.fixture
    def registry(self):
        """Create a fresh registry for each test"""
        return GlobalRegistry()

    def test_registry_initialization(self, registry):
        """Test registry initialization"""
        assert len(registry.floors) == 0
        assert len(registry.agents) == 0
        assert len(registry.service_index) == len(ServiceType)
        assert registry._initialized is False

        # Check all service types are in index
        for service in ServiceType:
            assert service in registry.service_index
            assert len(registry.service_index[service]) == 0

    def test_initialize(self, registry):
        """Test registry initialization"""
        assert registry._initialized is False

        registry.initialize()
        assert registry._initialized is True

        # Should be idempotent
        registry.initialize()
        assert registry._initialized is True

    def test_register_floor_basic(self, registry):
        """Test basic floor registration"""
        floor = registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS],
        )

        assert floor.floor_id == "floor-001"
        assert floor.floor_number == 1
        assert floor.language == "python"
        assert floor.domain == "backend"
        assert floor.status == FloorStatus.INITIALIZING
        assert ServiceType.CODE_ANALYSIS in floor.services
        assert floor.endpoint is None
        assert floor.process_id is None
        assert len(floor.metadata) == 0

        # Check it's in registry
        assert "floor-001" in registry.floors
        assert "floor-001" in registry.service_index[ServiceType.CODE_ANALYSIS]

    def test_register_floor_with_all_options(self, registry):
        """Test floor registration with all optional parameters"""
        metadata = {"version": "2.0", "cluster": "prod"}

        floor = registry.register_floor(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="systems",
            services=[ServiceType.BUILD, ServiceType.CODE_SECURITY],
            endpoint="http://localhost:8080",
            process_id=9999,
            metadata=metadata,
        )

        assert floor.endpoint == "http://localhost:8080"
        assert floor.process_id == 9999
        assert floor.metadata == metadata

        # Check service index
        assert "floor-002" in registry.service_index[ServiceType.BUILD]
        assert "floor-002" in registry.service_index[ServiceType.CODE_SECURITY]

    def test_register_floor_duplicate(self, registry):
        """Test that registering duplicate floor raises error"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        with pytest.raises(ValueError, match="already registered"):
            registry.register_floor(
                floor_id="floor-001",
                floor_number=2,
                language="javascript",
                domain="test2",
                services=[ServiceType.CODE_ANALYSIS],
            )

    def test_register_floor_multiple_services(self, registry):
        """Test registering floor with multiple services"""
        services = [ServiceType.CODE_ANALYSIS, ServiceType.CODE_GENERATION, ServiceType.CODE_TESTING, ServiceType.BUILD]

        floor = registry.register_floor(
            floor_id="floor-multi", floor_number=10, language="python", domain="full-stack", services=services
        )

        assert len(floor.services) == 4

        # Check all services are indexed
        for service in services:
            assert "floor-multi" in registry.service_index[service]

    def test_register_agent_basic(self, registry):
        """Test basic agent registration"""
        # First register a floor
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        agent = registry.register_agent(
            agent_id="agent-001",
            name="Test Agent",
            role="Builder",
            capabilities=["python", "testing"],
            floor_id="floor-001",
        )

        assert agent.agent_id == "agent-001"
        assert agent.name == "Test Agent"
        assert agent.role == "Builder"
        assert agent.capabilities == ["python", "testing"]
        assert agent.floor == "floor-001"

        # Check it's in registry
        assert "agent-001" in registry.agents

        # Check floor has agent
        floor = registry.floors["floor-001"]
        assert "agent-001" in floor.agents

    def test_register_agent_duplicate(self, registry):
        """Test that registering duplicate agent raises error"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent One", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        with pytest.raises(ValueError, match="already registered"):
            registry.register_agent(
                agent_id="agent-001", name="Agent Two", role="Tester", capabilities=["rust"], floor_id="floor-001"
            )

    def test_register_agent_floor_not_found(self, registry):
        """Test registering agent to non-existent floor"""
        with pytest.raises(ValueError, match="not found in registry"):
            registry.register_agent(
                agent_id="agent-001",
                name="Orphan Agent",
                role="Builder",
                capabilities=["python"],
                floor_id="nonexistent-floor",
            )

    def test_register_multiple_agents_to_floor(self, registry):
        """Test registering multiple agents to the same floor"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        agent1 = registry.register_agent(  # noqa: F841
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        agent2 = registry.register_agent(  # noqa: F841
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        floor = registry.floors["floor-001"]
        assert len(floor.agents) == 2
        assert "agent-001" in floor.agents
        assert "agent-002" in floor.agents

    def test_update_floor_status(self, registry):
        """Test updating floor status"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        floor = registry.floors["floor-001"]
        assert floor.status == FloorStatus.INITIALIZING

        old_heartbeat = floor.last_heartbeat
        time.sleep(0.01)  # Small delay to ensure timestamp changes

        registry.update_floor_status("floor-001", FloorStatus.READY)

        assert floor.status == FloorStatus.READY
        assert floor.last_heartbeat != old_heartbeat

    def test_update_floor_status_not_found(self, registry):
        """Test updating status of non-existent floor"""
        with pytest.raises(ValueError, match="not found"):
            registry.update_floor_status("nonexistent", FloorStatus.READY)

    def test_get_floor(self, registry):
        """Test getting floor by ID"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        floor = registry.get_floor("floor-001")
        assert floor is not None
        assert floor.floor_id == "floor-001"

        # Non-existent floor
        floor = registry.get_floor("nonexistent")
        assert floor is None

    def test_get_agent(self, registry):
        """Test getting agent by ID"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        agent = registry.get_agent("agent-001")
        assert agent is not None
        assert agent.agent_id == "agent-001"

        # Non-existent agent
        agent = registry.get_agent("nonexistent")
        assert agent is None

    def test_find_floors_by_language(self, registry):
        """Test finding floors by language"""
        registry.register_floor(
            floor_id="floor-py-1",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS],
        )

        registry.register_floor(
            floor_id="floor-py-2",
            floor_number=2,
            language="Python",  # Different case
            domain="ml",
            services=[ServiceType.DATA_PROCESSING],
        )

        registry.register_floor(
            floor_id="floor-rust", floor_number=3, language="rust", domain="systems", services=[ServiceType.BUILD]
        )

        # Case-insensitive search
        python_floors = registry.find_floors_by_language("python")
        assert len(python_floors) == 2

        python_floors = registry.find_floors_by_language("PYTHON")
        assert len(python_floors) == 2

        rust_floors = registry.find_floors_by_language("rust")
        assert len(rust_floors) == 1

        # No matches
        go_floors = registry.find_floors_by_language("go")
        assert len(go_floors) == 0

    def test_find_floors_by_service(self, registry):
        """Test finding floors by service type"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS, ServiceType.CODE_TESTING],
        )

        registry.register_floor(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="systems",
            services=[ServiceType.CODE_ANALYSIS, ServiceType.BUILD],
        )

        registry.register_floor(
            floor_id="floor-003", floor_number=3, language="go", domain="web", services=[ServiceType.WEB_SERVICE]
        )

        analysis_floors = registry.find_floors_by_service(ServiceType.CODE_ANALYSIS)
        assert len(analysis_floors) == 2
        assert any(f.floor_id == "floor-001" for f in analysis_floors)
        assert any(f.floor_id == "floor-002" for f in analysis_floors)

        build_floors = registry.find_floors_by_service(ServiceType.BUILD)
        assert len(build_floors) == 1
        assert build_floors[0].floor_id == "floor-002"

        # Service with no floors
        deployment_floors = registry.find_floors_by_service(ServiceType.DEPLOYMENT)
        assert len(deployment_floors) == 0

    def test_find_ready_floors_by_service(self, registry):
        """Test finding ready floors by service"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        registry.register_floor(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="systems",
            services=[ServiceType.CODE_ANALYSIS],
        )
        # Leave floor-002 in INITIALIZING state

        registry.register_floor(
            floor_id="floor-003", floor_number=3, language="go", domain="web", services=[ServiceType.CODE_ANALYSIS]
        )
        registry.update_floor_status("floor-003", FloorStatus.BUSY)

        ready_floors = registry.find_ready_floors_by_service(ServiceType.CODE_ANALYSIS)

        assert len(ready_floors) == 1
        assert ready_floors[0].floor_id == "floor-001"
        assert ready_floors[0].status == FloorStatus.READY

    def test_get_all_floors(self, registry):
        """Test getting all floors"""
        assert len(registry.get_all_floors()) == 0

        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_floor(
            floor_id="floor-002", floor_number=2, language="rust", domain="test", services=[ServiceType.BUILD]
        )

        all_floors = registry.get_all_floors()
        assert len(all_floors) == 2
        assert any(f.floor_id == "floor-001" for f in all_floors)
        assert any(f.floor_id == "floor-002" for f in all_floors)

    def test_get_all_agents(self, registry):
        """Test getting all agents"""
        assert len(registry.get_all_agents()) == 0

        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        all_agents = registry.get_all_agents()
        assert len(all_agents) == 2
        assert any(a.agent_id == "agent-001" for a in all_agents)
        assert any(a.agent_id == "agent-002" for a in all_agents)

    def test_get_agents_by_floor(self, registry):
        """Test getting agents for a specific floor"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_floor(
            floor_id="floor-002", floor_number=2, language="rust", domain="test", services=[ServiceType.BUILD]
        )

        registry.register_agent(
            agent_id="agent-001", name="Python Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Python Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-003", name="Rust Agent", role="Builder", capabilities=["rust"], floor_id="floor-002"
        )

        floor1_agents = registry.get_agents_by_floor("floor-001")
        assert len(floor1_agents) == 2
        assert any(a.agent_id == "agent-001" for a in floor1_agents)
        assert any(a.agent_id == "agent-002" for a in floor1_agents)

        floor2_agents = registry.get_agents_by_floor("floor-002")
        assert len(floor2_agents) == 1
        assert floor2_agents[0].agent_id == "agent-003"

        # Non-existent floor
        agents = registry.get_agents_by_floor("nonexistent")
        assert len(agents) == 0

    def test_deregister_floor(self, registry):
        """Test deregistering a floor"""
        # Register floor with agents
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS, ServiceType.CODE_TESTING],
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        assert "floor-001" in registry.floors
        assert len(registry.agents) == 2
        assert "floor-001" in registry.service_index[ServiceType.CODE_ANALYSIS]
        assert "floor-001" in registry.service_index[ServiceType.CODE_TESTING]

        # Deregister
        result = registry.deregister_floor("floor-001")

        assert result is True
        assert "floor-001" not in registry.floors
        assert len(registry.agents) == 0  # Agents should be removed too
        assert "floor-001" not in registry.service_index[ServiceType.CODE_ANALYSIS]
        assert "floor-001" not in registry.service_index[ServiceType.CODE_TESTING]

    def test_deregister_floor_not_found(self, registry):
        """Test deregistering non-existent floor"""
        result = registry.deregister_floor("nonexistent")
        assert result is False

    def test_deregister_agent(self, registry):
        """Test deregistering an agent"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        floor = registry.floors["floor-001"]
        assert len(floor.agents) == 2
        assert "agent-001" in registry.agents

        # Deregister
        result = registry.deregister_agent("agent-001")

        assert result is True
        assert "agent-001" not in registry.agents
        assert len(floor.agents) == 1
        assert "agent-001" not in floor.agents
        assert "agent-002" in floor.agents

    def test_deregister_agent_not_found(self, registry):
        """Test deregistering non-existent agent"""
        result = registry.deregister_agent("nonexistent")
        assert result is False

    def test_deregister_agent_floor_already_removed(self, registry):
        """Test deregistering agent when floor is already gone"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        # Manually remove floor (simulating unusual state)
        del registry.floors["floor-001"]

        # Should still succeed
        result = registry.deregister_agent("agent-001")
        assert result is True
        assert "agent-001" not in registry.agents

    def test_get_registry_stats_empty(self, registry):
        """Test registry stats with no data"""
        stats = registry.get_registry_stats()

        assert stats["total_floors"] == 0
        assert stats["total_agents"] == 0
        assert len(stats["floors_by_status"]) == 0
        assert len(stats["languages"]) == 0

        # All services should have 0 floors
        for service in ServiceType:
            assert stats["floors_by_service"][service.value] == 0

    def test_get_registry_stats_with_data(self, registry):
        """Test registry stats with data"""
        # Register multiple floors with different statuses
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS, ServiceType.CODE_TESTING],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        registry.register_floor(
            floor_id="floor-002", floor_number=2, language="rust", domain="systems", services=[ServiceType.BUILD]
        )
        registry.update_floor_status("floor-002", FloorStatus.READY)

        registry.register_floor(
            floor_id="floor-003", floor_number=3, language="python", domain="ml", services=[ServiceType.DATA_PROCESSING]
        )
        # Leave in INITIALIZING state

        registry.register_floor(
            floor_id="floor-004",
            floor_number=4,
            language="javascript",
            domain="frontend",
            services=[ServiceType.WEB_SERVICE],
        )
        registry.update_floor_status("floor-004", FloorStatus.BUSY)

        # Register some agents
        registry.register_agent(
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        stats = registry.get_registry_stats()

        assert stats["total_floors"] == 4
        assert stats["total_agents"] == 2

        # Check status counts
        assert stats["floors_by_status"]["ready"] == 2
        assert stats["floors_by_status"]["initializing"] == 1
        assert stats["floors_by_status"]["busy"] == 1

        # Check service counts
        assert stats["floors_by_service"]["code_analysis"] == 1
        assert stats["floors_by_service"]["code_testing"] == 1
        assert stats["floors_by_service"]["build"] == 1
        assert stats["floors_by_service"]["data_processing"] == 1
        assert stats["floors_by_service"]["web_service"] == 1

        # Check languages
        assert "python" in stats["languages"]
        assert "rust" in stats["languages"]
        assert "javascript" in stats["languages"]
        assert len(stats["languages"]) == 3

    def test_export_registry_empty(self, registry):
        """Test exporting empty registry"""
        exported = registry.export_registry()

        assert "floors" in exported
        assert "agents" in exported
        assert "stats" in exported
        assert "exported_at" in exported

        assert len(exported["floors"]) == 0
        assert len(exported["agents"]) == 0

        # Verify timestamp format
        datetime.fromisoformat(exported["exported_at"])

    def test_export_registry_with_data(self, registry):
        """Test exporting registry with data"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS],
            metadata={"version": "1.0"},
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        exported = registry.export_registry()

        assert len(exported["floors"]) == 1
        assert len(exported["agents"]) == 1

        # Verify floor data
        floor_data = exported["floors"]["floor-001"]
        assert floor_data["floor_id"] == "floor-001"
        assert floor_data["language"] == "python"
        assert floor_data["status"] == "initializing"
        assert "code_analysis" in floor_data["services"]
        assert floor_data["metadata"]["version"] == "1.0"

        # Verify agent data
        agent_data = exported["agents"]["agent-001"]
        assert agent_data["agent_id"] == "agent-001"
        assert agent_data["name"] == "Agent"
        assert agent_data["floor"] == "floor-001"

        # Verify stats
        assert exported["stats"]["total_floors"] == 1
        assert exported["stats"]["total_agents"] == 1

    def test_import_registry_empty(self, registry):
        """Test importing empty registry data"""
        data = {"floors": {}, "agents": {}, "stats": {}}

        registry.import_registry(data)

        assert len(registry.floors) == 0
        assert len(registry.agents) == 0

    def test_import_registry_with_data(self, registry):
        """Test importing registry with data"""
        data = {
            "floors": {
                "floor-001": {
                    "floor_id": "floor-001",
                    "floor_number": 1,
                    "language": "python",
                    "domain": "backend",
                    "status": "ready",
                    "services": ["code_analysis", "code_testing"],
                    "agents": ["agent-001"],
                    "endpoint": "http://localhost:8080",
                    "process_id": 1234,
                    "registered_at": "2026-01-01T00:00:00",
                    "last_heartbeat": "2026-01-01T00:00:00",
                    "metadata": {"version": "1.0"},
                }
            },
            "agents": {
                "agent-001": {
                    "agent_id": "agent-001",
                    "name": "Agent",
                    "role": "Builder",
                    "capabilities": ["python"],
                    "floor": "floor-001",
                    "registered_at": "2026-01-01T00:00:00",
                }
            },
        }

        registry.import_registry(data)

        assert len(registry.floors) == 1
        assert len(registry.agents) == 1

        # Verify floor
        floor = registry.floors["floor-001"]
        assert floor.floor_id == "floor-001"
        assert floor.language == "python"
        assert floor.status == FloorStatus.READY
        assert ServiceType.CODE_ANALYSIS in floor.services
        assert ServiceType.CODE_TESTING in floor.services
        assert floor.endpoint == "http://localhost:8080"
        assert floor.process_id == 1234

        # Verify agent
        agent = registry.agents["agent-001"]
        assert agent.agent_id == "agent-001"
        assert agent.name == "Agent"
        assert agent.floor == "floor-001"

        # Verify service index was rebuilt
        assert "floor-001" in registry.service_index[ServiceType.CODE_ANALYSIS]
        assert "floor-001" in registry.service_index[ServiceType.CODE_TESTING]

    def test_import_registry_clears_existing_data(self, registry):
        """Test that import clears existing data"""
        # Add some data
        registry.register_floor(
            floor_id="floor-old", floor_number=99, language="old", domain="old", services=[ServiceType.BUILD]
        )

        assert len(registry.floors) == 1

        # Import new data
        data = {
            "floors": {
                "floor-new": {
                    "floor_id": "floor-new",
                    "floor_number": 1,
                    "language": "python",
                    "domain": "new",
                    "status": "ready",
                    "services": ["code_analysis"],
                    "agents": [],
                    "registered_at": "2026-01-01T00:00:00",
                    "last_heartbeat": "2026-01-01T00:00:00",
                    "metadata": {},
                }
            },
            "agents": {},
        }

        registry.import_registry(data)

        # Old data should be gone
        assert "floor-old" not in registry.floors
        assert "floor-new" in registry.floors
        assert len(registry.floors) == 1


class TestGlobalRegistryThreadSafety:
    """Test thread safety of GlobalRegistry"""

    def test_concurrent_floor_registration(self):
        """Test concurrent floor registrations"""
        registry = GlobalRegistry()
        results = []
        errors = []

        def register_floor(floor_num):
            try:
                floor = registry.register_floor(
                    floor_id=f"floor-{floor_num}",
                    floor_number=floor_num,
                    language="python",
                    domain="test",
                    services=[ServiceType.CODE_ANALYSIS],
                )
                results.append(floor.floor_id)
            except Exception as e:
                errors.append(e)

        threads = []
        for i in range(10):
            thread = threading.Thread(target=register_floor, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        assert len(results) == 10
        assert len(errors) == 0
        assert len(registry.floors) == 10

    def test_concurrent_agent_registration(self):
        """Test concurrent agent registrations"""
        registry = GlobalRegistry()

        # Register floor first
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        results = []
        errors = []

        def register_agent(agent_num):
            try:
                agent = registry.register_agent(
                    agent_id=f"agent-{agent_num}",
                    name=f"Agent {agent_num}",
                    role="Builder",
                    capabilities=["python"],
                    floor_id="floor-001",
                )
                results.append(agent.agent_id)
            except Exception as e:
                errors.append(e)

        threads = []
        for i in range(10):
            thread = threading.Thread(target=register_agent, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        assert len(results) == 10
        assert len(errors) == 0
        assert len(registry.agents) == 10
        assert len(registry.floors["floor-001"].agents) == 10

    def test_concurrent_read_write(self):
        """Test concurrent reads and writes"""
        registry = GlobalRegistry()

        # Pre-populate some data
        for i in range(5):
            registry.register_floor(
                floor_id=f"floor-{i}",
                floor_number=i,
                language="python",
                domain="test",
                services=[ServiceType.CODE_ANALYSIS],
            )

        read_results = []
        write_results = []
        errors = []

        def read_floors():
            try:
                floors = registry.get_all_floors()
                read_results.append(len(floors))
            except Exception as e:
                errors.append(e)

        def write_floor(floor_num):
            try:
                floor = registry.register_floor(
                    floor_id=f"floor-new-{floor_num}",
                    floor_number=floor_num + 100,
                    language="rust",
                    domain="test",
                    services=[ServiceType.BUILD],
                )
                write_results.append(floor.floor_id)
            except Exception as e:
                errors.append(e)

        threads = []

        # Mix of read and write threads
        for i in range(5):
            read_thread = threading.Thread(target=read_floors)
            write_thread = threading.Thread(target=write_floor, args=(i,))
            threads.extend([read_thread, write_thread])

        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()

        assert len(read_results) == 5
        assert len(write_results) == 5
        assert len(errors) == 0
        assert len(registry.floors) == 10

    def test_concurrent_deregister(self):
        """Test concurrent deregistration"""
        registry = GlobalRegistry()

        # Pre-populate data
        for i in range(10):
            registry.register_floor(
                floor_id=f"floor-{i}",
                floor_number=i,
                language="python",
                domain="test",
                services=[ServiceType.CODE_ANALYSIS],
            )

        results = []

        def deregister_floor(floor_num):
            result = registry.deregister_floor(f"floor-{floor_num}")
            results.append(result)

        threads = []
        for i in range(10):
            thread = threading.Thread(target=deregister_floor, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        # All should succeed
        assert all(results)
        assert len(registry.floors) == 0


class TestGlobalRegistrySingleton:
    """Test global registry singleton function"""

    def test_get_global_registry_creates_instance(self):
        """Test that get_global_registry creates instance"""
        registry = get_global_registry()

        assert registry is not None
        assert isinstance(registry, GlobalRegistry)
        assert registry._initialized is True

    def test_get_global_registry_returns_same_instance(self):
        """Test that get_global_registry returns same instance"""
        registry1 = get_global_registry()
        registry2 = get_global_registry()

        assert registry1 is registry2

    def test_global_registry_persists_data(self):
        """Test that singleton persists data across calls"""
        registry1 = get_global_registry()

        # Clear any existing data first
        registry1.floors.clear()
        registry1.agents.clear()
        for service in registry1.service_index:
            registry1.service_index[service].clear()

        registry1.register_floor(
            floor_id="floor-persistent",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_TESTING],
        )

        registry2 = get_global_registry()

        # Should have the same data
        assert "floor-persistent" in registry2.floors
        floor = registry2.get_floor("floor-persistent")
        assert floor is not None
        assert floor.floor_id == "floor-persistent"


class TestEdgeCases:
    """Test edge cases and boundary conditions"""

    @pytest.fixture
    def registry(self):
        """Create a fresh registry for each test"""
        return GlobalRegistry()

    def test_empty_capabilities_list(self, registry):
        """Test agent with empty capabilities"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        agent = registry.register_agent(
            agent_id="agent-001", name="Simple Agent", role="Observer", capabilities=[], floor_id="floor-001"
        )

        assert agent.capabilities == []

    def test_empty_services_list(self, registry):
        """Test floor with no services"""
        floor = registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[]
        )

        assert floor.services == []

    def test_multiple_status_updates(self, registry):
        """Test multiple status updates in sequence"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        statuses = [
            FloorStatus.INITIALIZING,
            FloorStatus.READY,
            FloorStatus.BUSY,
            FloorStatus.READY,
            FloorStatus.ERROR,
            FloorStatus.STOPPED,
        ]

        for status in statuses:
            registry.update_floor_status("floor-001", status)
            floor = registry.get_floor("floor-001")
            assert floor.status == status

    def test_get_agents_by_floor_with_removed_agents(self, registry):
        """Test getting agents when some have been removed from registry"""
        registry.register_floor(
            floor_id="floor-001", floor_number=1, language="python", domain="test", services=[ServiceType.CODE_TESTING]
        )

        registry.register_agent(
            agent_id="agent-001", name="Agent 1", role="Builder", capabilities=["python"], floor_id="floor-001"
        )

        registry.register_agent(
            agent_id="agent-002", name="Agent 2", role="Tester", capabilities=["testing"], floor_id="floor-001"
        )

        # Manually remove one agent from registry (simulating inconsistent state)
        del registry.agents["agent-001"]

        # Should only return existing agents
        agents = registry.get_agents_by_floor("floor-001")
        assert len(agents) == 1
        assert agents[0].agent_id == "agent-002"

    def test_floor_with_all_service_types(self, registry):
        """Test floor with all possible service types"""
        all_services = [service for service in ServiceType]

        floor = registry.register_floor(
            floor_id="floor-all-services", floor_number=1, language="python", domain="full-stack", services=all_services
        )

        assert len(floor.services) == len(ServiceType)

        # Verify all are in service index
        for service in ServiceType:
            assert "floor-all-services" in registry.service_index[service]

    def test_metadata_preservation(self, registry):
        """Test that metadata is preserved through operations"""
        custom_metadata = {
            "version": "1.2.3",
            "environment": "production",
            "region": "us-west-2",
            "nested": {"key": "value"},
        }

        floor = registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="backend",
            services=[ServiceType.CODE_ANALYSIS],
            metadata=custom_metadata,
        )

        assert floor.metadata == custom_metadata

        # Export and import
        exported = registry.export_registry()

        new_registry = GlobalRegistry()
        new_registry.import_registry(exported)

        imported_floor = new_registry.get_floor("floor-001")
        assert imported_floor.metadata == custom_metadata
        assert imported_floor.metadata["nested"]["key"] == "value"


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
