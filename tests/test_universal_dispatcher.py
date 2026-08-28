"""
Comprehensive tests for src/core/universal_dispatcher.py to achieve 100% coverage

Tests cover:
- Enum classes (RoutingStrategy, DispatchStatus)
- Dataclasses (DispatchRequest, DispatchResponse)
- UniversalDispatcher class and all methods
- All routing strategies
- Error handling and edge cases
- Timeout scenarios
- Request history management
- Statistics and reporting
- Singleton pattern
"""

from datetime import datetime

import pytest

from src.core.global_registry import (
    FloorRegistration,
    FloorStatus,
    GlobalRegistry,
    ServiceType,
)
from src.core.universal_dispatcher import (
    DispatchRequest,
    DispatchResponse,
    DispatchStatus,
    RoutingStrategy,
    UniversalDispatcher,
    _global_dispatcher,
    get_universal_dispatcher,
)


class TestRoutingStrategy:
    """Test RoutingStrategy enum"""

    def test_routing_strategy_values(self):
        """Test all RoutingStrategy enum values"""
        assert RoutingStrategy.ROUND_ROBIN.value == "round_robin"
        assert RoutingStrategy.LEAST_LOADED.value == "least_loaded"
        assert RoutingStrategy.RANDOM.value == "random"
        assert RoutingStrategy.FIRST_AVAILABLE.value == "first_available"
        assert RoutingStrategy.LANGUAGE_SPECIFIC.value == "language_specific"

    def test_routing_strategy_count(self):
        """Test that all expected routing strategies are defined"""
        expected_strategies = [
            "round_robin",
            "least_loaded",
            "random",
            "first_available",
            "language_specific",
        ]
        actual_strategies = [strategy.value for strategy in RoutingStrategy]
        assert len(actual_strategies) == len(expected_strategies)
        for strategy in expected_strategies:
            assert strategy in actual_strategies


class TestDispatchStatus:
    """Test DispatchStatus enum"""

    def test_dispatch_status_values(self):
        """Test all DispatchStatus enum values"""
        assert DispatchStatus.PENDING.value == "pending"
        assert DispatchStatus.ROUTING.value == "routing"
        assert DispatchStatus.EXECUTING.value == "executing"
        assert DispatchStatus.COMPLETED.value == "completed"
        assert DispatchStatus.FAILED.value == "failed"
        assert DispatchStatus.TIMEOUT.value == "timeout"

    def test_dispatch_status_count(self):
        """Test that all expected statuses are defined"""
        expected_statuses = [
            "pending",
            "routing",
            "executing",
            "completed",
            "failed",
            "timeout",
        ]
        actual_statuses = [status.value for status in DispatchStatus]
        assert len(actual_statuses) == len(expected_statuses)
        for status in expected_statuses:
            assert status in actual_statuses


class TestDispatchRequest:
    """Test DispatchRequest dataclass"""

    def test_request_creation_minimal(self):
        """Test basic dispatch request creation with minimal parameters"""
        request = DispatchRequest(
            request_id="req-001",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={"file": "test.py"},
        )

        assert request.request_id == "req-001"
        assert request.service_type == ServiceType.CODE_ANALYSIS
        assert request.method == "analyze"
        assert request.params == {"file": "test.py"}
        assert request.routing_strategy == RoutingStrategy.FIRST_AVAILABLE
        assert request.preferred_language is None
        assert request.timeout == 30.0
        assert isinstance(request.metadata, dict)
        assert len(request.metadata) == 0
        assert isinstance(request.created_at, str)

    def test_request_creation_full(self):
        """Test dispatch request creation with all parameters"""
        metadata = {"priority": "high", "user": "test"}
        request = DispatchRequest(
            request_id="req-002",
            service_type=ServiceType.CODE_GENERATION,
            method="generate",
            params={"spec": "feature.md"},
            routing_strategy=RoutingStrategy.ROUND_ROBIN,
            preferred_language="python",
            timeout=60.0,
            metadata=metadata,
        )

        assert request.request_id == "req-002"
        assert request.service_type == ServiceType.CODE_GENERATION
        assert request.method == "generate"
        assert request.routing_strategy == RoutingStrategy.ROUND_ROBIN
        assert request.preferred_language == "python"
        assert request.timeout == 60.0
        assert request.metadata == metadata

    def test_request_timestamp_format(self):
        """Test that created_at timestamp is valid ISO format"""
        request = DispatchRequest(
            request_id="req-003",
            service_type=ServiceType.BUILD,
            method="build",
            params={},
        )

        # Should be able to parse the timestamp
        datetime.fromisoformat(request.created_at)

    def test_request_empty_params(self):
        """Test request with empty params"""
        request = DispatchRequest(
            request_id="req-004",
            service_type=ServiceType.CODE_TESTING,
            method="test",
            params={},
        )

        assert request.params == {}


class TestDispatchResponse:
    """Test DispatchResponse dataclass"""

    def test_response_creation_minimal(self):
        """Test basic dispatch response creation"""
        response = DispatchResponse(
            request_id="req-001", status=DispatchStatus.COMPLETED
        )

        assert response.request_id == "req-001"
        assert response.status == DispatchStatus.COMPLETED
        assert response.floor_id is None
        assert response.result is None
        assert response.error is None
        assert response.execution_time == 0.0
        assert isinstance(response.completed_at, str)

    def test_response_creation_full(self):
        """Test dispatch response with all fields"""
        result_data = {"output": "success", "lines": 100}
        response = DispatchResponse(
            request_id="req-002",
            status=DispatchStatus.COMPLETED,
            floor_id="floor-001",
            result=result_data,
            error=None,
            execution_time=1.234,
        )

        assert response.request_id == "req-002"
        assert response.status == DispatchStatus.COMPLETED
        assert response.floor_id == "floor-001"
        assert response.result == result_data
        assert response.error is None
        assert response.execution_time == 1.234

    def test_response_with_error(self):
        """Test dispatch response with error"""
        response = DispatchResponse(
            request_id="req-003",
            status=DispatchStatus.FAILED,
            error="Connection timeout",
            execution_time=30.0,
        )

        assert response.status == DispatchStatus.FAILED
        assert response.error == "Connection timeout"
        assert response.result is None

    def test_response_to_dict(self):
        """Test response serialization to dictionary"""
        result_data = {"status": "ok"}
        response = DispatchResponse(
            request_id="req-004",
            status=DispatchStatus.COMPLETED,
            floor_id="floor-002",
            result=result_data,
            execution_time=2.5,
        )

        result_dict = response.to_dict()

        assert result_dict["request_id"] == "req-004"
        assert result_dict["status"] == "completed"  # Converted to string
        assert result_dict["floor_id"] == "floor-002"
        assert result_dict["result"] == result_data
        assert result_dict["error"] is None
        assert result_dict["execution_time"] == 2.5
        assert "completed_at" in result_dict

    def test_response_timestamp_format(self):
        """Test that completed_at timestamp is valid ISO format"""
        response = DispatchResponse(
            request_id="req-005", status=DispatchStatus.COMPLETED
        )

        # Should be able to parse the timestamp
        datetime.fromisoformat(response.completed_at)


class TestUniversalDispatcher:
    """Test UniversalDispatcher class"""

    @pytest.fixture
    def registry(self):
        """Create a fresh registry for each test"""
        return GlobalRegistry()

    @pytest.fixture
    def dispatcher(self, registry):
        """Create a dispatcher with fresh registry"""
        return UniversalDispatcher(registry=registry)

    def test_dispatcher_initialization(self, dispatcher):
        """Test dispatcher initialization"""
        assert dispatcher.registry is not None
        assert isinstance(dispatcher._floor_handlers, dict)
        assert len(dispatcher._floor_handlers) == 0
        assert isinstance(dispatcher._round_robin_counters, dict)
        assert len(dispatcher._round_robin_counters) == 0
        assert isinstance(dispatcher._request_history, list)
        assert len(dispatcher._request_history) == 0
        assert dispatcher._max_history == 1000

    def test_dispatcher_with_default_registry(self):
        """Test dispatcher creation with default registry"""
        dispatcher = UniversalDispatcher()
        assert dispatcher.registry is not None

    def test_register_floor_handler(self, dispatcher):
        """Test registering a floor handler"""

        def mock_handler(method, params):
            return {"status": "success"}

        dispatcher.register_floor_handler("floor-001", mock_handler)

        assert "floor-001" in dispatcher._floor_handlers
        assert dispatcher._floor_handlers["floor-001"] == mock_handler

    def test_register_multiple_floor_handlers(self, dispatcher):
        """Test registering multiple floor handlers"""

        def handler1(method, params):
            return {"handler": 1}

        def handler2(method, params):
            return {"handler": 2}

        dispatcher.register_floor_handler("floor-001", handler1)
        dispatcher.register_floor_handler("floor-002", handler2)

        assert len(dispatcher._floor_handlers) == 2
        assert dispatcher._floor_handlers["floor-001"] == handler1
        assert dispatcher._floor_handlers["floor-002"] == handler2

    def test_dispatch_no_available_floors(self, dispatcher):
        """Test dispatch when no floors are available"""
        request = DispatchRequest(
            request_id="req-001",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.FAILED
        assert response.request_id == "req-001"
        assert "No available floors" in response.error
        assert response.floor_id is None
        assert response.result is None
        assert response.execution_time > 0

    def test_dispatch_select_floor_returns_none(self, dispatcher, registry):
        """Test dispatch when _select_floor returns None (edge case)"""
        # Register a floor
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        # Mock _select_floor to return None
        original_select = dispatcher._select_floor
        dispatcher._select_floor = lambda candidates, strategy: None

        request = DispatchRequest(
            request_id="req-001b",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.FAILED
        assert response.request_id == "req-001b"
        assert "Failed to select a floor" in response.error
        assert response.floor_id is None
        assert response.result is None

        # Restore original method
        dispatcher._select_floor = original_select

    def test_dispatch_success(self, dispatcher, registry):
        """Test successful dispatch"""
        # Register a floor
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        # Register handler
        def handler(method, params):
            return {"status": "analyzed", "issues": []}

        dispatcher.register_floor_handler("floor-001", handler)

        # Dispatch request
        request = DispatchRequest(
            request_id="req-002",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={"file": "test.py"},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.COMPLETED
        assert response.request_id == "req-002"
        assert response.floor_id == "floor-001"
        assert response.result == {"status": "analyzed", "issues": []}
        assert response.error is None
        assert response.execution_time > 0

    def test_dispatch_with_preferred_language(self, dispatcher, registry):
        """Test dispatch with language preference"""
        # Register Python floor
        registry.register_floor(
            floor_id="floor-python",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-python", FloorStatus.READY)

        # Register Rust floor
        registry.register_floor(
            floor_id="floor-rust",
            floor_number=2,
            language="rust",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-rust", FloorStatus.READY)

        # Register handlers
        dispatcher.register_floor_handler(
            "floor-python", lambda m, p: {"lang": "python"}
        )
        dispatcher.register_floor_handler("floor-rust", lambda m, p: {"lang": "rust"})

        # Request with Python preference
        request = DispatchRequest(
            request_id="req-003",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
            preferred_language="python",
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.COMPLETED
        assert response.floor_id == "floor-python"
        assert response.result == {"lang": "python"}

    def test_dispatch_language_not_available(self, dispatcher, registry):
        """Test dispatch when preferred language is not available"""
        # Register only Python floor
        registry.register_floor(
            floor_id="floor-python",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-python", FloorStatus.READY)

        # Request Rust (not available)
        request = DispatchRequest(
            request_id="req-004",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
            preferred_language="rust",
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.FAILED
        assert "No available floors" in response.error

    def test_dispatch_no_handler_registered(self, dispatcher, registry):
        """Test dispatch when floor has no handler registered"""
        # Register floor but no handler
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        request = DispatchRequest(
            request_id="req-005",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.FAILED
        assert "No handler registered" in response.error

    def test_dispatch_handler_raises_exception(self, dispatcher, registry):
        """Test dispatch when handler raises exception"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        # Handler that raises exception
        def failing_handler(method, params):
            raise ValueError("Handler error")

        dispatcher.register_floor_handler("floor-001", failing_handler)

        request = DispatchRequest(
            request_id="req-006",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.FAILED
        assert "Handler error" in response.error
        assert response.floor_id is None

    def test_dispatch_timeout_error(self, dispatcher, registry):
        """Test dispatch with timeout error"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        # Handler that raises TimeoutError
        def timeout_handler(method, params):
            raise TimeoutError("Operation timed out")

        dispatcher.register_floor_handler("floor-001", timeout_handler)

        request = DispatchRequest(
            request_id="req-007",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        response = dispatcher.dispatch(request)

        assert response.status == DispatchStatus.TIMEOUT
        assert "timed out" in response.error.lower()

    def test_find_candidate_floors(self, dispatcher, registry):
        """Test finding candidate floors for a service"""
        # Register multiple floors
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        registry.register_floor(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-002", FloorStatus.READY)

        request = DispatchRequest(
            request_id="req-008",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
        )

        candidates = dispatcher._find_candidate_floors(request)

        assert len(candidates) == 2
        floor_ids = [f.floor_id for f in candidates]
        assert "floor-001" in floor_ids
        assert "floor-002" in floor_ids

    def test_find_candidate_floors_with_language_filter(self, dispatcher, registry):
        """Test finding candidate floors with language filter"""
        registry.register_floor(
            floor_id="floor-python",
            floor_number=1,
            language="python",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-python", FloorStatus.READY)

        registry.register_floor(
            floor_id="floor-rust",
            floor_number=2,
            language="rust",
            domain="analysis",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-rust", FloorStatus.READY)

        request = DispatchRequest(
            request_id="req-009",
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={},
            preferred_language="python",
        )

        candidates = dispatcher._find_candidate_floors(request)

        assert len(candidates) == 1
        assert candidates[0].floor_id == "floor-python"

    def test_select_floor_first_available(self, dispatcher):
        """Test floor selection with FIRST_AVAILABLE strategy"""
        floor1 = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )
        floor2 = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )

        candidates = [floor1, floor2]
        selected = dispatcher._select_floor(candidates, RoutingStrategy.FIRST_AVAILABLE)

        assert selected == floor1

    def test_select_floor_round_robin(self, dispatcher):
        """Test floor selection with ROUND_ROBIN strategy"""
        floor1 = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )
        floor2 = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )

        candidates = [floor1, floor2]

        # First call should select floor1
        selected1 = dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)
        assert selected1 == floor1

        # Second call should select floor2
        selected2 = dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)
        assert selected2 == floor2

        # Third call should wrap around to floor1
        selected3 = dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)
        assert selected3 == floor1

    def test_select_floor_round_robin_no_services(self, dispatcher):
        """Test round robin with floor that has no services"""
        floor = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[],
        )

        candidates = [floor]
        selected = dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)

        # Should fall back to first candidate
        assert selected == floor

    def test_select_floor_random(self, dispatcher):
        """Test floor selection with RANDOM strategy"""
        floor1 = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )
        floor2 = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )

        candidates = [floor1, floor2]
        selected = dispatcher._select_floor(candidates, RoutingStrategy.RANDOM)

        # Should be one of the candidates
        assert selected in candidates

    def test_select_floor_least_loaded(self, dispatcher):
        """Test floor selection with LEAST_LOADED strategy"""
        floor1 = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
            agents=["agent-1", "agent-2", "agent-3"],  # More loaded
        )
        floor2 = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
            agents=["agent-4"],  # Less loaded
        )

        candidates = [floor1, floor2]
        selected = dispatcher._select_floor(candidates, RoutingStrategy.LEAST_LOADED)

        # Should select floor with fewer agents
        assert selected == floor2

    def test_select_floor_empty_candidates(self, dispatcher):
        """Test floor selection with empty candidates list"""
        candidates = []
        selected = dispatcher._select_floor(candidates, RoutingStrategy.FIRST_AVAILABLE)

        assert selected is None

    def test_execute_on_floor_success(self, dispatcher, registry):
        """Test successful execution on floor"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        def handler(method, params):
            return {"result": "success"}

        dispatcher.register_floor_handler("floor-001", handler)

        result = dispatcher._execute_on_floor(
            "floor-001", "analyze", {"file": "test.py"}, 30.0
        )

        assert result == {"result": "success"}

        # Floor should be back to READY status
        floor = registry.get_floor("floor-001")
        assert floor.status == FloorStatus.READY

    def test_execute_on_floor_no_handler(self, dispatcher, registry):
        """Test execution when no handler is registered"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )

        with pytest.raises(ValueError, match="No handler registered"):
            dispatcher._execute_on_floor("floor-001", "analyze", {}, 30.0)

    def test_execute_on_floor_handler_error(self, dispatcher, registry):
        """Test execution when handler raises error"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        def failing_handler(method, params):
            raise RuntimeError("Execution failed")

        dispatcher.register_floor_handler("floor-001", failing_handler)

        with pytest.raises(RuntimeError, match="Execution failed"):
            dispatcher._execute_on_floor("floor-001", "analyze", {}, 30.0)

        # Floor should be marked as ERROR
        floor = registry.get_floor("floor-001")
        assert floor.status == FloorStatus.ERROR

    def test_add_to_history(self, dispatcher):
        """Test adding responses to request history"""
        response1 = DispatchResponse(
            request_id="req-001", status=DispatchStatus.COMPLETED
        )
        response2 = DispatchResponse(
            request_id="req-002", status=DispatchStatus.COMPLETED
        )

        dispatcher._add_to_history(response1)
        dispatcher._add_to_history(response2)

        assert len(dispatcher._request_history) == 2
        assert dispatcher._request_history[0] == response1
        assert dispatcher._request_history[1] == response2

    def test_add_to_history_max_limit(self, dispatcher):
        """Test that history is trimmed when exceeding max limit"""
        # Set low limit for testing
        dispatcher._max_history = 10

        # Add more than max
        for i in range(15):
            response = DispatchResponse(
                request_id=f"req-{i}", status=DispatchStatus.COMPLETED
            )
            dispatcher._add_to_history(response)

        # Should be trimmed to max
        assert len(dispatcher._request_history) == 10

        # Should keep most recent
        assert dispatcher._request_history[0].request_id == "req-5"
        assert dispatcher._request_history[-1].request_id == "req-14"

    def test_dispatch_sync(self, dispatcher, registry):
        """Test simplified synchronous dispatch"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        dispatcher.register_floor_handler("floor-001", lambda m, p: {"status": "ok"})

        response = dispatcher.dispatch_sync(
            service_type=ServiceType.CODE_ANALYSIS,
            method="analyze",
            params={"file": "test.py"},
        )

        assert response.status == DispatchStatus.COMPLETED
        assert response.result == {"status": "ok"}
        # Should have generated a UUID for request_id
        assert len(response.request_id) > 0

    def test_dispatch_sync_with_all_params(self, dispatcher, registry):
        """Test dispatch_sync with all optional parameters"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.BUILD],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        dispatcher.register_floor_handler("floor-001", lambda m, p: {"built": True})

        response = dispatcher.dispatch_sync(
            service_type=ServiceType.BUILD,
            method="build",
            params={"target": "release"},
            preferred_language="python",
            routing_strategy=RoutingStrategy.LEAST_LOADED,
            timeout=60.0,
        )

        assert response.status == DispatchStatus.COMPLETED
        assert response.result == {"built": True}

    def test_get_statistics_empty(self, dispatcher):
        """Test statistics with empty history"""
        stats = dispatcher.get_statistics()

        assert stats["total_requests"] == 0
        assert stats["successful_requests"] == 0
        assert stats["failed_requests"] == 0
        assert stats["average_execution_time"] == 0.0
        assert stats["requests_by_status"] == {}

    def test_get_statistics_with_data(self, dispatcher):
        """Test statistics with request history"""
        # Add various responses
        dispatcher._add_to_history(
            DispatchResponse(
                request_id="req-001",
                status=DispatchStatus.COMPLETED,
                floor_id="floor-001",
                execution_time=1.0,
            )
        )
        dispatcher._add_to_history(
            DispatchResponse(
                request_id="req-002",
                status=DispatchStatus.COMPLETED,
                floor_id="floor-001",
                execution_time=2.0,
            )
        )
        dispatcher._add_to_history(
            DispatchResponse(
                request_id="req-003",
                status=DispatchStatus.FAILED,
                floor_id="floor-002",
                execution_time=0.5,
            )
        )
        dispatcher._add_to_history(
            DispatchResponse(
                request_id="req-004", status=DispatchStatus.TIMEOUT, execution_time=30.0
            )
        )

        stats = dispatcher.get_statistics()

        assert stats["total_requests"] == 4
        assert stats["successful_requests"] == 2
        assert stats["failed_requests"] == 1
        assert stats["success_rate"] == 0.5  # 2/4
        assert stats["average_execution_time"] == (1.0 + 2.0 + 0.5 + 30.0) / 4

        # Check status counts
        assert stats["requests_by_status"]["completed"] == 2
        assert stats["requests_by_status"]["failed"] == 1
        assert stats["requests_by_status"]["timeout"] == 1

        # Check floor usage
        assert stats["requests_by_floor"]["floor-001"] == 2
        assert stats["requests_by_floor"]["floor-002"] == 1

    def test_get_recent_requests(self, dispatcher):
        """Test getting recent requests"""
        # Add some requests
        for i in range(5):
            dispatcher._add_to_history(
                DispatchResponse(
                    request_id=f"req-{i}",
                    status=DispatchStatus.COMPLETED,
                    execution_time=float(i),
                )
            )

        recent = dispatcher.get_recent_requests(limit=3)

        # Should be in reverse order (most recent first)
        assert len(recent) == 3
        assert recent[0]["request_id"] == "req-4"
        assert recent[1]["request_id"] == "req-3"
        assert recent[2]["request_id"] == "req-2"

        # Each should be a dict
        assert isinstance(recent[0], dict)
        assert "status" in recent[0]
        assert "execution_time" in recent[0]

    def test_get_recent_requests_fewer_than_limit(self, dispatcher):
        """Test getting recent requests when fewer than limit exist"""
        dispatcher._add_to_history(
            DispatchResponse(request_id="req-001", status=DispatchStatus.COMPLETED)
        )

        recent = dispatcher.get_recent_requests(limit=10)

        assert len(recent) == 1
        assert recent[0]["request_id"] == "req-001"

    def test_get_recent_requests_default_limit(self, dispatcher):
        """Test getting recent requests with default limit"""
        # Add more than default limit
        for i in range(15):
            dispatcher._add_to_history(
                DispatchResponse(request_id=f"req-{i}", status=DispatchStatus.COMPLETED)
            )

        recent = dispatcher.get_recent_requests()

        # Default limit is 10
        assert len(recent) == 10
        assert recent[0]["request_id"] == "req-14"
        assert recent[-1]["request_id"] == "req-5"

    def test_request_history_added_on_success(self, dispatcher, registry):
        """Test that successful requests are added to history"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        dispatcher.register_floor_handler("floor-001", lambda m, p: {"ok": True})

        request = DispatchRequest(
            request_id="req-001",
            service_type=ServiceType.CODE_ANALYSIS,
            method="test",
            params={},
        )

        dispatcher.dispatch(request)

        assert len(dispatcher._request_history) == 1
        assert dispatcher._request_history[0].request_id == "req-001"
        assert dispatcher._request_history[0].status == DispatchStatus.COMPLETED

    def test_request_history_added_on_failure(self, dispatcher, registry):
        """Test that failed requests are added to history"""
        # Register a floor and handler that will fail during execution
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        def failing_handler(method, params):
            raise RuntimeError("Handler failed")

        dispatcher.register_floor_handler("floor-001", failing_handler)

        request = DispatchRequest(
            request_id="req-002",
            service_type=ServiceType.CODE_ANALYSIS,
            method="test",
            params={},
        )

        dispatcher.dispatch(request)

        assert len(dispatcher._request_history) == 1
        assert dispatcher._request_history[0].request_id == "req-002"
        assert dispatcher._request_history[0].status == DispatchStatus.FAILED

    def test_request_history_added_on_timeout(self, dispatcher, registry):
        """Test that timeout requests are added to history"""
        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        def timeout_handler(method, params):
            raise TimeoutError("timeout")

        dispatcher.register_floor_handler("floor-001", timeout_handler)

        request = DispatchRequest(
            request_id="req-003",
            service_type=ServiceType.CODE_ANALYSIS,
            method="test",
            params={},
        )

        dispatcher.dispatch(request)

        assert len(dispatcher._request_history) == 1
        assert dispatcher._request_history[0].request_id == "req-003"
        assert dispatcher._request_history[0].status == DispatchStatus.TIMEOUT


class TestGlobalDispatcher:
    """Test global dispatcher singleton"""

    def test_get_universal_dispatcher(self):
        """Test getting global dispatcher singleton"""
        dispatcher1 = get_universal_dispatcher()
        dispatcher2 = get_universal_dispatcher()

        assert dispatcher1 is dispatcher2
        assert isinstance(dispatcher1, UniversalDispatcher)

    def test_global_dispatcher_persists_data(self):
        """Test that global dispatcher persists data across calls"""
        dispatcher1 = get_universal_dispatcher()

        # Register a handler
        def test_handler(method, params):
            return {"test": True}

        dispatcher1.register_floor_handler("test-floor", test_handler)

        # Get dispatcher again
        dispatcher2 = get_universal_dispatcher()

        # Should have same handler
        assert "test-floor" in dispatcher2._floor_handlers
        assert dispatcher2._floor_handlers["test-floor"] == test_handler


class TestEdgeCases:
    """Test edge cases and boundary conditions"""

    @pytest.fixture
    def dispatcher(self):
        """Create a fresh dispatcher"""
        return UniversalDispatcher(registry=GlobalRegistry())

    def test_dispatch_with_empty_metadata(self, dispatcher):
        """Test dispatch request with empty metadata"""
        request = DispatchRequest(
            request_id="req-001",
            service_type=ServiceType.CODE_ANALYSIS,
            method="test",
            params={},
            metadata={},
        )

        response = dispatcher.dispatch(request)

        # Should still work
        assert response.status == DispatchStatus.FAILED  # No floors available

    def test_select_floor_unknown_strategy(self, dispatcher):
        """Test floor selection with unknown strategy falls back"""
        floor = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )

        candidates = [floor]

        # Use LANGUAGE_SPECIFIC which isn't explicitly handled
        selected = dispatcher._select_floor(
            candidates, RoutingStrategy.LANGUAGE_SPECIFIC
        )

        # Should fall back to returning first candidate
        assert selected == floor

    def test_find_candidate_floors_case_insensitive_language(self, dispatcher):
        """Test that language matching is case-insensitive"""
        registry = dispatcher.registry

        registry.register_floor(
            floor_id="floor-001",
            floor_number=1,
            language="Python",  # Capital P
            domain="test",
            services=[ServiceType.CODE_ANALYSIS],
        )
        registry.update_floor_status("floor-001", FloorStatus.READY)

        request = DispatchRequest(
            request_id="req-001",
            service_type=ServiceType.CODE_ANALYSIS,
            method="test",
            params={},
            preferred_language="python",  # lowercase
        )

        candidates = dispatcher._find_candidate_floors(request)

        assert len(candidates) == 1
        assert candidates[0].floor_id == "floor-001"

    def test_response_to_dict_with_none_values(self):
        """Test response serialization with None values"""
        response = DispatchResponse(
            request_id="req-001",
            status=DispatchStatus.FAILED,
            floor_id=None,
            result=None,
            error=None,
        )

        result_dict = response.to_dict()

        assert result_dict["floor_id"] is None
        assert result_dict["result"] is None
        assert result_dict["error"] is None

    def test_statistics_with_zero_division(self, dispatcher):
        """Test statistics calculation doesn't cause zero division"""
        stats = dispatcher.get_statistics()

        # With no requests, should handle gracefully
        assert stats["total_requests"] == 0
        # Note: success_rate is not returned when there are no requests
        assert "success_rate" not in stats

    def test_round_robin_counter_persistence(self, dispatcher):
        """Test that round robin counters persist across calls"""
        floor1 = FloorRegistration(
            floor_id="floor-001",
            floor_number=1,
            language="python",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )
        floor2 = FloorRegistration(
            floor_id="floor-002",
            floor_number=2,
            language="rust",
            domain="test",
            status=FloorStatus.READY,
            services=[ServiceType.CODE_ANALYSIS],
        )

        candidates = [floor1, floor2]

        # First selection
        dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)

        # Counter should be updated
        assert ServiceType.CODE_ANALYSIS in dispatcher._round_robin_counters
        assert dispatcher._round_robin_counters[ServiceType.CODE_ANALYSIS] == 1

        # Second selection
        dispatcher._select_floor(candidates, RoutingStrategy.ROUND_ROBIN)

        # Counter should increment
        assert dispatcher._round_robin_counters[ServiceType.CODE_ANALYSIS] == 2

    def test_multiple_services_same_floor(self, dispatcher):
        """Test floor with multiple services"""
        registry = dispatcher.registry

        registry.register_floor(
            floor_id="floor-multi",
            floor_number=1,
            language="python",
            domain="full-stack",
            services=[
                ServiceType.CODE_ANALYSIS,
                ServiceType.CODE_GENERATION,
                ServiceType.CODE_TESTING,
            ],
        )
        registry.update_floor_status("floor-multi", FloorStatus.READY)

        # Should be available for any of its services
        for service in [
            ServiceType.CODE_ANALYSIS,
            ServiceType.CODE_GENERATION,
            ServiceType.CODE_TESTING,
        ]:
            request = DispatchRequest(
                request_id=f"req-{service.value}",
                service_type=service,
                method="test",
                params={},
            )

            candidates = dispatcher._find_candidate_floors(request)
            assert len(candidates) == 1
            assert candidates[0].floor_id == "floor-multi"


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
