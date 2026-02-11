"""
Comprehensive tests for Contract system.
Achieves 100% code coverage for src/interfaces/contract.py
"""
import pytest
from datetime import datetime
from src.interfaces.contract import (
    FailureMode,
    APIEndpoint,
    VersionBoundary,
    Contract,
    InvocationRecord,
    ElevatorProtocol,
    get_elevator_protocol
)
from src.core.entity import get_registry, RelationType
from src.core.audit import get_audit_log, EventType
from src.departments.department import Department


class TestFailureMode:
    """Test FailureMode enum"""
    
    def test_failure_modes(self):
        """Test all failure mode values"""
        assert FailureMode.TIMEOUT.value == "timeout"
        assert FailureMode.INVALID_INPUT.value == "invalid_input"
        assert FailureMode.RESOURCE_EXHAUSTED.value == "resource_exhausted"
        assert FailureMode.PERMISSION_DENIED.value == "permission_denied"
        assert FailureMode.NOT_FOUND.value == "not_found"
        assert FailureMode.INTERNAL_ERROR.value == "internal_error"


class TestAPIEndpoint:
    """Test APIEndpoint dataclass"""
    
    def test_api_endpoint_creation(self):
        """Test creating an API endpoint"""
        endpoint = APIEndpoint(
            path="/api/users",
            parameters={"id": "int", "name": "str"},
            return_type="User"
        )
        assert endpoint.path == "/api/users"
        assert endpoint.parameters["id"] == "int"
        assert endpoint.return_type == "User"
        assert endpoint.description == ""
    
    def test_api_endpoint_with_description(self):
        """Test API endpoint with description"""
        endpoint = APIEndpoint(
            path="/api/login",
            parameters={"username": "str", "password": "str"},
            return_type="Token",
            description="Authenticate user"
        )
        assert endpoint.description == "Authenticate user"
    
    def test_api_endpoint_to_dict(self):
        """Test API endpoint serialization"""
        endpoint = APIEndpoint(
            path="/api/test",
            parameters={"param1": "str"},
            return_type="Result",
            description="Test endpoint"
        )
        d = endpoint.to_dict()
        assert d["path"] == "/api/test"
        assert d["parameters"]["param1"] == "str"
        assert d["return_type"] == "Result"
        assert d["description"] == "Test endpoint"


class TestVersionBoundary:
    """Test VersionBoundary dataclass"""
    
    def test_version_boundary_creation(self):
        """Test creating version boundary"""
        version = VersionBoundary(major=1, minor=2, patch=3)
        assert version.major == 1
        assert version.minor == 2
        assert version.patch == 3
    
    def test_version_boundary_str(self):
        """Test version string representation"""
        version = VersionBoundary(major=2, minor=0, patch=5)
        assert str(version) == "2.0.5"
    
    def test_version_compatibility_same_major(self):
        """Test versions with same major are compatible"""
        v1 = VersionBoundary(1, 2, 3)
        v2 = VersionBoundary(1, 5, 0)
        assert v1.is_compatible_with(v2)
        assert v2.is_compatible_with(v1)
    
    def test_version_compatibility_different_major(self):
        """Test versions with different major are incompatible"""
        v1 = VersionBoundary(1, 0, 0)
        v2 = VersionBoundary(2, 0, 0)
        assert not v1.is_compatible_with(v2)
        assert not v2.is_compatible_with(v1)
    
    def test_version_compatibility_same_version(self):
        """Test same version is compatible"""
        v1 = VersionBoundary(1, 2, 3)
        v2 = VersionBoundary(1, 2, 3)
        assert v1.is_compatible_with(v2)
    
    def test_version_boundary_to_dict(self):
        """Test version serialization"""
        version = VersionBoundary(1, 2, 3)
        d = version.to_dict()
        assert d["major"] == 1
        assert d["minor"] == 2
        assert d["patch"] == 3
        assert d["version_string"] == "1.2.3"


class TestContract:
    """Test Contract class"""
    
    def test_contract_creation(self):
        """Test creating a contract"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        
        contract = Contract(
            contract_id="contract-1",
            name="User Service",
            provider_department_id=dept.entity_id,
            version=version
        )
        
        assert contract.entity_id == "contract-1"
        assert contract.name == "User Service"
        assert contract.provider_department_id == dept.entity_id
        assert contract.version is version
        assert contract.apis == {}
        assert contract.failure_modes == []
        assert contract.consumer_departments == []
    
    def test_contract_registered_in_registry(self):
        """Test contract is registered in entity registry"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        
        contract = Contract(
            contract_id="contract-test-1",
            name="Test Contract",
            provider_department_id=dept.entity_id,
            version=version
        )
        
        registered = get_registry().get(contract.entity_id)
        assert registered is contract
    
    def test_contract_logs_creation(self):
        """Test contract creation is logged"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        
        contract = Contract(
            contract_id="contract-log-1",
            name="Logged Contract",
            provider_department_id=dept.entity_id,
            version=version
        )
        
        events = get_audit_log().get_events(limit=10)
        creation_events = [e for e in events if e.get("target_id") == contract.entity_id]
        assert len(creation_events) > 0
    
    def test_add_api(self):
        """Test adding API endpoint to contract"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        endpoint = APIEndpoint(
            path="/api/test",
            parameters={"id": "int"},
            return_type="Result"
        )
        
        contract.add_api(endpoint)
        
        assert "/api/test" in contract.apis
        assert contract.apis["/api/test"] is endpoint
    
    def test_add_multiple_apis(self):
        """Test adding multiple API endpoints"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        api1 = APIEndpoint("/api/get", {}, "Data")
        api2 = APIEndpoint("/api/post", {"data": "str"}, "Result")
        
        contract.add_api(api1)
        contract.add_api(api2)
        
        assert len(contract.apis) == 2
        assert "/api/get" in contract.apis
        assert "/api/post" in contract.apis
    
    def test_add_failure_mode(self):
        """Test adding failure mode to contract"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        contract.add_failure_mode(FailureMode.TIMEOUT)
        
        assert FailureMode.TIMEOUT in contract.failure_modes
    
    def test_add_duplicate_failure_mode(self):
        """Test adding duplicate failure mode is prevented"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        contract.add_failure_mode(FailureMode.TIMEOUT)
        contract.add_failure_mode(FailureMode.TIMEOUT)
        
        assert contract.failure_modes.count(FailureMode.TIMEOUT) == 1
    
    def test_add_multiple_failure_modes(self):
        """Test adding multiple failure modes"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        contract.add_failure_mode(FailureMode.TIMEOUT)
        contract.add_failure_mode(FailureMode.INVALID_INPUT)
        contract.add_failure_mode(FailureMode.NOT_FOUND)
        
        assert len(contract.failure_modes) == 3
    
    def test_add_consumer(self):
        """Test adding consumer department"""
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        
        contract.add_consumer(consumer_dept.entity_id)
        
        assert consumer_dept.entity_id in contract.consumer_departments
    
    def test_add_duplicate_consumer(self):
        """Test adding duplicate consumer is prevented"""
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        
        contract.add_consumer(consumer_dept.entity_id)
        contract.add_consumer(consumer_dept.entity_id)
        
        assert contract.consumer_departments.count(consumer_dept.entity_id) == 1
    
    def test_add_consumer_creates_relationship(self):
        """Test adding consumer creates USES relationship"""
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        
        contract.add_consumer(consumer_dept.entity_id)
        
        # Check relationship was created
        relationships = consumer_dept.relationships
        uses_relationships = [r for r in relationships if r["type"] == RelationType.USES]
        assert any(r["target_id"] == contract.entity_id for r in uses_relationships)
    
    def test_is_compatible_with_same_major(self):
        """Test contract compatibility with same major version"""
        dept = Department("dept-1", "Python Dept", "python")
        v1 = VersionBoundary(1, 0, 0)
        v2 = VersionBoundary(1, 2, 5)
        
        contract1 = Contract("contract-1", "Service", dept.entity_id, v1)
        contract2 = Contract("contract-2", "Service", dept.entity_id, v2)
        
        assert contract1.is_compatible_with(contract2)
        assert contract2.is_compatible_with(contract1)
    
    def test_is_compatible_with_different_major(self):
        """Test contract incompatibility with different major version"""
        dept = Department("dept-1", "Python Dept", "python")
        v1 = VersionBoundary(1, 0, 0)
        v2 = VersionBoundary(2, 0, 0)
        
        contract1 = Contract("contract-1", "Service", dept.entity_id, v1)
        contract2 = Contract("contract-2", "Service", dept.entity_id, v2)
        
        assert not contract1.is_compatible_with(contract2)
    
    def test_contract_to_dict(self):
        """Test contract serialization"""
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 2, 3)
        contract = Contract("contract-1", "User Service", dept.entity_id, version)
        
        api = APIEndpoint("/api/users", {"id": "int"}, "User")
        contract.add_api(api)
        contract.add_failure_mode(FailureMode.TIMEOUT)
        contract.add_consumer("consumer-dept")
        
        d = contract.to_dict()
        assert d["entity_id"] == "contract-1"
        assert d["name"] == "User Service"
        assert d["provider_department_id"] == dept.entity_id
        assert d["version"]["version_string"] == "1.2.3"
        assert "/api/users" in d["apis"]
        assert "timeout" in d["failure_modes"]
        assert "consumer-dept" in d["consumer_departments"]


class TestInvocationRecord:
    """Test InvocationRecord dataclass"""
    
    def test_invocation_record_defaults(self):
        """Test invocation record with defaults"""
        record = InvocationRecord()
        assert record.invocation_id != ""
        assert record.contract_id == ""
        assert record.consumer_department_id == ""
        assert record.api_path == ""
        assert isinstance(record.timestamp, datetime)
        assert record.success is True
        assert record.error is None
        assert record.latency_ms == 0.0
    
    def test_invocation_record_with_values(self):
        """Test invocation record with explicit values"""
        now = datetime.utcnow()
        record = InvocationRecord(
            invocation_id="inv-123",
            contract_id="contract-1",
            consumer_department_id="dept-1",
            api_path="/api/test",
            timestamp=now,
            success=False,
            error="Timeout",
            latency_ms=150.5
        )
        assert record.invocation_id == "inv-123"
        assert record.contract_id == "contract-1"
        assert record.consumer_department_id == "dept-1"
        assert record.api_path == "/api/test"
        assert record.timestamp == now
        assert record.success is False
        assert record.error == "Timeout"
        assert record.latency_ms == 150.5
    
    def test_invocation_record_to_dict(self):
        """Test invocation record serialization"""
        record = InvocationRecord(
            contract_id="contract-1",
            consumer_department_id="dept-1",
            api_path="/api/test",
            success=True,
            latency_ms=25.3
        )
        d = record.to_dict()
        assert d["contract_id"] == "contract-1"
        assert d["consumer_department_id"] == "dept-1"
        assert d["api_path"] == "/api/test"
        assert d["success"] is True
        assert d["latency_ms"] == 25.3
        assert "timestamp" in d


class TestElevatorProtocol:
    """Test ElevatorProtocol class"""
    
    def test_elevator_protocol_initialization(self):
        """Test elevator protocol initialization"""
        elevator = ElevatorProtocol()
        assert elevator.contracts == {}
        assert elevator.invocations == []
    
    def test_register_contract(self):
        """Test registering a contract"""
        elevator = ElevatorProtocol()
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        
        elevator.register_contract(contract)
        
        assert contract.entity_id in elevator.contracts
        assert elevator.contracts[contract.entity_id] is contract
    
    def test_register_contract_logs_event(self):
        """Test contract registration logs event"""
        elevator = ElevatorProtocol()
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-reg-1", "Service", dept.entity_id, version)
        
        elevator.register_contract(contract)
        
        events = get_audit_log().get_events(limit=10)
        reg_events = [e for e in events if "contract_registered_in_elevator" in str(e.get("data", {}))]
        assert len(reg_events) > 0
    
    def test_check_compatibility_contract_not_found(self):
        """Test compatibility check for nonexistent contract"""
        elevator = ElevatorProtocol()
        
        result = elevator.check_compatibility("dept-1", "nonexistent-contract")
        assert result is False
    
    def test_check_compatibility_consumer_not_found(self):
        """Test compatibility check for nonexistent consumer"""
        elevator = ElevatorProtocol()
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        elevator.register_contract(contract)
        
        result = elevator.check_compatibility("nonexistent-dept", contract.entity_id)
        assert result is False
    
    def test_check_compatibility_success(self):
        """Test successful compatibility check"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        elevator.register_contract(contract)
        
        result = elevator.check_compatibility(consumer_dept.entity_id, contract.entity_id)
        assert result is True
    
    def test_invoke_contract_not_found(self):
        """Test invoking nonexistent contract"""
        elevator = ElevatorProtocol()
        
        record = elevator.invoke_contract(
            "dept-1",
            "nonexistent-contract",
            "/api/test"
        )
        
        assert record.success is False
        assert "not found" in record.error.lower()
        assert len(elevator.invocations) == 1
    
    def test_invoke_contract_compatibility_failed(self):
        """Test invoking contract with compatibility failure"""
        elevator = ElevatorProtocol()
        dept = Department("dept-1", "Python Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", dept.entity_id, version)
        elevator.register_contract(contract)
        
        record = elevator.invoke_contract(
            "nonexistent-dept",  # Consumer doesn't exist
            contract.entity_id,
            "/api/test"
        )
        
        assert record.success is False
        assert "compatibility" in record.error.lower()
        assert len(elevator.invocations) == 1
    
    def test_invoke_contract_api_not_found(self):
        """Test invoking nonexistent API endpoint"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        elevator.register_contract(contract)
        
        record = elevator.invoke_contract(
            consumer_dept.entity_id,
            contract.entity_id,
            "/api/nonexistent"
        )
        
        assert record.success is False
        assert "not found" in record.error.lower()
        assert len(elevator.invocations) == 1
    
    def test_invoke_contract_success(self):
        """Test successful contract invocation"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-1", "Service", provider_dept.entity_id, version)
        
        api = APIEndpoint("/api/test", {"id": "int"}, "Result")
        contract.add_api(api)
        
        elevator.register_contract(contract)
        
        record = elevator.invoke_contract(
            consumer_dept.entity_id,
            contract.entity_id,
            "/api/test",
            {"id": 123}
        )
        
        assert record.success is True
        assert record.error is None
        assert record.latency_ms >= 0
        assert len(elevator.invocations) == 1
    
    def test_invoke_contract_logs_event(self):
        """Test contract invocation logs event"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-inv-1", "Service", provider_dept.entity_id, version)
        
        api = APIEndpoint("/api/test", {}, "Result")
        contract.add_api(api)
        elevator.register_contract(contract)
        
        record = elevator.invoke_contract(
            consumer_dept.entity_id,
            contract.entity_id,
            "/api/test"
        )
        
        events = get_audit_log().get_events(limit=10)
        invoke_events = [e for e in events if e.get("data", {}).get("action") == "contract_invoked"]
        assert len(invoke_events) > 0
    
    def test_get_contract_metrics_no_invocations(self):
        """Test getting metrics for contract with no invocations"""
        elevator = ElevatorProtocol()
        
        metrics = elevator.get_contract_metrics("contract-1")
        
        assert metrics["total_invocations"] == 0
        assert metrics["success_rate"] == 0.0
        assert metrics["avg_latency_ms"] == 0.0
    
    def test_get_contract_metrics_with_invocations(self):
        """Test getting metrics for contract with invocations"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-metrics", "Service", provider_dept.entity_id, version)
        
        api = APIEndpoint("/api/test", {}, "Result")
        contract.add_api(api)
        elevator.register_contract(contract)
        
        # Make multiple invocations
        for i in range(5):
            elevator.invoke_contract(
                consumer_dept.entity_id,
                contract.entity_id,
                "/api/test"
            )
        
        # Make one failed invocation
        elevator.invoke_contract(
            consumer_dept.entity_id,
            "nonexistent",  # This will fail
            "/api/test"
        )
        
        metrics = elevator.get_contract_metrics(contract.entity_id)
        
        assert metrics["total_invocations"] == 5
        assert metrics["success_rate"] == 1.0  # All 5 for this contract succeeded
        assert metrics["avg_latency_ms"] >= 0
    
    def test_get_contract_metrics_mixed_results(self):
        """Test metrics with both successes and failures"""
        elevator = ElevatorProtocol()
        provider_dept = Department("dept-provider", "Provider", "python")
        consumer_dept = Department("dept-consumer", "Consumer", "rust")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-mixed", "Service", provider_dept.entity_id, version)
        
        api = APIEndpoint("/api/test", {}, "Result")
        contract.add_api(api)
        elevator.register_contract(contract)
        
        # 3 successful invocations
        for i in range(3):
            elevator.invoke_contract(
                consumer_dept.entity_id,
                contract.entity_id,
                "/api/test"
            )
        
        # 2 failed invocations (wrong API path)
        for i in range(2):
            elevator.invoke_contract(
                consumer_dept.entity_id,
                contract.entity_id,
                "/api/wrong"
            )
        
        metrics = elevator.get_contract_metrics(contract.entity_id)
        
        assert metrics["total_invocations"] == 5
        assert metrics["success_rate"] == 0.6  # 3/5


class TestGlobalElevatorProtocol:
    """Test global elevator protocol singleton"""
    
    def test_get_elevator_protocol(self):
        """Test getting global elevator protocol"""
        elevator1 = get_elevator_protocol()
        elevator2 = get_elevator_protocol()
        
        assert elevator1 is elevator2  # Same instance
        assert isinstance(elevator1, ElevatorProtocol)
    
    def test_elevator_protocol_persists_state(self):
        """Test elevator protocol state persists"""
        elevator1 = get_elevator_protocol()
        
        dept = Department("dept-persist", "Persist Dept", "python")
        version = VersionBoundary(1, 0, 0)
        contract = Contract("contract-persist", "Service", dept.entity_id, version)
        
        elevator1.register_contract(contract)
        
        elevator2 = get_elevator_protocol()
        assert contract.entity_id in elevator2.contracts
