"""
Tests for Scarcity Economics System
Comprehensive coverage for resource management
"""
import pytest
from src.core.scarcity_economics import (
    ResourceType,
    ResourceAllocation,
    ResourceLedgerEntry,
    TaskCostProfile,
    PriorityBid,
    ResourceLedger,
    EconomicLaws,
    get_resource_ledger
)


class TestResourceAllocation:
    """Test ResourceAllocation class"""
    
    def test_allocation_defaults(self):
        """Test default resource allocation"""
        alloc = ResourceAllocation()
        assert alloc.agent_time == 0
        assert alloc.manager_attention == 0
        assert alloc.consensus_bandwidth == 0
        assert alloc.tool_slots == 0
        assert alloc.simulation_budget == 0
    
    def test_allocation_with_values(self):
        """Test resource allocation with values"""
        alloc = ResourceAllocation(
            agent_time=100,
            manager_attention=10,
            consensus_bandwidth=5,
            tool_slots=3,
            simulation_budget=2
        )
        assert alloc.agent_time == 100
        assert alloc.manager_attention == 10
        assert alloc.tool_slots == 3
    
    def test_has_capacity(self):
        """Test capacity checking"""
        alloc = ResourceAllocation(
            agent_time=100,
            manager_attention=10,
            tool_slots=5
        )
        
        assert alloc.has_capacity(ResourceType.AGENT_TIME, 50) is True
        assert alloc.has_capacity(ResourceType.AGENT_TIME, 150) is False
        assert alloc.has_capacity(ResourceType.MANAGER_ATTENTION, 10) is True
        assert alloc.has_capacity(ResourceType.TOOL_SLOTS, 6) is False
    
    def test_consume_resource(self):
        """Test consuming resources"""
        alloc = ResourceAllocation(
            agent_time=100,
            manager_attention=10
        )
        
        # Successful consumption
        result = alloc.consume(ResourceType.AGENT_TIME, 30)
        assert result is True
        assert alloc.agent_time == 70
        
        # Failed consumption (insufficient resources)
        result = alloc.consume(ResourceType.AGENT_TIME, 100)
        assert result is False
        assert alloc.agent_time == 70  # Unchanged
    
    def test_allocation_to_dict(self):
        """Test serialization"""
        alloc = ResourceAllocation(
            agent_time=50,
            tool_slots=3
        )
        result = alloc.to_dict()
        assert result['agent_time'] == 50
        assert result['tool_slots'] == 3


class TestResourceLedgerEntry:
    """Test ResourceLedgerEntry class"""
    
    def test_entry_creation(self):
        """Test creating ledger entry"""
        entry = ResourceLedgerEntry(
            entity_id="agent-001",
            resource_type=ResourceType.AGENT_TIME,
            amount=50,
            cost=10,
            tick=100,
            reason="Task execution"
        )
        
        assert entry.entity_id == "agent-001"
        assert entry.resource_type == ResourceType.AGENT_TIME
        assert entry.amount == 50
        assert entry.cost == 10
        assert entry.tick == 100
    
    def test_entry_to_dict(self):
        """Test entry serialization"""
        entry = ResourceLedgerEntry(
            entity_id="agent-002",
            resource_type=ResourceType.TOOL_SLOTS,
            amount=2,
            cost=5,
            tick=200,
            reason="Tool checkout"
        )
        
        result = entry.to_dict()
        assert result['entity_id'] == "agent-002"
        assert result['resource_type'] == 'tool_slots'
        assert result['amount'] == 2


class TestResourceLedger:
    """Test ResourceLedger class"""
    
    def test_ledger_creation(self):
        """Test creating resource ledger"""
        ledger = ResourceLedger()
        assert len(ledger.entries) == 0
        assert len(ledger.allocations) == 0
    
    def test_allocate_resources(self):
        """Test allocating resources to entity"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources(
            "agent-001",
            agent_time=100,
            tool_slots=5
        )
        
        assert "agent-001" in ledger.allocations
        alloc = ledger.allocations["agent-001"]
        assert alloc.agent_time == 100
        assert alloc.tool_slots == 5
    
    def test_get_allocation(self):
        """Test getting allocation for entity"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources("agent-001", agent_time=50)
        
        alloc = ledger.get_allocation("agent-001")
        assert alloc is not None
        assert alloc.agent_time == 50
        
        # Non-existent entity
        alloc2 = ledger.get_allocation("nonexistent")
        assert alloc2 is not None  # Returns new empty allocation
        assert alloc2.agent_time == 0
    
    def test_record_consumption(self):
        """Test recording resource consumption"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources("agent-001", agent_time=100)
        
        result = ledger.record_consumption(
            entity_id="agent-001",
            resource_type=ResourceType.AGENT_TIME,
            amount=30,
            cost=10,
            tick=100,
            reason="Task work"
        )
        
        assert result is True
        assert len(ledger.entries) == 1
        
        # Check consumption was applied
        alloc = ledger.get_allocation("agent-001")
        assert alloc.agent_time == 70
    
    def test_record_consumption_insufficient(self):
        """Test recording consumption with insufficient resources"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources("agent-001", agent_time=20)
        
        result = ledger.record_consumption(
            entity_id="agent-001",
            resource_type=ResourceType.AGENT_TIME,
            amount=50,
            cost=10,
            tick=100,
            reason="Task work"
        )
        
        assert result is False
        assert len(ledger.entries) == 0
    
    def test_get_entity_consumption(self):
        """Test getting entity's consumption history"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources("agent-001", agent_time=100)
        ledger.record_consumption("agent-001", ResourceType.AGENT_TIME, 20, 5, 100, "Task 1")
        ledger.record_consumption("agent-001", ResourceType.AGENT_TIME, 30, 8, 200, "Task 2")
        
        entries = ledger.get_entity_consumption("agent-001")
        assert len(entries) == 2
        assert entries[0].amount == 20
        assert entries[1].amount == 30
    
    def test_get_total_cost(self):
        """Test calculating total cost for entity"""
        ledger = ResourceLedger()
        
        ledger.allocate_resources("agent-001", agent_time=100)
        ledger.record_consumption("agent-001", ResourceType.AGENT_TIME, 20, 5, 100, "Task 1")
        ledger.record_consumption("agent-001", ResourceType.AGENT_TIME, 30, 10, 200, "Task 2")
        
        total_cost = ledger.get_total_cost("agent-001")
        assert total_cost == 15  # 5 + 10


class TestTaskCostProfile:
    """Test TaskCostProfile class"""
    
    def test_cost_profile_creation(self):
        """Test creating task cost profile"""
        profile = TaskCostProfile(
            task_id="task-001",
            agent_time_cost=50,
            manager_attention_cost=5,
            tool_slots_cost=2
        )
        
        assert profile.task_id == "task-001"
        assert profile.agent_time_cost == 50
        assert profile.manager_attention_cost == 5
    
    def test_cost_profile_to_dict(self):
        """Test cost profile serialization"""
        profile = TaskCostProfile(
            task_id="task-002",
            agent_time_cost=30,
            manager_attention_cost=3,
            tool_slots_cost=1
        )
        
        result = profile.to_dict()
        assert result['task_id'] == "task-002"
        assert result['agent_time_cost'] == 30


class TestPriorityBid:
    """Test PriorityBid class"""
    
    def test_bid_creation(self):
        """Test creating priority bid"""
        bid = PriorityBid(
            bid_id="bid-001",
            task_id="task-001",
            bid_amount=100,
            submitted_by="agent-001",
            submitted_at_tick=50
        )
        
        assert bid.bid_id == "bid-001"
        assert bid.task_id == "task-001"
        assert bid.bid_amount == 100
    
    def test_bid_to_dict(self):
        """Test bid serialization"""
        bid = PriorityBid(
            bid_id="bid-002",
            task_id="task-002",
            bid_amount=75,
            submitted_by="agent-002",
            submitted_at_tick=100
        )
        
        result = bid.to_dict()
        assert result['bid_id'] == "bid-002"
        assert result['bid_amount'] == 75


class TestEconomicLaws:
    """Test EconomicLaws class"""
    
    def test_get_all_laws(self):
        """Test getting all economic laws"""
        laws = EconomicLaws.get_all()
        
        assert len(laws) > 0
        assert all(isinstance(law, dict) for law in laws)
    
    def test_validate_allocation(self):
        """Test validating resource allocation"""
        result = EconomicLaws.validate_allocation({
            'agent_time': 100,
            'manager_attention': 10
        })
        
        assert 'valid' in result
    
    def test_check_scarcity(self):
        """Test checking scarcity compliance"""
        result = EconomicLaws.check_scarcity(
            ResourceType.AGENT_TIME,
            available=100,
            requested=50
        )
        
        assert 'sufficient' in result or 'compliant' in result or result is True or isinstance(result, bool)


class TestGlobalLedger:
    """Test global ledger accessor"""
    
    def test_get_resource_ledger(self):
        """Test getting global resource ledger"""
        ledger1 = get_resource_ledger()
        ledger2 = get_resource_ledger()
        
        # Should return same instance
        assert ledger1 is ledger2
