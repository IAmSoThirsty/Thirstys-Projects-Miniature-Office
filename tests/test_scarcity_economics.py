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
        allocated = ResourceAllocation(agent_time=100, tool_slots=5)
        
        entry = ResourceLedgerEntry(
            entity_id="agent-001",
            tick=100,
            allocated=allocated
        )
        
        assert entry.entity_id == "agent-001"
        assert entry.tick == 100
        assert entry.allocated.agent_time == 100
    
    def test_entry_to_dict(self):
        """Test entry serialization"""
        allocated = ResourceAllocation(agent_time=50, tool_slots=2)
        
        entry = ResourceLedgerEntry(
            entity_id="agent-002",
            tick=200,
            allocated=allocated
        )
        
        result = entry.to_dict()
        assert result['entity_id'] == "agent-002"
        assert result['tick'] == 200
        assert 'allocated' in result


class TestResourceLedger:
    """Test ResourceLedger class"""
    
    def test_ledger_creation(self):
        """Test creating resource ledger"""
        ledger = ResourceLedger()
        assert len(ledger.ledger) == 0
        assert len(ledger.priority_market) == 0
    
    def test_allocate_resources(self):
        """Test allocating resources to entity"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=100, tool_slots=5)
        entry = ledger.allocate_resources("agent-001", tick=100, allocation=allocation)
        
        assert "agent-001" in ledger.ledger
        assert entry.allocated.agent_time == 100
        assert entry.allocated.tool_slots == 5
    
    def test_get_entry(self):
        """Test getting ledger entry"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=50)
        ledger.allocate_resources("agent-001", tick=100, allocation=allocation)
        
        # Access entry from ledger
        entry = ledger.ledger["agent-001"][100]
        assert entry is not None
        assert entry.allocated.agent_time == 50
    
    def test_spend_resource(self):
        """Test spending resources"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=100)
        ledger.allocate_resources("agent-001", tick=100, allocation=allocation)
        
        # Spend some resources
        result = ledger.spend_resource("agent-001", tick=100, resource_type=ResourceType.AGENT_TIME, amount=30)
        
        assert result is True
        entry = ledger.ledger["agent-001"][100]
        assert entry.spent.agent_time == 30
    
    def test_spend_resource_insufficient(self):
        """Test spending with insufficient resources"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=20)
        ledger.allocate_resources("agent-001", tick=100, allocation=allocation)
        
        # Try to spend more than allocated
        result = ledger.spend_resource("agent-001", tick=100, resource_type=ResourceType.AGENT_TIME, amount=50)
        
        assert result is False


class TestTaskCostProfile:
    """Test TaskCostProfile class"""
    
    def test_calculate_cost_base(self):
        """Test basic cost calculation"""
        cost = TaskCostProfile.calculate_cost()
        
        assert cost.agent_time == 10  # Base cost
        assert cost.manager_attention == 1
    
    def test_calculate_cost_high_risk(self):
        """Test high risk cost multiplier"""
        cost = TaskCostProfile.calculate_cost(is_high_risk=True)
        
        assert cost.agent_time == 15  # 10 * 1.5
    
    def test_calculate_cost_rework(self):
        """Test rework cost multiplier"""
        cost = TaskCostProfile.calculate_cost(is_rework=True)
        
        assert cost.agent_time == 20  # 10 * 2.0 (double for rework)


class TestPriorityBid:
    """Test PriorityBid class"""
    
    def test_bid_creation(self):
        """Test creating priority bid"""
        bid = PriorityBid(
            bid_id="bid-001",
            task_id="task-001",
            resource_type=ResourceType.AGENT_TIME,
            amount_requested=50,
            priority_offered=10,
            justification="High priority task",
            submitted_at_tick=100
        )
        
        assert bid.bid_id == "bid-001"
        assert bid.task_id == "task-001"
        assert bid.amount_requested == 50
    
    def test_bid_to_dict(self):
        """Test bid serialization"""
        bid = PriorityBid(
            bid_id="bid-002",
            task_id="task-002",
            resource_type=ResourceType.TOOL_SLOTS,
            amount_requested=3,
            priority_offered=5,
            justification="Need tools",
            submitted_at_tick=200
        )
        
        result = bid.to_dict()
        assert result['bid_id'] == "bid-002"
        assert result['amount_requested'] == 3


class TestEconomicLaws:
    """Test EconomicLaws class"""
    
    def test_enforce_no_free_parallelism(self):
        """Test parallel task cost enforcement"""
        cost = EconomicLaws.enforce_no_free_parallelism(5)
        assert cost == 50  # 5 tasks * 10 time units each
    
    def test_calculate_rework_penalty(self):
        """Test rework penalty calculation"""
        penalty = EconomicLaws.calculate_rework_penalty(10)
        assert penalty == 20  # Double the original cost
    
    def test_calculate_blocked_overhead(self):
        """Test blocked task overhead"""
        overhead = EconomicLaws.calculate_blocked_overhead(5)
        assert overhead == 10  # Double attention cost
    
    def test_validate_meta_office_budget(self):
        """Test meta-office budget validation"""
        # Within budget
        assert EconomicLaws.validate_meta_office_budget(50, 100) is True
        
        # Over budget
        assert EconomicLaws.validate_meta_office_budget(150, 100) is False


class TestGlobalLedger:
    """Test global ledger accessor"""
    
    def test_get_resource_ledger(self):
        """Test getting global resource ledger"""
        ledger1 = get_resource_ledger()
        ledger2 = get_resource_ledger()
        
        # Should return same instance
        assert ledger1 is ledger2
