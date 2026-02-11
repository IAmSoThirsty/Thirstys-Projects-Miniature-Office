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


class TestResourceAllocationEdgeCases:
    """Test all resource types in ResourceAllocation"""
    
    def test_has_capacity_all_types(self):
        """Test has_capacity for all resource types"""
        alloc = ResourceAllocation(
            agent_time=100,
            manager_attention=10,
            consensus_bandwidth=20,
            tool_slots=5,
            simulation_budget=50
        )
        
        assert alloc.has_capacity(ResourceType.AGENT_TIME, 50) is True
        assert alloc.has_capacity(ResourceType.MANAGER_ATTENTION, 5) is True
        assert alloc.has_capacity(ResourceType.CONSENSUS_BANDWIDTH, 10) is True
        assert alloc.has_capacity(ResourceType.TOOL_SLOTS, 3) is True
        assert alloc.has_capacity(ResourceType.SIMULATION_BUDGET, 30) is True
        
        # Over capacity
        assert alloc.has_capacity(ResourceType.MANAGER_ATTENTION, 20) is False
        assert alloc.has_capacity(ResourceType.SIMULATION_BUDGET, 60) is False
    
    def test_consume_all_types(self):
        """Test consume for all resource types"""
        alloc = ResourceAllocation(
            agent_time=100,
            manager_attention=10,
            consensus_bandwidth=20,
            tool_slots=5,
            simulation_budget=50
        )
        
        # Consume different types
        assert alloc.consume(ResourceType.MANAGER_ATTENTION, 3) is True
        assert alloc.manager_attention == 7
        
        assert alloc.consume(ResourceType.CONSENSUS_BANDWIDTH, 5) is True
        assert alloc.consensus_bandwidth == 15
        
        assert alloc.consume(ResourceType.TOOL_SLOTS, 2) is True
        assert alloc.tool_slots == 3
        
        assert alloc.consume(ResourceType.SIMULATION_BUDGET, 20) is True
        assert alloc.simulation_budget == 30
        
        # Try to consume more than available
        assert alloc.consume(ResourceType.MANAGER_ATTENTION, 10) is False


class TestResourceLedgerEntryEdgeCases:
    """Test ResourceLedgerEntry edge cases"""
    
    def test_is_exhausted(self):
        """Test checking if resources are exhausted"""
        # Exhausted: agent_time is 0
        allocated = ResourceAllocation(agent_time=10, manager_attention=5, consensus_bandwidth=10, tool_slots=5, simulation_budget=10)
        spent = ResourceAllocation(agent_time=10, manager_attention=0, consensus_bandwidth=0, tool_slots=0, simulation_budget=0)
        
        entry = ResourceLedgerEntry(
            entity_id="agent-1",
            tick=100,
            allocated=allocated,
            spent=spent
        )
        
        # Agent time is exhausted (remaining = 0)
        assert entry.is_exhausted() is True
        
        # Not exhausted: all resources have capacity
        entry2 = ResourceLedgerEntry(
            entity_id="agent-2",
            tick=100,
            allocated=ResourceAllocation(agent_time=100, manager_attention=10, consensus_bandwidth=10, tool_slots=5, simulation_budget=10),
            spent=ResourceAllocation(agent_time=10, manager_attention=1, consensus_bandwidth=1, tool_slots=1, simulation_budget=1)
        )
        assert entry2.is_exhausted() is False


class TestResourceLedgerFullCoverage:
    """Test ResourceLedger methods for full coverage"""
    
    def test_spend_resource_all_types(self):
        """Test spending all resource types"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(
            agent_time=100,
            manager_attention=10,
            consensus_bandwidth=20,
            tool_slots=5,
            simulation_budget=50
        )
        ledger.allocate_resources("agent-1", tick=100, allocation=allocation)
        
        # Spend different resource types
        assert ledger.spend_resource("agent-1", 100, ResourceType.MANAGER_ATTENTION, 3) is True
        assert ledger.spend_resource("agent-1", 100, ResourceType.CONSENSUS_BANDWIDTH, 5) is True
        assert ledger.spend_resource("agent-1", 100, ResourceType.TOOL_SLOTS, 2) is True
        assert ledger.spend_resource("agent-1", 100, ResourceType.SIMULATION_BUDGET, 20) is True
        
        entry = ledger.ledger["agent-1"][100]
        assert entry.spent.manager_attention == 3
        assert entry.spent.consensus_bandwidth == 5
        assert entry.spent.tool_slots == 2
        assert entry.spent.simulation_budget == 20
    
    def test_spend_resource_missing_entry(self):
        """Test spending resource with missing entry"""
        ledger = ResourceLedger()
        
        # Entity/tick doesn't exist
        result = ledger.spend_resource("nonexistent", 100, ResourceType.AGENT_TIME, 10)
        assert result is False
    
    def test_get_ledger_entry(self):
        """Test getting ledger entry"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=100)
        ledger.allocate_resources("agent-1", tick=100, allocation=allocation)
        
        # Entry exists
        entry = ledger.get_ledger_entry("agent-1", 100)
        assert entry is not None
        assert entry.entity_id == "agent-1"
        
        # Entry doesn't exist
        entry2 = ledger.get_ledger_entry("nonexistent", 100)
        assert entry2 is None
        
        entry3 = ledger.get_ledger_entry("agent-1", 999)
        assert entry3 is None
    
    def test_submit_priority_bid(self):
        """Test submitting priority bid"""
        ledger = ResourceLedger()
        
        bid = PriorityBid(
            bid_id="bid-1",
            task_id="task-1",
            resource_type=ResourceType.AGENT_TIME,
            amount_requested=50,
            priority_offered=10,
            justification="High priority task",
            submitted_at_tick=100
        )
        
        bid_id = ledger.submit_priority_bid(bid)
        assert bid_id == "bid-1"
        assert "bid-1" in ledger.priority_market
    
    def test_resolve_priority_bids(self):
        """Test resolving priority bids"""
        ledger = ResourceLedger()
        
        # Allocate resources
        allocation = ResourceAllocation(agent_time=100)
        ledger.allocate_resources("task-1", tick=100, allocation=allocation)
        
        # Submit bids
        bid1 = PriorityBid(
            bid_id="bid-1",
            task_id="task-1",
            resource_type=ResourceType.AGENT_TIME,
            amount_requested=50,
            priority_offered=10,
            justification="High priority",
            submitted_at_tick=100
        )
        
        bid2 = PriorityBid(
            bid_id="bid-2",
            task_id="task-1",
            resource_type=ResourceType.AGENT_TIME,
            amount_requested=200,  # More than available
            priority_offered=5,
            justification="Lower priority",
            submitted_at_tick=100
        )
        
        ledger.submit_priority_bid(bid1)
        ledger.submit_priority_bid(bid2)
        
        # Resolve bids
        results = ledger.resolve_priority_bids(100)
        
        assert results["bid-1"] == "accepted"
        assert results["bid-2"] == "rejected"
    
    def test_get_entity_resources(self):
        """Test getting entity resources"""
        ledger = ResourceLedger()
        
        allocation = ResourceAllocation(agent_time=100, tool_slots=5)
        ledger.allocate_resources("agent-1", tick=100, allocation=allocation)
        
        # Get resources
        resources = ledger.get_entity_resources("agent-1", 100)
        assert resources is not None
        assert resources.agent_time == 100
        
        # Nonexistent entity
        resources2 = ledger.get_entity_resources("nonexistent", 100)
        assert resources2 is None


class TestTaskCostProfileEdgeCases:
    """Test all TaskCostProfile scenarios"""
    
    def test_calculate_cost_blocked(self):
        """Test blocked task cost"""
        cost = TaskCostProfile.calculate_cost(is_blocked=True)
        assert cost.manager_attention > 1  # Doubled
    
    def test_calculate_cost_meta_office(self):
        """Test meta-office decision cost"""
        cost = TaskCostProfile.calculate_cost(is_meta_office=True)
        assert cost.agent_time > 10  # Tripled
        assert cost.manager_attention > 1  # Tripled
    
    def test_calculate_cost_combined(self):
        """Test combined flags"""
        cost = TaskCostProfile.calculate_cost(is_high_risk=True, is_rework=True)
        # 10 * 1.5 * 2.0 = 30
        assert cost.agent_time == 30



class TestResourceAllocationUnknownType:
    """Test unknown resource type fallback"""
    
    def test_has_capacity_unknown_type(self):
        """Test has_capacity with mock unknown resource type"""
        from enum import Enum
        
        class FakeResourceType(Enum):
            FAKE = "fake_resource"
        
        alloc = ResourceAllocation(agent_time=100)
        # Should return False for unknown type
        result = alloc.has_capacity(FakeResourceType.FAKE, 10)
        assert result is False
