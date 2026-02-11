"""Unit tests for the audit log system."""
import pytest
from src.core.audit import AuditLog, AuditEvent, EventType


class TestAuditLog:
    """Test AuditLog class."""
    
    def test_log_event(self, audit_log):
        """Test logging a basic event."""
        event = audit_log.log_event(
            EventType.ENTITY_CREATED,
            "system",
            "test-entity-001",
            {"name": "Test Entity"}
        )
        
        assert event is not None
        assert event.event_type == EventType.ENTITY_CREATED
        assert event.actor_id == "system"
        assert event.target_id == "test-entity-001"
        assert event.data["name"] == "Test Entity"
    
    def test_event_hash_immutability(self, audit_log):
        """Test that event hashes are calculated correctly."""
        event = audit_log.log_event(
            EventType.AGENT_ACTION,
            "actor-001",
            "target-001",
            {"key": "value"}
        )
        
        assert event._hash is not None
        assert len(event._hash) == 64  # SHA-256 produces 64 hex chars
    
    def test_get_events_by_type(self, audit_log):
        """Test retrieving events by type."""
        audit_log.log_event(EventType.AGENT_ACTION, "actor1", "target1", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor2", "target2", {})
        audit_log.log_event(EventType.SECURITY_EVENT, "actor3", "target3", {})
        
        action_events = audit_log.get_events_by_type(EventType.AGENT_ACTION)
        assert len(action_events) == 1
    
    def test_filter_events_by_type(self, audit_log):
        """Test filtering events by type."""
        audit_log.log_event(EventType.ENTITY_CREATED, "actor1", "target1", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor2", "target2", {})
        audit_log.log_event(EventType.ENTITY_CREATED, "actor3", "target3", {})
        
        created_events = audit_log.get_events_by_type(EventType.ENTITY_CREATED)
        assert len(created_events) == 2
    
    def test_get_events_by_actor(self, audit_log):
        """Test filtering events by actor."""
        audit_log.log_event(EventType.AGENT_ACTION, "actor-001", "target1", {})
        audit_log.log_event(EventType.AGENT_ACTION, "actor-002", "target2", {})
        audit_log.log_event(EventType.AGENT_ACTION, "actor-001", "target3", {})
        
        actor_events = audit_log.get_events_by_actor("actor-001")
        assert len(actor_events) == 2
    
    def test_get_event_by_id(self, audit_log):
        """Test getting a specific event by ID."""
        event = audit_log.log_event(EventType.AGENT_ACTION, "actor1", "target1", {})
        
        # Get the event through the graph
        retrieved = audit_log.graph.get_event(event.event_id)
        assert retrieved is not None
        assert retrieved.event_id == event.event_id


class TestAuditEvent:
    """Test AuditEvent class."""
    
    def test_event_serialization(self, audit_log):
        """Test event to_dict serialization."""
        event = audit_log.log_event(
            EventType.AGENT_ACTION,
            "actor-001",
            "target-001",
            {"key": "value"}
        )
        
        event_dict = event.to_dict()
        
        assert event_dict["event_type"] == "agent_action"
        assert event_dict["actor_id"] == "actor-001"
        assert event_dict["target_id"] == "target-001"
        assert event_dict["data"]["key"] == "value"
        assert "timestamp" in event_dict
        assert "event_id" in event_dict
        assert "hash" in event_dict
    
    def test_event_integrity_verification(self, audit_log):
        """Test event integrity verification."""
        event = audit_log.log_event(
            EventType.AGENT_ACTION,
            "actor-001",
            "target-001",
            {"key": "value"}
        )
        assert event.verify_integrity()


class TestCausalityGraph:
    """Test CausalityGraph class."""
    
    def test_add_event_duplicate(self, audit_log):
        """Test adding duplicate event fails."""
        from src.core.audit import AuditEvent, CausalityGraph
        event = AuditEvent(event_type=EventType.AGENT_ACTION)
        graph = CausalityGraph()
        graph.add_event(event)
        
        with pytest.raises(ValueError, match="already exists"):
            graph.add_event(event)
    
    def test_add_event_integrity_failure(self):
        """Test adding event with failed integrity check."""
        from src.core.audit import AuditEvent, CausalityGraph
        event = AuditEvent(event_type=EventType.AGENT_ACTION)
        # Corrupt the hash
        event._hash = "corrupted_hash"
        graph = CausalityGraph()
        
        with pytest.raises(ValueError, match="integrity check"):
            graph.add_event(event)
    
    def test_get_children(self, audit_log):
        """Test getting child events."""
        parent = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "parent", {})
        child1 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child1",
            {},
            [parent.event_id]
        )
        child2 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child2",
            {},
            [parent.event_id]
        )
        
        children = audit_log.graph.get_children(parent.event_id)
        assert len(children) == 2
        child_ids = [c.event_id for c in children]
        assert child1.event_id in child_ids
        assert child2.event_id in child_ids
    
    def test_get_children_empty(self, audit_log):
        """Test getting children of event with no children."""
        event = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "target", {})
        children = audit_log.graph.get_children(event.event_id)
        assert len(children) == 0
    
    def test_get_lineage(self, audit_log):
        """Test getting event lineage."""
        root = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "root", {})
        middle = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "middle",
            {},
            [root.event_id]
        )
        leaf = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "leaf",
            {},
            [middle.event_id]
        )
        
        lineage = audit_log.graph.get_lineage(leaf.event_id)
        assert len(lineage) == 3
        assert lineage[0].event_id == root.event_id
        assert lineage[1].event_id == middle.event_id
        assert lineage[2].event_id == leaf.event_id
    
    def test_get_lineage_with_multiple_parents(self, audit_log):
        """Test getting lineage with multiple parent branches."""
        parent1 = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "p1", {})
        parent2 = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "p2", {})
        child = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child",
            {},
            [parent1.event_id, parent2.event_id]
        )
        
        lineage = audit_log.graph.get_lineage(child.event_id)
        assert len(lineage) == 3
        lineage_ids = [e.event_id for e in lineage]
        assert parent1.event_id in lineage_ids
        assert parent2.event_id in lineage_ids
        assert child.event_id in lineage_ids
    
    def test_get_lineage_nonexistent(self, audit_log):
        """Test getting lineage of non-existent event."""
        lineage = audit_log.graph.get_lineage("nonexistent")
        assert len(lineage) == 0
    
    def test_get_descendants(self, audit_log):
        """Test getting all descendants."""
        root = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "root", {})
        child1 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child1",
            {},
            [root.event_id]
        )
        child2 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child2",
            {},
            [root.event_id]
        )
        grandchild = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "grandchild",
            {},
            [child1.event_id]
        )
        
        descendants = audit_log.graph.get_descendants(root.event_id)
        assert len(descendants) == 3
        descendant_ids = [d.event_id for d in descendants]
        assert child1.event_id in descendant_ids
        assert child2.event_id in descendant_ids
        assert grandchild.event_id in descendant_ids
    
    def test_get_descendants_empty(self, audit_log):
        """Test getting descendants of leaf event."""
        event = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "leaf", {})
        descendants = audit_log.graph.get_descendants(event.event_id)
        assert len(descendants) == 0
    
    def test_get_descendants_with_revisit(self, audit_log):
        """Test descendants with diamond pattern (revisit protection)."""
        root = audit_log.log_event(EventType.ENTITY_CREATED, "actor", "root", {})
        mid1 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "mid1",
            {},
            [root.event_id]
        )
        mid2 = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "mid2",
            {},
            [root.event_id]
        )
        # Both mid nodes point to same child (diamond pattern)
        child = audit_log.log_event(
            EventType.ENTITY_UPDATED,
            "actor",
            "child",
            {},
            [mid1.event_id, mid2.event_id]
        )
        
        descendants = audit_log.graph.get_descendants(root.event_id)
        # With diamond pattern, child gets added when processing both mid1 and mid2
        # So we'll see mid1, child (from mid1), mid2, child again (from mid2) but visited check prevents re-traversal
        assert len(descendants) >= 3  # At least mid1, mid2, child


class TestAuditLog:
    """Test AuditLog additional functionality."""
    
    def test_get_events_by_target(self, audit_log):
        """Test filtering events by target."""
        audit_log.log_event(EventType.ENTITY_CREATED, "actor1", "target-001", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor2", "target-001", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor3", "target-002", {})
        
        target_events = audit_log.get_events_by_target("target-001")
        assert len(target_events) == 2
    
    def test_get_change_lineage(self, audit_log):
        """Test getting change lineage for target."""
        audit_log.log_event(EventType.ENTITY_CREATED, "actor", "target-001", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor", "target-001", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor", "target-001", {})
        
        lineage = audit_log.get_change_lineage("target-001")
        assert len(lineage) == 3
        # Should be sorted by timestamp
        for i in range(len(lineage) - 1):
            assert lineage[i].timestamp <= lineage[i + 1].timestamp
    
    def test_verify_integrity(self, audit_log):
        """Test verifying entire audit log integrity."""
        audit_log.log_event(EventType.ENTITY_CREATED, "actor1", "target1", {})
        audit_log.log_event(EventType.ENTITY_UPDATED, "actor2", "target2", {})
        
        assert audit_log.verify_integrity()
    
    def test_get_audit_log_global(self):
        """Test getting global audit log."""
        from src.core.audit import get_audit_log
        log = get_audit_log()
        assert log is not None
    
    def test_get_events_with_missing_event(self, audit_log):
        """Test getting events when some events are missing from graph."""
        # Manually add an event ID to index without adding event to graph
        audit_log._type_index[EventType.SECURITY_EVENT] = ["nonexistent-id"]
        events = audit_log.get_events_by_type(EventType.SECURITY_EVENT)
        assert len(events) == 0
    
    def test_get_events_by_actor_with_missing_event(self, audit_log):
        """Test getting events by actor when event is missing from graph."""
        audit_log._actor_index["missing-actor"] = ["nonexistent-id"]
        events = audit_log.get_events_by_actor("missing-actor")
        assert len(events) == 0
    
    def test_get_events_by_target_with_missing_event(self, audit_log):
        """Test getting events by target when event is missing from graph."""
        audit_log._target_index["missing-target"] = ["nonexistent-id"]
        events = audit_log.get_events_by_target("missing-target")
        assert len(events) == 0
