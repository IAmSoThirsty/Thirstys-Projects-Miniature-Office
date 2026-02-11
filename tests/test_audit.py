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
