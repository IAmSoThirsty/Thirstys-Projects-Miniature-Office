"""
Audit log — in-memory event log with a SHA-256 hash chain.

Each event hashes its own fields plus `prev_hash` (the previous event's
hash, or 64 zero hex digits for the first event) and the hashes of any
explicit causality parents. This is a process-local chain, not a
persisted ledger and not a signature.
"""

import hashlib
import json
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Any, Dict, List, Optional, Set

GENESIS_HASH = "0" * 64


class EventType(Enum):
    """Types of events tracked in the audit log"""

    ENTITY_CREATED = "entity_created"
    ENTITY_UPDATED = "entity_updated"
    RELATIONSHIP_DECLARED = "relationship_declared"
    TASK_STATE_CHANGED = "task_state_changed"
    DIRECTIVE_CREATED = "directive_created"
    CONSENSUS_REACHED = "consensus_reached"
    OVERRIDE_ISSUED = "override_issued"
    MEETING_HELD = "meeting_held"
    ARTIFACT_PRODUCED = "artifact_produced"
    TOOL_CHECKED_OUT = "tool_checked_out"
    CODEX_AMENDMENT = "codex_amendment"
    AGENT_ACTION = "agent_action"
    SECURITY_EVENT = "security_event"


@dataclass
class AuditEvent:
    """
    Audit event with a SHA-256 over its fields, prev_hash, and parent hashes.
    Not persisted. Not signed. Restarting the process drops the chain.
    """

    event_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    event_type: EventType = EventType.AGENT_ACTION
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    actor_id: Optional[str] = None  # Entity that caused the event
    target_id: Optional[str] = None  # Entity affected by the event
    data: Dict[str, Any] = field(default_factory=dict)
    parent_events: List[str] = field(default_factory=list)  # Causality links
    parent_hashes: List[str] = field(default_factory=list)
    prev_hash: str = GENESIS_HASH
    _hash: Optional[str] = None

    def __post_init__(self):
        """Calculate hash for immutability verification"""
        if not self._hash:
            self._hash = self._calculate_hash()

    def _calculate_hash(self) -> str:
        """Calculate SHA-256 hash of event content plus chain links."""
        content = {
            "event_id": self.event_id,
            "event_type": self.event_type.value,
            "timestamp": self.timestamp.isoformat(),
            "actor_id": self.actor_id,
            "target_id": self.target_id,
            "data": self.data,
            "parent_events": sorted(self.parent_events),
            "parent_hashes": sorted(self.parent_hashes),
            "prev_hash": self.prev_hash,
        }
        content_str = json.dumps(content, sort_keys=True)
        return hashlib.sha256(content_str.encode()).hexdigest()

    def verify_integrity(self) -> bool:
        """Verify the stored hash still matches the event's fields."""
        return self._hash == self._calculate_hash()

    def to_dict(self) -> Dict:
        """Serialize event to dictionary"""
        return {
            "event_id": self.event_id,
            "event_type": self.event_type.value,
            "timestamp": self.timestamp.isoformat(),
            "actor_id": self.actor_id,
            "target_id": self.target_id,
            "data": self.data,
            "parent_events": self.parent_events,
            "parent_hashes": self.parent_hashes,
            "prev_hash": self.prev_hash,
            "hash": self._hash,
        }


class CausalityGraph:
    """
    Maintains the causality relationships between events.
    Implements known causality graph (Codex 8.2)
    """

    def __init__(self):
        self.events: Dict[str, AuditEvent] = {}
        self.children: Dict[str, Set[str]] = {}  # parent_id -> set of child_ids

    def add_event(self, event: AuditEvent) -> None:
        """Add event to the causality graph"""
        if event.event_id in self.events:
            raise ValueError(f"Event {event.event_id} already exists")

        if not event.verify_integrity():
            raise ValueError(f"Event {event.event_id} failed integrity check")

        if len(event.parent_events) != len(event.parent_hashes):
            raise ValueError("parent_events and parent_hashes length mismatch")

        for parent_id, parent_hash in zip(event.parent_events, event.parent_hashes):
            parent = self.events.get(parent_id)
            if parent is None:
                raise ValueError(f"Unknown parent event {parent_id}")
            if parent._hash != parent_hash:
                raise ValueError(f"Parent hash mismatch for {parent_id}")

        self.events[event.event_id] = event

        # Build reverse index for efficient child lookup
        for parent_id in event.parent_events:
            if parent_id not in self.children:
                self.children[parent_id] = set()
            self.children[parent_id].add(event.event_id)

    def get_event(self, event_id: str) -> Optional[AuditEvent]:
        """Retrieve an event by ID"""
        return self.events.get(event_id)

    def get_children(self, event_id: str) -> List[AuditEvent]:
        """Get all events caused by a given event"""
        child_ids = self.children.get(event_id, set())
        return [self.events[cid] for cid in child_ids if cid in self.events]

    def get_lineage(self, event_id: str) -> List[AuditEvent]:
        """
        Get complete lineage (ancestry chain) of an event.
        Returns events from root to the specified event.
        """
        lineage = []
        visited = set()

        def trace_back(eid: str):
            if eid in visited or eid not in self.events:
                return

            event = self.events[eid]
            visited.add(eid)

            # Recursively trace parents first
            for parent_id in event.parent_events:
                trace_back(parent_id)

            lineage.append(event)

        trace_back(event_id)
        return lineage

    def get_descendants(self, event_id: str) -> List[AuditEvent]:
        """Get all events descended from a given event"""
        descendants = []
        visited = set()

        def traverse(eid: str):
            if eid in visited:
                return
            visited.add(eid)

            for child in self.get_children(eid):
                descendants.append(child)
                traverse(child.event_id)

        traverse(event_id)
        return descendants


class AuditLog:
    """
    In-memory audit log with a SHA-256 hash chain.

    `prev_hash` links each event to the previous event in insertion order.
    Explicit `parent_events` also bind to those parents' hashes.
    The chain lives in this process only.
    """

    def __init__(self):
        self.graph = CausalityGraph()
        self._type_index: Dict[EventType, List[str]] = {}
        self._actor_index: Dict[str, List[str]] = {}
        self._target_index: Dict[str, List[str]] = {}
        self._order: List[str] = []
        self._tip_hash: str = GENESIS_HASH

    def log_event(
        self,
        event_type: EventType,
        actor_id: Optional[str] = None,
        target_id: Optional[str] = None,
        data: Optional[Dict] = None,
        parent_events: Optional[List[str]] = None,
    ) -> AuditEvent:
        """
        Log a new event, hashing it over prev_hash and any parent hashes.
        Returns the created event.
        """
        parents = parent_events or []
        parent_hashes: List[str] = []
        for parent_id in parents:
            parent = self.graph.get_event(parent_id)
            if parent is None or parent._hash is None:
                raise ValueError(f"Unknown parent event {parent_id}")
            parent_hashes.append(parent._hash)

        event = AuditEvent(
            event_type=event_type,
            actor_id=actor_id,
            target_id=target_id,
            data=data or {},
            parent_events=parents,
            parent_hashes=parent_hashes,
            prev_hash=self._tip_hash,
        )

        self.graph.add_event(event)
        assert event._hash is not None
        self._order.append(event.event_id)
        self._tip_hash = event._hash

        # Update indexes for efficient querying
        if event_type not in self._type_index:
            self._type_index[event_type] = []
        self._type_index[event_type].append(event.event_id)

        if actor_id:
            if actor_id not in self._actor_index:
                self._actor_index[actor_id] = []
            self._actor_index[actor_id].append(event.event_id)

        if target_id:
            if target_id not in self._target_index:
                self._target_index[target_id] = []
            self._target_index[target_id].append(event.event_id)

        return event

    def get_events_by_type(self, event_type: EventType) -> List[AuditEvent]:
        """Get all events of a specific type"""
        event_ids = self._type_index.get(event_type, [])
        return [
            self.graph.get_event(eid) for eid in event_ids if self.graph.get_event(eid)
        ]

    def get_events_by_actor(self, actor_id: str) -> List[AuditEvent]:
        """Get all events performed by a specific actor"""
        event_ids = self._actor_index.get(actor_id, [])
        return [
            self.graph.get_event(eid) for eid in event_ids if self.graph.get_event(eid)
        ]

    def get_events_by_target(self, target_id: str) -> List[AuditEvent]:
        """Get all events affecting a specific target"""
        event_ids = self._target_index.get(target_id, [])
        return [
            self.graph.get_event(eid) for eid in event_ids if self.graph.get_event(eid)
        ]

    def get_change_lineage(self, target_id: str) -> List[AuditEvent]:
        """
        Get complete change lineage for a target entity.
        Implements change lineage tracking (Codex 8.2)
        """
        events = self.get_events_by_target(target_id)
        # Sort by timestamp to show evolution
        return sorted(events, key=lambda e: e.timestamp)

    def get_events(self, limit: Optional[int] = None) -> List[Dict]:
        """
        Get recent events as dictionaries.
        Returns events in reverse chronological order.
        """
        all_events = list(self.graph.events.values())
        # Sort by timestamp descending
        all_events.sort(key=lambda e: e.timestamp, reverse=True)

        if limit:
            all_events = all_events[:limit]

        # Convert to dict format
        return [e.to_dict() for e in all_events]

    def verify_integrity(self) -> bool:
        """Verify each event hash and that prev_hash forms a chain."""
        prev = GENESIS_HASH
        for event_id in self._order:
            event = self.graph.get_event(event_id)
            if event is None:
                return False
            if event.prev_hash != prev:
                return False
            if not event.verify_integrity():
                return False
            for parent_id, parent_hash in zip(event.parent_events, event.parent_hashes):
                parent = self.graph.get_event(parent_id)
                if parent is None or parent._hash != parent_hash:
                    return False
            prev = event._hash
        return True


# Global audit log instance
_audit_log = AuditLog()


def get_audit_log() -> AuditLog:
    """Get the global audit log instance"""
    return _audit_log
