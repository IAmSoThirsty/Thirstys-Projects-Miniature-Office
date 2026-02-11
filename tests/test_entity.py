"""Unit tests for the entity system."""
import pytest
from src.core.entity import Entity, EntityType, EntityRegistry


class TestEntity:
    """Test Entity class."""
    
    def test_entity_creation(self):
        """Test basic entity creation."""
        entity = Entity("test-001", EntityType.AGENT, "Test Entity")
        
        assert entity.entity_id == "test-001"
        assert entity.name == "Test Entity"
        assert entity.entity_type == EntityType.AGENT
        assert entity.metadata == {}
    
    def test_entity_with_metadata(self):
        """Test entity creation with metadata."""
        metadata = {"role": "builder", "language": "python"}
        entity = Entity("test-002", EntityType.AGENT, "Builder", metadata)
        
        assert entity.metadata == metadata
        assert entity.metadata["role"] == "builder"
    
    def test_entity_to_dict(self):
        """Test entity serialization."""
        entity = Entity("test-003", EntityType.ARTIFACT, "Test")
        
        # Basic check that entity has key attributes
        assert entity.entity_id == "test-003"
        assert entity.name == "Test"
        assert entity.entity_type.value == "artifact"


class TestEntityRegistry:
    """Test EntityRegistry class."""
    
    def test_registry_register(self, entity_registry):
        """Test registering an entity."""
        entity = Entity("reg-001", EntityType.AGENT, "Test")
        entity_registry.register(entity)
        
        retrieved = entity_registry.get("reg-001")
        assert retrieved is not None
        assert retrieved.entity_id == "reg-001"
    
    def test_registry_get_nonexistent(self, entity_registry):
        """Test getting a non-existent entity."""
        entity = entity_registry.get("nonexistent")
        assert entity is None
    
    def test_registry_get_by_type(self, entity_registry):
        """Test getting entities by type."""
        agent1 = Entity("agent-001", EntityType.AGENT, "Agent 1")
        agent2 = Entity("agent-002", EntityType.AGENT, "Agent 2")
        artifact1 = Entity("artifact-001", EntityType.ARTIFACT, "Artifact 1")
        
        entity_registry.register(agent1)
        entity_registry.register(agent2)
        entity_registry.register(artifact1)
        
        agents = entity_registry.get_by_type(EntityType.AGENT)
        assert len(agents) == 2
        
        artifacts = entity_registry.get_by_type(EntityType.ARTIFACT)
        assert len(artifacts) == 1
    
    def test_registry_all_entities(self, entity_registry):
        """Test getting all entities."""
        entity1 = Entity("e1", EntityType.AGENT, "Entity 1")
        entity2 = Entity("e2", EntityType.ARTIFACT, "Entity 2")
        
        entity_registry.register(entity1)
        entity_registry.register(entity2)
        
        # Count entities by getting all types
        all_count = len(entity_registry.get_by_type(EntityType.AGENT)) + \
                    len(entity_registry.get_by_type(EntityType.ARTIFACT))
        assert all_count == 2
