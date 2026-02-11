"""Unit tests for the entity system."""
import pytest
from src.core.entity import Entity, EntityType, EntityRegistry, RelationType, Relationship, get_registry


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
        
        result = entity.to_dict()
        assert result['entity_id'] == "test-003"
        assert result['name'] == "Test"
        assert result['entity_type'] == "artifact"
        assert 'created_at' in result
        assert 'relationships' in result
    
    def test_entity_to_dict_with_relationships(self):
        """Test entity serialization with relationships."""
        entity1 = Entity("e1", EntityType.AGENT, "Agent 1")
        entity2 = Entity("e2", EntityType.TOOL, "Tool 1")
        
        entity1.declare_relationship(entity2, RelationType.USES, {"frequency": "daily"})
        
        result = entity1.to_dict()
        assert len(result['relationships']) == 1
        assert result['relationships'][0]['target_id'] == "e2"
        assert result['relationships'][0]['relation_type'] == "uses"
        assert result['relationships'][0]['metadata']['frequency'] == "daily"
    
    def test_declare_relationship(self):
        """Test declaring a relationship between entities."""
        agent = Entity("agent-1", EntityType.AGENT, "Agent")
        tool = Entity("tool-1", EntityType.TOOL, "Tool")
        
        relationship = agent.declare_relationship(tool, RelationType.USES)
        
        assert relationship.source_id == "agent-1"
        assert relationship.target_id == "tool-1"
        assert relationship.relation_type == RelationType.USES
        assert len(agent.relationships) == 1
    
    def test_declare_relationship_with_metadata(self):
        """Test declaring relationship with metadata."""
        agent = Entity("agent-2", EntityType.AGENT, "Agent")
        artifact = Entity("artifact-1", EntityType.ARTIFACT, "Artifact")
        
        metadata = {"created_by": "agent-2", "timestamp": "2026-01-01"}
        relationship = agent.declare_relationship(artifact, RelationType.PRODUCES, metadata)
        
        assert relationship.metadata == metadata
        assert relationship.metadata["created_by"] == "agent-2"
    
    def test_has_relationship(self):
        """Test checking if relationship exists."""
        agent = Entity("agent-3", EntityType.AGENT, "Agent")
        tool = Entity("tool-2", EntityType.TOOL, "Tool")
        
        # No relationship yet
        assert agent.has_relationship("tool-2", RelationType.USES) is False
        
        # Add relationship
        agent.declare_relationship(tool, RelationType.USES)
        assert agent.has_relationship("tool-2", RelationType.USES) is True
        
        # Different relation type
        assert agent.has_relationship("tool-2", RelationType.DEPENDS_ON) is False
    
    def test_get_relationships(self):
        """Test getting relationships."""
        agent = Entity("agent-4", EntityType.AGENT, "Agent")
        tool1 = Entity("tool-3", EntityType.TOOL, "Tool 1")
        tool2 = Entity("tool-4", EntityType.TOOL, "Tool 2")
        artifact = Entity("artifact-2", EntityType.ARTIFACT, "Artifact")
        
        agent.declare_relationship(tool1, RelationType.USES)
        agent.declare_relationship(tool2, RelationType.USES)
        agent.declare_relationship(artifact, RelationType.PRODUCES)
        
        # Get all relationships
        all_rels = agent.get_relationships()
        assert len(all_rels) == 3
        
        # Get filtered relationships
        uses_rels = agent.get_relationships(RelationType.USES)
        assert len(uses_rels) == 2
        
        produces_rels = agent.get_relationships(RelationType.PRODUCES)
        assert len(produces_rels) == 1


class TestEntityRegistry:
    """Test EntityRegistry class."""
    
    def test_registry_register(self, entity_registry):
        """Test registering an entity."""
        entity = Entity("reg-001", EntityType.AGENT, "Test")
        entity_registry.register(entity)
        
        retrieved = entity_registry.get("reg-001")
        assert retrieved is not None
        assert retrieved.entity_id == "reg-001"
    
    def test_registry_register_duplicate(self, entity_registry):
        """Test that registering duplicate entity raises error."""
        entity = Entity("reg-002", EntityType.AGENT, "Test")
        entity_registry.register(entity)
        
        # Try to register again
        with pytest.raises(ValueError, match="already registered"):
            entity_registry.register(entity)
    
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
    
    def test_validate_relationship_valid(self, entity_registry):
        """Test validating valid relationships."""
        manager = Entity("mgr-1", EntityType.MANAGER, "Manager")
        agent = Entity("agent-1", EntityType.AGENT, "Agent")
        
        entity_registry.register(manager)
        entity_registry.register(agent)
        
        # Manager manages agent (valid)
        is_valid = entity_registry.validate_relationship("mgr-1", "agent-1", RelationType.MANAGES)
        assert is_valid is True
    
    def test_validate_relationship_invalid(self, entity_registry):
        """Test validating invalid relationships."""
        agent = Entity("agent-2", EntityType.AGENT, "Agent")
        tool = Entity("tool-1", EntityType.TOOL, "Tool")
        
        entity_registry.register(agent)
        entity_registry.register(tool)
        
        # Agent manages tool (invalid)
        is_valid = entity_registry.validate_relationship("agent-2", "tool-1", RelationType.MANAGES)
        assert is_valid is False
    
    def test_validate_relationship_nonexistent_entity(self, entity_registry):
        """Test validation with non-existent entity."""
        agent = Entity("agent-3", EntityType.AGENT, "Agent")
        entity_registry.register(agent)
        
        # Target entity doesn't exist
        is_valid = entity_registry.validate_relationship("agent-3", "nonexistent", RelationType.USES)
        assert is_valid is False
    
    def test_find_related(self, entity_registry):
        """Test finding related entities."""
        agent = Entity("agent-4", EntityType.AGENT, "Agent")
        tool1 = Entity("tool-2", EntityType.TOOL, "Tool 1")
        tool2 = Entity("tool-3", EntityType.TOOL, "Tool 2")
        
        entity_registry.register(agent)
        entity_registry.register(tool1)
        entity_registry.register(tool2)
        
        # Declare relationships
        agent.declare_relationship(tool1, RelationType.USES)
        agent.declare_relationship(tool2, RelationType.USES)
        
        # Find related entities
        related = entity_registry.find_related("agent-4", RelationType.USES)
        assert len(related) == 2
        assert tool1 in related
        assert tool2 in related
    
    def test_find_related_nonexistent_entity(self, entity_registry):
        """Test finding related for non-existent entity."""
        related = entity_registry.find_related("nonexistent", RelationType.USES)
        assert related == []
    
    def test_find_related_no_relationships(self, entity_registry):
        """Test finding related when entity has no relationships."""
        agent = Entity("agent-5", EntityType.AGENT, "Agent")
        entity_registry.register(agent)
        
        related = entity_registry.find_related("agent-5", RelationType.USES)
        assert related == []


class TestGlobalRegistry:
    """Test global registry accessor."""
    
    def test_get_registry(self):
        """Test getting global registry."""
        registry1 = get_registry()
        registry2 = get_registry()
        
        # Should return same instance
        assert registry1 is registry2
