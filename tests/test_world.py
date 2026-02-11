"""Unit tests for the world system."""
import pytest
import json
from src.core.world import (
    World, Floor, Office, 
    WorldSchema, FloorSchema, OfficeSchema,
    create_world, get_world
)
from src.departments.department import Department
from src.agents.agent import Agent, Manager, AgentRole


class TestOfficeSchema:
    """Test OfficeSchema class."""
    
    def test_schema_to_dict(self):
        """Test schema serialization."""
        schema = OfficeSchema(
            officeId="office-001",
            roles=["agent-001", "agent-002"],
            manager="manager-001"
        )
        data = schema.to_dict()
        assert data['officeId'] == "office-001"
        assert len(data['roles']) == 2
    
    def test_schema_from_dict(self):
        """Test schema deserialization."""
        data = {
            'officeId': "office-001",
            'roles': ["agent-001"],
            'manager': "manager-001"
        }
        schema = OfficeSchema.from_dict(data)
        assert schema.officeId == "office-001"


class TestOffice:
    """Test Office class."""
    
    def test_office_creation(self):
        """Test creating an office."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        assert office.office_id == "office-001"
        assert office.department == dept
        assert len(office.agents) == 0
    
    def test_add_agent(self):
        """Test adding agent to office."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        office.add_agent(agent)
        assert len(office.agents) == 1
        assert "agent-001" in office.agents
    
    def test_add_agent_duplicate(self):
        """Test adding same agent twice."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        office.add_agent(agent)
        office.add_agent(agent)
        assert len(office.agents) == 1
    
    def test_set_manager(self):
        """Test setting office manager."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        manager = Manager("mgr-001", "Test Manager", dept.entity_id)
        office.set_manager(manager)
        assert office.manager == manager
    
    def test_get_agents(self):
        """Test getting agents from office."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        office.add_agent(agent)
        agents = office.get_agents()
        assert len(agents) == 1
        assert agents[0].entity_id == "agent-001"
    
    def test_to_schema(self):
        """Test converting office to schema."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        manager = Manager("mgr-001", "Test Manager", dept.entity_id)
        office.set_manager(manager)
        schema = office.to_schema()
        assert schema.officeId == "office-001"
        assert schema.manager == "mgr-001"
    
    def test_to_dict(self):
        """Test office to dict."""
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        data = office.to_dict()
        assert data['officeId'] == "office-001"


class TestFloorSchema:
    """Test FloorSchema class."""
    
    def test_schema_to_dict(self):
        """Test schema serialization."""
        schema = FloorSchema(
            floorId="floor-001",
            language="Python",
            offices=[]
        )
        data = schema.to_dict()
        assert data['floorId'] == "floor-001"
        assert data['language'] == "Python"
    
    def test_schema_from_dict(self):
        """Test schema deserialization."""
        data = {
            'floorId': "floor-001",
            'language': "Python",
            'offices': []
        }
        schema = FloorSchema.from_dict(data)
        assert schema.floorId == "floor-001"


class TestFloor:
    """Test Floor class."""
    
    def test_floor_creation(self):
        """Test creating a floor."""
        floor = Floor("floor-001", "Python")
        assert floor.floor_id == "floor-001"
        assert floor.language == "Python"
        assert len(floor.offices) == 0
    
    def test_add_office(self):
        """Test adding office to floor."""
        floor = Floor("floor-001", "Python")
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        floor.add_office(office)
        assert len(floor.offices) == 1
        assert "office-001" in floor.offices
    
    def test_get_office(self):
        """Test getting office from floor."""
        floor = Floor("floor-001", "Python")
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        floor.add_office(office)
        retrieved = floor.get_office("office-001")
        assert retrieved is not None
        assert retrieved.office_id == "office-001"
    
    def test_get_office_not_found(self):
        """Test getting non-existent office."""
        floor = Floor("floor-001", "Python")
        result = floor.get_office("nonexistent")
        assert result is None
    
    def test_get_all_agents(self):
        """Test getting all agents from floor."""
        floor = Floor("floor-001", "Python")
        dept = Department("dept-001", "Engineering", "Python")
        office = Office("office-001", dept)
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        office.add_agent(agent)
        floor.add_office(office)
        agents = floor.get_all_agents()
        assert len(agents) == 1
    
    def test_to_schema(self):
        """Test converting floor to schema."""
        floor = Floor("floor-001", "Python")
        schema = floor.to_schema()
        assert schema.floorId == "floor-001"
        assert schema.language == "Python"
    
    def test_to_dict(self):
        """Test floor to dict."""
        floor = Floor("floor-001", "Python")
        data = floor.to_dict()
        assert data['floorId'] == "floor-001"


class TestWorldSchema:
    """Test WorldSchema class."""
    
    def test_schema_to_dict(self):
        """Test schema serialization."""
        schema = WorldSchema(
            worldId="world-001",
            name="Test World",
            floors=[],
            supplyStore={},
            time=0
        )
        data = schema.to_dict()
        assert data['worldId'] == "world-001"
        assert data['name'] == "Test World"
    
    def test_schema_from_dict(self):
        """Test schema deserialization."""
        data = {
            'worldId': "world-001",
            'name': "Test World",
            'floors': [],
            'supplyStore': {},
            'time': 0
        }
        schema = WorldSchema.from_dict(data)
        assert schema.worldId == "world-001"


class TestWorld:
    """Test World class."""
    
    def test_world_creation(self):
        """Test creating a world."""
        world = World("world-001", "Test World")
        assert world.world_id == "world-001"
        assert world.name == "Test World"
        assert world.time == 0
        assert world.is_active
        assert len(world.floors) == 0
    
    def test_add_floor(self):
        """Test adding floor to world."""
        world = World("world-001", "Test World")
        floor = Floor("floor-001", "Python")
        world.add_floor(floor)
        assert len(world.floors) == 1
        assert "floor-001" in world.floors
    
    def test_get_floor(self):
        """Test getting floor from world."""
        world = World("world-001", "Test World")
        floor = Floor("floor-001", "Python")
        world.add_floor(floor)
        retrieved = world.get_floor("floor-001")
        assert retrieved is not None
        assert retrieved.floor_id == "floor-001"
    
    def test_get_floor_not_found(self):
        """Test getting non-existent floor."""
        world = World("world-001", "Test World")
        result = world.get_floor("nonexistent")
        assert result is None
    
    def test_get_floor_by_language(self):
        """Test getting floor by language."""
        world = World("world-001", "Test World")
        floor = Floor("floor-001", "Python")
        world.add_floor(floor)
        retrieved = world.get_floor_by_language("python")
        assert retrieved is not None
        assert retrieved.language == "Python"
    
    def test_get_floor_by_language_not_found(self):
        """Test getting floor by non-existent language."""
        world = World("world-001", "Test World")
        result = world.get_floor_by_language("Ruby")
        assert result is None
    
    def test_tick(self):
        """Test world tick advancement."""
        world = World("world-001", "Test World")
        assert world.time == 0
        world.tick()
        assert world.time == 1
        world.tick()
        assert world.time == 2
    
    def test_to_schema(self):
        """Test converting world to schema."""
        world = World("world-001", "Test World")
        schema = world.to_schema()
        assert schema.worldId == "world-001"
        assert schema.name == "Test World"
        assert schema.time == 0
    
    def test_to_dict(self):
        """Test world to dict."""
        world = World("world-001", "Test World")
        data = world.to_dict()
        assert data['worldId'] == "world-001"
        assert 'supplyStore' in data
    
    def test_to_json(self):
        """Test world to JSON."""
        world = World("world-001", "Test World")
        json_str = world.to_json()
        assert isinstance(json_str, str)
        data = json.loads(json_str)
        assert data['worldId'] == "world-001"
    
    def test_from_schema(self):
        """Test creating world from schema."""
        schema = WorldSchema(
            worldId="world-001",
            name="Test World",
            floors=[],
            supplyStore={},
            time=5
        )
        world = World.from_schema(schema)
        assert world.world_id == "world-001"
        assert world.time == 5
    
    def test_from_json(self):
        """Test creating world from JSON."""
        json_str = json.dumps({
            'worldId': "world-001",
            'name': "Test World",
            'floors': [],
            'supplyStore': {},
            'time': 0
        })
        world = World.from_json(json_str)
        assert world.world_id == "world-001"
    
    def test_from_schema_with_floors(self):
        """Test creating world from schema with floors."""
        schema = WorldSchema(
            worldId="world-001",
            name="Test World",
            floors=[
                {'floorId': 'floor-001', 'language': 'Python', 'offices': []}
            ],
            supplyStore={},
            time=0
        )
        world = World.from_schema(schema)
        assert len(world.floors) == 1
        assert 'floor-001' in world.floors


class TestWorldGlobal:
    """Test global world functions."""
    
    def test_create_world(self):
        """Test creating global world."""
        world = create_world("world-001", "Test World")
        assert world is not None
        assert world.world_id == "world-001"
    
    def test_get_world(self):
        """Test getting global world."""
        create_world("world-002", "Test World 2")
        world = get_world()
        assert world is not None
        assert world.world_id == "world-002"
