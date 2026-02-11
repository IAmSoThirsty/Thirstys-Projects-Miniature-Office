"""Integration tests for the Flask API."""
import pytest
import json
from src.server import app as app_module
from src.core.entity import EntityRegistry
from src.core.audit import AuditLog
from src.core.world import World
from src.departments.department import DepartmentRegistry
from src.tools.supply_store import SupplyStore


@pytest.fixture
def client():
    """Create a test client for the Flask app."""
    # Reset global singletons before each test
    app_module.simulation = None
    
    # Reset registries
    import src.core.entity as entity_mod
    entity_mod._registry = EntityRegistry()
    
    import src.core.audit as audit_mod
    audit_mod._audit_log = AuditLog()
    
    import src.departments.department as dept_mod
    dept_mod._department_registry = DepartmentRegistry()
    
    import src.core.world as world_mod
    world_mod._world = None
    
    import src.tools.supply_store as supply_mod
    supply_mod._supply_store = SupplyStore()
    
    # Configure app for testing
    app_module.app.config['TESTING'] = True
    
    # Initialize simulation for testing
    app_module.simulation = app_module.init_simulation()
    
    with app_module.app.test_client() as client:
        yield client


class TestAPIEndpoints:
    """Test API endpoints."""
    
    def test_health_check(self, client):
        """Test the health check endpoint."""
        response = client.get('/health')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert data['status'] == 'healthy'
    
    def test_get_world_state(self, client):
        """Test getting the world state."""
        response = client.get('/api/world/state')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'world' in data
        assert 'name' in data['world']
    
    def test_step_simulation(self, client):
        """Test stepping the simulation."""
        response = client.post('/api/world/step')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'success' in data
        assert data['success'] is True
    
    def test_list_agents(self, client):
        """Test listing agents."""
        response = client.get('/api/agents')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'agents' in data
        assert isinstance(data['agents'], list)
    
    def test_list_departments(self, client):
        """Test listing departments."""
        response = client.get('/api/departments')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'departments' in data
    
    def test_get_supply_store(self, client):
        """Test getting supply store."""
        response = client.get('/api/supply-store')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'available_count' in data
    
    def test_get_audit_events(self, client):
        """Test getting audit events."""
        response = client.get('/api/audit/events')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'events' in data
        assert isinstance(data['events'], list)
    
    def test_invalid_endpoint(self, client):
        """Test accessing an invalid endpoint."""
        response = client.get('/api/invalid')
        assert response.status_code == 404
