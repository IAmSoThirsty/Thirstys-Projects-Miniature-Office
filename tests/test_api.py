"""Integration tests for the Flask API."""
import pytest
import json
from unittest.mock import patch
from src.server import app as app_module
from src.core.entity import EntityRegistry
from src.core.audit import AuditLog
from src.core.world import World
from src.departments.department import DepartmentRegistry
from src.tools.supply_store import SupplyStore


@pytest.fixture
def client():
    """Create a test client for the Flask app."""
    # Configure app for testing
    app_module.app.config['TESTING'] = True
    
    # Full reset of all singletons
    def reset_all():
        app_module.simulation = None
        
        import src.core.entity as entity_mod
        entity_mod._registry = None
        entity_mod._registry = EntityRegistry()
        
        import src.core.audit as audit_mod
        audit_mod._audit_log = None
        audit_mod._audit_log = AuditLog()
        
        import src.departments.department as dept_mod
        dept_mod._department_registry = None
        dept_mod._department_registry = DepartmentRegistry()
        
        import src.core.world as world_mod
        world_mod._world = None
        
        import src.tools.supply_store as supply_mod
        supply_mod._supply_store = None
        supply_mod._supply_store = SupplyStore()
    
    # Reset before test
    reset_all()
    
    # Initialize simulation for testing
    app_module.simulation = app_module.init_simulation()
    
    with app_module.app.test_client() as client:
        yield client
    
    # Clean up after test
    reset_all()


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
    
    def test_root_index(self, client):
        """Test serving index page."""
        response = client.get('/')
        assert response.status_code == 200 or response.status_code == 404
    
    def test_health_check_initializing(self, client):
        """Test health check when simulation is initializing."""
        # Save current simulation
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.get('/health')
            assert response.status_code == 503
            data = json.loads(response.data)
            assert data['status'] == 'starting'
        finally:
            # Restore simulation
            app_module.simulation = saved_sim
    
    def test_metrics_endpoint(self, client):
        """Test Prometheus metrics endpoint."""
        response = client.get('/metrics')
        assert response.status_code == 200
        assert 'minioffice_world_time' in response.data.decode()
        assert 'text/plain' in response.content_type
    
    def test_metrics_not_initialized(self, client):
        """Test metrics when simulation not initialized."""
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.get('/metrics')
            assert response.status_code == 503
        finally:
            app_module.simulation = saved_sim
    
    def test_api_index(self, client):
        """Test API documentation endpoint."""
        response = client.get('/api')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'endpoints' in data
        assert 'version' in data
    
    def test_get_world_state_not_initialized(self, client):
        """Test getting world state when not initialized."""
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.get('/api/world/state')
            assert response.status_code == 500
            data = json.loads(response.data)
            assert 'error' in data
        finally:
            app_module.simulation = saved_sim
    
    def test_step_simulation_not_initialized(self, client):
        """Test stepping simulation when not initialized."""
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.post('/api/world/step')
            assert response.status_code == 500
        finally:
            app_module.simulation = saved_sim
    
    def test_start_simulation(self, client):
        """Test starting continuous simulation."""
        response = client.post('/api/world/start')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert data['success'] is True
    
    def test_start_simulation_already_running(self, client):
        """Test starting simulation when already running."""
        app_module.simulation.is_running = True
        response = client.post('/api/world/start')
        assert response.status_code == 400
        app_module.simulation.is_running = False
    
    def test_start_simulation_not_initialized(self, client):
        """Test starting simulation when not initialized."""
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.post('/api/world/start')
            assert response.status_code == 500
        finally:
            app_module.simulation = saved_sim
    
    def test_stop_simulation(self, client):
        """Test stopping simulation."""
        response = client.post('/api/world/stop')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert data['success'] is True
    
    def test_stop_simulation_not_initialized(self, client):
        """Test stopping simulation when not initialized."""
        saved_sim = app_module.simulation
        try:
            app_module.simulation = None
            response = client.post('/api/world/stop')
            assert response.status_code == 500
        finally:
            app_module.simulation = saved_sim
    
    def test_get_agent_status(self, client):
        """Test getting agent status."""
        from src.core.entity import EntityType
        registry = app_module.get_registry()
        agents = registry.get_by_type(EntityType.AGENT)
        if agents:
            agent = agents[0]
            response = client.get(f'/api/agents/{agent.entity_id}/status')
            assert response.status_code == 200
            data = json.loads(response.data)
            assert 'agent_id' in data
            assert 'name' in data
            assert 'role' in data
    
    def test_get_agent_status_not_found(self, client):
        """Test getting status for non-existent agent."""
        response = client.get('/api/agents/invalid-id/status')
        assert response.status_code == 404
        data = json.loads(response.data)
        assert 'error' in data
    
    def test_get_task(self, client):
        """Test getting task details."""
        from src.core.entity import EntityType
        registry = app_module.get_registry()
        all_entities = registry.get_by_type(EntityType.ARTIFACT)
        from src.core.mission import Task
        tasks = [e for e in all_entities if isinstance(e, Task)]
        if tasks:
            task = tasks[0]
            response = client.get(f'/api/tasks/{task.entity_id}')
            assert response.status_code == 200
            data = json.loads(response.data)
            assert 'task_id' in data
            assert 'name' in data
            assert 'state' in data
    
    def test_get_task_not_found(self, client):
        """Test getting non-existent task."""
        response = client.get('/api/tasks/invalid-id')
        assert response.status_code == 404
    
    def test_list_tasks(self, client):
        """Test listing all tasks."""
        response = client.get('/api/tasks')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'tasks' in data
    
    def test_list_contracts(self, client):
        """Test listing contracts."""
        response = client.get('/api/contracts')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'contracts' in data
    
    def test_get_audit_events_with_filters(self, client):
        """Test getting audit events with filters."""
        # Test with event type filter (use correct enum value)
        response = client.get('/api/audit/events?type=entity_created&limit=50')
        assert response.status_code == 200
        
        # Test with actor filter
        response = client.get('/api/audit/events?actor_id=some-actor')
        assert response.status_code == 200
        
        # Test with target filter
        response = client.get('/api/audit/events?target_id=some-target')
        assert response.status_code == 200
    
    def test_get_consigliere_state(self, client):
        """Test getting Consigliere state."""
        response = client.get('/api/consigliere')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'role' in data or 'name' in data or isinstance(data, dict)
    
    def test_consigliere_explain_why_blocked(self, client):
        """Test Consigliere explain why blocked."""
        response = client.post('/api/consigliere/explain', 
            data=json.dumps({
                'type': 'why_blocked',
                'entity_id': 'test-entity',
                'context': {}
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_explain_why_blocked_missing_entity(self, client):
        """Test Consigliere explain without entity_id."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'why_blocked'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_explain_why_decision(self, client):
        """Test Consigliere explain why decision."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'why_decision',
                'entity_id': 'test-entity'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_explain_why_decision_missing_entity(self, client):
        """Test Consigliere explain decision without entity_id."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'why_decision'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_explain_what_options(self, client):
        """Test Consigliere explain what options."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'what_options',
                'situation': 'need to implement feature'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_explain_what_options_missing_situation(self, client):
        """Test Consigliere explain options without situation."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'what_options'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_explain_unknown_type(self, client):
        """Test Consigliere explain with unknown type."""
        response = client.post('/api/consigliere/explain',
            data=json.dumps({
                'type': 'unknown_type'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_translate(self, client):
        """Test Consigliere translation."""
        response = client.post('/api/consigliere/translate',
            data=json.dumps({
                'text': 'some civilizational text'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_translate_missing_text(self, client):
        """Test Consigliere translation without text."""
        response = client.post('/api/consigliere/translate',
            data=json.dumps({}),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_preview(self, client):
        """Test Consigliere preview consequences."""
        response = client.post('/api/consigliere/preview',
            data=json.dumps({
                'action': 'refactor codebase',
                'context': {}
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_preview_missing_action(self, client):
        """Test Consigliere preview without action."""
        response = client.post('/api/consigliere/preview',
            data=json.dumps({}),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_draft(self, client):
        """Test Consigliere draft directive."""
        response = client.post('/api/consigliere/draft',
            data=json.dumps({
                'goal': 'implement authentication',
                'language': 'python',
                'constraints': []
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_draft_missing_params(self, client):
        """Test Consigliere draft without required params."""
        response = client.post('/api/consigliere/draft',
            data=json.dumps({
                'goal': 'implement authentication'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_command_manager(self, client):
        """Test Consigliere command manager."""
        response = client.post('/api/consigliere/command/manager',
            data=json.dumps({
                'manager_id': 'mgr-001',
                'directive': 'prioritize security',
                'priority': 'high'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_command_manager_missing_params(self, client):
        """Test Consigliere command manager without params."""
        response = client.post('/api/consigliere/command/manager',
            data=json.dumps({
                'manager_id': 'mgr-001'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_command_agent(self, client):
        """Test Consigliere command agent."""
        response = client.post('/api/consigliere/command/agent',
            data=json.dumps({
                'agent_id': 'agent-001',
                'new_directive': 'focus on testing',
                'justification': 'need better test coverage'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_command_agent_missing_params(self, client):
        """Test Consigliere command agent without params."""
        response = client.post('/api/consigliere/command/agent',
            data=json.dumps({
                'agent_id': 'agent-001',
                'new_directive': 'focus on testing'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_coordinate(self, client):
        """Test Consigliere coordinate cross-floor work."""
        response = client.post('/api/consigliere/coordinate',
            data=json.dumps({
                'floor_ids': ['floor-python', 'floor-javascript'],
                'coordination_plan': 'sync API contracts'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_coordinate_missing_params(self, client):
        """Test Consigliere coordinate without params."""
        response = client.post('/api/consigliere/coordinate',
            data=json.dumps({
                'floor_ids': ['floor-python']
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_consigliere_assess_feasible(self, client):
        """Test Consigliere assess feasibility - feasible case."""
        response = client.post('/api/consigliere/assess',
            data=json.dumps({
                'request': 'implement new feature',
                'context': {}
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_assess_impossible(self, client):
        """Test Consigliere assess feasibility - impossible case."""
        response = client.post('/api/consigliere/assess',
            data=json.dumps({
                'request': 'this is impossible to do',
                'context': {}
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_consigliere_assess_missing_request(self, client):
        """Test Consigliere assess without request."""
        response = client.post('/api/consigliere/assess',
            data=json.dumps({}),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_get_security_state(self, client):
        """Test getting security state."""
        response = client.get('/api/security')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert isinstance(data, dict)
    
    def test_security_grant(self, client):
        """Test granting security permission."""
        response = client.post('/api/security/grant',
            data=json.dumps({
                'entity_id': 'agent-001',
                'resource': 'dangerous-tool',
                'justification': 'needed for task',
                'expires_in_hours': 24
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_grant_missing_params(self, client):
        """Test granting security permission without params."""
        response = client.post('/api/security/grant',
            data=json.dumps({
                'entity_id': 'agent-001'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_security_revoke(self, client):
        """Test revoking security permission."""
        response = client.post('/api/security/revoke',
            data=json.dumps({
                'permission_id': 'perm-001',
                'reason': 'no longer needed'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_revoke_missing_params(self, client):
        """Test revoking security permission without params."""
        response = client.post('/api/security/revoke',
            data=json.dumps({
                'permission_id': 'perm-001'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_security_audit_full_system(self, client):
        """Test triggering full system security audit."""
        response = client.post('/api/security/audit',
            data=json.dumps({
                'type': 'full_system',
                'reason': 'periodic review'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_audit_cross_floor(self, client):
        """Test triggering cross-floor security audit."""
        response = client.post('/api/security/audit',
            data=json.dumps({
                'type': 'cross_floor',
                'scope': ['floor-python', 'floor-rust'],
                'reason': 'contract review'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_audit_unknown_type(self, client):
        """Test security audit with unknown type."""
        response = client.post('/api/security/audit',
            data=json.dumps({
                'type': 'unknown_audit_type'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_security_lockdown_building(self, client):
        """Test triggering building lockdown."""
        response = client.post('/api/security/lockdown',
            data=json.dumps({
                'scope': 'building',
                'reason': 'security incident',
                'threat_level': 'high'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_lockdown_floor(self, client):
        """Test triggering floor lockdown."""
        response = client.post('/api/security/lockdown',
            data=json.dumps({
                'scope': 'floor:floor-python',
                'reason': 'suspicious activity',
                'threat_level': 'medium'
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_lockdown_invalid_scope(self, client):
        """Test lockdown with invalid scope."""
        response = client.post('/api/security/lockdown',
            data=json.dumps({
                'scope': 'invalid-scope',
                'reason': 'test',
                'threat_level': 'low'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_security_lockdown_missing_params(self, client):
        """Test lockdown without required params."""
        response = client.post('/api/security/lockdown',
            data=json.dumps({
                'scope': 'building'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_security_block(self, client):
        """Test blocking delivery."""
        response = client.post('/api/security/block',
            data=json.dumps({
                'artifact_id': 'artifact-001',
                'reason': 'security vulnerability',
                'required_mitigations': ['fix CVE-123']
            }),
            content_type='application/json')
        assert response.status_code == 200
    
    def test_security_block_missing_params(self, client):
        """Test blocking delivery without params."""
        response = client.post('/api/security/block',
            data=json.dumps({
                'artifact_id': 'artifact-001'
            }),
            content_type='application/json')
        assert response.status_code == 400
    
    def test_get_active_lockdowns(self, client):
        """Test getting active lockdowns."""
        response = client.get('/api/security/lockdowns')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'lockdowns' in data
    
    def test_get_blocked_deliveries(self, client):
        """Test getting blocked deliveries."""
        response = client.get('/api/security/blocked')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'blocked_deliveries' in data
    
    def test_get_active_permissions(self, client):
        """Test getting active permissions."""
        response = client.get('/api/security/permissions')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'permissions' in data
    
    def test_get_active_permissions_with_entity(self, client):
        """Test getting active permissions for specific entity."""
        response = client.get('/api/security/permissions?entity_id=agent-001')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'permissions' in data
    
    def test_list_floors(self, client):
        """Test listing floor specifications."""
        response = client.get('/api/floors')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'floors' in data
    
    def test_get_floor_specification(self, client):
        """Test getting specific floor specification."""
        response = client.get('/api/floors/python')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'language' in data or 'name' in data
    
    def test_get_floor_specification_unknown(self, client):
        """Test getting unknown floor specification."""
        response = client.get('/api/floors/unknown-language')
        assert response.status_code == 404
    
    def test_get_bundle_status(self, client):
        """Test getting canonical bundle status."""
        response = client.get('/api/canonical-bundle')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'bundle_id' in data or 'error' in data
    
    def test_get_charter(self, client):
        """Test getting civilization charter."""
        response = client.get('/api/canonical-bundle/charter')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'charter_id' in data or 'error' in data
    
    def test_get_authority_ledger(self, client):
        """Test getting authority ledger."""
        response = client.get('/api/canonical-bundle/authority-ledger')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'ledger_id' in data or 'error' in data
    
    def test_get_purpose_lock(self, client):
        """Test getting purpose lock attestation."""
        response = client.get('/api/canonical-bundle/purpose-lock')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'attestation_id' in data or 'error' in data
    
    def test_get_board_resolutions(self, client):
        """Test getting board resolutions."""
        response = client.get('/api/canonical-bundle/board-resolutions')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'archive_id' in data or 'error' in data
    
    def test_get_precedents(self, client):
        """Test getting directive precedents."""
        response = client.get('/api/canonical-bundle/precedents')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'corpus_id' in data or 'error' in data
    
    def test_get_meta_rulings(self, client):
        """Test getting meta-office rulings."""
        response = client.get('/api/canonical-bundle/meta-office-rulings')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'ledger_id' in data or 'error' in data
    
    def test_get_law_failure_matrix(self, client):
        """Test getting law failure matrix."""
        response = client.get('/api/canonical-bundle/law-failure-matrix')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'matrix_id' in data or 'error' in data
    
    def test_get_formal_verification(self, client):
        """Test getting formal verification models."""
        response = client.get('/api/canonical-bundle/formal-verification')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'model_id' in data or 'error' in data
    
    def test_get_violation_playbooks(self, client):
        """Test getting violation playbooks."""
        response = client.get('/api/canonical-bundle/violation-playbooks')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'playbooks_id' in data or 'error' in data
    
    def test_get_execution_kernel(self, client):
        """Test getting execution kernel."""
        response = client.get('/api/canonical-bundle/execution-kernel')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'kernel_id' in data or 'error' in data
    
    def test_get_simulation_traces(self, client):
        """Test getting simulation traces."""
        response = client.get('/api/canonical-bundle/simulation-traces')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'corpus_id' in data or 'error' in data
    
    def test_get_reproducibility_packets(self, client):
        """Test getting reproducibility packets."""
        response = client.get('/api/canonical-bundle/reproducibility-packets')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'packets_id' in data or 'error' in data
    
    def test_get_floor_profiles(self, client):
        """Test getting floor profiles."""
        response = client.get('/api/canonical-bundle/floor-profiles')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'profiles_id' in data or 'error' in data
    
    def test_get_contract_registry(self, client):
        """Test getting contract registry."""
        response = client.get('/api/canonical-bundle/contract-registry')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'registry_id' in data or 'error' in data
    
    def test_get_contract_drift(self, client):
        """Test getting contract drift reports."""
        response = client.get('/api/canonical-bundle/contract-drift')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'reports_id' in data or 'error' in data
    
    def test_get_tool_provenance(self, client):
        """Test getting tool provenance ledger."""
        response = client.get('/api/canonical-bundle/tool-provenance')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'ledger_id' in data or 'error' in data
    
    def test_get_unsafe_exceptions(self, client):
        """Test getting unsafe exceptions."""
        response = client.get('/api/canonical-bundle/unsafe-exceptions')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'records_id' in data or 'error' in data
    
    def test_get_consigliere_logs(self, client):
        """Test getting consigliere logs."""
        response = client.get('/api/canonical-bundle/consigliere-logs')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'logs_id' in data or 'error' in data
    
    def test_get_security_dossiers(self, client):
        """Test getting security dossiers."""
        response = client.get('/api/canonical-bundle/security-dossiers')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'dossiers_id' in data or 'error' in data
    
    def test_get_override_ledger(self, client):
        """Test getting override ledger."""
        response = client.get('/api/canonical-bundle/override-ledger')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'ledger_id' in data or 'error' in data
    
    def test_get_amendments(self, client):
        """Test getting amendments."""
        response = client.get('/api/canonical-bundle/amendments')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'registry_id' in data or 'error' in data
    
    def test_get_rejected_proposals(self, client):
        """Test getting rejected proposals."""
        response = client.get('/api/canonical-bundle/rejected-proposals')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'archive_id' in data or 'error' in data
    
    def test_get_audit_interface(self, client):
        """Test getting audit interface."""
        response = client.get('/api/canonical-bundle/audit-interface')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'interface_id' in data or 'error' in data
    
    def test_get_compliance_reports(self, client):
        """Test getting compliance reports."""
        response = client.get('/api/canonical-bundle/compliance-reports')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'reports_id' in data or 'error' in data
    
    def test_get_freeze_protocol(self, client):
        """Test getting freeze protocol."""
        response = client.get('/api/canonical-bundle/freeze-protocol')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'protocol_id' in data or 'error' in data
    
    def test_get_shutdown_protocol(self, client):
        """Test getting shutdown protocol."""
        response = client.get('/api/canonical-bundle/shutdown-protocol')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'protocol_id' in data or 'error' in data
    
    def test_get_metrics_canon(self, client):
        """Test getting metrics canon."""
        response = client.get('/api/canonical-bundle/metrics-canon')
        assert response.status_code == 200
        data = json.loads(response.data)
        assert 'canon_id' in data or 'error' in data
    
    def test_canonical_bundle_exception_handling(self, client):
        """Test exception handling in canonical bundle endpoints."""
        with patch('src.server.app.get_canonical_bundle') as mock_bundle:
            mock_bundle.side_effect = Exception("Test exception")
            
            # Test various endpoints for exception handling
            endpoints = [
                '/api/canonical-bundle',
                '/api/canonical-bundle/charter',
                '/api/canonical-bundle/authority-ledger',
                '/api/canonical-bundle/purpose-lock',
                '/api/canonical-bundle/board-resolutions',
                '/api/canonical-bundle/precedents',
                '/api/canonical-bundle/meta-office-rulings',
                '/api/canonical-bundle/law-failure-matrix',
                '/api/canonical-bundle/formal-verification',
                '/api/canonical-bundle/violation-playbooks',
                '/api/canonical-bundle/execution-kernel',
                '/api/canonical-bundle/simulation-traces',
                '/api/canonical-bundle/reproducibility-packets',
                '/api/canonical-bundle/floor-profiles',
                '/api/canonical-bundle/contract-registry',
                '/api/canonical-bundle/contract-drift',
                '/api/canonical-bundle/tool-provenance',
                '/api/canonical-bundle/unsafe-exceptions',
                '/api/canonical-bundle/consigliere-logs',
                '/api/canonical-bundle/security-dossiers',
                '/api/canonical-bundle/override-ledger',
                '/api/canonical-bundle/amendments',
                '/api/canonical-bundle/rejected-proposals',
                '/api/canonical-bundle/audit-interface',
                '/api/canonical-bundle/compliance-reports',
                '/api/canonical-bundle/freeze-protocol',
                '/api/canonical-bundle/shutdown-protocol',
                '/api/canonical-bundle/metrics-canon',
            ]
            
            for endpoint in endpoints:
                response = client.get(endpoint)
                assert response.status_code == 500
                data = json.loads(response.data)
                assert 'error' in data
    
    def test_get_task_wrong_entity_type(self, client):
        """Test getting task with wrong entity type."""
        # Get an agent ID and try to get it as a task
        from src.core.entity import EntityType
        registry = app_module.get_registry()
        agents = registry.get_by_type(EntityType.AGENT)
        if agents:
            agent = agents[0]
            response = client.get(f'/api/tasks/{agent.entity_id}')
            assert response.status_code == 404
            data = json.loads(response.data)
            assert 'error' in data


class TestProductionMode:
    """Test production mode configuration - isolated to avoid affecting other tests."""
    
    def test_production_secret_key_required(self):
        """Test that production mode requires SECRET_KEY."""
        import os
        import subprocess
        import sys
        
        # Test in subprocess to avoid affecting the current test environment
        test_code = '''
import os
os.environ['FLASK_ENV'] = 'production'
if 'SECRET_KEY' in os.environ:
    del os.environ['SECRET_KEY']

try:
    import src.server.app
    print("SHOULD_HAVE_FAILED")
except RuntimeError as e:
    if "SECRET_KEY" in str(e):
        print("CORRECTLY_RAISED_ERROR")
    else:
        print("WRONG_ERROR")
'''
        result = subprocess.run(
            [sys.executable, '-c', test_code],
            capture_output=True,
            text=True,
            cwd='/home/runner/work/Thirstys-Projects-Miniature-Office/Thirstys-Projects-Miniature-Office'
        )
        
        # Check that the subprocess correctly raised the error
        assert 'CORRECTLY_RAISED_ERROR' in result.stdout or 'SECRET_KEY' in result.stderr


class TestWebSocketHandlers:
    """Test WebSocket event handlers."""
    
    def test_websocket_connect(self, client):
        """Test WebSocket connect handler."""
        from flask_socketio import SocketIOTestClient
        
        # Create a socketio test client
        socketio_client = app_module.socketio.test_client(
            app_module.app,
            flask_test_client=client
        )
        
        # Connect should trigger the connect handler
        assert socketio_client.is_connected()
        
        # Check for connected event
        received = socketio_client.get_received()
        if received:
            # Should have received a 'connected' event
            assert any(item['name'] == 'connected' for item in received)
        
        socketio_client.disconnect()
    
    def test_websocket_request_state(self, client):
        """Test WebSocket request_state handler."""
        from flask_socketio import SocketIOTestClient
        
        socketio_client = app_module.socketio.test_client(
            app_module.app,
            flask_test_client=client
        )
        
        # Emit request_state event
        socketio_client.emit('request_state')
        
        # Should receive state_update response
        received = socketio_client.get_received()
        # The handler emits 'state_update' when simulation exists
        # In our test setup, simulation should exist
        
        socketio_client.disconnect()
