"""
REST API Server for Simulation Control
Implements Simulation API Endpoints (Section II.10)
"""
from flask import Flask, jsonify, request, send_from_directory
from flask_socketio import SocketIO, emit
from typing import Dict, Optional
import json
import os

from src.core.world import World, Floor, Office, create_world, get_world
from src.core.simulation import SimulationEngine, create_simulation, SimulationConfig
from src.core.entity import get_registry
from src.core.audit import get_audit_log
from src.agents.agent import Agent, Manager, AgentRole, CapabilityProfile
from src.departments.department import Department, get_department_registry
from src.core.mission import Task, DirectiveLevel, TaskState
from src.tools.supply_store import get_supply_store, Tool, ToolMetadata, ToolTag
from src.interfaces.contract import get_elevator_protocol, Contract, APIEndpoint, VersionBoundary
from src.core.consigliere import get_consigliere, ExplanationType, TranslationType
from src.core.head_of_security import get_head_of_security, ThreatLevel
from src.core.floor_specifications import get_all_floors, get_floor_specification, ProgrammingLanguage


app = Flask(__name__)
app.config['SECRET_KEY'] = 'miniature-office-secret'
socketio = SocketIO(app, cors_allowed_origins="*")

# Get client directory path
CLIENT_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'client')

# Global simulation instance
simulation: Optional[SimulationEngine] = None


def init_simulation():
    """Initialize the simulation with a default world"""
    global simulation
    
    # Create world
    world = create_world("world-001", "Miniature Office IDE")
    
    # Create Python floor
    python_floor = Floor("floor-python", "Python")
    world.add_floor(python_floor)
    
    # Create Python department
    python_dept = Department("dept-python", "Python Development", "Python")
    dept_registry = get_department_registry()
    dept_registry.register_department(python_dept)
    python_floor.department = python_dept
    
    # Create an office
    office1 = Office("office-1", python_dept)
    python_floor.add_office(office1)
    
    # Create manager
    manager = Manager("mgr-001", "Alice Manager", python_dept.entity_id)
    office1.set_manager(manager)
    python_dept.add_agent(manager)
    
    # The department auto-spawns assistant agents for missing roles
    # which should have already happened during registration
    
    # Add some tools to supply store
    supply_store = get_supply_store()
    
    python_tool = Tool(
        "tool-python",
        "Python Interpreter",
        ToolMetadata(
            tag=ToolTag.COMPILER,
            version="3.11.0",
            trust_score=0.9,
            security_rating=4,
            capabilities={"python", "scripting"}
        )
    )
    supply_store.add_tool(python_tool)
    
    pytest_tool = Tool(
        "tool-pytest",
        "PyTest Framework",
        ToolMetadata(
            tag=ToolTag.TEST_FRAMEWORK,
            version="7.4.0",
            trust_score=0.95,
            security_rating=4,
            capabilities={"testing", "python"}
        )
    )
    supply_store.add_tool(pytest_tool)
    
    # Create a sample task
    task1 = Task(
        "task-001",
        "Implement User Authentication",
        "Create secure user authentication system",
        None,
        None
    )
    task1.add_precondition("Requirements document exists")
    task1.add_postcondition("Authentication module is implemented")
    task1.add_acceptance_criterion("Unit tests pass")
    task1.add_acceptance_criterion("Security review complete")
    
    # Create JavaScript floor
    js_floor = Floor("floor-javascript", "JavaScript")
    world.add_floor(js_floor)
    
    js_dept = Department("dept-javascript", "Frontend Development", "JavaScript")
    dept_registry.register_department(js_dept)
    js_floor.department = js_dept
    
    # Create simulation
    simulation = create_simulation(world, SimulationConfig(
        tick_duration_ms=1000,  # 1 second per tick
        auto_assign_tasks=True
    ))
    
    # Register event handlers to broadcast via WebSocket
    simulation.register_handler('tick_start', lambda data: socketio.emit('tick_start', data))
    simulation.register_handler('tick_end', lambda data: socketio.emit('tick_end', data))
    
    return simulation


@app.route('/')
def index():
    """Serve the main HTML page"""
    return send_from_directory(CLIENT_DIR, 'index.html')


@app.route('/api')
def api_index():
    """API documentation"""
    return jsonify({
        'name': 'Miniature Office - Cognitive IDE',
        'version': '0.1.0',
        'description': 'A spatialized, agent-orchestrated development environment',
        'endpoints': {
            'GET /': 'Main application interface',
            'GET /api': 'API documentation',
            'GET /api/world/state': 'Get current world state',
            'POST /api/world/step': 'Advance simulation by one tick',
            'POST /api/world/start': 'Start continuous simulation',
            'POST /api/world/stop': 'Stop simulation',
            'GET /api/agents/<id>/status': 'Get agent status',
            'GET /api/agents': 'List all agents',
            'GET /api/tasks/<id>': 'Get task details',
            'GET /api/tasks': 'List all tasks',
            'GET /api/contracts': 'List all contracts',
            'GET /api/departments': 'List all departments',
            'GET /api/audit/events': 'Get audit events',
            'GET /api/supply-store': 'Get supply store inventory',
            'GET /api/consigliere': 'Get Consigliere state',
            'POST /api/consigliere/explain': 'Request explanation from Consigliere',
            'POST /api/consigliere/translate': 'Request translation from Consigliere',
            'POST /api/consigliere/preview': 'Preview consequences',
            'POST /api/consigliere/draft': 'Prepare draft directive',
            'GET /api/security': 'Get Head of Security state',
            'POST /api/security/grant': 'Grant permission',
            'POST /api/security/revoke': 'Revoke permission',
            'POST /api/security/audit': 'Trigger security audit',
            'POST /api/security/lockdown': 'Trigger lockdown',
            'POST /api/security/block': 'Block delivery',
            'GET /api/floors': 'List all floor specifications',
            'GET /api/floors/<language>': 'Get specific floor specification'
        }
    })


# ===== World Endpoints =====

@app.route('/api/world/state', methods=['GET'])
def get_world_state():
    """
    Get current world state.
    Endpoint: /world/state
    """
    if not simulation:
        return jsonify({'error': 'Simulation not initialized'}), 500
    
    return jsonify(simulation.get_state())


@app.route('/api/world/step', methods=['POST'])
def step_simulation():
    """
    Advance simulation by one tick.
    Endpoint: /world/step
    """
    if not simulation:
        return jsonify({'error': 'Simulation not initialized'}), 500
    
    simulation.step()
    
    return jsonify({
        'success': True,
        'time': simulation.world.time,
        'tick_count': simulation.tick_count
    })


@app.route('/api/world/start', methods=['POST'])
def start_simulation():
    """Start continuous simulation"""
    if not simulation:
        return jsonify({'error': 'Simulation not initialized'}), 500
    
    if simulation.is_running:
        return jsonify({'error': 'Simulation already running'}), 400
    
    # Start in background thread
    socketio.start_background_task(simulation.run)
    
    return jsonify({'success': True, 'message': 'Simulation started'})


@app.route('/api/world/stop', methods=['POST'])
def stop_simulation():
    """Stop simulation"""
    if not simulation:
        return jsonify({'error': 'Simulation not initialized'}), 500
    
    simulation.stop()
    
    return jsonify({'success': True, 'message': 'Simulation stopped'})


# ===== Agent Endpoints =====

@app.route('/api/agents/<agent_id>/status', methods=['GET'])
def get_agent_status(agent_id: str):
    """
    Get agent status.
    Endpoint: /agents/{id}/status
    """
    registry = get_registry()
    agent = registry.get(agent_id)
    
    if not agent or not isinstance(agent, Agent):
        return jsonify({'error': 'Agent not found'}), 404
    
    return jsonify({
        'agent_id': agent.entity_id,
        'name': agent.name,
        'role': agent.role.value,
        'status': agent.status,
        'department_id': agent.department_id,
        'current_task_id': agent.current_task_id,
        'capabilities': agent.capabilities.to_dict(),
        'task_history': agent.task_history
    })


@app.route('/api/agents', methods=['GET'])
def list_agents():
    """List all agents"""
    from src.core.entity import EntityType
    registry = get_registry()
    agents = registry.get_by_type(EntityType.AGENT)
    
    return jsonify({
        'agents': [
            {
                'agent_id': a.entity_id,
                'name': a.name,
                'role': a.role.value if hasattr(a, 'role') else 'unknown',
                'status': a.status if hasattr(a, 'status') else 'unknown'
            }
            for a in agents if isinstance(a, Agent)
        ]
    })


# ===== Task Endpoints =====

@app.route('/api/tasks/<task_id>', methods=['GET'])
def get_task(task_id: str):
    """
    Get task details.
    Endpoint: /tasks/{id}
    """
    registry = get_registry()
    task = registry.get(task_id)
    
    if not task or not isinstance(task, Task):
        return jsonify({'error': 'Task not found'}), 404
    
    return jsonify({
        'task_id': task.entity_id,
        'name': task.name,
        'description': task.description,
        'state': task.state.value,
        'assigned_agent_id': task.assigned_agent_id,
        'ambiguity_score': task.ambiguity_score,
        'blocked_reason': task.blocked_reason,
        'preconditions': [
            {'description': c.description, 'is_satisfied': c.is_satisfied}
            for c in task.preconditions
        ],
        'postconditions': [
            {'description': c.description, 'is_satisfied': c.is_satisfied}
            for c in task.postconditions
        ],
        'acceptance_criteria': [
            {'description': c.description, 'is_met': c.is_met}
            for c in task.acceptance_criteria
        ],
        'state_history': task.state_history
    })


@app.route('/api/tasks', methods=['GET'])
def list_tasks():
    """List all tasks"""
    from src.core.entity import EntityType
    registry = get_registry()
    # Tasks are stored as ARTIFACT entities
    all_entities = registry.get_by_type(EntityType.ARTIFACT)
    tasks = [e for e in all_entities if isinstance(e, Task)]
    
    return jsonify({
        'tasks': [
            {
                'task_id': t.entity_id,
                'name': t.name,
                'state': t.state.value,
                'assigned_agent_id': t.assigned_agent_id
            }
            for t in tasks
        ]
    })


# ===== Contract Endpoints =====

@app.route('/api/contracts', methods=['GET'])
def list_contracts():
    """List all contracts"""
    elevator = get_elevator_protocol()
    
    return jsonify({
        'contracts': [
            c.to_dict()
            for c in elevator.contracts.values()
        ]
    })


# ===== Department Endpoints =====

@app.route('/api/departments', methods=['GET'])
def list_departments():
    """List all departments"""
    dept_registry = get_department_registry()
    
    return jsonify({
        'departments': [
            {
                'department_id': d.entity_id,
                'name': d.name,
                'domain': d.domain,
                'is_fully_staffed': d.is_fully_staffed(),
                'missing_roles': [r.value for r in d.get_missing_roles()]
            }
            for d in dept_registry.departments.values()
        ]
    })


# ===== Audit Endpoints =====

@app.route('/api/audit/events', methods=['GET'])
def get_audit_events():
    """Get audit events"""
    audit_log = get_audit_log()
    
    # Get query parameters
    event_type = request.args.get('type')
    actor_id = request.args.get('actor_id')
    target_id = request.args.get('target_id')
    limit = int(request.args.get('limit', 100))
    
    # Get events based on filters
    if event_type:
        from src.core.audit import EventType
        events = audit_log.get_events_by_type(EventType(event_type))
    elif actor_id:
        events = audit_log.get_events_by_actor(actor_id)
    elif target_id:
        events = audit_log.get_events_by_target(target_id)
    else:
        # Get all events
        events = list(audit_log.graph.events.values())
    
    # Limit results
    events = sorted(events, key=lambda e: e.timestamp, reverse=True)[:limit]
    
    return jsonify({
        'events': [e.to_dict() for e in events]
    })


# ===== Supply Store Endpoints =====

@app.route('/api/supply-store', methods=['GET'])
def get_supply_store_inventory():
    """Get supply store inventory"""
    store = get_supply_store()
    
    return jsonify({
        'tools': [t.to_dict() for t in store.tools.values()],
        'available_count': len(store.get_available_tools()),
        'checked_out_count': len([t for t in store.tools.values() if not t.is_available])
    })


# ===== Consigliere Endpoints (Advisory Executive) =====

@app.route('/api/consigliere', methods=['GET'])
def get_consigliere_state():
    """Get Consigliere state and capabilities"""
    consigliere = get_consigliere()
    return jsonify(consigliere.to_dict())


@app.route('/api/consigliere/explain', methods=['POST'])
def consigliere_explain():
    """
    Request explanation from Consigliere.
    
    Body: {
        "type": "why_blocked" | "why_decision" | "what_options",
        "entity_id": "...",  # for why_blocked, why_decision
        "situation": "...",  # for what_options
        "context": {}  # optional
    }
    """
    data = request.get_json()
    consigliere = get_consigliere()
    
    explanation_type = data.get('type')
    entity_id = data.get('entity_id')
    situation = data.get('situation')
    context = data.get('context', {})
    
    if explanation_type == 'why_blocked':
        if not entity_id:
            return jsonify({'error': 'entity_id required for why_blocked'}), 400
        explanation = consigliere.explain_why_blocked(entity_id, context)
    elif explanation_type == 'why_decision':
        if not entity_id:
            return jsonify({'error': 'entity_id required for why_decision'}), 400
        explanation = consigliere.explain_why_decision(entity_id, context)
    elif explanation_type == 'what_options':
        if not situation:
            return jsonify({'error': 'situation required for what_options'}), 400
        explanation = consigliere.explain_what_options(situation, context)
    else:
        return jsonify({'error': f'Unknown explanation type: {explanation_type}'}), 400
    
    return jsonify(explanation.to_dict())


@app.route('/api/consigliere/translate', methods=['POST'])
def consigliere_translate():
    """
    Request translation from Consigliere.
    
    Body: {
        "text": "civilizational text to translate"
    }
    """
    data = request.get_json()
    text = data.get('text')
    
    if not text:
        return jsonify({'error': 'text required'}), 400
    
    consigliere = get_consigliere()
    translation = consigliere.translate_civilization_to_human(text)
    
    return jsonify(translation.to_dict())


@app.route('/api/consigliere/preview', methods=['POST'])
def consigliere_preview():
    """
    Preview consequences of an action.
    
    Body: {
        "action": "proposed action description",
        "context": {}  # optional
    }
    """
    data = request.get_json()
    action = data.get('action')
    context = data.get('context', {})
    
    if not action:
        return jsonify({'error': 'action required'}), 400
    
    consigliere = get_consigliere()
    preview = consigliere.preview_consequences(action, context)
    
    return jsonify(preview.to_dict())


@app.route('/api/consigliere/draft', methods=['POST'])
def consigliere_draft():
    """
    Prepare draft directive.
    
    Body: {
        "goal": "what to accomplish",
        "language": "python" | "rust" | etc.,
        "constraints": []  # optional
    }
    """
    data = request.get_json()
    goal = data.get('goal')
    language = data.get('language')
    constraints = data.get('constraints', [])
    
    if not goal or not language:
        return jsonify({'error': 'goal and language required'}), 400
    
    consigliere = get_consigliere()
    draft = consigliere.prepare_draft_directive(goal, language, constraints)
    
    return jsonify(draft.to_dict())


# ===== Head of Security Endpoints (Security Sovereign) =====

@app.route('/api/security', methods=['GET'])
def get_security_state():
    """Get Head of Security state and capabilities"""
    security = get_head_of_security()
    return jsonify(security.to_dict())


@app.route('/api/security/grant', methods=['POST'])
def security_grant():
    """
    Grant permission.
    
    Body: {
        "entity_id": "...",
        "resource": "tool_name or operation",
        "justification": "why this is needed",
        "expires_in_hours": 24  # optional
    }
    """
    data = request.get_json()
    entity_id = data.get('entity_id')
    resource = data.get('resource')
    justification = data.get('justification')
    expires_in_hours = data.get('expires_in_hours')
    
    if not entity_id or not resource or not justification:
        return jsonify({'error': 'entity_id, resource, and justification required'}), 400
    
    security = get_head_of_security()
    permission = security.grant_tool_access(entity_id, resource, justification, expires_in_hours)
    
    return jsonify(permission.to_dict())


@app.route('/api/security/revoke', methods=['POST'])
def security_revoke():
    """
    Revoke permission.
    
    Body: {
        "permission_id": "...",
        "reason": "why revoking"
    }
    """
    data = request.get_json()
    permission_id = data.get('permission_id')
    reason = data.get('reason')
    
    if not permission_id or not reason:
        return jsonify({'error': 'permission_id and reason required'}), 400
    
    security = get_head_of_security()
    success = security.revoke_access(permission_id, reason)
    
    return jsonify({'success': success})


@app.route('/api/security/audit', methods=['POST'])
def security_audit():
    """
    Trigger security audit.
    
    Body: {
        "type": "full_system" | "floor" | "cross_floor",
        "scope": [],  # for floor or cross_floor
        "reason": "why auditing"
    }
    """
    data = request.get_json()
    audit_type = data.get('type', 'full_system')
    scope = data.get('scope', [])
    reason = data.get('reason', 'Manual audit trigger')
    
    security = get_head_of_security()
    
    if audit_type == 'full_system':
        audit = security.trigger_full_audit(reason)
    elif audit_type == 'cross_floor':
        audit = security.trigger_cross_floor_review(scope, reason)
    else:
        return jsonify({'error': f'Unknown audit type: {audit_type}'}), 400
    
    return jsonify(audit.to_dict())


@app.route('/api/security/lockdown', methods=['POST'])
def security_lockdown():
    """
    Trigger lockdown.
    
    Body: {
        "scope": "building" | "floor:<floor_id>",
        "reason": "emergency reason",
        "threat_level": "low" | "medium" | "high" | "critical"
    }
    """
    data = request.get_json()
    scope = data.get('scope')
    reason = data.get('reason')
    threat_level_str = data.get('threat_level', 'medium')
    
    if not scope or not reason:
        return jsonify({'error': 'scope and reason required'}), 400
    
    threat_level = ThreatLevel(threat_level_str)
    security = get_head_of_security()
    
    if scope == 'building':
        lockdown = security.freeze_building(reason, threat_level)
    elif scope.startswith('floor:'):
        floor_id = scope.split(':', 1)[1]
        lockdown = security.trigger_floor_lockdown(floor_id, reason, threat_level)
    else:
        return jsonify({'error': f'Invalid scope: {scope}'}), 400
    
    return jsonify(lockdown.to_dict())


@app.route('/api/security/block', methods=['POST'])
def security_block():
    """
    Block delivery.
    
    Body: {
        "artifact_id": "...",
        "reason": "security concern",
        "required_mitigations": []
    }
    """
    data = request.get_json()
    artifact_id = data.get('artifact_id')
    reason = data.get('reason')
    required_mitigations = data.get('required_mitigations', [])
    
    if not artifact_id or not reason:
        return jsonify({'error': 'artifact_id and reason required'}), 400
    
    security = get_head_of_security()
    block = security.block_delivery(artifact_id, reason, required_mitigations)
    
    return jsonify(block.to_dict())


@app.route('/api/security/lockdowns', methods=['GET'])
def get_active_lockdowns():
    """Get active lockdowns"""
    security = get_head_of_security()
    return jsonify({'lockdowns': security.get_active_lockdowns()})


@app.route('/api/security/blocked', methods=['GET'])
def get_blocked_deliveries():
    """Get blocked deliveries"""
    security = get_head_of_security()
    return jsonify({'blocked_deliveries': security.get_blocked_deliveries()})


@app.route('/api/security/permissions', methods=['GET'])
def get_active_permissions():
    """Get active permissions"""
    entity_id = request.args.get('entity_id')
    security = get_head_of_security()
    return jsonify({'permissions': security.get_active_permissions(entity_id)})


# ===== Floor Specification Endpoints =====

@app.route('/api/floors', methods=['GET'])
def list_floors():
    """List all floor specifications"""
    floors = get_all_floors()
    return jsonify({
        'floors': [floor.to_dict() for floor in floors]
    })


@app.route('/api/floors/<language>', methods=['GET'])
def get_floor(language: str):
    """Get specific floor specification"""
    try:
        lang_enum = ProgrammingLanguage(language.lower())
        floor = get_floor_specification(lang_enum)
        return jsonify(floor.to_dict())
    except ValueError:
        return jsonify({'error': f'Unknown language: {language}'}), 404


# ===== WebSocket Events =====

@socketio.on('connect')
def handle_connect():
    """Handle client connection"""
    emit('connected', {'message': 'Connected to Miniature Office simulation'})


@socketio.on('request_state')
def handle_request_state():
    """Handle state request via WebSocket"""
    if simulation:
        emit('state_update', simulation.get_state())


def run_server(host='0.0.0.0', port=5000, debug=False):
    """Run the API server"""
    # Initialize simulation on startup
    init_simulation()
    
    print(f"Starting Miniature Office API Server on {host}:{port}")
    print(f"Simulation initialized with world: {simulation.world.name}")
    
    socketio.run(app, host=host, port=port, debug=debug, allow_unsafe_werkzeug=True)


if __name__ == '__main__':
    run_server(debug=True)
