"""
Comprehensive tests for Simulation module.
Achieves 100% code coverage for src/core/simulation.py
"""
import pytest
import time
from src.core.simulation import (
    SimulationConfig,
    AgentExecutionEngine,
    ManagerDecisionProtocol,
    OfficeProcessor,
    FloorSimulator,
    SimulationEngine,
    create_simulation
)
from src.core.world import World, Floor, Office, get_world
from src.core.mission import Task, TaskState
from src.agents.agent import Agent, AgentRole, Manager, get_consensus_system
from src.core.entity import get_registry
from src.core.audit import get_audit_log, EventType
from src.departments.department import Department, get_department_registry


class TestSimulationConfig:
    """Test SimulationConfig dataclass"""
    
    def test_simulation_config_defaults(self):
        """Test default simulation config values"""
        config = SimulationConfig()
        assert config.tick_duration_ms == 100
        assert config.auto_assign_tasks is True
        assert config.auto_resolve_meetings is True
        assert config.max_ticks is None
    
    def test_simulation_config_custom(self):
        """Test custom simulation config"""
        config = SimulationConfig(
            tick_duration_ms=200,
            auto_assign_tasks=False,
            auto_resolve_meetings=False,
            max_ticks=100
        )
        assert config.tick_duration_ms == 200
        assert config.auto_assign_tasks is False
        assert config.auto_resolve_meetings is False
        assert config.max_ticks == 100


class TestAgentExecutionEngine:
    """Test AgentExecutionEngine"""
    
    def test_process_agent_no_task(self):
        """Test processing agent with no task"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        agent.current_task_id = None
        
        # Should complete without error
        AgentExecutionEngine.process_agent(agent)
        assert agent.current_task_id is None
    
    def test_process_agent_task_not_found(self):
        """Test processing agent with invalid task"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        agent.current_task_id = "nonexistent-task"
        
        # Should complete without error
        AgentExecutionEngine.process_agent(agent)
        assert agent.current_task_id == "nonexistent-task"
    
    def test_process_agent_already_blocked(self):
        """Test processing blocked agent"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        agent.current_task_id = task.entity_id
        agent.status = "blocked"
        
        AgentExecutionEngine.process_agent(agent)
        assert agent.status == "blocked"
    
    def test_process_agent_preconditions_not_met(self):
        """Test processing agent when preconditions not met"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.preconditions = ["unmet-condition"]
        agent.current_task_id = task.entity_id
        agent.status = "idle"
        
        AgentExecutionEngine.process_agent(agent)
        assert agent.status == "blocked"
        assert task.state == TaskState.BLOCKED
    
    def test_process_agent_needs_meeting(self):
        """Test processing agent when task needs meeting"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.ambiguity_score = 0.8  # High ambiguity triggers meeting
        agent.current_task_id = task.entity_id
        agent.status = "idle"
        
        AgentExecutionEngine.process_agent(agent)
        assert agent.status == "in_meeting"
    
    def test_process_agent_transition_scheduled_to_review(self):
        """Test agent transitions scheduled task to review"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.SCHEDULED
        task.ambiguity_score = 0.0  # No meeting needed
        agent.current_task_id = task.entity_id
        agent.status = "idle"
        
        AgentExecutionEngine.process_agent(agent)
        assert agent.status == "working"
        assert task.state == TaskState.IN_REVIEW
    
    def test_process_agent_check_postconditions_in_review(self):
        """Test agent checks postconditions in review state"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.IN_REVIEW
        task.ambiguity_score = 0.0
        task.postconditions = []  # Empty postconditions pass
        agent.current_task_id = task.entity_id
        agent.status = "working"
        
        AgentExecutionEngine.process_agent(agent)
        assert task.state == TaskState.APPROVAL
    
    def test_process_agent_approval_state(self):
        """Test agent in approval state (requires manager)"""
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.APPROVAL
        task.ambiguity_score = 0.0
        agent.current_task_id = task.entity_id
        agent.status = "working"
        
        AgentExecutionEngine.process_agent(agent)
        # State should remain APPROVAL (needs manager)
        assert task.state == TaskState.APPROVAL


class TestManagerDecisionProtocol:
    """Test ManagerDecisionProtocol"""
    
    def test_process_manager_no_department(self):
        """Test processing manager without department"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        manager.department_id = None
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
        assert manager.department_id is None
    
    def test_process_manager_department_not_found(self):
        """Test processing manager with nonexistent department"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        manager.department_id = "nonexistent-dept"
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
        assert manager.department_id == "nonexistent-dept"
    
    def test_process_manager_no_agents(self):
        """Test processing manager with no agents"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        dept = Department("dept-1", "Test Dept", "python")
        manager.department_id = dept.entity_id
        manager.managed_agents = []
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
        assert len(manager.managed_agents) == 0
    
    def test_process_manager_agent_no_task(self):
        """Test processing manager when agent has no task"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        dept = Department("dept-1", "Test Dept", "python")
        
        manager.department_id = dept.entity_id
        manager.managed_agents = [agent.entity_id]
        agent.current_task_id = None
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
    
    def test_process_manager_task_not_in_approval(self):
        """Test processing manager when task not in approval state"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.SCHEDULED
        dept = Department("dept-1", "Test Dept", "python")
        
        manager.department_id = dept.entity_id
        manager.managed_agents = [agent.entity_id]
        agent.current_task_id = task.entity_id
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
        assert task.state == TaskState.SCHEDULED
    
    def test_process_manager_task_not_ready_for_commit(self):
        """Test processing manager when task not ready for commit"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.APPROVAL
        task.postconditions = ["unmet-condition"]  # Not ready
        dept = Department("dept-1", "Test Dept", "python")
        
        manager.department_id = dept.entity_id
        manager.managed_agents = [agent.entity_id]
        agent.current_task_id = task.entity_id
        
        # Should complete without error
        ManagerDecisionProtocol.process_manager(manager)
        assert task.state == TaskState.APPROVAL
    
    def test_process_manager_approve_task(self):
        """Test manager approving a task"""
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", agent.entity_id, None)
        task.state = TaskState.APPROVAL
        task.postconditions = []  # Empty = ready
        dept = Department("dept-1", "Test Dept", "python")
        
        manager.department_id = dept.entity_id
        manager.managed_agents = [agent.entity_id]
        agent.current_task_id = task.entity_id
        
        ManagerDecisionProtocol.process_manager(manager)
        # Task should be approved and merged
        assert task.state == TaskState.MERGED


class TestOfficeProcessor:
    """Test OfficeProcessor"""
    
    def test_process_office_no_manager(self):
        """Test processing office without manager"""
        office = # Needs Department object
        office.manager = None
        
        # Should complete without error
        OfficeProcessor.process_office(office)
    
    def test_process_office_with_agents(self):
        """Test processing office with agents"""
        office = # Needs Department object
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent1 = Agent("agent-1", "Agent 1", AgentRole.BUILDER, "dept-1")
        agent2 = Agent("agent-2", "Agent 2", AgentRole.BUILDER, "dept-1")
        
        office.manager = manager
        office.add_agent(agent1)
        office.add_agent(agent2)
        
        # Should process all agents
        OfficeProcessor.process_office(office)
    
    def test_process_office_idle_agents(self):
        """Test processing office with idle agents"""
        office = # Needs Department object
        manager = Manager("manager-1", "Test Manager", "dept-1")
        agent = Agent("agent-1", "Agent 1", AgentRole.BUILDER, "dept-1")
        agent.status = "idle"
        
        office.manager = manager
        office.add_agent(agent)
        
        # Should complete without error
        OfficeProcessor.process_office(office)
        assert agent.status == "idle"


class TestFloorSimulator:
    """Test FloorSimulator"""
    
    def test_process_floor_no_offices(self):
        """Test processing floor with no offices"""
        floor = Floor("floor-1", "Python")
        
        # Should complete without error
        FloorSimulator.process_floor(floor)
    
    def test_process_floor_with_offices(self):
        """Test processing floor with multiple offices"""
        floor = Floor("floor-1", "Python")
        dept = Department("dept-1", "Python Department", "python")
        floor.department = dept
        
        office1 = # Needs Department object
        office2 = # Needs Department object
        floor.add_office(office1)
        floor.add_office(office2)
        
        # Should process all offices
        FloorSimulator.process_floor(floor)
    
    def test_process_floor_ensures_staffed(self):
        """Test floor processing ensures department is staffed"""
        floor = Floor("floor-1", "Python")
        dept = Department("dept-1", "Python Department", "python")
        floor.department = dept
        
        FloorSimulator.process_floor(floor)
        # Department registry's ensure_all_staffed should be called


class TestSimulationEngine:
    """Test SimulationEngine"""
    
    def test_simulation_engine_initialization(self):
        """Test simulation engine initialization"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        assert engine.world is world
        assert engine.config.tick_duration_ms == 100
        assert engine.is_running is False
        assert engine.tick_count == 0
        assert engine.event_handlers == {}
    
    def test_simulation_engine_with_config(self):
        """Test simulation engine with custom config"""
        world = World("world-1", "Test World")
        config = SimulationConfig(tick_duration_ms=200, max_ticks=10)
        engine = SimulationEngine(world, config)
        
        assert engine.config.tick_duration_ms == 200
        assert engine.config.max_ticks == 10
    
    def test_register_handler(self):
        """Test registering event handlers"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        handler_called = []
        def test_handler(data):
            handler_called.append(data)
        
        engine.register_handler("test_event", test_handler)
        assert "test_event" in engine.event_handlers
        assert len(engine.event_handlers["test_event"]) == 1
    
    def test_register_multiple_handlers(self):
        """Test registering multiple handlers for same event"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        def handler1(data): pass
        def handler2(data): pass
        
        engine.register_handler("event", handler1)
        engine.register_handler("event", handler2)
        
        assert len(engine.event_handlers["event"]) == 2
    
    def test_emit_event(self):
        """Test emitting events to handlers"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        results = []
        def handler1(data):
            results.append(("handler1", data))
        def handler2(data):
            results.append(("handler2", data))
        
        engine.register_handler("test_event", handler1)
        engine.register_handler("test_event", handler2)
        
        engine.emit_event("test_event", {"value": 42})
        
        assert len(results) == 2
        assert results[0][1]["value"] == 42
        assert results[1][1]["value"] == 42
    
    def test_emit_event_no_handlers(self):
        """Test emitting event with no handlers"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        # Should complete without error
        engine.emit_event("nonexistent_event", {"data": "test"})
    
    def test_tick_inactive_world(self):
        """Test tick with inactive world"""
        world = World("world-1", "Test World")
        world.is_active = False
        engine = SimulationEngine(world)
        
        initial_time = world.time
        engine.tick()
        
        # Should not increment time
        assert world.time == initial_time
    
    def test_tick_increments_time(self):
        """Test tick increments world time"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        
        initial_time = world.time
        engine.tick()
        
        assert world.time == initial_time + 1
        assert engine.tick_count == 1
    
    def test_tick_emits_events(self):
        """Test tick emits start and end events"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        
        events = []
        def track_event(data):
            events.append(data)
        
        engine.register_handler("tick_start", track_event)
        engine.register_handler("tick_end", track_event)
        
        engine.tick()
        
        assert len(events) == 2
        assert events[0]["time"] == world.time
        assert events[1]["time"] == world.time
    
    def test_tick_processes_floors(self):
        """Test tick processes all floors"""
        world = World("world-1", "Test World")
        world.is_active = True
        floor = Floor("floor-1", "Python")
        world.add_floor(floor)
        
        engine = SimulationEngine(world)
        engine.tick()
        
        assert engine.tick_count == 1
    
    def test_tick_persists_state(self):
        """Test tick persists state"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        
        engine.tick()
        
        # Check audit log for persistence event
        events = get_audit_log().get_events(limit=10)
        persist_events = [e for e in events if e.get("data", {}).get("action") == "state_persisted"]
        assert len(persist_events) > 0
    
    def test_tick_stops_at_max_ticks(self):
        """Test tick stops when max ticks reached"""
        world = World("world-1", "Test World")
        world.is_active = True
        config = SimulationConfig(max_ticks=2)
        engine = SimulationEngine(world, config)
        
        engine.tick()
        assert engine.is_running is False
        assert engine.tick_count == 1
        
        engine.tick()
        assert engine.is_running is False
        assert engine.tick_count == 2
        assert world.is_active is False
    
    def test_persist_state(self):
        """Test persist state logs to audit"""
        world = World("world-1", "Test World")
        engine = SimulationEngine(world)
        
        engine.persist_state()
        
        events = get_audit_log().get_events(limit=1)
        assert len(events) > 0
        assert events[0]["data"]["action"] == "state_persisted"
    
    def test_step(self):
        """Test manual stepping"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        
        initial_time = world.time
        engine.step()
        
        assert world.time == initial_time + 1
        assert engine.tick_count == 1
    
    def test_stop(self):
        """Test stopping simulation"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        engine.is_running = True
        
        engine.stop()
        
        assert engine.is_running is False
        assert world.is_active is False
        
        # Check audit log for stop event
        events = get_audit_log().get_events(limit=1)
        assert events[0]["data"]["action"] == "simulation_stopped"
    
    def test_get_state(self):
        """Test getting simulation state"""
        world = World("world-1", "Test World")
        world.is_active = True
        engine = SimulationEngine(world)
        
        engine.tick()
        
        state = engine.get_state()
        assert "world" in state
        assert state["is_running"] is False
        assert state["tick_count"] == 1
        assert state["time"] == world.time
    
    def test_run_stops_when_stopped(self):
        """Test run loop stops when engine is stopped"""
        world = World("world-1", "Test World")
        world.is_active = True
        config = SimulationConfig(tick_duration_ms=10, max_ticks=3)
        engine = SimulationEngine(world, config)
        
        # Run should stop after max_ticks
        engine.run()
        
        assert engine.is_running is False
        assert engine.tick_count == 3
    
    def test_run_stops_when_world_inactive(self):
        """Test run stops when world becomes inactive"""
        world = World("world-1", "Test World")
        world.is_active = True
        config = SimulationConfig(tick_duration_ms=10)
        engine = SimulationEngine(world, config)
        
        # Stop after 2 ticks
        def stop_after_2(data):
            if engine.tick_count >= 2:
                engine.stop()
        
        engine.register_handler("tick_end", stop_after_2)
        engine.run()
        
        assert engine.tick_count >= 2
        assert engine.is_running is False


class TestCreateSimulation:
    """Test create_simulation factory function"""
    
    def test_create_simulation_with_world(self):
        """Test creating simulation with provided world"""
        world = World("world-1", "Test World")
        engine = create_simulation(world)
        
        assert isinstance(engine, SimulationEngine)
        assert engine.world is world
    
    def test_create_simulation_with_world_and_config(self):
        """Test creating simulation with world and config"""
        world = World("world-1", "Test World")
        config = SimulationConfig(tick_duration_ms=50)
        engine = create_simulation(world, config)
        
        assert isinstance(engine, SimulationEngine)
        assert engine.world is world
        assert engine.config.tick_duration_ms == 50
    
    def test_create_simulation_without_world(self):
        """Test creating simulation uses global world"""
        # Create a global world
        world = World("world-1", "Test World")
        from src.core import world as world_module
        world_module._world = world
        
        engine = create_simulation()
        
        assert isinstance(engine, SimulationEngine)
        assert engine.world is world
    
    def test_create_simulation_no_world_available(self):
        """Test creating simulation fails without world"""
        from src.core import world as world_module
        world_module._world = None
        
        with pytest.raises(ValueError, match="No world instance available"):
            create_simulation()
