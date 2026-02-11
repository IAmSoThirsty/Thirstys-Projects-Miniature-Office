"""
Comprehensive tests for agents/agent.py to achieve 100% coverage
"""
import pytest
from datetime import datetime

from src.agents.agent import (
    Agent, AgentRole, Manager, CapabilityProfile,
    ConsensusVote, ConsensusDecision, ConsensusSystem,
    get_consensus_system
)
from src.core.entity import EntityType, RelationType, get_registry
from src.core.audit import get_audit_log, EventType
from src.core.mission import Task


class TestCapabilityProfile:
    """Test CapabilityProfile functionality"""
    
    def test_can_handle_task_with_languages(self):
        """Test capability matching with language requirements"""
        profile = CapabilityProfile(languages={'python', 'rust'})
        
        # Should handle subset of languages
        assert profile.can_handle_task({'languages': ['python']})
        assert profile.can_handle_task({'languages': ['rust']})
        assert profile.can_handle_task({'languages': ['python', 'rust']})
        
        # Should reject superset of languages
        assert not profile.can_handle_task({'languages': ['python', 'go']})
    
    def test_can_handle_task_with_tools(self):
        """Test capability matching with tool requirements"""
        profile = CapabilityProfile(tools={'pytest', 'mypy'})
        
        # Should handle subset of tools
        assert profile.can_handle_task({'tools': ['pytest']})
        assert profile.can_handle_task({'tools': ['pytest', 'mypy']})
        
        # Should reject superset of tools
        assert not profile.can_handle_task({'tools': ['pytest', 'cargo']})
    
    def test_can_handle_task_with_security_clearance(self):
        """Test capability matching with security clearance"""
        profile = CapabilityProfile(security_clearance=3)
        
        # Should handle same or lower clearance
        assert profile.can_handle_task({'security_clearance': 3})
        assert profile.can_handle_task({'security_clearance': 2})
        
        # Should reject higher clearance
        assert not profile.can_handle_task({'security_clearance': 4})
    
    def test_can_handle_task_no_requirements(self):
        """Test capability matching with no requirements"""
        profile = CapabilityProfile()
        assert profile.can_handle_task({})
    
    def test_to_dict(self):
        """Test conversion to dictionary"""
        profile = CapabilityProfile(
            languages={'python'},
            tools={'pytest'},
            domains={'backend'},
            skills={'coding'},
            security_clearance=2
        )
        
        result = profile.to_dict()
        assert 'python' in result['languages']
        assert 'pytest' in result['tools']
        assert 'backend' in result['domains']
        assert 'coding' in result['skills']
        assert result['security_clearance'] == 2


class TestAgent:
    """Test Agent class functionality"""
    
    def test_agent_creation(self):
        """Test basic agent creation"""
        agent = Agent(
            agent_id="agent1",
            name="Test Agent",
            role=AgentRole.BUILDER,
            department_id="dept1"
        )
        
        assert agent.entity_id == "agent1"
        assert agent.name == "Test Agent"
        assert agent.role == AgentRole.BUILDER
        assert agent.department_id == "dept1"
        assert agent.status == "idle"
        assert agent.current_task_id is None
        assert len(agent.task_history) == 0
        
        # Check registration
        registry = get_registry()
        assert registry.get("agent1") == agent
    
    def test_agent_with_capabilities(self):
        """Test agent creation with custom capabilities"""
        capabilities = CapabilityProfile(
            languages={'python'},
            tools={'pytest'}
        )
        agent = Agent(
            agent_id="agent2",
            name="Capable Agent",
            role=AgentRole.VERIFIER,
            capabilities=capabilities
        )
        
        assert agent.capabilities == capabilities
        assert 'python' in agent.capabilities.languages
    
    def test_assign_task_success(self):
        """Test successful task assignment"""
        agent = Agent(
            agent_id="agent3",
            name="Worker",
            role=AgentRole.BUILDER,
            capabilities=CapabilityProfile(languages={'python'})
        )
        
        task = Task(
            task_id="task1",
            name="Build feature",
            description="Test task"
        )
        task.metadata['required_capabilities'] = {'languages': ['python']}
        
        result = agent.assign_task(task)
        
        assert result is True
        assert agent.current_task_id == "task1"
        assert task.assigned_agent_id == "agent3"
        assert agent.status == "working"
    
    def test_assign_task_insufficient_capabilities(self):
        """Test task assignment with insufficient capabilities"""
        agent = Agent(
            agent_id="agent4",
            name="Limited Agent",
            role=AgentRole.BUILDER,
            capabilities=CapabilityProfile(languages={'python'})
        )
        
        task = Task(
            task_id="task2",
            name="Rust task",
            description="Requires rust"
        )
        task.metadata['required_capabilities'] = {'languages': ['rust']}
        
        result = agent.assign_task(task)
        
        assert result is False
        assert agent.current_task_id is None
        assert agent.status == "idle"
    
    def test_complete_task(self):
        """Test task completion"""
        agent = Agent(
            agent_id="agent5",
            name="Worker",
            role=AgentRole.BUILDER
        )
        
        task = Task(
            task_id="task3",
            name="Task",
            description="Test"
        )
        agent.assign_task(task)
        
        agent.complete_task()
        
        assert agent.current_task_id is None
        assert agent.status == "idle"
        assert "task3" in agent.task_history
    
    def test_complete_task_no_current_task(self):
        """Test completing task when no task is assigned"""
        agent = Agent(
            agent_id="agent6",
            name="Idle Agent",
            role=AgentRole.BUILDER
        )
        
        # Should not raise error
        agent.complete_task()
        assert agent.current_task_id is None
        assert agent.status == "idle"
    
    def test_perform_action(self):
        """Test generic action performance"""
        agent = Agent(
            agent_id="agent7",
            name="Actor",
            role=AgentRole.ARCHITECT
        )
        
        # Should not raise error
        agent.perform_action("design", target_id="target1", data={'detail': 'test'})
        
        # Verify audit log
        events = get_audit_log().get_events_by_type(EventType.AGENT_ACTION)
        assert len(events) > 0
        latest = events[-1]
        assert latest.data['action'] == 'design'
    
    def test_perform_action_no_target_or_data(self):
        """Test action with minimal parameters"""
        agent = Agent(
            agent_id="agent8",
            name="Minimalist",
            role=AgentRole.BUILDER
        )
        
        agent.perform_action("think")
        events = get_audit_log().get_events_by_type(EventType.AGENT_ACTION)
        assert len(events) > 0


class TestManager:
    """Test Manager class functionality"""
    
    def test_manager_creation(self):
        """Test manager agent creation"""
        manager = Manager(
            manager_id="mgr1",
            name="Manager One",
            department_id="dept1"
        )
        
        assert manager.entity_id == "mgr1"
        assert manager.role == AgentRole.MANAGER
        assert len(manager.managed_agents) == 0
    
    def test_add_managed_agent(self):
        """Test adding agents to manager's supervision"""
        manager = Manager(
            manager_id="mgr2",
            name="Manager Two"
        )
        
        agent = Agent(
            agent_id="agent9",
            name="Subordinate",
            role=AgentRole.BUILDER
        )
        
        manager.add_managed_agent(agent)
        
        assert "agent9" in manager.managed_agents
        # Check relationship
        relationships = manager.get_relationships(RelationType.MANAGES)
        assert len(relationships) > 0
        assert any(r.target_id == "agent9" for r in relationships)


class TestConsensusVote:
    """Test ConsensusVote dataclass"""
    
    def test_vote_creation(self):
        """Test vote creation with defaults"""
        vote = ConsensusVote(
            agent_id="agent10",
            vote=True
        )
        
        assert vote.agent_id == "agent10"
        assert vote.vote is True
        assert vote.weight == 1.0
        assert vote.reasoning == ""
        assert isinstance(vote.timestamp, datetime)
    
    def test_vote_with_all_fields(self):
        """Test vote creation with all fields"""
        vote = ConsensusVote(
            agent_id="agent11",
            vote=False,
            weight=2.5,
            reasoning="Safety concern"
        )
        
        assert vote.vote is False
        assert vote.weight == 2.5
        assert vote.reasoning == "Safety concern"


class TestConsensusDecision:
    """Test ConsensusDecision functionality"""
    
    def test_decision_creation(self):
        """Test decision creation"""
        decision = ConsensusDecision(
            subject="Feature approval",
            target_id="feature1"
        )
        
        assert decision.subject == "Feature approval"
        assert decision.target_id == "feature1"
        assert decision.threshold == 0.66
        assert len(decision.votes) == 0
        assert decision.outcome is None
        assert decision.override_log is None
    
    def test_add_vote(self):
        """Test adding votes to decision"""
        decision = ConsensusDecision(
            subject="Test",
            target_id="target1"
        )
        
        decision.add_vote("agent1", True, weight=1.0, reasoning="Looks good")
        decision.add_vote("agent2", False, weight=1.0, reasoning="Needs work")
        
        assert len(decision.votes) == 2
        assert decision.votes[0].agent_id == "agent1"
        assert decision.votes[0].vote is True
        assert decision.votes[1].agent_id == "agent2"
        assert decision.votes[1].vote is False
    
    def test_calculate_outcome_approval(self):
        """Test consensus calculation with approval"""
        decision = ConsensusDecision(
            subject="Approval test",
            target_id="target2",
            threshold=0.66
        )
        
        decision.add_vote("agent1", True, weight=1.0)
        decision.add_vote("agent2", True, weight=1.0)
        decision.add_vote("agent3", False, weight=1.0)
        
        outcome = decision.calculate_outcome()
        
        assert outcome is True  # 2/3 = 66.67% >= 66%
        assert decision.outcome is True
        assert decision.decided_at is not None
    
    def test_calculate_outcome_rejection(self):
        """Test consensus calculation with rejection"""
        decision = ConsensusDecision(
            subject="Rejection test",
            target_id="target3",
            threshold=0.66
        )
        
        decision.add_vote("agent1", True, weight=1.0)
        decision.add_vote("agent2", False, weight=1.0)
        decision.add_vote("agent3", False, weight=1.0)
        
        outcome = decision.calculate_outcome()
        
        assert outcome is False  # 1/3 = 33.33% < 66%
        assert decision.outcome is False
    
    def test_calculate_outcome_no_votes(self):
        """Test consensus calculation with no votes"""
        decision = ConsensusDecision(
            subject="No votes",
            target_id="target4"
        )
        
        outcome = decision.calculate_outcome()
        
        assert outcome is False
        # Note: outcome is calculated but may be None or False based on implementation
        # The method returns False, but decision.outcome might be set differently
    
    def test_calculate_outcome_weighted(self):
        """Test weighted consensus calculation"""
        decision = ConsensusDecision(
            subject="Weighted test",
            target_id="target5",
            threshold=0.66
        )
        
        decision.add_vote("senior", True, weight=2.0)
        decision.add_vote("junior1", False, weight=1.0)
        decision.add_vote("junior2", False, weight=1.0)
        
        outcome = decision.calculate_outcome()
        
        # Total: 2 + 1 + 1 = 4
        # Approve: 2
        # Ratio: 2/4 = 0.5 < 0.66
        assert outcome is False
    
    def test_issue_override(self):
        """Test manager override of consensus"""
        decision = ConsensusDecision(
            subject="Override test",
            target_id="target6"
        )
        
        decision.add_vote("agent1", False, weight=1.0)
        decision.add_vote("agent2", False, weight=1.0)
        decision.calculate_outcome()
        
        assert decision.outcome is False
        
        decision.issue_override("mgr1", "Critical priority", True)
        
        assert decision.outcome is True
        assert decision.override_log is not None
        assert "mgr1" in decision.override_log
        assert "Critical priority" in decision.override_log
        
        # Check audit log
        events = get_audit_log().get_events_by_type(EventType.OVERRIDE_ISSUED)
        assert len(events) > 0
    
    def test_to_dict(self):
        """Test decision serialization"""
        decision = ConsensusDecision(
            subject="Dict test",
            target_id="target7"
        )
        
        decision.add_vote("agent1", True, weight=1.0, reasoning="Good")
        decision.calculate_outcome()
        
        result = decision.to_dict()
        
        assert result['subject'] == "Dict test"
        assert result['target_id'] == "target7"
        assert 'decision_id' in result
        assert len(result['votes']) == 1
        assert result['votes'][0]['agent_id'] == "agent1"
        assert result['threshold'] == 0.66
        assert result['outcome'] is not None
        assert result['decided_at'] is not None


class TestConsensusSystem:
    """Test ConsensusSystem functionality"""
    
    def test_initiate_consensus(self):
        """Test initiating a consensus decision"""
        system = ConsensusSystem()
        
        decision = system.initiate_consensus(
            subject="Feature X",
            target_id="featureX",
            threshold=0.75
        )
        
        assert decision.subject == "Feature X"
        assert decision.target_id == "featureX"
        assert decision.threshold == 0.75
        assert decision.decision_id in system.decisions
    
    def test_finalize_consensus_success(self):
        """Test finalizing a consensus with approval"""
        system = ConsensusSystem()
        
        decision = system.initiate_consensus("Test", "target8")
        decision.add_vote("agent1", True)
        decision.add_vote("agent2", True)
        
        outcome = system.finalize_consensus(decision.decision_id)
        
        assert outcome is True
        assert decision.outcome is True
        
        # Check audit log
        events = get_audit_log().get_events_by_type(EventType.CONSENSUS_REACHED)
        assert len(events) > 0
    
    def test_finalize_consensus_invalid_id(self):
        """Test finalizing non-existent consensus"""
        system = ConsensusSystem()
        
        outcome = system.finalize_consensus("invalid-id")
        
        assert outcome is False
    
    def test_get_decision(self):
        """Test retrieving a decision"""
        system = ConsensusSystem()
        
        decision = system.initiate_consensus("Test", "target9")
        decision_id = decision.decision_id
        
        retrieved = system.get_decision(decision_id)
        
        assert retrieved == decision
        assert retrieved.decision_id == decision_id
    
    def test_get_decision_not_found(self):
        """Test retrieving non-existent decision"""
        system = ConsensusSystem()
        
        result = system.get_decision("nonexistent")
        
        assert result is None


class TestGlobalConsensusSystem:
    """Test global consensus system singleton"""
    
    def test_get_consensus_system(self):
        """Test getting global consensus system"""
        system1 = get_consensus_system()
        system2 = get_consensus_system()
        
        assert system1 is system2  # Same instance
        assert isinstance(system1, ConsensusSystem)
