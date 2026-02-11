"""Unit tests for the mission system."""
import pytest
from src.core.mission import (
    Task, TaskState, Directive, DirectiveLevel, 
    AcceptanceCriteria, Condition, DecisionTranscript,
    MeetingSystem, get_meeting_system
)


class TestAcceptanceCriteria:
    """Test AcceptanceCriteria class."""
    
    def test_criteria_creation(self):
        """Test creating acceptance criteria."""
        criteria = AcceptanceCriteria(description="Test criteria")
        assert criteria.description == "Test criteria"
        assert not criteria.is_met
        assert criteria.checked_at is None
    
    def test_criteria_check_without_validator(self):
        """Test checking criteria without validator."""
        criteria = AcceptanceCriteria(description="Test")
        result = criteria.check()
        assert not result
        assert criteria.checked_at is not None
    
    def test_criteria_check_with_validator(self):
        """Test checking criteria with validator."""
        criteria = AcceptanceCriteria(
            description="Test",
            validator=lambda: True
        )
        result = criteria.check()
        assert result
        assert criteria.is_met


class TestCondition:
    """Test Condition class."""
    
    def test_condition_creation(self):
        """Test creating a condition."""
        condition = Condition(description="Test condition")
        assert condition.description == "Test condition"
        assert not condition.is_satisfied
    
    def test_condition_check_without_checker(self):
        """Test checking condition without checker."""
        condition = Condition(description="Test")
        result = condition.check()
        assert not result
    
    def test_condition_check_with_checker(self):
        """Test checking condition with checker."""
        condition = Condition(
            description="Test",
            checker=lambda: True
        )
        result = condition.check()
        assert result
        assert condition.is_satisfied


class TestDirective:
    """Test Directive class."""
    
    def test_directive_creation(self):
        """Test creating a directive."""
        directive = Directive(
            "dir-001",
            "Test Directive",
            DirectiveLevel.USER_INTENT,
            "Test description"
        )
        assert directive.entity_id == "dir-001"
        assert directive.name == "Test Directive"
        assert directive.level == DirectiveLevel.USER_INTENT
        assert directive.description == "Test description"
    
    def test_add_precondition(self):
        """Test adding preconditions."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        cond = directive.add_precondition("Must be ready", lambda: True)
        assert len(directive.preconditions) == 1
        assert cond.description == "Must be ready"
    
    def test_add_postcondition(self):
        """Test adding postconditions."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        cond = directive.add_postcondition("Must be complete", lambda: True)
        assert len(directive.postconditions) == 1
        assert cond.description == "Must be complete"
    
    def test_add_acceptance_criterion(self):
        """Test adding acceptance criteria."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        criterion = directive.add_acceptance_criterion("Must pass", lambda: True)
        assert len(directive.acceptance_criteria) == 1
        assert criterion.description == "Must pass"
    
    def test_check_preconditions(self):
        """Test checking all preconditions."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        directive.add_precondition("Cond 1", lambda: True)
        directive.add_precondition("Cond 2", lambda: True)
        assert directive.check_preconditions()
    
    def test_check_postconditions(self):
        """Test checking all postconditions."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        directive.add_postcondition("Cond 1", lambda: True)
        directive.add_postcondition("Cond 2", lambda: False)
        assert not directive.check_postconditions()
    
    def test_check_acceptance(self):
        """Test checking acceptance criteria."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        directive.add_acceptance_criterion("Criterion 1", lambda: True)
        assert directive.check_acceptance()
    
    def test_is_ready_for_commit(self):
        """Test readiness for commit."""
        directive = Directive("dir-001", "Test", DirectiveLevel.USER_INTENT)
        directive.add_precondition("Pre", lambda: True)
        directive.add_postcondition("Post", lambda: True)
        directive.add_acceptance_criterion("Accept", lambda: True)
        assert directive.is_ready_for_commit()


class TestTask:
    """Test Task class."""
    
    def test_task_creation(self):
        """Test creating a task."""
        task = Task("task-001", "Test Task", "Test description")
        assert task.entity_id == "task-001"
        assert task.name == "Test Task"
        assert task.state == TaskState.SCHEDULED
        assert task.description == "Test description"
        assert len(task.state_history) == 1
    
    def test_task_with_agent(self):
        """Test creating task with assigned agent."""
        task = Task("task-001", "Test", assigned_agent_id="agent-001")
        assert task.assigned_agent_id == "agent-001"
    
    def test_can_transition_to(self):
        """Test valid state transitions."""
        task = Task("task-001", "Test")
        assert task.can_transition_to(TaskState.IN_REVIEW)
        assert task.can_transition_to(TaskState.BLOCKED)
        assert not task.can_transition_to(TaskState.MERGED)
    
    def test_transition_to_valid(self):
        """Test valid state transition."""
        task = Task("task-001", "Test")
        result = task.transition_to(TaskState.IN_REVIEW, "Ready for review")
        assert result
        assert task.state == TaskState.IN_REVIEW
        assert len(task.state_history) == 2
    
    def test_transition_to_invalid(self):
        """Test invalid state transition."""
        task = Task("task-001", "Test")
        result = task.transition_to(TaskState.MERGED, "Invalid")
        assert not result
        assert task.state == TaskState.SCHEDULED
    
    def test_transition_to_approval_without_postconditions(self):
        """Test transition to approval requires postconditions."""
        task = Task("task-001", "Test")
        task.transition_to(TaskState.IN_REVIEW)
        task.add_postcondition("Must be done", lambda: False)
        result = task.transition_to(TaskState.APPROVAL)
        assert not result
    
    def test_transition_to_approval_with_postconditions(self):
        """Test transition to approval with postconditions met."""
        task = Task("task-001", "Test")
        task.transition_to(TaskState.IN_REVIEW)
        task.add_postcondition("Must be done", lambda: True)
        result = task.transition_to(TaskState.APPROVAL)
        assert result
        assert task.state == TaskState.APPROVAL
    
    def test_transition_to_merged_without_criteria(self):
        """Test transition to merged requires all criteria."""
        task = Task("task-001", "Test")
        task.transition_to(TaskState.IN_REVIEW)
        task.add_postcondition("Post", lambda: True)
        task.transition_to(TaskState.APPROVAL)
        task.add_precondition("Pre", lambda: False)
        result = task.transition_to(TaskState.MERGED)
        assert not result
    
    def test_transition_to_merged_with_criteria(self):
        """Test transition to merged with all criteria met."""
        task = Task("task-001", "Test")
        task.add_precondition("Pre", lambda: True)
        task.add_postcondition("Post", lambda: True)
        task.add_acceptance_criterion("Accept", lambda: True)
        task.transition_to(TaskState.IN_REVIEW)
        task.transition_to(TaskState.APPROVAL)
        result = task.transition_to(TaskState.MERGED)
        assert result
        assert task.state == TaskState.MERGED
    
    def test_block(self):
        """Test blocking a task."""
        task = Task("task-001", "Test")
        task.block("Waiting for dependencies")
        assert task.state == TaskState.BLOCKED
        assert task.blocked_reason == "Waiting for dependencies"
    
    def test_unblock(self):
        """Test unblocking a task."""
        task = Task("task-001", "Test")
        task.block("Blocked")
        task.unblock()
        assert task.state == TaskState.SCHEDULED
        assert task.blocked_reason is None
    
    def test_set_ambiguity_score(self):
        """Test setting ambiguity score."""
        task = Task("task-001", "Test")
        task.set_ambiguity_score(0.7)
        assert task.ambiguity_score == 0.7
    
    def test_set_ambiguity_score_bounds(self):
        """Test ambiguity score is bounded."""
        task = Task("task-001", "Test")
        task.set_ambiguity_score(1.5)
        assert task.ambiguity_score == 1.0
        task.set_ambiguity_score(-0.5)
        assert task.ambiguity_score == 0.0
    
    def test_needs_meeting_below_threshold(self):
        """Test meeting not needed below threshold."""
        task = Task("task-001", "Test")
        task.set_ambiguity_score(0.3)
        assert not task.needs_meeting()
    
    def test_needs_meeting_above_threshold(self):
        """Test meeting needed above threshold."""
        task = Task("task-001", "Test")
        task.set_ambiguity_score(0.7)
        assert task.needs_meeting()
    
    def test_needs_meeting_custom_threshold(self):
        """Test meeting with custom threshold."""
        task = Task("task-001", "Test")
        task.set_ambiguity_score(0.6)
        assert not task.needs_meeting(threshold=0.7)
        assert task.needs_meeting(threshold=0.5)


class TestDecisionTranscript:
    """Test DecisionTranscript class."""
    
    def test_transcript_creation(self):
        """Test creating a decision transcript."""
        transcript = DecisionTranscript(
            task_id="task-001",
            participants=["agent-001", "agent-002"],
            ambiguity_addressed="Unclear requirements",
            decisions_made=["Use approach A"],
            resolution="Requirements clarified"
        )
        assert transcript.task_id == "task-001"
        assert len(transcript.participants) == 2
    
    def test_transcript_to_dict(self):
        """Test serializing transcript to dict."""
        transcript = DecisionTranscript(
            task_id="task-001",
            participants=["agent-001"],
            decisions_made=["Decision 1"]
        )
        data = transcript.to_dict()
        assert data['task_id'] == "task-001"
        assert 'transcript_id' in data
        assert 'meeting_date' in data


class TestMeetingSystem:
    """Test MeetingSystem class."""
    
    def test_meeting_system_creation(self):
        """Test creating a meeting system."""
        system = MeetingSystem()
        assert len(system.transcripts) == 0
    
    def test_hold_meeting(self):
        """Test holding a meeting."""
        system = MeetingSystem()
        task = Task("task-001", "Test")
        task.set_ambiguity_score(0.8)
        
        transcript = system.hold_meeting(
            task,
            ["agent-001", "agent-002"],
            "Unclear scope",
            ["Narrow scope to X"],
            "Scope clarified"
        )
        
        assert transcript is not None
        assert transcript.task_id == "task-001"
        assert task.ambiguity_score == 0.0
        assert len(system.transcripts) == 1
    
    def test_get_transcript(self):
        """Test retrieving a transcript."""
        system = MeetingSystem()
        task = Task("task-001", "Test")
        
        transcript = system.hold_meeting(
            task,
            ["agent-001"],
            "Issue",
            ["Decision"],
            "Resolved"
        )
        
        retrieved = system.get_transcript(transcript.transcript_id)
        assert retrieved is not None
        assert retrieved.transcript_id == transcript.transcript_id
    
    def test_get_transcript_not_found(self):
        """Test retrieving non-existent transcript."""
        system = MeetingSystem()
        result = system.get_transcript("nonexistent")
        assert result is None
    
    def test_get_task_transcripts(self):
        """Test getting all transcripts for a task."""
        system = MeetingSystem()
        task = Task("task-001", "Test")
        
        system.hold_meeting(task, ["agent-001"], "Issue 1", ["Dec 1"], "Res 1")
        system.hold_meeting(task, ["agent-002"], "Issue 2", ["Dec 2"], "Res 2")
        
        transcripts = system.get_task_transcripts("task-001")
        assert len(transcripts) == 2
    
    def test_get_meeting_system_global(self):
        """Test getting global meeting system."""
        system = get_meeting_system()
        assert system is not None
        assert isinstance(system, MeetingSystem)
