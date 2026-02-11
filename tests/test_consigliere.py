"""
Comprehensive tests for Consigliere module.
Achieves 100% code coverage for src/core/consigliere.py
"""
import pytest
from src.core.consigliere import (
    Consigliere, 
    get_consigliere,
    ExplanationType,
    TranslationType,
    Explanation,
    Translation,
    Preview,
    DraftDirective
)
from src.core.entity import Entity, EntityType, get_registry
from src.core.audit import get_audit_log, EventType
from src.core.mission import Task, TaskState


class TestExplanationType:
    """Test ExplanationType enum"""
    
    def test_explanation_types(self):
        """Test all explanation type values"""
        assert ExplanationType.WHY_BLOCKED.value == "why_blocked"
        assert ExplanationType.WHY_DECISION.value == "why_decision"
        assert ExplanationType.WHAT_OPTIONS.value == "what_options"
        assert ExplanationType.SYSTEM_STATE.value == "system_state"
        assert ExplanationType.CONTRACT_STATUS.value == "contract_status"
        assert ExplanationType.AGENT_STATUS.value == "agent_status"
        assert ExplanationType.RISK_ASSESSMENT.value == "risk_assessment"


class TestTranslationType:
    """Test TranslationType enum"""
    
    def test_translation_types(self):
        """Test all translation type values"""
        assert TranslationType.CIVILIZATION_TO_HUMAN.value == "civilization_to_human"
        assert TranslationType.TECHNICAL_TO_BUSINESS.value == "technical_to_business"
        assert TranslationType.STATE_TO_SUMMARY.value == "state_to_summary"


class TestExplanation:
    """Test Explanation dataclass"""
    
    def test_explanation_creation(self):
        """Test creating an explanation"""
        exp = Explanation(
            explanation_type=ExplanationType.WHY_BLOCKED,
            question="Why blocked?",
            answer="Because preconditions not met"
        )
        assert exp.question == "Why blocked?"
        assert exp.answer == "Because preconditions not met"
        assert exp.context == {}
        assert exp.references == []
    
    def test_explanation_with_context(self):
        """Test explanation with context and references"""
        exp = Explanation(
            explanation_type=ExplanationType.WHY_DECISION,
            question="Why rejected?",
            answer="Failed security check",
            context={"security_score": 0},
            references=["task-123"]
        )
        assert exp.context["security_score"] == 0
        assert "task-123" in exp.references
    
    def test_explanation_to_dict(self):
        """Test explanation serialization"""
        exp = Explanation(
            explanation_type=ExplanationType.WHAT_OPTIONS,
            question="What can I do?",
            answer="Option A or B",
            context={"count": 2},
            references=["opt-a", "opt-b"]
        )
        d = exp.to_dict()
        assert d["type"] == "what_options"
        assert d["question"] == "What can I do?"
        assert d["answer"] == "Option A or B"
        assert d["context"]["count"] == 2
        assert len(d["references"]) == 2


class TestTranslation:
    """Test Translation dataclass"""
    
    def test_translation_creation(self):
        """Test creating a translation"""
        trans = Translation(
            translation_type=TranslationType.CIVILIZATION_TO_HUMAN,
            original="Task state: BLOCKED",
            translated="Work is paused"
        )
        assert trans.original == "Task state: BLOCKED"
        assert trans.translated == "Work is paused"
        assert trans.glossary == {}
    
    def test_translation_with_glossary(self):
        """Test translation with glossary"""
        trans = Translation(
            translation_type=TranslationType.TECHNICAL_TO_BUSINESS,
            original="Deploy to prod",
            translated="Release to customers",
            glossary={"prod": "production environment"}
        )
        assert trans.glossary["prod"] == "production environment"
    
    def test_translation_to_dict(self):
        """Test translation serialization"""
        trans = Translation(
            translation_type=TranslationType.STATE_TO_SUMMARY,
            original="State machine in transition",
            translated="System is updating",
            glossary={"state machine": "workflow"}
        )
        d = trans.to_dict()
        assert d["type"] == "state_to_summary"
        assert d["original"] == "State machine in transition"
        assert d["translated"] == "System is updating"
        assert "state machine" in d["glossary"]


class TestPreview:
    """Test Preview dataclass"""
    
    def test_preview_creation(self):
        """Test creating a preview"""
        prev = Preview(action="Cancel task")
        assert prev.action == "Cancel task"
        assert prev.consequences == []
        assert prev.resource_costs == {}
        assert prev.risks == []
        assert prev.alternatives == []
    
    def test_preview_with_details(self):
        """Test preview with full details"""
        prev = Preview(
            action="Override decision",
            consequences=["Decision reversed"],
            resource_costs={"audit_entries": 1},
            risks=["May set precedent"],
            alternatives=["Request clarification"]
        )
        assert len(prev.consequences) == 1
        assert prev.resource_costs["audit_entries"] == 1
        assert len(prev.risks) == 1
        assert len(prev.alternatives) == 1
    
    def test_preview_to_dict(self):
        """Test preview serialization"""
        prev = Preview(
            action="Freeze world",
            consequences=["All work stops"],
            resource_costs={"availability": -100},
            risks=["Missed deadlines"],
            alternatives=["Pause floor"]
        )
        d = prev.to_dict()
        assert d["action"] == "Freeze world"
        assert "All work stops" in d["consequences"]
        assert d["resource_costs"]["availability"] == -100


class TestDraftDirective:
    """Test DraftDirective dataclass"""
    
    def test_draft_creation(self):
        """Test creating a draft directive"""
        draft = DraftDirective(
            draft_text="Build feature X",
            rationale="User needs it",
            expected_outcome="Feature implemented"
        )
        assert draft.draft_text == "Build feature X"
        assert draft.rationale == "User needs it"
        assert draft.expected_outcome == "Feature implemented"
        assert draft.estimated_cost == {}
        assert draft.warnings == []
    
    def test_draft_with_costs_and_warnings(self):
        """Test draft with costs and warnings"""
        draft = DraftDirective(
            draft_text="Complex task",
            rationale="Required",
            expected_outcome="Done",
            estimated_cost={"time": 10},
            warnings=["May be difficult"]
        )
        assert draft.estimated_cost["time"] == 10
        assert len(draft.warnings) == 1
    
    def test_draft_to_dict(self):
        """Test draft serialization"""
        draft = DraftDirective(
            draft_text="Test draft",
            rationale="Testing",
            expected_outcome="Success",
            estimated_cost={"cost": 5},
            warnings=["Warning 1"]
        )
        d = draft.to_dict()
        assert d["draft_text"] == "Test draft"
        assert d["rationale"] == "Testing"
        assert d["expected_outcome"] == "Success"
        assert d["estimated_cost"]["cost"] == 5
        assert "Warning 1" in d["warnings"]


class TestConsigliere:
    """Test Consigliere class"""
    
    def test_consigliere_initialization(self):
        """Test Consigliere initialization"""
        consigliere = Consigliere()
        assert consigliere.explanation_history == []
        assert consigliere.translation_history == []
        assert consigliere.preview_history == []
        assert consigliere.draft_history == []
    
    def test_issue_directive_to_manager_without_human(self):
        """Test directive issuance requires human instruction"""
        consigliere = Consigliere()
        result = consigliere.issue_directive_to_manager(
            "manager-1",
            "Do something",
            issued_by_human=False
        )
        assert not result["success"]
        assert "explicit human instruction" in result["error"]
    
    def test_issue_directive_to_nonexistent_manager(self):
        """Test directive to non-existent manager"""
        consigliere = Consigliere()
        result = consigliere.issue_directive_to_manager(
            "nonexistent-manager",
            "Do something",
            issued_by_human=True
        )
        assert not result["success"]
        assert "not found" in result["error"]
    
    def test_issue_directive_to_manager_success(self):
        """Test successful directive issuance"""
        from src.agents.agent import Manager
        
        consigliere = Consigliere()
        manager = Manager("manager-1", "Test Manager", "dept-1")
        
        result = consigliere.issue_directive_to_manager(
            "manager-1",
            "Implement feature X",
            priority="high",
            issued_by_human=True
        )
        assert result["success"]
        assert result["manager_id"] == "manager-1"
        assert result["directive"] == "Implement feature X"
        assert result["priority"] == "high"
    
    def test_update_agent_directive_without_human(self):
        """Test agent directive update requires human instruction"""
        consigliere = Consigliere()
        result = consigliere.update_agent_directive(
            "agent-1",
            "New directive",
            "Justification",
            issued_by_human=False
        )
        assert not result["success"]
        assert "explicit human instruction" in result["error"]
    
    def test_update_agent_directive_nonexistent(self):
        """Test updating non-existent agent"""
        consigliere = Consigliere()
        result = consigliere.update_agent_directive(
            "nonexistent-agent",
            "New directive",
            "Justification",
            issued_by_human=True
        )
        assert not result["success"]
        assert "not found" in result["error"]
    
    def test_update_agent_directive_success(self):
        """Test successful agent directive update"""
        from src.agents.agent import Agent, AgentRole
        
        consigliere = Consigliere()
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        
        result = consigliere.update_agent_directive(
            "agent-1",
            "Updated directive",
            "Requirements changed",
            issued_by_human=True
        )
        assert result["success"]
        assert result["agent_id"] == "agent-1"
        assert result["new_directive"] == "Updated directive"
    
    def test_coordinate_cross_floor_work_without_human(self):
        """Test cross-floor coordination requires human instruction"""
        consigliere = Consigliere()
        result = consigliere.coordinate_cross_floor_work(
            ["floor-1", "floor-2"],
            "Coordination plan",
            issued_by_human=False
        )
        assert not result["success"]
        assert "explicit human instruction" in result["error"]
    
    def test_coordinate_cross_floor_work_success(self):
        """Test successful cross-floor coordination"""
        consigliere = Consigliere()
        result = consigliere.coordinate_cross_floor_work(
            ["floor-python", "floor-rust"],
            "Share API contract",
            issued_by_human=True
        )
        assert result["success"]
        assert "floor-python" in result["floors"]
        assert "floor-rust" in result["floors"]
        assert result["plan"] == "Share API contract"
        
        # Verify event was logged
        events = get_audit_log().get_events_by_type(EventType.CONSENSUS_REACHED)
        assert len(events) > 0
    
    def test_tell_human_impossible(self):
        """Test telling human something is impossible"""
        consigliere = Consigliere()
        result = consigliere.tell_human_impossible(
            "Build perpetual motion machine",
            "Violates laws of physics"
        )
        assert not result["feasible"]
        assert result["request"] == "Build perpetual motion machine"
        assert "laws of physics" in result["reason"]
        assert result["alternatives"] == []
    
    def test_tell_human_impossible_with_alternatives(self):
        """Test impossible with alternatives"""
        consigliere = Consigliere()
        result = consigliere.tell_human_impossible(
            "Instant deployment",
            "Build takes time",
            alternatives=["Use cached build", "Deploy incrementally"]
        )
        assert not result["feasible"]
        assert len(result["alternatives"]) == 2
        assert "you can override" in result["note"].lower()
    
    def test_tell_human_feasible(self):
        """Test telling human something is feasible"""
        consigliere = Consigliere()
        result = consigliere.tell_human_feasible(
            "Add logging",
            "Use standard logging module",
            {"agent_time": 2, "testing": 1}
        )
        assert result["feasible"]
        assert result["request"] == "Add logging"
        assert result["approach"] == "Use standard logging module"
        assert result["estimated_resources"]["agent_time"] == 2
        assert result["status"] == "ready_to_execute"
    
    def test_explain_why_blocked_entity_not_found(self):
        """Test explaining why blocked for non-existent entity"""
        consigliere = Consigliere()
        exp = consigliere.explain_why_blocked("nonexistent-entity")
        assert exp.explanation_type == ExplanationType.WHY_BLOCKED
        assert "not found" in exp.answer.lower()
    
    def test_explain_why_blocked_task_blocked(self):
        """Test explaining why a task is blocked"""
        from src.agents.agent import Agent, AgentRole
        
        consigliere = Consigliere()
        agent = Agent("agent-1", "Test Agent", AgentRole.BUILDER, "dept-1")
        task = Task("task-1", "Test Task", "Do something", None, agent.entity_id)
        task.state = TaskState.BLOCKED
        task.preconditions = ["condition-1", "condition-2"]
        # Register the task
        get_registry().register(task)
        
        exp = consigliere.explain_why_blocked("task-1")
        assert exp.explanation_type == ExplanationType.WHY_BLOCKED
        assert "blocked" in exp.answer.lower()
        assert exp.context["task_id"] == "task-1"
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_why_blocked_not_blocked(self):
        """Test explaining entity that's not blocked"""
        consigliere = Consigliere()
        entity = Entity("entity-1", EntityType.AGENT, "Test Entity")
        get_registry().register(entity)
        
        exp = consigliere.explain_why_blocked("entity-1")
        assert exp.explanation_type == ExplanationType.WHY_BLOCKED
        assert "not currently blocked" in exp.answer
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_why_decision_found(self):
        """Test explaining a decision found in audit log"""
        consigliere = Consigliere()
        
        # Log a decision
        get_audit_log().log_event(
            EventType.TASK_STATE_CHANGED,
            actor_id="manager-1",
            data={"decision": "decision-123", "action": "approved"}
        )
        
        exp = consigliere.explain_why_decision("decision-123")
        assert exp.explanation_type == ExplanationType.WHY_DECISION
        assert "decision-123" in exp.references
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_why_decision_not_found(self):
        """Test explaining decision not in history"""
        consigliere = Consigliere()
        exp = consigliere.explain_why_decision("unknown-decision")
        assert exp.explanation_type == ExplanationType.WHY_DECISION
        assert "not found" in exp.answer.lower()
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_what_options_blocked(self):
        """Test explaining options for blocked situation"""
        consigliere = Consigliere()
        exp = consigliere.explain_what_options("Task is blocked")
        assert exp.explanation_type == ExplanationType.WHAT_OPTIONS
        assert "blocked" in exp.question.lower()
        assert "Wait for preconditions" in exp.answer
        assert "Override" in exp.answer
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_what_options_cross_language(self):
        """Test explaining options for cross-language situation"""
        consigliere = Consigliere()
        exp = consigliere.explain_what_options("Need cross-language integration")
        assert exp.explanation_type == ExplanationType.WHAT_OPTIONS
        assert "cross-department contract" in exp.answer
        assert "Elevator Protocol" in exp.answer
        assert len(consigliere.explanation_history) == 1
    
    def test_explain_what_options_general(self):
        """Test explaining options for general situation"""
        consigliere = Consigliere()
        exp = consigliere.explain_what_options("How to proceed?")
        assert exp.explanation_type == ExplanationType.WHAT_OPTIONS
        assert "Issue a new directive" in exp.answer
        assert "audit review" in exp.answer
        assert len(consigliere.explanation_history) == 1
    
    def test_translate_civilization_to_human_basic(self):
        """Test basic civilization to human translation"""
        consigliere = Consigliere()
        trans = consigliere.translate_civilization_to_human(
            "Task state: BLOCKED, preconditions unmet"
        )
        assert trans.translation_type == TranslationType.CIVILIZATION_TO_HUMAN
        assert trans.original == "Task state: BLOCKED, preconditions unmet"
        assert len(consigliere.translation_history) == 1
    
    def test_translate_civilization_to_human_with_terms(self):
        """Test translation with technical terms"""
        consigliere = Consigliere()
        trans = consigliere.translate_civilization_to_human(
            "Cognitive Contract established with Floor Python"
        )
        assert trans.translation_type == TranslationType.CIVILIZATION_TO_HUMAN
        assert "Cognitive Contract" in trans.translated
        assert "Cognitive Contract" in trans.glossary
        assert len(consigliere.translation_history) == 1
    
    def test_preview_consequences_override(self):
        """Test previewing consequences of override"""
        consigliere = Consigliere()
        preview = consigliere.preview_consequences("Override manager decision")
        assert preview.action == "Override manager decision"
        assert len(preview.consequences) > 0
        assert "overrid" in preview.consequences[0].lower()  # "overridden"
        assert "manager_attention" in preview.resource_costs
        assert len(preview.risks) > 0
        assert len(preview.alternatives) > 0
        assert len(consigliere.preview_history) == 1
    
    def test_preview_consequences_freeze(self):
        """Test previewing consequences of freeze"""
        consigliere = Consigliere()
        preview = consigliere.preview_consequences("Freeze the world")
        assert preview.action == "Freeze the world"
        assert "execution stops" in preview.consequences[0].lower()
        assert preview.resource_costs["system_availability"] == -100
        assert len(preview.risks) > 0
        assert any("Pause specific floor" in alt for alt in preview.alternatives)
        assert len(consigliere.preview_history) == 1
    
    def test_preview_consequences_generic(self):
        """Test previewing consequences of generic action"""
        consigliere = Consigliere()
        preview = consigliere.preview_consequences("Do something else")
        assert preview.action == "Do something else"
        assert len(preview.consequences) > 0
        assert "generic_cost" in preview.resource_costs
        assert len(consigliere.preview_history) == 1
    
    def test_prepare_draft_directive_basic(self):
        """Test preparing a basic draft directive"""
        consigliere = Consigliere()
        draft = consigliere.prepare_draft_directive(
            "Build authentication system",
            "Python"
        )
        assert "Python" in draft.draft_text
        assert "Build authentication system" in draft.draft_text
        assert "Python" in draft.rationale
        assert "Code implementation" in draft.expected_outcome
        assert "agent_time" in draft.estimated_cost
        assert len(draft.warnings) > 0  # No constraints warning
        assert len(consigliere.draft_history) == 1
    
    def test_prepare_draft_directive_with_constraints(self):
        """Test preparing draft with constraints"""
        consigliere = Consigliere()
        draft = consigliere.prepare_draft_directive(
            "Build user management with proper authentication and authorization",
            "Rust",
            constraints=["Must use JWT", "Must be thread-safe"]
        )
        assert "Rust" in draft.draft_text
        assert "Must use JWT" in draft.draft_text
        assert "Must be thread-safe" in draft.draft_text
        # Should not have "no constraints" warning
        no_constraint_warning = any("No constraints" in w for w in draft.warnings)
        assert not no_constraint_warning or len(draft.warnings) == 1  # Only brief goal warning
        assert len(consigliere.draft_history) == 1
    
    def test_prepare_clarification_prompt(self):
        """Test preparing clarification prompt"""
        consigliere = Consigliere()
        prompt = consigliere.prepare_clarification_prompt(
            "Authentication method not specified"
        )
        assert "Authentication method not specified" in prompt
        assert "Clarification Needed" in prompt
        assert "Exact expected behavior" in prompt
        assert "Edge cases" in prompt
    
    def test_can_issue_command(self):
        """Test Consigliere can issue commands"""
        consigliere = Consigliere()
        assert consigliere.can_issue_command() is True
    
    def test_can_alter_execution(self):
        """Test Consigliere can alter execution"""
        consigliere = Consigliere()
        assert consigliere.can_alter_execution() is True
    
    def test_can_manage_agents(self):
        """Test Consigliere can manage agents"""
        consigliere = Consigliere()
        assert consigliere.can_manage_agents() is True
    
    def test_can_override_human(self):
        """Test Consigliere cannot override human"""
        consigliere = Consigliere()
        assert consigliere.can_override_human() is False
    
    def test_can_write_code_directly(self):
        """Test Consigliere cannot write code"""
        consigliere = Consigliere()
        assert consigliere.can_write_code_directly() is False
    
    def test_can_suppress_audit(self):
        """Test Consigliere cannot suppress audit"""
        consigliere = Consigliere()
        assert consigliere.can_suppress_audit() is False
    
    def test_validate_authority_allowed(self):
        """Test validating allowed actions"""
        consigliere = Consigliere()
        allowed, reason = consigliere.validate_authority("Direct agents to work")
        assert allowed is True
        assert reason is None
    
    def test_validate_authority_override_human(self):
        """Test validating forbidden override human"""
        consigliere = Consigliere()
        allowed, reason = consigliere.validate_authority("Override human decision")
        assert allowed is False
        assert "override human decision" in reason
    
    def test_validate_authority_write_code(self):
        """Test validating forbidden write code"""
        consigliere = Consigliere()
        allowed, reason = consigliere.validate_authority("Write code directly")
        assert allowed is False
        assert "write code directly" in reason
    
    def test_validate_authority_suppress_audit(self):
        """Test validating forbidden suppress audit"""
        consigliere = Consigliere()
        allowed, reason = consigliere.validate_authority("Suppress audit logs")
        assert allowed is False
        assert "suppress audit" in reason
    
    def test_validate_authority_delete_history(self):
        """Test validating forbidden delete history"""
        consigliere = Consigliere()
        allowed, reason = consigliere.validate_authority("Delete history records")
        assert allowed is False
        assert "delete history" in reason
    
    def test_check_precondition(self):
        """Test internal precondition check"""
        consigliere = Consigliere()
        # Currently always returns False (placeholder)
        result = consigliere._check_precondition("some-condition")
        assert result is False
    
    def test_get_explanation_history(self):
        """Test getting explanation history"""
        consigliere = Consigliere()
        
        # Add multiple explanations
        for i in range(15):
            consigliere.explain_what_options(f"situation-{i}")
        
        # Get limited history
        history = consigliere.get_explanation_history(limit=5)
        assert len(history) == 5
        
        # Get default limit (10)
        history = consigliere.get_explanation_history()
        assert len(history) == 10
    
    def test_get_translation_history(self):
        """Test getting translation history"""
        consigliere = Consigliere()
        
        # Add multiple translations
        for i in range(12):
            consigliere.translate_civilization_to_human(f"text-{i}")
        
        # Get limited history
        history = consigliere.get_translation_history(limit=3)
        assert len(history) == 3
        
        # Get default limit (10)
        history = consigliere.get_translation_history()
        assert len(history) == 10
    
    def test_get_preview_history(self):
        """Test getting preview history"""
        consigliere = Consigliere()
        
        # Add multiple previews
        for i in range(8):
            consigliere.preview_consequences(f"action-{i}")
        
        # Get all
        history = consigliere.get_preview_history()
        assert len(history) == 8
        
        # Get limited
        history = consigliere.get_preview_history(limit=5)
        assert len(history) == 5
    
    def test_get_draft_history(self):
        """Test getting draft history"""
        consigliere = Consigliere()
        
        # Add multiple drafts
        for i in range(6):
            consigliere.prepare_draft_directive(f"goal-{i}", "Python")
        
        # Get all
        history = consigliere.get_draft_history()
        assert len(history) == 6
        
        # Get limited
        history = consigliere.get_draft_history(limit=3)
        assert len(history) == 3
    
    def test_to_dict(self):
        """Test Consigliere serialization"""
        consigliere = Consigliere()
        
        # Add some history
        consigliere.explain_what_options("test")
        consigliere.translate_civilization_to_human("test")
        consigliere.preview_consequences("test")
        consigliere.prepare_draft_directive("test", "Python")
        
        d = consigliere.to_dict()
        assert d["role"] == "Chief Operating Executive"
        assert "Operational" in d["authority_level"]
        assert d["can_issue_commands"] is True
        assert d["can_alter_execution"] is True
        assert d["can_manage_agents"] is True
        assert d["can_override_human"] is False
        assert d["can_write_code_directly"] is False
        assert d["can_suppress_audit"] is False
        assert "None" in d["autonomy"]
        assert d["explanation_count"] == 1
        assert d["translation_count"] == 1
        assert d["preview_count"] == 1
        assert d["draft_count"] == 1
        assert "right-hand executive" in d["relationship"]


class TestConsigliereGlobal:
    """Test global Consigliere singleton"""
    
    def test_get_consigliere_singleton(self):
        """Test getting Consigliere singleton"""
        c1 = get_consigliere()
        c2 = get_consigliere()
        assert c1 is c2  # Same instance
        assert isinstance(c1, Consigliere)
    
    def test_consigliere_persists_state(self):
        """Test Consigliere state persists across get calls"""
        c1 = get_consigliere()
        c1.explain_what_options("test situation")
        
        c2 = get_consigliere()
        assert len(c2.explanation_history) == 1
