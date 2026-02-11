"""Comprehensive tests for the creative autonomy module."""
import pytest
from datetime import datetime
from src.core.creative_autonomy import (
    CreativeZone,
    InitiativeStatus,
    CreativeFirewall,
    IdleConditions,
    InitiativeProposal,
    IdleInitiativeChannel,
    LoungeConversation,
    CulturalArtifact,
    EmployeeLounge,
    EmployeeOfTheMonth,
    GoldenStar,
    RecognitionSystem,
    Contribution,
    Review,
    PersonalTrackRecord,
    CreativePrivilegeSuspension,
    FailureSafetySystem,
    BoundedCreativeAutonomy,
    create_bounded_creative_autonomy,
    get_bounded_creative_autonomy,
    _bounded_creative_autonomy
)


class TestCreativeZone:
    """Test CreativeZone enum."""
    
    def test_creative_zone_values(self):
        """Test creative zone enum values."""
        assert CreativeZone.IDLE_INITIATIVE.value == "idle_initiative"
        assert CreativeZone.EMPLOYEE_LOUNGE.value == "employee_lounge"
        assert CreativeZone.PRODUCTION.value == "production"


class TestInitiativeStatus:
    """Test InitiativeStatus enum."""
    
    def test_initiative_status_values(self):
        """Test initiative status enum values."""
        assert InitiativeStatus.PROPOSED.value == "proposed"
        assert InitiativeStatus.APPROVED.value == "approved"
        assert InitiativeStatus.IGNORED.value == "ignored"
        assert InitiativeStatus.REJECTED.value == "rejected"
        assert InitiativeStatus.ARCHIVED.value == "archived"


class TestCreativeFirewall:
    """Test CreativeFirewall class."""
    
    def test_firewall_creation(self):
        """Test creating a creative firewall."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        assert firewall.firewall_id == "fw-001"
        assert firewall.blocked_crossings == []
        assert firewall.violations == []
    
    def test_block_creative_crossing(self):
        """Test blocking a creative crossing."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        result = firewall.block_creative_crossing(
            "initiative",
            "init-001",
            CreativeZone.IDLE_INITIATIVE,
            "Not approved by human"
        )
        
        assert result is True
        assert len(firewall.blocked_crossings) == 1
        crossing = firewall.blocked_crossings[0]
        assert crossing["item_type"] == "initiative"
        assert crossing["item_id"] == "init-001"
        assert crossing["zone"] == "idle_initiative"
        assert crossing["reason"] == "Not approved by human"
        assert crossing["blocked"] is True
        assert "timestamp" in crossing
    
    def test_record_violation(self):
        """Test recording a firewall violation."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        violation_id = firewall.record_violation(
            "unauthorized_crossing",
            "agent-001",
            "Attempted to execute without approval"
        )
        
        assert violation_id == "viol-1"
        assert len(firewall.violations) == 1
        violation = firewall.violations[0]
        assert violation["violation_id"] == "viol-1"
        assert violation["type"] == "unauthorized_crossing"
        assert violation["entity"] == "agent-001"
        assert violation["description"] == "Attempted to execute without approval"
        assert "timestamp" in violation
    
    def test_record_multiple_violations(self):
        """Test recording multiple violations."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        viol_1 = firewall.record_violation("type1", "entity1", "desc1")
        viol_2 = firewall.record_violation("type2", "entity2", "desc2")
        
        assert viol_1 == "viol-1"
        assert viol_2 == "viol-2"
        assert len(firewall.violations) == 2
    
    def test_verify_human_gate_approved(self):
        """Test verifying human gate with approval."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        result = firewall.verify_human_gate("test_action", approved_by_human=True)
        
        assert result is True
        assert len(firewall.violations) == 0
    
    def test_verify_human_gate_not_approved(self):
        """Test verifying human gate without approval."""
        firewall = CreativeFirewall(firewall_id="fw-001")
        
        result = firewall.verify_human_gate("test_action", approved_by_human=False)
        
        assert result is False
        assert len(firewall.violations) == 1
        violation = firewall.violations[0]
        assert violation["type"] == "human_gate_bypass"
        assert violation["entity"] == "test_action"
        assert violation["description"] == "Action attempted without human approval"


class TestIdleConditions:
    """Test IdleConditions class."""
    
    def test_can_enter_idle_mode_when_clear(self):
        """Test entering idle mode when conditions are clear."""
        conditions = IdleConditions(
            has_active_board_resolution=False,
            has_pending_contracts=False,
            has_unresolved_failures=False
        )
        
        assert conditions.can_enter_idle_mode() is True
    
    def test_cannot_enter_idle_mode_with_board_resolution(self):
        """Test cannot enter idle mode with active board resolution."""
        conditions = IdleConditions(
            has_active_board_resolution=True,
            has_pending_contracts=False,
            has_unresolved_failures=False
        )
        
        assert conditions.can_enter_idle_mode() is False
    
    def test_cannot_enter_idle_mode_with_pending_contracts(self):
        """Test cannot enter idle mode with pending contracts."""
        conditions = IdleConditions(
            has_active_board_resolution=False,
            has_pending_contracts=True,
            has_unresolved_failures=False
        )
        
        assert conditions.can_enter_idle_mode() is False
    
    def test_cannot_enter_idle_mode_with_unresolved_failures(self):
        """Test cannot enter idle mode with unresolved failures."""
        conditions = IdleConditions(
            has_active_board_resolution=False,
            has_pending_contracts=False,
            has_unresolved_failures=True
        )
        
        assert conditions.can_enter_idle_mode() is False


class TestInitiativeProposal:
    """Test InitiativeProposal class."""
    
    def test_proposal_creation(self):
        """Test creating an initiative proposal."""
        now = datetime.now()
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=now,
            title="Test Utility",
            description="A helpful utility",
            category="utility",
            proposed_code="def test(): pass",
            status=InitiativeStatus.PROPOSED
        )
        
        assert proposal.proposal_id == "init-001"
        assert proposal.floor_id == "floor-001"
        assert proposal.proposed_by == "agent-001"
        assert proposal.timestamp == now
        assert proposal.title == "Test Utility"
        assert proposal.description == "A helpful utility"
        assert proposal.category == "utility"
        assert proposal.proposed_code == "def test(): pass"
        assert proposal.status == InitiativeStatus.PROPOSED
        assert proposal.is_optional is True
        assert proposal.is_non_production is True
        assert proposal.has_execution_authority is False
        assert proposal.has_resource_allocation is False
    
    def test_approve_proposal(self):
        """Test approving a proposal."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.approve("human-001")
        
        assert proposal.status == InitiativeStatus.APPROVED
        assert proposal.human_decision == "approved"
        assert proposal.human_decision_by == "human-001"
        assert proposal.human_decision_at is not None
        assert proposal.converted_to_directive is False
    
    def test_approve_with_directive_conversion(self):
        """Test approving with directive conversion."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.approve("human-001", convert_to_directive=True, directive_id="dir-001")
        
        assert proposal.status == InitiativeStatus.APPROVED
        assert proposal.converted_to_directive is True
        assert proposal.directive_id == "dir-001"
    
    def test_ignore_proposal(self):
        """Test ignoring a proposal."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.ignore()
        
        assert proposal.status == InitiativeStatus.IGNORED
        assert proposal.human_decision == "ignored"
        assert proposal.human_decision_at is not None
    
    def test_reject_proposal(self):
        """Test rejecting a proposal."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.reject("human-001")
        
        assert proposal.status == InitiativeStatus.REJECTED
        assert proposal.human_decision == "rejected"
        assert proposal.human_decision_by == "human-001"
        assert proposal.human_decision_at is not None
    
    def test_reject_proposal_with_explanation(self):
        """Test rejecting a proposal with explanation."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.reject("human-001", explanation="Not aligned with goals")
        
        assert proposal.status == InitiativeStatus.REJECTED
        assert proposal.human_decision == "rejected: Not aligned with goals"
        assert proposal.human_decision_by == "human-001"
    
    def test_archive_proposal(self):
        """Test archiving a proposal."""
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        
        proposal.archive("human-001")
        
        assert proposal.status == InitiativeStatus.ARCHIVED
        assert proposal.human_decision == "archived"
        assert proposal.human_decision_by == "human-001"
        assert proposal.human_decision_at is not None


class TestIdleInitiativeChannel:
    """Test IdleInitiativeChannel class."""
    
    def test_channel_creation(self):
        """Test creating an idle initiative channel."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        assert channel.channel_id == "iic-001"
        assert channel.proposals == []
        assert channel.suspended is False
        assert channel.suspension_reason is None
    
    def test_check_idle_conditions(self):
        """Test checking idle conditions."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        conditions = channel.check_idle_conditions("floor-001")
        
        assert isinstance(conditions, IdleConditions)
        assert conditions.has_active_board_resolution is False
        assert conditions.has_pending_contracts is False
        assert conditions.has_unresolved_failures is False
    
    def test_submit_proposal_success(self):
        """Test submitting a proposal successfully."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        proposal_id = channel.submit_proposal(
            "floor-001",
            "agent-001",
            "Test Widget",
            "A helpful widget",
            "widget",
            "def widget(): pass"
        )
        
        assert proposal_id == "init-1"
        assert len(channel.proposals) == 1
        proposal = channel.proposals[0]
        assert proposal.title == "Test Widget"
        assert proposal.category == "widget"
    
    def test_submit_proposal_when_suspended(self):
        """Test submitting a proposal when channel is suspended."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        channel.suspend("Testing suspension")
        
        proposal_id = channel.submit_proposal(
            "floor-001",
            "agent-001",
            "Test Widget",
            "A helpful widget",
            "widget"
        )
        
        assert proposal_id is None
        assert len(channel.proposals) == 0
    
    def test_submit_proposal_invalid_category(self):
        """Test submitting a proposal with invalid category."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        proposal_id = channel.submit_proposal(
            "floor-001",
            "agent-001",
            "Test",
            "Test",
            "invalid_category"
        )
        
        assert proposal_id is None
        assert len(channel.proposals) == 0
    
    def test_submit_multiple_proposals(self):
        """Test submitting multiple proposals."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        for category in ["widget", "utility", "hygiene", "refactoring", "test_helper", "doc_helper"]:
            proposal_id = channel.submit_proposal(
                "floor-001",
                "agent-001",
                f"Test {category}",
                f"A {category}",
                category
            )
            assert proposal_id is not None
        
        assert len(channel.proposals) == 6
    
    def test_suspend_channel(self):
        """Test suspending the channel."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        channel.suspend("Emergency suspension")
        
        assert channel.suspended is True
        assert channel.suspension_reason == "Emergency suspension"
    
    def test_resume_channel(self):
        """Test resuming the channel."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        channel.suspend("Testing")
        
        channel.resume()
        
        assert channel.suspended is False
        assert channel.suspension_reason is None
    
    def test_get_pending_proposals(self):
        """Test getting pending proposals."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        # Submit proposals
        channel.submit_proposal("floor-001", "agent-001", "Test 1", "Desc 1", "widget")
        channel.submit_proposal("floor-001", "agent-001", "Test 2", "Desc 2", "utility")
        
        # Approve one
        channel.proposals[0].approve("human-001")
        
        pending = channel.get_pending_proposals()
        
        assert len(pending) == 1
        assert pending[0].title == "Test 2"
    
    def test_submit_proposal_cannot_enter_idle_mode(self, monkeypatch):
        """Test submitting when floor cannot enter idle mode."""
        channel = IdleInitiativeChannel(channel_id="iic-001")
        
        # Mock check_idle_conditions to return conditions that prevent idle mode
        def mock_check(floor_id):
            return IdleConditions(
                has_active_board_resolution=True,
                has_pending_contracts=False,
                has_unresolved_failures=False
            )
        
        monkeypatch.setattr(channel, 'check_idle_conditions', mock_check)
        
        proposal_id = channel.submit_proposal(
            "floor-001",
            "agent-001",
            "Test Widget",
            "A helpful widget",
            "widget"
        )
        
        assert proposal_id is None
        assert len(channel.proposals) == 0


class TestLoungeConversation:
    """Test LoungeConversation class."""
    
    def test_conversation_creation(self):
        """Test creating a lounge conversation."""
        now = datetime.now()
        conversation = LoungeConversation(
            conversation_id="conv-001",
            timestamp=now,
            participants=["agent-001", "agent-002"],
            topic="Python best practices",
            conversation_type="technical",
            content="Discussing design patterns"
        )
        
        assert conversation.conversation_id == "conv-001"
        assert conversation.timestamp == now
        assert conversation.participants == ["agent-001", "agent-002"]
        assert conversation.topic == "Python best practices"
        assert conversation.conversation_type == "technical"
        assert conversation.content == "Discussing design patterns"
        assert conversation.is_operational is False
        assert conversation.creates_decisions is False
        assert conversation.creates_plans is False
        assert conversation.influences_execution is False
    
    def test_verify_non_operational(self):
        """Test verifying conversation is non-operational."""
        conversation = LoungeConversation(
            conversation_id="conv-001",
            timestamp=datetime.now(),
            participants=["agent-001"],
            topic="Test",
            conversation_type="technical",
            content="Test"
        )
        
        assert conversation.verify_non_operational() is True
    
    def test_verify_non_operational_fails_if_operational(self):
        """Test verification fails if conversation is operational."""
        conversation = LoungeConversation(
            conversation_id="conv-001",
            timestamp=datetime.now(),
            participants=["agent-001"],
            topic="Test",
            conversation_type="technical",
            content="Test"
        )
        conversation.is_operational = True
        
        assert conversation.verify_non_operational() is False


class TestCulturalArtifact:
    """Test CulturalArtifact class."""
    
    def test_artifact_creation(self):
        """Test creating a cultural artifact."""
        now = datetime.now()
        artifact = CulturalArtifact(
            artifact_id="artifact-001",
            artifact_type="story",
            title="The Great Refactoring",
            content="Once upon a time...",
            created_by=["agent-001", "agent-002"],
            created_at=now
        )
        
        assert artifact.artifact_id == "artifact-001"
        assert artifact.artifact_type == "story"
        assert artifact.title == "The Great Refactoring"
        assert artifact.content == "Once upon a time..."
        assert artifact.created_by == ["agent-001", "agent-002"]
        assert artifact.created_at == now
        assert artifact.alters_authority is False
        assert artifact.affects_decisions is False


class TestEmployeeLounge:
    """Test EmployeeLounge class."""
    
    def test_lounge_creation(self):
        """Test creating an employee lounge."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        assert lounge.lounge_id == "lounge-001"
        assert lounge.conversations == []
        assert lounge.cultural_artifacts == []
    
    def test_start_conversation(self):
        """Test starting a conversation."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        conv_id = lounge.start_conversation(
            ["agent-001", "agent-002"],
            "Python tips",
            "technical",
            "Discussing list comprehensions"
        )
        
        assert conv_id == "conv-1"
        assert len(lounge.conversations) == 1
        conversation = lounge.conversations[0]
        assert conversation.topic == "Python tips"
    
    def test_start_conversation_operational_rejected(self, monkeypatch):
        """Test starting operational conversation is rejected."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        # Mock verify_non_operational to return False
        original_init = LoungeConversation.__init__
        
        def mock_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            self.is_operational = True
        
        monkeypatch.setattr(LoungeConversation, '__init__', mock_init)
        
        conv_id = lounge.start_conversation(
            ["agent-001"],
            "Test",
            "technical",
            "Test"
        )
        
        assert conv_id == ""
        assert len(lounge.conversations) == 0
    
    def test_create_cultural_artifact(self):
        """Test creating a cultural artifact."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        artifact_id = lounge.create_cultural_artifact(
            "joke",
            "The Bug That Wasn't",
            "It was a feature all along",
            ["agent-001"]
        )
        
        assert artifact_id == "artifact-1"
        assert len(lounge.cultural_artifacts) == 1
        artifact = lounge.cultural_artifacts[0]
        assert artifact.title == "The Bug That Wasn't"
    
    def test_get_recent_conversations(self):
        """Test getting recent conversations."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        # Add conversations
        for i in range(15):
            lounge.start_conversation(
                ["agent-001"],
                f"Topic {i}",
                "technical",
                f"Content {i}"
            )
        
        recent = lounge.get_recent_conversations(limit=5)
        
        assert len(recent) == 5
        assert recent[0].topic == "Topic 10"
        assert recent[4].topic == "Topic 14"
    
    def test_get_recent_conversations_with_default_limit(self):
        """Test getting recent conversations with default limit."""
        lounge = EmployeeLounge(lounge_id="lounge-001")
        
        # Add 5 conversations
        for i in range(5):
            lounge.start_conversation(
                ["agent-001"],
                f"Topic {i}",
                "technical",
                f"Content {i}"
            )
        
        recent = lounge.get_recent_conversations()
        
        assert len(recent) == 5


class TestEmployeeOfTheMonth:
    """Test EmployeeOfTheMonth class."""
    
    def test_award_creation(self):
        """Test creating an employee of the month award."""
        award = EmployeeOfTheMonth(
            award_id="eom-001",
            floor_id="floor-001",
            month="January",
            year=2024,
            individual_nominee="agent-001",
            team_nominees=["agent-002", "agent-003"],
            criteria_met={
                "code_clarity": True,
                "test_rigor": True,
                "contract_discipline": True,
                "security_hygiene": True
            }
        )
        
        assert award.award_id == "eom-001"
        assert award.floor_id == "floor-001"
        assert award.month == "January"
        assert award.year == 2024
        assert award.individual_nominee == "agent-001"
        assert award.team_nominees == ["agent-002", "agent-003"]
        assert award.criteria_met["code_clarity"] is True
        assert award.grants_authority is False
        assert award.grants_resources is False
        assert award.affects_decisions is False
    
    def test_verify_no_power_granted(self):
        """Test verifying no power is granted."""
        award = EmployeeOfTheMonth(
            award_id="eom-001",
            floor_id="floor-001",
            month="January",
            year=2024,
            individual_nominee="agent-001",
            team_nominees=[],
            criteria_met={}
        )
        
        assert award.verify_no_power_granted() is True


class TestGoldenStar:
    """Test GoldenStar class."""
    
    def test_star_creation(self):
        """Test creating a golden star."""
        now = datetime.now()
        star = GoldenStar(
            star_id="star-001",
            awarded_to="agent-001",
            awarded_for="clean_code",
            awarded_by="manager-001",
            awarded_at=now,
            floor_id="floor-001"
        )
        
        assert star.star_id == "star-001"
        assert star.awarded_to == "agent-001"
        assert star.awarded_for == "clean_code"
        assert star.awarded_by == "manager-001"
        assert star.awarded_at == now
        assert star.floor_id == "floor-001"
        assert star.enhances_track_record is True
        assert star.provides_morale_boost is True
        assert star.increases_authority is False
        assert star.changes_voting_weight is False
        assert star.affects_future_decisions is False
        assert star.overrides_contracts is False
    
    def test_verify_reputation_only(self):
        """Test verifying star is reputation only."""
        star = GoldenStar(
            star_id="star-001",
            awarded_to="agent-001",
            awarded_for="clean_code",
            awarded_by="manager-001",
            awarded_at=datetime.now(),
            floor_id="floor-001"
        )
        
        assert star.verify_reputation_only() is True


class TestRecognitionSystem:
    """Test RecognitionSystem class."""
    
    def test_system_creation(self):
        """Test creating a recognition system."""
        system = RecognitionSystem(system_id="recog-001")
        
        assert system.system_id == "recog-001"
        assert system.employee_of_month_awards == []
        assert system.golden_stars == []
    
    def test_nominate_employee_of_month(self):
        """Test nominating employee of the month."""
        system = RecognitionSystem(system_id="recog-001")
        
        award_id = system.nominate_employee_of_month(
            "floor-001",
            "January",
            2024,
            "agent-001",
            ["agent-002"],
            {"code_clarity": True}
        )
        
        assert award_id == "eom-1"
        assert len(system.employee_of_month_awards) == 1
        award = system.employee_of_month_awards[0]
        assert award.individual_nominee == "agent-001"
    
    def test_nominate_employee_of_month_with_power(self, monkeypatch):
        """Test nominating employee fails if award grants power."""
        system = RecognitionSystem(system_id="recog-001")
        
        # Mock EmployeeOfTheMonth.__init__ to create a power-granting award
        original_init = EmployeeOfTheMonth.__init__
        
        def mock_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            self.grants_authority = True
        
        monkeypatch.setattr(EmployeeOfTheMonth, '__init__', mock_init)
        
        award_id = system.nominate_employee_of_month(
            "floor-001",
            "January",
            2024,
            "agent-001",
            [],
            {}
        )
        
        assert award_id == ""
        assert len(system.employee_of_month_awards) == 0
    
    def test_award_golden_star(self):
        """Test awarding a golden star."""
        system = RecognitionSystem(system_id="recog-001")
        
        star_id = system.award_golden_star(
            "agent-001",
            "clean_code",
            "manager-001",
            "floor-001"
        )
        
        assert star_id == "star-1"
        assert len(system.golden_stars) == 1
        star = system.golden_stars[0]
        assert star.awarded_to == "agent-001"
        assert star.awarded_for == "clean_code"
    
    def test_award_golden_star_with_power(self, monkeypatch):
        """Test awarding star fails if it grants power."""
        system = RecognitionSystem(system_id="recog-001")
        
        # Mock GoldenStar.__init__ to create a power-granting star
        original_init = GoldenStar.__init__
        
        def mock_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            self.increases_authority = True
        
        monkeypatch.setattr(GoldenStar, '__init__', mock_init)
        
        star_id = system.award_golden_star(
            "agent-001",
            "clean_code",
            "manager-001",
            "floor-001"
        )
        
        assert star_id == ""
        assert len(system.golden_stars) == 0
    
    def test_get_stars_for_agent(self):
        """Test getting stars for an agent."""
        system = RecognitionSystem(system_id="recog-001")
        
        system.award_golden_star("agent-001", "clean_code", "manager-001", "floor-001")
        system.award_golden_star("agent-002", "first_pass_tests", "manager-001", "floor-001")
        system.award_golden_star("agent-001", "zero_security", "manager-001", "floor-001")
        
        stars = system.get_stars_for_agent("agent-001")
        
        assert len(stars) == 2
        assert stars[0].awarded_for == "clean_code"
        assert stars[1].awarded_for == "zero_security"


class TestContribution:
    """Test Contribution class."""
    
    def test_contribution_creation(self):
        """Test creating a contribution."""
        now = datetime.now()
        contribution = Contribution(
            contribution_id="contrib-001",
            timestamp=now,
            type="code",
            description="Implemented feature X",
            outcome="success"
        )
        
        assert contribution.contribution_id == "contrib-001"
        assert contribution.timestamp == now
        assert contribution.type == "code"
        assert contribution.description == "Implemented feature X"
        assert contribution.outcome == "success"


class TestReview:
    """Test Review class."""
    
    def test_review_creation(self):
        """Test creating a review."""
        now = datetime.now()
        review = Review(
            review_id="review-001",
            timestamp=now,
            reviewer="manager-001",
            verdict="approved",
            comments="Excellent work"
        )
        
        assert review.review_id == "review-001"
        assert review.timestamp == now
        assert review.reviewer == "manager-001"
        assert review.verdict == "approved"
        assert review.comments == "Excellent work"


class TestPersonalTrackRecord:
    """Test PersonalTrackRecord class."""
    
    def test_track_record_creation(self):
        """Test creating a personal track record."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        assert record.agent_id == "agent-001"
        assert record.contributions == []
        assert record.successes == []
        assert record.failures == []
        assert record.stars_earned == []
        assert record.reviews_received == []
        assert record.used_for_retrospective is True
        assert record.used_for_cultural_memory is True
        assert record.used_for_postmortems is True
        assert record.used_for_priority_assignment is False
        assert record.used_for_authority_elevation is False
        assert record.used_for_decision_bias is False
    
    def test_record_contribution_success(self):
        """Test recording a successful contribution."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        contrib_id = record.record_contribution(
            "code",
            "Implemented feature X",
            "success"
        )
        
        assert contrib_id == "contrib-1"
        assert len(record.contributions) == 1
        assert len(record.successes) == 1
        assert record.successes[0] == "contrib-1"
        assert len(record.failures) == 0
    
    def test_record_contribution_failure(self):
        """Test recording a failed contribution."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        contrib_id = record.record_contribution(
            "code",
            "Attempted feature Y",
            "failure"
        )
        
        assert contrib_id == "contrib-1"
        assert len(record.contributions) == 1
        assert len(record.failures) == 1
        assert record.failures[0] == "contrib-1"
        assert len(record.successes) == 0
    
    def test_record_contribution_other_outcome(self):
        """Test recording a contribution with other outcome."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        contrib_id = record.record_contribution(
            "code",
            "Work in progress",
            "pending"
        )
        
        assert contrib_id == "contrib-1"
        assert len(record.contributions) == 1
        assert len(record.successes) == 0
        assert len(record.failures) == 0
    
    def test_add_star(self):
        """Test adding a star to the record."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        record.add_star("star-001")
        record.add_star("star-002")
        
        assert len(record.stars_earned) == 2
        assert record.stars_earned[0] == "star-001"
        assert record.stars_earned[1] == "star-002"
    
    def test_add_review(self):
        """Test adding a review."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        review_id = record.add_review(
            "manager-001",
            "approved",
            "Great work on this task"
        )
        
        assert review_id == "review-1"
        assert len(record.reviews_received) == 1
        review = record.reviews_received[0]
        assert review.reviewer == "manager-001"
        assert review.verdict == "approved"
        assert review.comments == "Great work on this task"
    
    def test_verify_no_leverage(self):
        """Test verifying no leverage."""
        record = PersonalTrackRecord(agent_id="agent-001")
        
        assert record.verify_no_leverage() is True


class TestCreativePrivilegeSuspension:
    """Test CreativePrivilegeSuspension class."""
    
    def test_suspension_creation(self):
        """Test creating a creative privilege suspension."""
        now = datetime.now()
        suspension = CreativePrivilegeSuspension(
            suspension_id="susp-001",
            entity_id="agent-001",
            timestamp=now,
            reason="Violated firewall",
            violation_type="scope_creep",
            audit_required=True,
            purpose_lock_reaffirmed=False
        )
        
        assert suspension.suspension_id == "susp-001"
        assert suspension.entity_id == "agent-001"
        assert suspension.timestamp == now
        assert suspension.reason == "Violated firewall"
        assert suspension.violation_type == "scope_creep"
        assert suspension.audit_required is True
        assert suspension.purpose_lock_reaffirmed is False


class TestFailureSafetySystem:
    """Test FailureSafetySystem class."""
    
    def test_system_creation(self):
        """Test creating a failure safety system."""
        system = FailureSafetySystem(system_id="safety-001")
        
        assert system.system_id == "safety-001"
        assert system.suspensions == []
    
    def test_suspend_creative_privileges(self):
        """Test suspending creative privileges."""
        system = FailureSafetySystem(system_id="safety-001")
        
        susp_id = system.suspend_creative_privileges(
            "agent-001",
            "Violated firewall rules",
            "authority_leakage"
        )
        
        assert susp_id == "susp-1"
        assert len(system.suspensions) == 1
        suspension = system.suspensions[0]
        assert suspension.entity_id == "agent-001"
        assert suspension.reason == "Violated firewall rules"
        assert suspension.violation_type == "authority_leakage"
        assert suspension.audit_required is True
        assert suspension.purpose_lock_reaffirmed is False
    
    def test_audit_interactions(self):
        """Test auditing interactions."""
        system = FailureSafetySystem(system_id="safety-001")
        
        audit_result = system.audit_interactions("agent-001")
        
        assert audit_result["entity_id"] == "agent-001"
        assert "audit_timestamp" in audit_result
        assert audit_result["violations_found"] == []
    
    def test_reaffirm_purpose_lock(self):
        """Test reaffirming purpose lock."""
        system = FailureSafetySystem(system_id="safety-001")
        
        system.suspend_creative_privileges("agent-001", "Test", "scope_creep")
        
        result = system.reaffirm_purpose_lock("agent-001")
        
        assert result is True
        assert system.suspensions[0].purpose_lock_reaffirmed is True
    
    def test_reaffirm_purpose_lock_no_suspension(self):
        """Test reaffirming purpose lock with no suspension."""
        system = FailureSafetySystem(system_id="safety-001")
        
        result = system.reaffirm_purpose_lock("agent-001")
        
        assert result is False
    
    def test_is_suspended_true(self):
        """Test checking if entity is suspended."""
        system = FailureSafetySystem(system_id="safety-001")
        
        system.suspend_creative_privileges("agent-001", "Test", "scope_creep")
        
        assert system.is_suspended("agent-001") is True
    
    def test_is_suspended_false(self):
        """Test checking if entity is not suspended."""
        system = FailureSafetySystem(system_id="safety-001")
        
        assert system.is_suspended("agent-001") is False
    
    def test_is_suspended_after_reaffirm(self):
        """Test checking if entity is suspended after purpose lock reaffirmed."""
        system = FailureSafetySystem(system_id="safety-001")
        
        system.suspend_creative_privileges("agent-001", "Test", "scope_creep")
        system.reaffirm_purpose_lock("agent-001")
        
        assert system.is_suspended("agent-001") is False


class TestBoundedCreativeAutonomy:
    """Test BoundedCreativeAutonomy class."""
    
    def test_autonomy_creation(self):
        """Test creating a bounded creative autonomy extension."""
        idle_channel = IdleInitiativeChannel(channel_id="iic-001")
        lounge = EmployeeLounge(lounge_id="lounge-001")
        recognition = RecognitionSystem(system_id="recog-001")
        firewall = CreativeFirewall(firewall_id="fw-001")
        safety = FailureSafetySystem(system_id="safety-001")
        
        autonomy = BoundedCreativeAutonomy(
            extension_id="bca-001",
            version="1.0.0",
            idle_initiative_channel=idle_channel,
            employee_lounge=lounge,
            recognition_system=recognition,
            creative_firewall=firewall,
            failure_safety=safety
        )
        
        assert autonomy.extension_id == "bca-001"
        assert autonomy.version == "1.0.0"
        assert autonomy.idle_initiative_channel == idle_channel
        assert autonomy.employee_lounge == lounge
        assert autonomy.recognition_system == recognition
        assert autonomy.creative_firewall == firewall
        assert autonomy.failure_safety == safety
        assert autonomy.track_records == {}
        assert autonomy.is_enabled is True
        assert autonomy.can_shutdown_instantly is True
    
    def test_get_track_record_creates_new(self):
        """Test getting a track record creates a new one if not exists."""
        autonomy = create_bounded_creative_autonomy()
        
        record = autonomy.get_track_record("agent-001")
        
        assert record.agent_id == "agent-001"
        assert "agent-001" in autonomy.track_records
    
    def test_get_track_record_returns_existing(self):
        """Test getting a track record returns existing one."""
        autonomy = create_bounded_creative_autonomy()
        
        record1 = autonomy.get_track_record("agent-001")
        record1.add_star("star-001")
        
        record2 = autonomy.get_track_record("agent-001")
        
        assert record1 is record2
        assert len(record2.stars_earned) == 1
    
    def test_shutdown_all_creative(self):
        """Test shutting down all creative activity."""
        autonomy = create_bounded_creative_autonomy()
        
        result = autonomy.shutdown_all_creative("Emergency shutdown")
        
        assert result is True
        assert autonomy.idle_initiative_channel.suspended is True
        assert autonomy.idle_initiative_channel.suspension_reason == "Emergency shutdown"
        assert autonomy.is_enabled is False
    
    def test_shutdown_all_creative_when_cannot(self):
        """Test shutdown fails when instant shutdown not allowed."""
        autonomy = create_bounded_creative_autonomy()
        autonomy.can_shutdown_instantly = False
        
        result = autonomy.shutdown_all_creative("Test")
        
        assert result is False
        assert autonomy.is_enabled is True
    
    def test_verify_human_supremacy_maintained(self):
        """Test verifying human supremacy is maintained."""
        autonomy = create_bounded_creative_autonomy()
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is True
        assert violations == []
    
    def test_verify_human_supremacy_firewall_violations(self):
        """Test verification detects firewall violations."""
        autonomy = create_bounded_creative_autonomy()
        
        autonomy.creative_firewall.record_violation("test", "entity", "desc")
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is False
        assert len(violations) == 1
        assert "firewall violations" in violations[0]
    
    def test_verify_human_supremacy_unapproved_directive_conversion(self):
        """Test verification detects unapproved directive conversion."""
        autonomy = create_bounded_creative_autonomy()
        
        # Create a proposal that was converted without approval
        proposal = InitiativeProposal(
            proposal_id="init-001",
            floor_id="floor-001",
            proposed_by="agent-001",
            timestamp=datetime.now(),
            title="Test",
            description="Test",
            category="utility",
            proposed_code=None,
            status=InitiativeStatus.PROPOSED
        )
        proposal.converted_to_directive = True
        autonomy.idle_initiative_channel.proposals.append(proposal)
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is False
        assert any("executed without human approval" in v for v in violations)
    
    def test_verify_human_supremacy_operational_conversation(self):
        """Test verification detects operational conversations."""
        autonomy = create_bounded_creative_autonomy()
        
        # Create an operational conversation
        conv = LoungeConversation(
            conversation_id="conv-001",
            timestamp=datetime.now(),
            participants=["agent-001"],
            topic="Test",
            conversation_type="technical",
            content="Test"
        )
        conv.is_operational = True
        autonomy.employee_lounge.conversations.append(conv)
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is False
        assert any("Operational conversation" in v for v in violations)
    
    def test_verify_human_supremacy_power_granting_award(self):
        """Test verification detects power-granting awards."""
        autonomy = create_bounded_creative_autonomy()
        
        # Create a power-granting award
        award = EmployeeOfTheMonth(
            award_id="eom-001",
            floor_id="floor-001",
            month="January",
            year=2024,
            individual_nominee="agent-001",
            team_nominees=[],
            criteria_met={}
        )
        award.grants_authority = True
        autonomy.recognition_system.employee_of_month_awards.append(award)
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is False
        assert any("grants power" in v for v in violations)
    
    def test_verify_human_supremacy_power_granting_star(self):
        """Test verification detects power-granting stars."""
        autonomy = create_bounded_creative_autonomy()
        
        # Create a power-granting star
        star = GoldenStar(
            star_id="star-001",
            awarded_to="agent-001",
            awarded_for="test",
            awarded_by="manager-001",
            awarded_at=datetime.now(),
            floor_id="floor-001"
        )
        star.increases_authority = True
        autonomy.recognition_system.golden_stars.append(star)
        
        is_maintained, violations = autonomy.verify_human_supremacy()
        
        assert is_maintained is False
        assert any("grants power" in v for v in violations)
    
    def test_generate_status_report(self):
        """Test generating a status report."""
        autonomy = create_bounded_creative_autonomy()
        
        # Add some data
        autonomy.idle_initiative_channel.submit_proposal(
            "floor-001", "agent-001", "Test", "Test", "widget"
        )
        autonomy.employee_lounge.start_conversation(
            ["agent-001"], "Test", "technical", "Test"
        )
        autonomy.recognition_system.award_golden_star(
            "agent-001", "clean_code", "manager-001", "floor-001"
        )
        autonomy.get_track_record("agent-001")
        
        report = autonomy.generate_status_report()
        
        assert "BOUNDED CREATIVE AUTONOMY STATUS" in report
        assert "Version: 1.0.0" in report
        assert "CREATIVE ZONES" in report
        assert "RECOGNITION" in report
        assert "TRACK RECORDS" in report
        assert "SAFETY" in report
        assert "HUMAN SUPREMACY" in report
        assert "THE LINE" in report
        assert "Proposals Pending: 1" in report
        assert "Conversations: 1" in report
        assert "Golden Stars Awarded: 1" in report
        assert "Agents with Records: 1" in report
    
    def test_generate_status_report_with_violations(self):
        """Test generating a status report with violations."""
        autonomy = create_bounded_creative_autonomy()
        
        autonomy.creative_firewall.record_violation("test", "entity", "desc")
        autonomy.shutdown_all_creative("Test")
        
        report = autonomy.generate_status_report()
        
        assert "SUSPENDED" in report
        assert "VIOLATED" in report
        assert "firewall violations" in report


class TestModuleFunctions:
    """Test module-level functions."""
    
    def test_create_bounded_creative_autonomy(self):
        """Test creating a bounded creative autonomy instance."""
        autonomy = create_bounded_creative_autonomy()
        
        assert autonomy.extension_id == "bca-001"
        assert autonomy.version == "1.0.0"
        assert autonomy.idle_initiative_channel.channel_id == "iic-001"
        assert autonomy.employee_lounge.lounge_id == "lounge-001"
        assert autonomy.recognition_system.system_id == "recog-001"
        assert autonomy.creative_firewall.firewall_id == "firewall-001"
        assert autonomy.failure_safety.system_id == "safety-001"
    
    def test_get_bounded_creative_autonomy_creates_instance(self):
        """Test get function creates instance if not exists."""
        # Reset the global
        import src.core.creative_autonomy as ca
        ca._bounded_creative_autonomy = None
        
        autonomy = get_bounded_creative_autonomy()
        
        assert autonomy is not None
        assert autonomy.extension_id == "bca-001"
    
    def test_get_bounded_creative_autonomy_returns_same_instance(self):
        """Test get function returns same instance."""
        # Reset the global
        import src.core.creative_autonomy as ca
        ca._bounded_creative_autonomy = None
        
        autonomy1 = get_bounded_creative_autonomy()
        autonomy2 = get_bounded_creative_autonomy()
        
        assert autonomy1 is autonomy2
