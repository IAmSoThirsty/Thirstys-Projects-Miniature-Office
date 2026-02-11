"""
Tests for Constitutional Mutation Engine
Comprehensive coverage for controlled system evolution
"""
import pytest
from src.core.constitutional_mutation import (
    ConstitutionalLaw,
    LawScope,
    EnforcementLevel,
    MutationStatus,
    MutationProposal,
    AmendmentRules,
    SimulationResult,
    RiskAssessment,
    ConstitutionalMutationEngine,
    get_mutation_engine
)


class TestAmendmentRules:
    """Test AmendmentRules dataclass"""
    
    def test_amendment_rules_defaults(self):
        """Test default amendment rules"""
        rules = AmendmentRules()
        assert rules.cooldown_ticks == 5000
        assert rules.requires_simulation is True
        assert rules.requires_rollback_path is True
        assert len(rules.requires) == 0
    
    def test_amendment_rules_to_dict(self):
        """Test amendment rules serialization"""
        rules = AmendmentRules(
            requires=["MetaOffice"],
            cooldown_ticks=1000,
            requires_simulation=False,
            requires_rollback_path=False
        )
        result = rules.to_dict()
        assert result['cooldown_ticks'] == 1000
        assert result['requires_simulation'] is False
        assert 'MetaOffice' in result['requires']


class TestConstitutionalLaw:
    """Test ConstitutionalLaw class"""
    
    def test_law_creation(self):
        """Test creating a constitutional law"""
        law = ConstitutionalLaw(
            law_id="law-001",
            scope=LawScope.GLOBAL,
            statement="All agents must log actions",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=100
        )
        
        assert law.law_id == "law-001"
        assert law.scope == LawScope.GLOBAL
        assert law.statement == "All agents must log actions"
        assert law.enforcement == EnforcementLevel.HARD
        assert law.introduced_at_tick == 100
        assert law.last_amended_tick is None
    
    def test_can_be_amended_no_previous_amendment(self):
        """Test amendment check with no previous amendments"""
        law = ConstitutionalLaw(
            law_id="law-002",
            scope=LawScope.DEPARTMENT,
            statement="Test law",
            enforcement=EnforcementLevel.SOFT,
            introduced_at_tick=0
        )
        
        assert law.can_be_amended(1000) is True
    
    def test_can_be_amended_with_cooldown(self):
        """Test amendment cooldown enforcement"""
        law = ConstitutionalLaw(
            law_id="law-003",
            scope=LawScope.OFFICE,
            statement="Test law",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=0
        )
        
        law.last_amended_tick = 100
        law.amendment_rules.cooldown_ticks = 1000
        
        # Too soon
        assert law.can_be_amended(500) is False
        
        # Exactly at cooldown
        assert law.can_be_amended(1100) is True
        
        # After cooldown
        assert law.can_be_amended(2000) is True
    
    def test_record_amendment(self):
        """Test recording an amendment"""
        law = ConstitutionalLaw(
            law_id="law-004",
            scope=LawScope.GLOBAL,
            statement="Original statement",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=0
        )
        
        law.record_amendment("Updated statement", 1000)
        
        # Statement doesn't change, only amendment is recorded
        assert law.last_amended_tick == 1000
        assert len(law.amendment_history) == 1
        assert law.amendment_history[0]['description'] == "Updated statement"
    
    def test_law_to_dict(self):
        """Test law serialization"""
        law = ConstitutionalLaw(
            law_id="law-005",
            scope=LawScope.DEPARTMENT,
            statement="Test statement",
            enforcement=EnforcementLevel.SOFT,
            introduced_at_tick=50
        )
        
        result = law.to_dict()
        assert result['lawId'] == "law-005"  # Note: camelCase in to_dict()
        assert result['scope'] == 'department'
        assert result['statement'] == "Test statement"
        assert result['enforcement'] == 'soft'


class TestMutationProposal:
    """Test MutationProposal class"""
    
    def test_proposal_creation(self):
        """Test creating a mutation proposal"""
        proposal = MutationProposal(
            proposal_id="prop-001",
            proposed_change="Add new validation",
            justification="Needed for security",
            proposer_id="manager-001"
        )
        
        assert proposal.proposal_id == "prop-001"
        assert proposal.status == MutationStatus.DRAFT
        assert proposal.submitted_at_tick == 0  # Default value
        assert proposal.proposed_change == "Add new validation"
    
    def test_proposal_transition(self):
        """Test status transitions"""
        proposal = MutationProposal(
            proposal_id="prop-002",
            proposed_change="Test change",
            justification="Test justification",
            proposer_id="manager-001"
        )
        
        proposal.transition_to(MutationStatus.UNDER_REVIEW, "Ready for review")
        assert proposal.status == MutationStatus.UNDER_REVIEW
        assert len(proposal.status_history) == 1
    
    def test_proposal_to_dict(self):
        """Test proposal serialization"""
        proposal = MutationProposal(
            proposal_id="prop-003",
            proposed_change="Change",
            justification="Justification",
            proposer_id="manager-001"
        )
        
        result = proposal.to_dict()
        assert result['proposalId'] == "prop-003"  # Note: camelCase
        assert result['status'] == 'draft'
        assert result['submittedAtTick'] == 0  # Default


class TestConstitutionalMutationEngine:
    """Test ConstitutionalMutationEngine class"""
    
    def test_engine_creation(self):
        """Test creating constitution engine"""
        engine = ConstitutionalMutationEngine()
        # Engine initializes with 5 core laws
        assert len(engine.laws) == 5
        assert len(engine.proposals) == 0
    
    def test_register_law(self):
        """Test registering a law to the constitution"""
        engine = ConstitutionalMutationEngine()
        
        law = ConstitutionalLaw(
            law_id="law-100",
            scope=LawScope.GLOBAL,
            statement="Test law",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=0
        )
        
        engine.register_law(law)
        assert len(engine.laws) == 6  # 5 core + 1 new
        assert "law-100" in engine.laws
    
    def test_get_law(self):
        """Test getting a law"""
        engine = ConstitutionalMutationEngine()
        
        # Get one of the core laws
        law = engine.laws.get("LAW-001")
        assert law is not None
        assert law.law_id == "LAW-001"
    
    def test_get_law_not_found(self):
        """Test getting non-existent law"""
        engine = ConstitutionalMutationEngine()
        law = engine.laws.get("nonexistent")
        assert law is None
    
    def test_submit_proposal(self):
        """Test submitting a mutation proposal"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-test-1",
            proposed_change="Add feature",
            justification="Improves functionality",
            proposer_id="manager-001"
        )
        
        proposal_id = engine.submit_proposal(proposal)
        
        assert proposal_id is not None
        assert proposal_id == "prop-test-1"
        assert len(engine.proposals) == 1
        # After submission, status transitions to UNDER_REVIEW
        assert proposal.status == MutationStatus.UNDER_REVIEW
    
    def test_meta_office_ruling(self):
        """Test meta-office ruling"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-test-2",
            proposed_change="Add validation rule",
            justification="Security improvement",
            proposer_id="manager-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Approve the proposal
        result = engine.meta_office_ruling("prop-test-2", True, "Approved by meta-office")
        
        assert result is True
        assert proposal.meta_office_ruling is True
        assert proposal.status == MutationStatus.APPROVED
    
    def test_meta_office_ruling_rejection_of_authority_change(self):
        """Test that meta-office rejects changes to its own authority"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-test-3",
            proposed_change="Change meta-office authority rules",
            justification="Attempting to change authority",
            proposer_id="manager-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Should reject proposals that try to change meta-office authority
        result = engine.meta_office_ruling("prop-test-3", True, "Should be rejected")
        
        assert result is False
        assert proposal.status == MutationStatus.REJECTED


class TestGlobalEngine:
    """Test global engine accessor"""
    
    def test_get_mutation_engine(self):
        """Test getting global mutation engine"""
        engine1 = get_mutation_engine()
        engine2 = get_mutation_engine()
        
        # Should return same instance
        assert engine1 is engine2
