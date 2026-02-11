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
    
    def test_amend_law(self):
        """Test amending a law"""
        law = ConstitutionalLaw(
            law_id="law-004",
            scope=LawScope.GLOBAL,
            statement="Original statement",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=0
        )
        
        law.amend("Updated statement", 1000)
        
        assert law.statement == "Updated statement"
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
        assert result['law_id'] == "law-005"
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
            impact_assessment="Low impact",
            rollback_path="Revert to version 1",
            submitted_at_tick=100
        )
        
        assert proposal.proposal_id == "prop-001"
        assert proposal.status == MutationStatus.DRAFT
        assert proposal.submitted_at_tick == 100
        assert proposal.approved_at_tick is None
    
    def test_proposal_transition(self):
        """Test status transitions"""
        proposal = MutationProposal(
            proposal_id="prop-002",
            proposed_change="Test change",
            impact_assessment="Test impact",
            rollback_path="Test rollback",
            submitted_at_tick=0
        )
        
        proposal.transition_to(MutationStatus.UNDER_REVIEW, "Ready for review")
        assert proposal.status == MutationStatus.UNDER_REVIEW
        assert len(proposal.status_history) == 1
    
    def test_proposal_to_dict(self):
        """Test proposal serialization"""
        proposal = MutationProposal(
            proposal_id="prop-003",
            proposed_change="Change",
            impact_assessment="Impact",
            rollback_path="Rollback",
            submitted_at_tick=10
        )
        
        result = proposal.to_dict()
        assert result['proposal_id'] == "prop-003"
        assert result['status'] == 'draft'
        assert result['submitted_at_tick'] == 10


class TestConstitutionalMutationEngine:
    """Test ConstitutionalMutationEngine class"""
    
    def test_engine_creation(self):
        """Test creating constitution engine"""
        engine = ConstitutionalMutationEngine()
        assert len(engine.laws) == 0
        assert len(engine.proposals) == 0
    
    def test_add_law(self):
        """Test adding a law to the constitution"""
        engine = ConstitutionalMutationEngine()
        
        law = ConstitutionalLaw(
            law_id="law-100",
            scope=LawScope.GLOBAL,
            statement="Test law",
            enforcement=EnforcementLevel.HARD,
            introduced_at_tick=0
        )
        
        engine.add_law(law)
        assert len(engine.laws) == 1
        assert engine.get_law("law-100") == law
    
    def test_get_law_not_found(self):
        """Test getting non-existent law"""
        engine = ConstitutionalMutationEngine()
        assert engine.get_law("nonexistent") is None
    
    def test_submit_proposal(self):
        """Test submitting a mutation proposal"""
        engine = ConstitutionalMutationEngine()
        
        proposal_id = engine.submit_proposal(
            proposed_change="Add feature",
            impact_assessment="Low impact",
            rollback_path="Revert changes",
            submitted_at_tick=100
        )
        
        assert proposal_id is not None
        assert len(engine.proposals) == 1
        proposal = engine.proposals[proposal_id]
        assert proposal.status == MutationStatus.DRAFT
    
    def test_approve_proposal(self):
        """Test approving a proposal"""
        engine = ConstitutionalMutationEngine()
        
        proposal_id = engine.submit_proposal(
            proposed_change="Test",
            impact_assessment="Test",
            rollback_path="Test",
            submitted_at_tick=0
        )
        
        result = engine.approve_proposal(proposal_id, 1000)
        assert result is True
        
        proposal = engine.proposals[proposal_id]
        assert proposal.status == MutationStatus.APPROVED
        assert proposal.approved_at_tick == 1000
    
    def test_reject_proposal(self):
        """Test rejecting a proposal"""
        engine = ConstitutionalMutationEngine()
        
        proposal_id = engine.submit_proposal(
            proposed_change="Test",
            impact_assessment="Test",
            rollback_path="Test",
            submitted_at_tick=0
        )
        
        engine.reject_proposal(proposal_id, "Not needed")
        
        proposal = engine.proposals[proposal_id]
        assert proposal.status == MutationStatus.REJECTED
        assert proposal.rejection_reason == "Not needed"
    
    def test_meta_office_ruling(self):
        """Test meta-office ruling validation"""
        engine = ConstitutionalMutationEngine()
        
        proposal_id = engine.submit_proposal(
            proposed_change="Change meta-office authority rules",
            impact_assessment="High impact",
            rollback_path="Revert",
            submitted_at_tick=0
        )
        
        result = engine.require_meta_office_ruling(proposal_id, "Approved")
        
        # Should reject proposals that try to change meta-office authority
        assert result is False
        proposal = engine.proposals[proposal_id]
        assert proposal.status == MutationStatus.REJECTED


class TestGlobalEngine:
    """Test global engine accessor"""
    
    def test_get_mutation_engine(self):
        """Test getting global mutation engine"""
        engine1 = get_mutation_engine()
        engine2 = get_mutation_engine()
        
        # Should return same instance
        assert engine1 is engine2
