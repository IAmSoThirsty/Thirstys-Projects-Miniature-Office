"""
Tests for Constitutional Mutation Engine
Comprehensive coverage for controlled system evolution
"""
import pytest
from unittest.mock import Mock, patch
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


class TestSimulationResult:
    """Test SimulationResult dataclass"""
    
    def test_simulation_result_to_dict(self):
        """Test simulation result serialization - covers line 134"""
        result = SimulationResult(
            simulation_id="sim-001",
            success=True,
            outcome_summary="Test passed",
            metrics={'stability': 0.95, 'performance': 0.92},
            risks_identified=["risk1", "risk2"]
        )
        
        data = result.to_dict()
        assert data['simulation_id'] == "sim-001"
        assert data['success'] is True
        assert data['outcome_summary'] == "Test passed"
        assert data['metrics']['stability'] == 0.95
        assert len(data['risks_identified']) == 2


class TestMutationProposalAdvanced:
    """Test advanced MutationProposal features"""
    
    def test_add_simulation(self):
        """Test adding simulation results - covers lines 236-237"""
        proposal = MutationProposal(
            proposal_id="prop-sim-1",
            proposed_change="Test change",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        sim_result = SimulationResult(
            simulation_id="sim-001",
            success=True,
            outcome_summary="Success",
            metrics={},
            risks_identified=[]
        )
        
        proposal.add_simulation(sim_result)
        
        assert len(proposal.simulations) == 1
        assert len(proposal.impact_simulation_ids) == 1
        assert proposal.impact_simulation_ids[0] == "sim-001"
    
    def test_calculate_approval_no_votes(self):
        """Test approval calculation with no votes - covers line 241-242"""
        proposal = MutationProposal(
            proposal_id="prop-vote-1",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        assert proposal.calculate_approval() is False
    
    def test_calculate_approval_passes(self):
        """Test approval calculation that passes - covers lines 244-247"""
        proposal = MutationProposal(
            proposal_id="prop-vote-2",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        proposal.votes = {
            "mgr-001": True,
            "mgr-002": True,
            "mgr-003": True,
            "mgr-004": False
        }
        
        # 3/4 = 75% >= 67% threshold (should pass)
        assert proposal.calculate_approval() is True
    
    def test_calculate_approval_fails(self):
        """Test approval calculation that fails - covers lines 244-247"""
        proposal = MutationProposal(
            proposal_id="prop-vote-3",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        proposal.votes = {
            "mgr-001": True,
            "mgr-002": False,
            "mgr-003": False
        }
        
        # 1/3 = 33.33% < 67% threshold
        assert proposal.calculate_approval() is False
    
    def test_schedule_activation(self):
        """Test scheduling delayed activation - covers lines 254-256"""
        proposal = MutationProposal(
            proposal_id="prop-activate-1",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        proposal.activation_delay_ticks = 100
        proposal.schedule_activation(1000)
        
        assert proposal.delayed_activation_tick == 1100
    
    def test_rollback_without_path(self):
        """Test rollback without rollback path - covers line 270-271"""
        proposal = MutationProposal(
            proposal_id="prop-rollback-1",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        # Should raise ValueError if no rollback path
        with pytest.raises(ValueError, match="No rollback path defined"):
            proposal.rollback("Testing rollback")
    
    def test_rollback_with_path(self):
        """Test rollback with rollback path - covers lines 270-276"""
        proposal = MutationProposal(
            proposal_id="prop-rollback-2",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        proposal.rollback_path = '{"revert": "to previous state"}'
        proposal.rollback("Critical issue detected")
        
        assert proposal.is_rolled_back is True
        assert proposal.status == MutationStatus.ROLLED_BACK


class TestConstitutionalMutationEngineAdvanced:
    """Test advanced engine features"""
    
    def test_simulate_mutation(self):
        """Test mutation simulation - covers lines 391-408"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-sim-test",
            proposed_change="Test change",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        results = engine.simulate_mutation("prop-sim-test")
        
        assert len(results) == 1
        assert results[0].success is True
        assert proposal.status == MutationStatus.SIMULATING
        assert len(proposal.simulations) == 1
    
    def test_simulate_mutation_invalid_proposal(self):
        """Test simulating invalid proposal - covers lines 392-393"""
        engine = ConstitutionalMutationEngine()
        
        results = engine.simulate_mutation("nonexistent-id")
        assert results == []
    
    def test_assess_risk_low(self):
        """Test risk assessment - LOW risk - covers lines 412-437"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-risk-1",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        # Add simulation with no risks
        sim = SimulationResult(
            simulation_id="sim-1",
            success=True,
            outcome_summary="Success",
            risks_identified=[]
        )
        proposal.add_simulation(sim)
        
        engine.submit_proposal(proposal)
        risk = engine.assess_risk("prop-risk-1")
        
        assert risk.level == "LOW"
    
    def test_assess_risk_medium(self):
        """Test risk assessment - MEDIUM risk - covers lines 422-425"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-risk-2",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        # Add simulation with 2 risks
        sim = SimulationResult(
            simulation_id="sim-2",
            success=True,
            outcome_summary="Success",
            risks_identified=["risk1", "risk2"]
        )
        proposal.add_simulation(sim)
        
        engine.submit_proposal(proposal)
        risk = engine.assess_risk("prop-risk-2")
        
        assert risk.level == "MEDIUM"
    
    def test_assess_risk_high(self):
        """Test risk assessment - HIGH risk - covers lines 426-427"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-risk-3",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        # Add simulation with 4 risks
        sim = SimulationResult(
            simulation_id="sim-3",
            success=True,
            outcome_summary="Success",
            risks_identified=["risk1", "risk2", "risk3", "risk4"]
        )
        proposal.add_simulation(sim)
        
        engine.submit_proposal(proposal)
        risk = engine.assess_risk("prop-risk-3")
        
        assert risk.level == "HIGH"
    
    def test_assess_risk_critical(self):
        """Test risk assessment - CRITICAL risk - covers lines 428-429"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-risk-4",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        # Add simulation with 5+ risks
        sim = SimulationResult(
            simulation_id="sim-4",
            success=True,
            outcome_summary="Success",
            risks_identified=["risk1", "risk2", "risk3", "risk4", "risk5"]
        )
        proposal.add_simulation(sim)
        
        engine.submit_proposal(proposal)
        risk = engine.assess_risk("prop-risk-4")
        
        assert risk.level == "CRITICAL"
    
    def test_assess_risk_invalid_proposal(self):
        """Test risk assessment on invalid proposal - covers lines 412-414"""
        engine = ConstitutionalMutationEngine()
        
        risk = engine.assess_risk("nonexistent")
        assert risk.level == "UNKNOWN"
    
    def test_initiate_vote(self):
        """Test initiating vote - covers lines 441-443"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-vote-test",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        engine.initiate_vote("prop-vote-test")
        
        assert proposal.status == MutationStatus.VOTING
    
    def test_initiate_vote_invalid_proposal(self):
        """Test initiating vote on invalid proposal - covers line 441-442"""
        engine = ConstitutionalMutationEngine()
        
        # Should not crash on invalid proposal
        engine.initiate_vote("nonexistent")
    
    def test_record_vote(self):
        """Test recording manager vote - covers lines 447-451"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-record-vote",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        engine.record_vote("prop-record-vote", "mgr-001", True)
        engine.record_vote("prop-record-vote", "mgr-002", False)
        
        assert len(proposal.votes) == 2
        assert proposal.votes["mgr-001"] is True
        assert proposal.votes["mgr-002"] is False
    
    def test_record_vote_invalid_proposal(self):
        """Test recording vote on invalid proposal - covers line 447-448"""
        engine = ConstitutionalMutationEngine()
        
        # Should not crash on invalid proposal
        engine.record_vote("nonexistent", "mgr-001", True)
    
    def test_meta_office_ruling_invalid_proposal(self):
        """Test meta-office ruling on invalid proposal - covers line 465"""
        engine = ConstitutionalMutationEngine()
        
        result = engine.meta_office_ruling("nonexistent", True, "Test")
        assert result is False
    
    def test_meta_office_ruling_rejection(self):
        """Test meta-office rejection - covers lines 484-486"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-reject",
            proposed_change="Valid change",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        result = engine.meta_office_ruling("prop-reject", False, "Not ready")
        
        assert result is False
        assert proposal.status == MutationStatus.REJECTED
    
    def test_activate_mutation_invalid_proposal(self):
        """Test activating invalid proposal - covers lines 506-508"""
        engine = ConstitutionalMutationEngine()
        
        result = engine.activate_mutation("nonexistent", 1000)
        assert result is False
    
    def test_activate_mutation_wrong_status(self):
        """Test activating proposal with wrong status - covers lines 510-511"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-activate-wrong",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        # Status is UNDER_REVIEW, not APPROVED
        result = engine.activate_mutation("prop-activate-wrong", 1000)
        
        assert result is False
    
    def test_activate_mutation_no_activation_tick(self):
        """Test activating proposal without activation tick - covers lines 513-514"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-activate-no-tick",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Set status to approved after submission
        proposal.status = MutationStatus.APPROVED
        # Explicitly ensure delayed_activation_tick is None
        proposal.delayed_activation_tick = None
        
        result = engine.activate_mutation("prop-activate-no-tick", 1000)
        assert result is False
    
    def test_activate_mutation_too_early(self):
        """Test activating proposal too early - covers lines 516-517"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-activate-early",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Set status to approved and activation tick after submission
        proposal.status = MutationStatus.APPROVED
        proposal.delayed_activation_tick = 2000
        
        # Current tick is before activation tick
        result = engine.activate_mutation("prop-activate-early", 1000)
        assert result is False
    
    def test_activate_mutation_success(self):
        """Test successful mutation activation - covers lines 506-522"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-activate-success",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Set status to approved and activation tick manually
        proposal.status = MutationStatus.APPROVED
        proposal.delayed_activation_tick = 1000
        
        # Current tick is at or after activation tick
        result = engine.activate_mutation("prop-activate-success", 1000)
        
        assert result is True
        assert proposal.status == MutationStatus.ACTIVE
    
    def test_get_active_laws(self):
        """Test getting all active laws - covers line 526"""
        engine = ConstitutionalMutationEngine()
        
        laws = engine.get_active_laws()
        
        # Should have 5 core laws
        assert len(laws) == 5
        assert all(isinstance(law, ConstitutionalLaw) for law in laws)
    
    def test_get_proposal(self):
        """Test getting a proposal - covers line 530"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-get-test",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        
        retrieved = engine.get_proposal("prop-get-test")
        assert retrieved is not None
        assert retrieved.proposal_id == "prop-get-test"
        
        # Test non-existent
        none_proposal = engine.get_proposal("nonexistent")
        assert none_proposal is None
    
    def test_meta_office_ruling_with_world(self):
        """Test meta-office ruling with world for activation scheduling - covers line 484"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-with-world",
            proposed_change="Add new rule",
            justification="Security improvement",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        
        # Mock the world to cover line 484
        mock_world = Mock()
        mock_world.time = 500
        
        with patch('src.core.world.get_world', return_value=mock_world):
            result = engine.meta_office_ruling("prop-with-world", True, "Approved")
        
        assert result is True
        assert proposal.status == MutationStatus.APPROVED
        assert proposal.delayed_activation_tick == 600  # 500 + 100 default delay
    
    def test_activate_mutation_at_exact_tick(self):
        """Test activating mutation exactly at activation tick - covers lines 513-522"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-exact-tick",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        proposal.status = MutationStatus.APPROVED
        proposal.delayed_activation_tick = 1000
        
        # At exact tick (should activate)
        result = engine.activate_mutation("prop-exact-tick", 1000)
        assert result is True
        assert proposal.status == MutationStatus.ACTIVE
    
    def test_activate_mutation_after_tick(self):
        """Test activating mutation after activation tick - covers lines 513-522"""
        engine = ConstitutionalMutationEngine()
        
        proposal = MutationProposal(
            proposal_id="prop-after-tick",
            proposed_change="Test",
            justification="Test",
            proposer_id="mgr-001"
        )
        
        engine.submit_proposal(proposal)
        proposal.status = MutationStatus.APPROVED
        proposal.delayed_activation_tick = 1000
        
        # After activation tick (should activate)
        result = engine.activate_mutation("prop-after-tick", 1500)
        assert result is True
        assert proposal.status == MutationStatus.ACTIVE
