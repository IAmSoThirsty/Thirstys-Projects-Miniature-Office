"""Unit tests for the cognitive contract system."""
import pytest
from src.core.cognitive_contract import (
    CognitiveContract, ContractRegistry, BindingLevel, Severity, 
    ContractStatus, Intent, DesignRationale, Stakeholders, 
    RiskProfile, ContractChallenge, RevocationJustification,
    get_contract_registry
)
from src.core.world import create_world


class TestIntent:
    """Test Intent class."""
    
    def test_intent_creation(self):
        """Test creating an intent."""
        intent = Intent(
            goal="Build feature X",
            constraints=["Must be secure"],
            non_goals=["Don't support legacy API"]
        )
        assert intent.goal == "Build feature X"
        assert len(intent.constraints) == 1
        assert len(intent.non_goals) == 1
    
    def test_intent_to_dict(self):
        """Test serializing intent."""
        intent = Intent(goal="Test goal")
        data = intent.to_dict()
        assert data['goal'] == "Test goal"
        assert 'constraints' in data


class TestDesignRationale:
    """Test DesignRationale class."""
    
    def test_rationale_creation(self):
        """Test creating design rationale."""
        rationale = DesignRationale(
            assumptions=["Assumption 1"],
            tradeoffs=["Tradeoff 1"],
            alternatives_rejected=["Alternative A"]
        )
        assert len(rationale.assumptions) == 1
    
    def test_rationale_to_dict(self):
        """Test serializing rationale."""
        rationale = DesignRationale()
        data = rationale.to_dict()
        assert 'assumptions' in data


class TestStakeholders:
    """Test Stakeholders class."""
    
    def test_stakeholders_creation(self):
        """Test creating stakeholders."""
        stakeholders = Stakeholders(
            departments=["dept-001"],
            managers=["mgr-001"],
            agents=["agent-001"]
        )
        assert len(stakeholders.departments) == 1
    
    def test_stakeholders_to_dict(self):
        """Test serializing stakeholders."""
        stakeholders = Stakeholders()
        data = stakeholders.to_dict()
        assert 'departments' in data


class TestRiskProfile:
    """Test RiskProfile class."""
    
    def test_risk_profile_creation(self):
        """Test creating risk profile."""
        profile = RiskProfile(
            severity=Severity.HIGH,
            failure_modes=["Mode 1", "Mode 2"]
        )
        assert profile.severity == Severity.HIGH
        assert len(profile.failure_modes) == 2
    
    def test_risk_profile_to_dict(self):
        """Test serializing risk profile."""
        profile = RiskProfile(severity=Severity.LOW)
        data = profile.to_dict()
        assert data['severity'] == "low"


class TestContractChallenge:
    """Test ContractChallenge class."""
    
    def test_challenge_creation(self):
        """Test creating a challenge."""
        challenge = ContractChallenge(
            challenger_id="agent-001",
            contract_id="contract-001",
            reason="Outdated assumptions",
            evidence=["Evidence 1"]
        )
        assert challenge.challenger_id == "agent-001"
        assert challenge.status == "pending"
    
    def test_challenge_to_dict(self):
        """Test serializing challenge."""
        challenge = ContractChallenge(
            challenger_id="agent-001",
            contract_id="contract-001",
            reason="Test"
        )
        data = challenge.to_dict()
        assert 'challenge_id' in data


class TestRevocationJustification:
    """Test RevocationJustification class."""
    
    def test_justification_creation(self):
        """Test creating justification."""
        justification = RevocationJustification(
            reason="Requirements changed",
            impact_analysis="Minimal impact",
            affected_tasks=["task-001"]
        )
        assert justification.reason == "Requirements changed"
        assert not justification.meta_office_approval
    
    def test_justification_to_dict(self):
        """Test serializing justification."""
        justification = RevocationJustification(
            reason="Test",
            impact_analysis="None"
        )
        data = justification.to_dict()
        assert data['reason'] == "Test"


class TestCognitiveContract:
    """Test CognitiveContract class."""
    
    def test_contract_creation(self):
        """Test creating a contract."""
        intent = Intent(goal="Build feature")
        contract = CognitiveContract(
            contract_id="contract-001",
            issued_at_tick=0,
            issuer="user-001",
            intent=intent
        )
        assert contract.entity_id == "contract-001"
        assert contract.status == ContractStatus.DRAFT
        assert contract.binding_level == BindingLevel.MANDATORY
    
    def test_contract_with_binding_level(self):
        """Test creating contract with specific binding level."""
        intent = Intent(goal="Build feature")
        contract = CognitiveContract(
            contract_id="contract-001",
            issued_at_tick=0,
            issuer="user-001",
            intent=intent,
            binding_level=BindingLevel.CONSTITUTIONAL
        )
        assert contract.binding_level == BindingLevel.CONSTITUTIONAL
    
    def test_transition_to_valid(self):
        """Test valid status transition."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        result = contract.transition_to(ContractStatus.REVIEW, "Ready for review")
        assert result
        assert contract.status == ContractStatus.REVIEW
    
    def test_transition_to_invalid(self):
        """Test invalid status transition."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        result = contract.transition_to(ContractStatus.ACTIVE, "Invalid")
        assert not result
        assert contract.status == ContractStatus.DRAFT
    
    def test_transition_to_ratified(self):
        """Test transition to ratified sets tick."""
        create_world("world-001", "Test World")
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        assert contract.ratified_at_tick is not None
    
    def test_add_stakeholder_department(self):
        """Test adding department stakeholder."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.add_stakeholder("department", "dept-001")
        assert "dept-001" in contract.stakeholders.departments
    
    def test_add_stakeholder_manager(self):
        """Test adding manager stakeholder."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.add_stakeholder("manager", "mgr-001")
        assert "mgr-001" in contract.stakeholders.managers
    
    def test_add_stakeholder_agent(self):
        """Test adding agent stakeholder."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.add_stakeholder("agent", "agent-001")
        assert "agent-001" in contract.stakeholders.agents
    
    def test_add_stakeholder_duplicate(self):
        """Test adding duplicate stakeholder."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.add_stakeholder("agent", "agent-001")
        contract.add_stakeholder("agent", "agent-001")
        assert len(contract.stakeholders.agents) == 1
    
    def test_bind_task(self):
        """Test binding task to contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.bind_task("task-001")
        assert "task-001" in contract.bound_tasks
    
    def test_bind_task_duplicate(self):
        """Test binding same task twice."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.bind_task("task-001")
        contract.bind_task("task-001")
        assert len(contract.bound_tasks) == 1
    
    def test_challenge(self):
        """Test challenging a contract."""
        create_world("world-001", "Test World")
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        
        challenge = contract.challenge(
            "agent-001",
            "Outdated assumptions",
            ["Evidence 1", "Evidence 2"]
        )
        
        assert challenge is not None
        assert len(contract.challenges) == 1
        assert challenge.contract_id == "c-001"
    
    def test_revoke_without_approval(self):
        """Test revoking without meta-office approval."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        contract.transition_to(ContractStatus.ACTIVE)
        
        justification = RevocationJustification(
            reason="Test",
            impact_analysis="None",
            meta_office_approval=False
        )
        
        result = contract.revoke(justification, "meta-office-001")
        assert not result
    
    def test_revoke_with_approval(self):
        """Test revoking with meta-office approval."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        contract.transition_to(ContractStatus.ACTIVE)
        
        justification = RevocationJustification(
            reason="Requirements changed",
            impact_analysis="Minimal",
            meta_office_approval=True
        )
        
        result = contract.revoke(justification, "meta-office-001")
        assert result
        assert contract.status == ContractStatus.REVOKED
        assert contract.revocation_justification is not None
    
    def test_supersede(self):
        """Test superseding a contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        contract.transition_to(ContractStatus.ACTIVE)
        
        contract.supersede("c-002")
        assert contract.status == ContractStatus.SUPERSEDED
        assert contract.superseded_by == "c-002"
    
    def test_validate_task_scope_within_intent(self):
        """Test validating task scope within intent."""
        intent = Intent(
            goal="Build authentication system",
            non_goals=["Don't handle payments"]
        )
        contract = CognitiveContract("c-001", 0, "user", intent)
        
        result = contract.validate_task_scope("Implement login flow")
        assert result
    
    def test_validate_task_scope_against_non_goals(self):
        """Test validating task scope against non-goals."""
        intent = Intent(
            goal="Build authentication",
            non_goals=["payment processing integration"]
        )
        contract = CognitiveContract("c-001", 0, "user", intent)
        
        result = contract.validate_task_scope("payment processing integration")
        assert not result
    
    def test_is_immutable_draft(self):
        """Test immutability of draft contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        assert not contract.is_immutable()
    
    def test_is_immutable_ratified(self):
        """Test immutability of ratified contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        assert contract.is_immutable()
    
    def test_is_immutable_active(self):
        """Test immutability of active contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        contract.transition_to(ContractStatus.ACTIVE)
        assert contract.is_immutable()
    
    def test_to_dict(self):
        """Test serializing contract."""
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 5, "user", intent)
        data = contract.to_dict()
        assert data['contractId'] == "c-001"
        assert data['issuedAtTick'] == 5
        assert data['status'] == "draft"


class TestContractRegistry:
    """Test ContractRegistry class."""
    
    def test_registry_creation(self):
        """Test creating a registry."""
        registry = ContractRegistry()
        assert len(registry.contracts) == 0
        assert len(registry.challenges_pending) == 0
    
    def test_register_contract(self):
        """Test registering a contract."""
        registry = ContractRegistry()
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        registry.register_contract(contract)
        assert len(registry.contracts) == 1
    
    def test_get_contract(self):
        """Test getting a contract."""
        registry = ContractRegistry()
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        registry.register_contract(contract)
        
        retrieved = registry.get_contract("c-001")
        assert retrieved is not None
        assert retrieved.entity_id == "c-001"
    
    def test_get_contract_not_found(self):
        """Test getting non-existent contract."""
        registry = ContractRegistry()
        result = registry.get_contract("nonexistent")
        assert result is None
    
    def test_get_contracts_by_status(self):
        """Test getting contracts by status."""
        registry = ContractRegistry()
        intent1 = Intent(goal="Test 1")
        intent2 = Intent(goal="Test 2")
        
        contract1 = CognitiveContract("c-001", 0, "user", intent1)
        contract2 = CognitiveContract("c-002", 0, "user", intent2)
        contract2.transition_to(ContractStatus.REVIEW)
        
        registry.register_contract(contract1)
        registry.register_contract(contract2)
        
        drafts = registry.get_contracts_by_status(ContractStatus.DRAFT)
        assert len(drafts) == 1
        
        reviews = registry.get_contracts_by_status(ContractStatus.REVIEW)
        assert len(reviews) == 1
    
    def test_get_active_contracts(self):
        """Test getting active contracts."""
        registry = ContractRegistry()
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.transition_to(ContractStatus.REVIEW)
        contract.transition_to(ContractStatus.RATIFIED)
        contract.transition_to(ContractStatus.ACTIVE)
        registry.register_contract(contract)
        
        active = registry.get_active_contracts()
        assert len(active) == 1
    
    def test_enforce_task_contract_binding_bound(self):
        """Test enforcement of task-contract binding."""
        registry = ContractRegistry()
        intent = Intent(goal="Test")
        contract = CognitiveContract("c-001", 0, "user", intent)
        contract.bind_task("task-001")
        registry.register_contract(contract)
        
        result = registry.enforce_task_contract_binding("task-001")
        assert result
    
    def test_enforce_task_contract_binding_unbound(self):
        """Test enforcement fails for unbound task."""
        registry = ContractRegistry()
        result = registry.enforce_task_contract_binding("task-001")
        assert not result
    
    def test_submit_challenge(self):
        """Test submitting a challenge."""
        registry = ContractRegistry()
        challenge = ContractChallenge(
            challenger_id="agent-001",
            contract_id="c-001",
            reason="Test"
        )
        registry.submit_challenge(challenge)
        assert len(registry.challenges_pending) == 1
    
    def test_get_pending_challenges(self):
        """Test getting pending challenges."""
        registry = ContractRegistry()
        challenge1 = ContractChallenge(
            challenger_id="agent-001",
            contract_id="c-001",
            reason="Test"
        )
        challenge2 = ContractChallenge(
            challenger_id="agent-002",
            contract_id="c-002",
            reason="Test"
        )
        challenge2.status = "accepted"
        
        registry.submit_challenge(challenge1)
        registry.submit_challenge(challenge2)
        
        pending = registry.get_pending_challenges()
        assert len(pending) == 1
    
    def test_get_contract_registry_global(self):
        """Test getting global contract registry."""
        registry = get_contract_registry()
        assert registry is not None
        assert isinstance(registry, ContractRegistry)
