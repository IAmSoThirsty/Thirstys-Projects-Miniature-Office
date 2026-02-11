"""
Tests for Density Codex - Primitive Axioms and Foundation
Comprehensive coverage for civilization-tier architecture foundations
"""
import pytest
from src.core.density_codex import (
    PrimitiveAxiom,
    OntologicalLayer,
    LawClass,
    InviolableLaw,
    InviolableLaws,
    AuthorityNode,
    AuthorityRelation,
    AuthorityGraph,
    FailureType,
    FirstClassFailure,
    MetaOfficeCapability,
    HumanPower,
    HumanAction,
    DensityCodex,
    get_density_codex
)


class TestPrimitiveAxiom:
    """Test PrimitiveAxiom enum"""
    
    def test_axioms_exist(self):
        """Test all primitive axioms are defined"""
        assert PrimitiveAxiom.INTENT_PRECEDES_EXECUTION is not None
        assert PrimitiveAxiom.AUTHORITY_PRECEDES_ACTION is not None
        assert PrimitiveAxiom.CAUSALITY_PRECEDES_STATE is not None
        assert PrimitiveAxiom.SCARCITY_PRECEDES_VALUE is not None
        assert PrimitiveAxiom.HISTORY_PRECEDES_OPTIMIZATION is not None
        assert PrimitiveAxiom.GOVERNANCE_PRECEDES_INTELLIGENCE is not None
    
    def test_axiom_values(self):
        """Test axiom string values"""
        assert PrimitiveAxiom.INTENT_PRECEDES_EXECUTION.value == "intent_precedes_execution"
        assert PrimitiveAxiom.AUTHORITY_PRECEDES_ACTION.value == "authority_precedes_action"


class TestOntologicalLayer:
    """Test OntologicalLayer enum"""
    
    def test_layers_ordered(self):
        """Test layers are properly ordered"""
        assert OntologicalLayer.LAYER_0_REALITY.value == 0
        assert OntologicalLayer.LAYER_1_WORLD.value == 1
        assert OntologicalLayer.LAYER_2_ACTORS.value == 2
        assert OntologicalLayer.LAYER_3_INTENT.value == 3
        assert OntologicalLayer.LAYER_4_GOVERNANCE.value == 4
        assert OntologicalLayer.LAYER_5_ECONOMICS.value == 5
        assert OntologicalLayer.LAYER_6_COGNITION.value == 6
    
    def test_layer_hierarchy(self):
        """Test layer hierarchy is maintained"""
        layers = [
            OntologicalLayer.LAYER_0_REALITY,
            OntologicalLayer.LAYER_1_WORLD,
            OntologicalLayer.LAYER_2_ACTORS,
            OntologicalLayer.LAYER_3_INTENT,
            OntologicalLayer.LAYER_4_GOVERNANCE,
            OntologicalLayer.LAYER_5_ECONOMICS,
            OntologicalLayer.LAYER_6_COGNITION
        ]
        
        for i in range(len(layers) - 1):
            assert layers[i].value < layers[i + 1].value


class TestLawClass:
    """Test LawClass enum"""
    
    def test_law_classes_exist(self):
        """Test all law classes are defined"""
        assert LawClass.CLASS_A_INVIOLABLE is not None
        assert LawClass.CLASS_B_STRUCTURAL is not None
        assert LawClass.CLASS_C_OPERATIONAL is not None
    
    def test_law_class_values(self):
        """Test law class values"""
        assert LawClass.CLASS_A_INVIOLABLE.value == "inviolable"
        assert LawClass.CLASS_B_STRUCTURAL.value == "structural"
        assert LawClass.CLASS_C_OPERATIONAL.value == "operational"


class TestInviolableLaw:
    """Test InviolableLaw dataclass"""
    
    def test_inviolable_law_creation(self):
        """Test creating an inviolable law"""
        law = InviolableLaw(
            law_id="law-a1",
            principle="No action without intent",
            rationale="Prevents autonomous goal formation"
        )
        
        assert law.law_id == "law-a1"
        assert law.principle == "No action without intent"
        assert law.rationale is not None
    
    def test_inviolable_law_to_dict(self):
        """Test inviolable law serialization"""
        law = InviolableLaw(
            law_id="law-a2",
            principle="Authority precedes action",
            rationale="Maintains accountability"
        )
        
        result = law.to_dict()
        assert result['law_id'] == "law-a2"
        assert result['class'] == 'A_INVIOLABLE'
        assert result['principle'] == "Authority precedes action"
        assert result['rationale'] == "Maintains accountability"


class TestInviolableLaws:
    """Test InviolableLaws class"""
    
    def test_get_all_inviolable_laws(self):
        """Test getting all inviolable laws"""
        laws = InviolableLaws.get_all()
        
        assert len(laws) == 5
        assert all(isinstance(law, InviolableLaw) for law in laws)
    
    def test_access_specific_law(self):
        """Test accessing specific law"""
        law = InviolableLaws.CAUSALITY_PRESERVATION
        
        assert law is not None
        assert law.law_id == "INVIOLABLE-001"
        assert "Causality" in law.principle


class TestAuthorityNode:
    """Test AuthorityNode enum"""
    
    def test_authority_nodes_exist(self):
        """Test authority node types are defined"""
        # Just check the enum exists and has values
        nodes = list(AuthorityNode)
        assert len(nodes) > 0


class TestAuthorityRelation:
    """Test AuthorityRelation class"""
    
    def test_relation_creation(self):
        """Test creating authority relation"""
        relation = AuthorityRelation(
            from_node="human",
            to_node="meta_office",
            relation_type="oversees"
        )
        
        assert relation.from_node == "human"
        assert relation.to_node == "meta_office"
        assert relation.relation_type == "oversees"


class TestAuthorityRelation:
    """Test AuthorityRelation class"""
    
    def test_relation_creation(self):
        """Test creating authority relation"""
        relation = AuthorityRelation(
            from_node=AuthorityNode.HUMAN,
            to_node=AuthorityNode.META_OFFICE,
            relation_type="authority"
        )
        
        assert relation.from_node == AuthorityNode.HUMAN
        assert relation.to_node == AuthorityNode.META_OFFICE
        assert relation.relation_type == "authority"
    
    def test_relation_validity(self):
        """Test relation validity"""
        # Valid authority relation (flows down)
        relation = AuthorityRelation(
            from_node=AuthorityNode.META_OFFICE,
            to_node=AuthorityNode.MANAGER,
            relation_type="authority"
        )
        assert relation.is_valid() is True
        
        # Invalid authority relation (would flow up)
        relation2 = AuthorityRelation(
            from_node=AuthorityNode.MANAGER,
            to_node=AuthorityNode.META_OFFICE,
            relation_type="authority"
        )
        assert relation2.is_valid() is False


class TestAuthorityGraph:
    """Test AuthorityGraph class"""
    
    def test_graph_creation(self):
        """Test creating authority graph"""
        graph = AuthorityGraph()
        assert graph is not None
        assert len(graph.relations) == 0
    
    def test_add_relation(self):
        """Test adding authority relation"""
        graph = AuthorityGraph()
        
        relation = AuthorityRelation(
            from_node=AuthorityNode.HUMAN,
            to_node=AuthorityNode.META_OFFICE,
            relation_type="authority"
        )
        
        result = graph.add_relation(relation)
        
        assert result is True
        assert len(graph.relations) == 1
    
    def test_can_execute(self):
        """Test checking execution authorization"""
        graph = AuthorityGraph()
        
        # Higher authority can execute on lower
        result = graph.can_execute(AuthorityNode.META_OFFICE, AuthorityNode.AGENT)
        assert result is True
        
        # Lower cannot execute on higher
        result2 = graph.can_execute(AuthorityNode.AGENT, AuthorityNode.META_OFFICE)
        assert result2 is False


class TestFailureType:
    """Test FailureType enum"""
    
    def test_failure_types_exist(self):
        """Test failure type enum"""
        types = list(FailureType)
        assert len(types) > 0


class TestFirstClassFailure:
    """Test FirstClassFailure class"""
    
    def test_failure_creation(self):
        """Test creating first-class failure"""
        failure = FirstClassFailure(
            failure_id="fail-001",
            failure_type=FailureType.AXIOM_VIOLATION,
            occurred_at_tick=100,
            caused_by="agent-001",
            affected_entities=["entity-001", "entity-002"],
            escalated_to=AuthorityNode.META_OFFICE,
            resource_cost=50,
            consequence="System halted for review"
        )
        
        assert failure.failure_id == "fail-001"
        assert failure.occurred_at_tick == 100
        assert failure.recorded is True  # Default value
    
    def test_failure_to_dict(self):
        """Test failure serialization"""
        failure = FirstClassFailure(
            failure_id="fail-002",
            failure_type=FailureType.AUTHORITY_INVERSION,
            occurred_at_tick=200,
            caused_by="agent-002",
            affected_entities=["entity-003"],
            escalated_to=AuthorityNode.MANAGER,
            resource_cost=25,
            consequence="Authority check failed"
        )
        
        result = failure.to_dict()
        assert result['failure_id'] == "fail-002"
        assert result['occurred_at_tick'] == 200


class TestMetaOfficeCapability:
    """Test MetaOfficeCapability enum"""
    
    def test_capabilities_exist(self):
        """Test meta-office capabilities defined"""
        caps = list(MetaOfficeCapability)
        assert len(caps) > 0


class TestHumanPower:
    """Test HumanPower enum"""
    
    def test_powers_exist(self):
        """Test human powers defined"""
        powers = list(HumanPower)
        assert len(powers) > 0


class TestHumanAction:
    """Test HumanAction class"""
    
    def test_action_creation(self):
        """Test creating human action"""
        action = HumanAction(
            action_id="action-001",
            human_id="human-001",
            power_exercised=HumanPower.ISSUE_BINDING_CONTRACT,
            justification="Need to override for safety",
            cost=100,
            logged_at_tick=50
        )
        
        assert action.action_id == "action-001"
        assert action.logged_at_tick == 50
        assert action.cost == 100
    
    def test_action_to_dict(self):
        """Test action serialization"""
        action = HumanAction(
            action_id="action-002",
            human_id="human-002",
            power_exercised=HumanPower.FREEZE_WORLD,
            justification="Emergency freeze",
            cost=500,
            logged_at_tick=100
        )
        
        result = action.to_dict()
        assert result['action_id'] == "action-002"
        assert result['cost'] == 500


class TestDensityCodex:
    """Test DensityCodex class"""
    
    def test_codex_creation(self):
        """Test creating density codex"""
        codex = DensityCodex()
        
        # Should load axioms and laws
        assert len(codex.axioms) == 6
        assert len(codex.layers) == 7
        assert len(codex.inviolable_laws) == 5
    
    def test_get_axioms(self):
        """Test getting all axioms"""
        codex = DensityCodex()
        axioms = codex.axioms
        
        assert len(axioms) == 6
        assert all(isinstance(axiom, PrimitiveAxiom) for axiom in axioms)
    
    def test_get_layers(self):
        """Test getting ontological layers"""
        codex = DensityCodex()
        layers = codex.layers
        
        assert len(layers) == 7
        assert all(isinstance(layer, OntologicalLayer) for layer in layers)
    
    def test_validate_axiom(self):
        """Test validating axioms"""
        codex = DensityCodex()
        
        # Test intent precedes execution
        result = codex.validate_axiom(
            PrimitiveAxiom.INTENT_PRECEDES_EXECUTION,
            {'intent': 'test intent'}
        )
        assert result is True
        
        # Test without intent
        result = codex.validate_axiom(
            PrimitiveAxiom.INTENT_PRECEDES_EXECUTION,
            {}
        )
        assert result is False
    
    def test_validate_layer_access(self):
        """Test validating layer access"""
        codex = DensityCodex()
        
        # Test valid access
        result = codex.validate_layer_access(
            OntologicalLayer.LAYER_3_INTENT,
            OntologicalLayer.LAYER_2_ACTORS,
            "read"
        )
        assert isinstance(result, bool)
    
    def test_record_failure(self):
        """Test recording failures"""
        codex = DensityCodex()
        
        failure = FirstClassFailure(
            failure_id="fail-test",
            failure_type=FailureType.LAYER_BYPASS,
            occurred_at_tick=100,
            caused_by="agent-test",
            affected_entities=["entity-001"],
            escalated_to=AuthorityNode.META_OFFICE,
            resource_cost=50,
            consequence="Layer bypass detected"
        )
        
        codex.record_failure(failure)
        assert len(codex.failures) > 0
    
    def test_record_human_action(self):
        """Test recording human actions"""
        codex = DensityCodex()
        
        action = HumanAction(
            action_id="action-test",
            human_id="human-test",
            power_exercised=HumanPower.TRIGGER_AUDIT,
            justification="Routine audit",
            cost=10,
            logged_at_tick=50
        )
        
        codex.record_human_action(action)
        assert len(codex.human_actions) > 0


class TestGlobalCodex:
    """Test global codex accessor"""
    
    def test_get_density_codex(self):
        """Test getting global density codex"""
        codex1 = get_density_codex()
        codex2 = get_density_codex()
        
        # Should return same instance
        assert codex1 is codex2
    
    def test_codex_has_all_laws(self):
        """Test global codex has all required laws"""
        codex = get_density_codex()
        
        assert len(codex.inviolable_laws) == 5
        assert len(codex.axioms) == 6
        assert len(codex.layers) == 7


class TestAuthorityRelationValidation:
    """Test AuthorityRelation validation edge cases"""
    
    def test_relation_validity_appeal(self):
        """Test appeal relation validity (flows up)"""
        # Valid appeal (agent to manager)
        relation = AuthorityRelation(
            from_node=AuthorityNode.AGENT,
            to_node=AuthorityNode.MANAGER,
            relation_type="appeal"
        )
        assert relation.is_valid() is True
        
        # Invalid appeal (would flow down)
        relation2 = AuthorityRelation(
            from_node=AuthorityNode.MANAGER,
            to_node=AuthorityNode.AGENT,
            relation_type="appeal"
        )
        assert relation2.is_valid() is False
    
    def test_relation_validity_execution(self):
        """Test execution relation validity (lateral)"""
        # Valid execution (same level)
        relation = AuthorityRelation(
            from_node=AuthorityNode.AGENT,
            to_node=AuthorityNode.AGENT,
            relation_type="execution"
        )
        assert relation.is_valid() is True
        
        # Invalid execution (different levels)
        relation2 = AuthorityRelation(
            from_node=AuthorityNode.AGENT,
            to_node=AuthorityNode.MANAGER,
            relation_type="execution"
        )
        assert relation2.is_valid() is False
    
    def test_relation_validity_unknown_type(self):
        """Test relation with unknown type"""
        relation = AuthorityRelation(
            from_node=AuthorityNode.AGENT,
            to_node=AuthorityNode.MANAGER,
            relation_type="unknown"
        )
        assert relation.is_valid() is False


class TestAuthorityGraphEdgeCases:
    """Test AuthorityGraph edge cases"""
    
    def test_add_invalid_relation(self):
        """Test adding invalid relation returns False"""
        graph = AuthorityGraph()
        
        # Invalid relation (authority flowing up)
        relation = AuthorityRelation(
            from_node=AuthorityNode.AGENT,
            to_node=AuthorityNode.HUMAN,
            relation_type="authority"
        )
        
        result = graph.add_relation(relation)
        assert result is False
        assert len(graph.relations) == 0
    
    def test_add_valid_relation_with_cycle_check(self):
        """Test adding valid relation triggers cycle check"""
        graph = AuthorityGraph()
        
        # Add a valid relation (will trigger _creates_cycle call)
        relation = AuthorityRelation(
            from_node=AuthorityNode.HUMAN,
            to_node=AuthorityNode.META_OFFICE,
            relation_type="authority"
        )
        
        result = graph.add_relation(relation)
        assert result is True
        assert len(graph.relations) == 1
    
    def test_can_appeal(self):
        """Test appeal authorization"""
        graph = AuthorityGraph()
        
        # Agent can appeal to manager (up hierarchy)
        assert graph.can_appeal(AuthorityNode.AGENT, AuthorityNode.MANAGER) is True
        
        # Manager cannot appeal to agent (down hierarchy)
        assert graph.can_appeal(AuthorityNode.MANAGER, AuthorityNode.AGENT) is False


class TestDensityCodexAxiomValidation:
    """Test all axiom validation paths"""
    
    def test_validate_intent_precedes_execution(self):
        """Test intent precedes execution axiom"""
        codex = DensityCodex()
        
        # Valid: has intent
        context = {'intent': 'do something'}
        assert codex.validate_axiom(PrimitiveAxiom.INTENT_PRECEDES_EXECUTION, context) is True
        
        # Invalid: no intent
        context2 = {}
        assert codex.validate_axiom(PrimitiveAxiom.INTENT_PRECEDES_EXECUTION, context2) is False
    
    def test_validate_authority_precedes_action(self):
        """Test authority precedes action axiom"""
        codex = DensityCodex()
        
        # Valid: has authority
        context = {'authority': 'manager-001'}
        assert codex.validate_axiom(PrimitiveAxiom.AUTHORITY_PRECEDES_ACTION, context) is True
        
        # Invalid: no authority
        context2 = {}
        assert codex.validate_axiom(PrimitiveAxiom.AUTHORITY_PRECEDES_ACTION, context2) is False
    
    def test_validate_causality_precedes_state(self):
        """Test causality precedes state axiom"""
        codex = DensityCodex()
        
        # Valid: has cause
        context = {'cause': 'event-001'}
        assert codex.validate_axiom(PrimitiveAxiom.CAUSALITY_PRECEDES_STATE, context) is True
        
        # Invalid: no cause
        context2 = {}
        assert codex.validate_axiom(PrimitiveAxiom.CAUSALITY_PRECEDES_STATE, context2) is False
    
    def test_validate_scarcity_precedes_value(self):
        """Test scarcity precedes value axiom"""
        codex = DensityCodex()
        
        # Valid: has scarcity > 0
        context = {'scarcity': 10}
        assert codex.validate_axiom(PrimitiveAxiom.SCARCITY_PRECEDES_VALUE, context) is True
        
        # Invalid: scarcity = 0
        context2 = {'scarcity': 0}
        assert codex.validate_axiom(PrimitiveAxiom.SCARCITY_PRECEDES_VALUE, context2) is False
    
    def test_validate_history_precedes_optimization(self):
        """Test history precedes optimization axiom"""
        codex = DensityCodex()
        
        # Valid: has history
        context = {'history': ['event1', 'event2']}
        assert codex.validate_axiom(PrimitiveAxiom.HISTORY_PRECEDES_OPTIMIZATION, context) is True
        
        # Invalid: empty history
        context2 = {'history': []}
        assert codex.validate_axiom(PrimitiveAxiom.HISTORY_PRECEDES_OPTIMIZATION, context2) is False
    
    def test_validate_governance_precedes_intelligence(self):
        """Test governance precedes intelligence axiom"""
        codex = DensityCodex()
        
        # Valid: has governance
        context = {'governance': 'enabled'}
        assert codex.validate_axiom(PrimitiveAxiom.GOVERNANCE_PRECEDES_INTELLIGENCE, context) is True
        
        # Invalid: no governance
        context2 = {}
        assert codex.validate_axiom(PrimitiveAxiom.GOVERNANCE_PRECEDES_INTELLIGENCE, context2) is False


class TestDensityCodexLayerValidation:
    """Test layer access validation"""
    
    def test_validate_layer_access_upward(self):
        """Test upward layer access validation"""
        codex = DensityCodex()
        
        # Valid: adjacent upper layer
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_1_WORLD,
            OntologicalLayer.LAYER_2_ACTORS,
            "upward"
        ) is True
        
        # Invalid: skip a layer
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_1_WORLD,
            OntologicalLayer.LAYER_3_INTENT,
            "upward"
        ) is False
    
    def test_validate_layer_access_downward(self):
        """Test downward layer access validation"""
        codex = DensityCodex()
        
        # Valid: access lower layers
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_3_INTENT,
            OntologicalLayer.LAYER_1_WORLD,
            "downward"
        ) is True
        
        # Invalid: trying to go up
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_1_WORLD,
            OntologicalLayer.LAYER_3_INTENT,
            "downward"
        ) is False
    
    def test_validate_layer_access_lateral(self):
        """Test lateral layer access validation"""
        codex = DensityCodex()
        
        # Valid: same layer
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_2_ACTORS,
            OntologicalLayer.LAYER_2_ACTORS,
            "lateral"
        ) is True
        
        # Invalid: different layers
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_2_ACTORS,
            OntologicalLayer.LAYER_3_INTENT,
            "lateral"
        ) is False
    
    def test_validate_layer_access_unknown_type(self):
        """Test layer access with unknown type"""
        codex = DensityCodex()
        
        assert codex.validate_layer_access(
            OntologicalLayer.LAYER_1_WORLD,
            OntologicalLayer.LAYER_2_ACTORS,
            "unknown"
        ) is False


class TestDensityCodexToDict:
    """Test codex serialization"""
    
    def test_to_dict_complete(self):
        """Test complete codex serialization"""
        codex = DensityCodex()
        
        result = codex.to_dict()
        
        assert 'axioms' in result
        assert len(result['axioms']) == 6
        assert 'layers' in result
        assert len(result['layers']) == 7
        assert 'inviolable_laws' in result
        assert len(result['inviolable_laws']) == 5
        assert 'failures_recorded' in result
        assert 'human_actions_recorded' in result


class TestCivilizationTierValidation:
    """Test civilization tier validation"""
    
    def test_validate_civilization_tier_valid(self):
        """Test validation with all axioms satisfied"""
        from src.core.density_codex import validate_civilization_tier
        
        system_state = {
            'intent': 'execute',
            'authority': 'manager-001',
            'cause': 'event-001',
            'scarcity': 10,
            'history': ['event1', 'event2'],
            'governance': 'enabled'
        }
        
        assert validate_civilization_tier(system_state) is True
    
    def test_validate_civilization_tier_invalid(self):
        """Test validation with missing axioms"""
        from src.core.density_codex import validate_civilization_tier
        
        # Missing intent
        system_state = {
            'authority': 'manager-001',
            'cause': 'event-001',
            'scarcity': 10,
            'history': ['event1'],
            'governance': 'enabled'
        }
        
        assert validate_civilization_tier(system_state) is False



class TestDensityCodexEdgeCases:
    """Test edge cases for full coverage"""
    
    def test_validate_axiom_unknown(self):
        """Test axiom validation fallback for unknown axiom"""
        from enum import Enum
        codex = DensityCodex()
        
        # Create a temporary enum value not in PrimitiveAxiom
        class FakeAxiom(Enum):
            FAKE = "fake_axiom"
        
        # Call validate_axiom with something that won't match any if/elif
        result = codex.validate_axiom(FakeAxiom.FAKE, {})
        assert result is False
    
    def test_authority_graph_cycle_detection(self):
        """Test cycle detection path"""
        from unittest.mock import patch
        graph = AuthorityGraph()
        
        relation = AuthorityRelation(
            from_node=AuthorityNode.HUMAN,
            to_node=AuthorityNode.META_OFFICE,
            relation_type="authority"
        )
        
        # Patch _creates_cycle to return True to trigger the raise
        with patch.object(graph, "_creates_cycle", return_value=True):
            try:
                graph.add_relation(relation)
                assert False, "Should have raised ValueError"
            except ValueError as e:
                assert "cycle detected" in str(e)
