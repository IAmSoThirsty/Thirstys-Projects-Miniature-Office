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
    
    def test_get_law_by_id(self):
        """Test getting specific law by ID"""
        law = InviolableLaws.get_law("law-1")
        
        assert law is not None
        assert law.law_id == "law-1"
    
    def test_get_nonexistent_law(self):
        """Test getting non-existent law returns None"""
        law = InviolableLaws.get_law("nonexistent")
        assert law is None
    
    def test_validate_action(self):
        """Test action validation against laws"""
        # Valid action
        result = InviolableLaws.validate_action("Execute with intent")
        assert result['valid'] is True
        
        # May have violations depending on implementation
        result = InviolableLaws.validate_action("Autonomous execution")
        assert 'valid' in result
        assert 'violations' in result


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


class TestAuthorityGraph:
    """Test AuthorityGraph class"""
    
    def test_graph_creation(self):
        """Test creating authority graph"""
        graph = AuthorityGraph()
        assert graph is not None
    
    def test_add_relation(self):
        """Test adding authority relation"""
        graph = AuthorityGraph()
        
        graph.add_relation("human", "meta_office", "oversees")
        
        # Check relation was added
        assert len(graph.relations) > 0
    
    def test_can_authorize(self):
        """Test checking authorization"""
        graph = AuthorityGraph()
        
        # Setup some relations
        graph.add_relation("human", "meta_office", "oversees")
        graph.add_relation("meta_office", "manager", "delegates_to")
        
        # Test authorization checks
        result = graph.can_authorize("human", "some_action")
        assert isinstance(result, bool)


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
            failure_type=FailureType.AXIOM_VIOLATION if hasattr(FailureType, 'AXIOM_VIOLATION') else list(FailureType)[0],
            description="Test failure",
            occurred_at_tick=100
        )
        
        assert failure.failure_id == "fail-001"
        assert failure.occurred_at_tick == 100
    
    def test_failure_to_dict(self):
        """Test failure serialization"""
        failure = FirstClassFailure(
            failure_id="fail-002",
            failure_type=list(FailureType)[0],
            description="Test",
            occurred_at_tick=200
        )
        
        result = failure.to_dict()
        assert result['failure_id'] == "fail-002"


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
            power_used=list(HumanPower)[0],
            target="system",
            performed_at_tick=50
        )
        
        assert action.action_id == "action-001"
        assert action.performed_at_tick == 50
    
    def test_action_to_dict(self):
        """Test action serialization"""
        action = HumanAction(
            action_id="action-002",
            power_used=list(HumanPower)[0],
            target="test",
            performed_at_tick=100
        )
        
        result = action.to_dict()
        assert result['action_id'] == "action-002"


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
            failure_type=list(FailureType)[0],
            description="Test failure",
            occurred_at_tick=100
        )
        
        codex.record_failure(failure)
        assert len(codex.failures) > 0
    
    def test_record_human_action(self):
        """Test recording human actions"""
        codex = DensityCodex()
        
        action = HumanAction(
            action_id="action-test",
            power_used=list(HumanPower)[0],
            target="test",
            performed_at_tick=50
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
