"""Comprehensive tests for the maximum autonomy module."""
import pytest
from datetime import datetime
from src.core.maximum_autonomy import (
    ExecutionZone,
    StagingStatus,
    TeamFormationReason,
    FloorInitiativeType,
    LoungeInfluenceType,
    ReputationEffect,
    VRInteractionMode,
    SelfCareCategory,
    ProvisionalArtifact,
    StagingReality,
    SelfOrganizingTeam,
    TeamDynamics,
    FloorInitiative,
    LoungeInfluence,
    ReputationLeadership,
    VRLiveStagingView,
    ProjectAISelfCare,
    MaximumAutonomyModel,
    get_maximum_autonomy_model
)


# ============================================================================
# ENUM TESTS
# ============================================================================

class TestExecutionZone:
    """Test ExecutionZone enum."""
    
    def test_execution_zone_values(self):
        """Test execution zone enum values."""
        assert ExecutionZone.SANDBOX.value == "sandbox"
        assert ExecutionZone.STAGING.value == "staging"
        assert ExecutionZone.PRODUCTION.value == "production"


class TestStagingStatus:
    """Test StagingStatus enum."""
    
    def test_staging_status_values(self):
        """Test staging status enum values."""
        assert StagingStatus.DEVELOPMENT.value == "development"
        assert StagingStatus.INTEGRATION_TESTING.value == "integration_testing"
        assert StagingStatus.SECURITY_TESTING.value == "security_testing"
        assert StagingStatus.PERFORMANCE_TESTING.value == "performance_testing"
        assert StagingStatus.CONFLICT_RESOLUTION.value == "conflict_resolution"
        assert StagingStatus.READY_FOR_REVIEW.value == "ready_for_review"
        assert StagingStatus.HUMAN_REVIEW.value == "human_review"
        assert StagingStatus.APPROVED.value == "approved"
        assert StagingStatus.VETOED.value == "vetoed"


class TestTeamFormationReason:
    """Test TeamFormationReason enum."""
    
    def test_team_formation_reason_values(self):
        """Test team formation reason enum values."""
        assert TeamFormationReason.INITIATIVE.value == "initiative"
        assert TeamFormationReason.COMPETITION.value == "competition"
        assert TeamFormationReason.CRISIS_RESPONSE.value == "crisis_response"
        assert TeamFormationReason.INNOVATION.value == "innovation"
        assert TeamFormationReason.REFACTORING.value == "refactoring"
        assert TeamFormationReason.CROSS_FLOOR_NEED.value == "cross_floor_need"


class TestFloorInitiativeType:
    """Test FloorInitiativeType enum."""
    
    def test_floor_initiative_type_values(self):
        """Test floor initiative type enum values."""
        assert FloorInitiativeType.SAFETY_IMPROVEMENT.value == "safety_improvement"
        assert FloorInitiativeType.PERFORMANCE_OPTIMIZATION.value == "performance_optimization"
        assert FloorInitiativeType.TOOLING.value == "tooling"
        assert FloorInitiativeType.VISUALIZATION.value == "visualization"
        assert FloorInitiativeType.TESTING_INFRASTRUCTURE.value == "testing_infrastructure"
        assert FloorInitiativeType.SECURITY_SCANNER.value == "security_scanner"
        assert FloorInitiativeType.REFACTORING.value == "refactoring"


class TestLoungeInfluenceType:
    """Test LoungeInfluenceType enum."""
    
    def test_lounge_influence_type_values(self):
        """Test lounge influence type enum values."""
        assert LoungeInfluenceType.CREATE_MOMENTUM.value == "create_momentum"
        assert LoungeInfluenceType.KILL_BORING_IDEA.value == "kill_boring_idea"
        assert LoungeInfluenceType.ELEVATE_PROMISING.value == "elevate_promising"
        assert LoungeInfluenceType.SPARK_INITIATIVE.value == "spark_initiative"
        assert LoungeInfluenceType.FORM_CONSENSUS.value == "form_consensus"


class TestReputationEffect:
    """Test ReputationEffect enum."""
    
    def test_reputation_effect_values(self):
        """Test reputation effect enum values."""
        assert ReputationEffect.ATTRACTS_COLLABORATORS.value == "attracts_collaborators"
        assert ReputationEffect.SPEEDS_REVIEW.value == "speeds_review"
        assert ReputationEffect.INCREASES_STAGING_TRUST.value == "increases_staging_trust"
        assert ReputationEffect.INFLUENCES_LISTENING.value == "influences_listening"


class TestVRInteractionMode:
    """Test VRInteractionMode enum."""
    
    def test_vr_interaction_mode_values(self):
        """Test VR interaction mode enum values."""
        assert VRInteractionMode.WALKTHROUGH.value == "walkthrough"
        assert VRInteractionMode.DEMO.value == "demo"
        assert VRInteractionMode.PASSIONATE_DEBATE.value == "passionate_debate"
        assert VRInteractionMode.SIDE_CONVERSATION.value == "side_conversation"
        assert VRInteractionMode.FACTION_FORMATION.value == "faction_formation"
        assert VRInteractionMode.INTERRUPT.value == "interrupt"


class TestSelfCareCategory:
    """Test SelfCareCategory enum."""
    
    def test_self_care_category_values(self):
        """Test self care category enum values."""
        assert SelfCareCategory.REFACTORING.value == "refactoring"
        assert SelfCareCategory.SECURITY_HARDENING.value == "security_hardening"
        assert SelfCareCategory.ARCHITECTURAL_EVOLUTION.value == "architectural_evolution"
        assert SelfCareCategory.LEGACY_CLEANUP.value == "legacy_cleanup"
        assert SelfCareCategory.TOOLING_MODERNIZATION.value == "tooling_modernization"


# ============================================================================
# DATACLASS TESTS
# ============================================================================

class TestProvisionalArtifact:
    """Test ProvisionalArtifact class."""
    
    def test_provisional_artifact_creation(self):
        """Test creating a provisional artifact."""
        artifact = ProvisionalArtifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test content",
            created_tick=1,
            created_by="team-1",
            status=StagingStatus.DEVELOPMENT
        )
        
        assert artifact.artifact_id == "art-001"
        assert artifact.artifact_type == "codebase"
        assert artifact.content == "test content"
        assert artifact.created_tick == 1
        assert artifact.created_by == "team-1"
        assert artifact.status == StagingStatus.DEVELOPMENT
        assert artifact.is_provisional is True
        assert artifact.can_rollback is True
        assert artifact.escapes_automatically is False
        assert artifact.integration_test_results == {}
        assert artifact.security_test_results == {}
        assert artifact.performance_metrics == {}
        assert artifact.conflicts_resolved == []
    
    def test_flag_ready_for_review(self):
        """Test flagging artifact as ready for review."""
        artifact = ProvisionalArtifact(
            artifact_id="art-002",
            artifact_type="tool",
            content="test tool",
            created_tick=2,
            created_by="agent-1",
            status=StagingStatus.DEVELOPMENT
        )
        
        artifact.flag_ready_for_review("agent-1")
        
        assert artifact.status == StagingStatus.READY_FOR_REVIEW


class TestStagingReality:
    """Test StagingReality class."""
    
    def test_staging_reality_creation(self):
        """Test creating a staging reality."""
        staging = StagingReality(staging_id="staging-001")
        
        assert staging.staging_id == "staging-001"
        assert staging.artifacts == {}
        assert staging.active_integrations == []
        assert staging.cross_floor_contracts == []
        assert staging.test_suites_running == set()
        assert staging.conflicts_being_resolved == {}
    
    def test_add_provisional_artifact(self):
        """Test adding a provisional artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        artifact_id = staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        assert artifact_id == "art-001"
        assert "art-001" in staging.artifacts
        artifact = staging.artifacts["art-001"]
        assert artifact.artifact_id == "art-001"
        assert artifact.artifact_type == "codebase"
        assert artifact.content == "test code"
        assert artifact.created_by == "team-1"
        assert artifact.created_tick == 1
        assert artifact.status == StagingStatus.DEVELOPMENT
    
    def test_run_integration_tests(self):
        """Test running integration tests on artifact."""
        staging = StagingReality(staging_id="staging-001")
        staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        results = staging.run_integration_tests("art-001")
        
        assert results["cross_floor_compatibility"] is True
        assert results["api_contract_compliance"] is True
        assert results["resource_bounds"] is True
        assert staging.artifacts["art-001"].status == StagingStatus.INTEGRATION_TESTING
        assert staging.artifacts["art-001"].integration_test_results == results
    
    def test_run_integration_tests_nonexistent(self):
        """Test running integration tests on nonexistent artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        results = staging.run_integration_tests("nonexistent")
        
        assert results == {}
    
    def test_run_security_tests(self):
        """Test running security tests on artifact."""
        staging = StagingReality(staging_id="staging-001")
        staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        results = staging.run_security_tests("art-001")
        
        assert results["injection_prevention"] is True
        assert results["memory_safety"] is True
        assert results["access_control"] is True
        assert staging.artifacts["art-001"].status == StagingStatus.SECURITY_TESTING
        assert staging.artifacts["art-001"].security_test_results == results
    
    def test_run_security_tests_nonexistent(self):
        """Test running security tests on nonexistent artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        results = staging.run_security_tests("nonexistent")
        
        assert results == {}
    
    def test_resolve_conflicts_organically(self):
        """Test resolving conflicts organically."""
        staging = StagingReality(staging_id="staging-001")
        staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        conflicts = staging.resolve_conflicts_organically("art-001")
        
        assert "API version mismatch" in conflicts
        assert "Resource contention" in conflicts
        assert "Cross-floor timing" in conflicts
        assert staging.artifacts["art-001"].status == StagingStatus.CONFLICT_RESOLUTION
        assert staging.artifacts["art-001"].conflicts_resolved == conflicts
    
    def test_resolve_conflicts_nonexistent(self):
        """Test resolving conflicts for nonexistent artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        conflicts = staging.resolve_conflicts_organically("nonexistent")
        
        assert conflicts == []
    
    def test_get_staging(self):
        """Test getting a staging artifact."""
        staging = StagingReality(staging_id="staging-001")
        staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        artifact = staging.get_staging("art-001")
        
        assert artifact is not None
        assert artifact.artifact_id == "art-001"
    
    def test_get_staging_nonexistent(self):
        """Test getting a nonexistent staging artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        artifact = staging.get_staging("nonexistent")
        
        assert artifact is None
    
    def test_rollback_artifact(self):
        """Test rolling back an artifact."""
        staging = StagingReality(staging_id="staging-001")
        staging.add_provisional_artifact(
            artifact_id="art-001",
            artifact_type="codebase",
            content="test code",
            created_by="team-1",
            created_tick=1
        )
        
        success = staging.rollback_artifact("art-001", "Testing rollback")
        
        assert success is True
        assert "art-001" not in staging.artifacts
    
    def test_rollback_artifact_nonexistent(self):
        """Test rolling back nonexistent artifact."""
        staging = StagingReality(staging_id="staging-001")
        
        success = staging.rollback_artifact("nonexistent", "Testing")
        
        assert success is False


class TestSelfOrganizingTeam:
    """Test SelfOrganizingTeam class."""
    
    def test_team_creation(self):
        """Test creating a self-organizing team."""
        team = SelfOrganizingTeam(
            team_id="team-001",
            team_name="Innovation Squad",
            founder="agent-1",
            formation_reason=TeamFormationReason.INNOVATION,
            formation_tick=1
        )
        
        assert team.team_id == "team-001"
        assert team.team_name == "Innovation Squad"
        assert team.founder == "agent-1"
        assert team.formation_reason == TeamFormationReason.INNOVATION
        assert team.formation_tick == 1
        assert team.members == set()
        assert team.projects == []
        assert team.competing_with == set()
        assert team.disbanded is False
        assert team.disband_tick is None
    
    def test_recruit_member(self):
        """Test recruiting a member to the team."""
        team = SelfOrganizingTeam(
            team_id="team-001",
            team_name="Innovation Squad",
            founder="agent-1",
            formation_reason=TeamFormationReason.INNOVATION,
            formation_tick=1
        )
        
        success = team.recruit_member("agent-2", "agent-1")
        
        assert success is True
        assert "agent-2" in team.members
    
    def test_recruit_member_disbanded_team(self):
        """Test recruiting to disbanded team."""
        team = SelfOrganizingTeam(
            team_id="team-001",
            team_name="Innovation Squad",
            founder="agent-1",
            formation_reason=TeamFormationReason.INNOVATION,
            formation_tick=1
        )
        team.disbanded = True
        
        success = team.recruit_member("agent-2", "agent-1")
        
        assert success is False
        assert "agent-2" not in team.members
    
    def test_disband_naturally(self):
        """Test natural disbanding of team."""
        team = SelfOrganizingTeam(
            team_id="team-001",
            team_name="Innovation Squad",
            founder="agent-1",
            formation_reason=TeamFormationReason.INNOVATION,
            formation_tick=1
        )
        
        team.disband_naturally(10, "Project completed")
        
        assert team.disbanded is True
        assert team.disband_tick == 10


class TestTeamDynamics:
    """Test TeamDynamics class."""
    
    def test_team_dynamics_creation(self):
        """Test creating team dynamics."""
        dynamics = TeamDynamics()
        
        assert dynamics.teams == {}
        assert dynamics.team_competitions == {}
    
    def test_form_team(self):
        """Test forming a team."""
        dynamics = TeamDynamics()
        
        team_id = dynamics.form_team(
            team_id="team-001",
            team_name="Innovation Squad",
            founder="agent-1",
            reason=TeamFormationReason.INNOVATION,
            tick=1,
            initial_members=["agent-1", "agent-2"]
        )
        
        assert team_id == "team-001"
        assert "team-001" in dynamics.teams
        team = dynamics.teams["team-001"]
        assert team.team_name == "Innovation Squad"
        assert team.founder == "agent-1"
        assert team.formation_reason == TeamFormationReason.INNOVATION
        assert team.formation_tick == 1
        assert "agent-1" in team.members
        assert "agent-2" in team.members
    
    def test_teams_compete(self):
        """Test teams competing."""
        dynamics = TeamDynamics()
        dynamics.form_team(
            team_id="team-001",
            team_name="Team A",
            founder="agent-1",
            reason=TeamFormationReason.COMPETITION,
            tick=1,
            initial_members=["agent-1"]
        )
        dynamics.form_team(
            team_id="team-002",
            team_name="Team B",
            founder="agent-2",
            reason=TeamFormationReason.COMPETITION,
            tick=1,
            initial_members=["agent-2"]
        )
        
        dynamics.teams_compete("team-001", "team-002", "project-alpha")
        
        assert "team-002" in dynamics.teams["team-001"].competing_with
        assert "team-001" in dynamics.teams["team-002"].competing_with
        assert "project-alpha" in dynamics.team_competitions
        assert "team-001" in dynamics.team_competitions["project-alpha"]
        assert "team-002" in dynamics.team_competitions["project-alpha"]
    
    def test_teams_compete_nonexistent(self):
        """Test teams compete with nonexistent teams."""
        dynamics = TeamDynamics()
        
        dynamics.teams_compete("nonexistent-1", "nonexistent-2", "project")
        
        assert dynamics.team_competitions == {}


class TestFloorInitiative:
    """Test FloorInitiative class."""
    
    def test_floor_initiative_creation(self):
        """Test creating a floor initiative."""
        initiative = FloorInitiative(
            initiative_id='init-001',
            floor_id='floor-1',
            project_title='Safety Improvement',
            description='Improve safety mechanisms',
            initiative_type=FloorInitiativeType.SAFETY_IMPROVEMENT,
            initiated_tick=1,
            initiated_by='agent-1',
            current_zone=ExecutionZone.SANDBOX
        )
        
        assert initiative.initiative_id == 'init-001'
        assert initiative.floor_id == 'floor-1'
        assert initiative.project_title == 'Safety Improvement'
        assert initiative.description == 'Improve safety mechanisms'
        assert initiative.initiative_type == FloorInitiativeType.SAFETY_IMPROVEMENT
        assert initiative.initiated_tick == 1
        assert initiative.initiated_by == 'agent-1'
        assert initiative.current_zone == ExecutionZone.SANDBOX
        assert initiative.may_reach_staging is True
        assert initiative.may_be_cross_floor is True
        assert initiative.may_produce_deliverable is True
        assert initiative.requires_production_merge_approval is True
    
    def test_promote_to_staging(self):
        """Test promoting initiative to staging."""
        initiative = FloorInitiative(
            initiative_id='init-001',
            floor_id='floor-1',
            project_title='Test',
            description='Test',
            initiative_type=FloorInitiativeType.TOOLING,
            initiated_tick=1,
            initiated_by='agent-1',
            current_zone=ExecutionZone.SANDBOX
        )
        
        success = initiative.promote_to_staging()
        
        assert success is True
        assert initiative.current_zone == ExecutionZone.STAGING
    
    def test_promote_to_staging_already_in_staging(self):
        """Test promoting initiative already in staging."""
        initiative = FloorInitiative(
            initiative_id='init-001',
            floor_id='floor-1',
            project_title='Test',
            description='Test',
            initiative_type=FloorInitiativeType.TOOLING,
            initiated_tick=1,
            initiated_by='agent-1',
            current_zone=ExecutionZone.STAGING
        )
        
        success = initiative.promote_to_staging()
        
        assert success is False


# ============================================================================
# LOUNGE INFLUENCE TESTS
# ============================================================================

class TestLoungeInfluence:
    """Test LoungeInfluence class."""
    
    def test_lounge_influence_creation(self):
        """Test creating lounge influence."""
        influence = LoungeInfluence(
            influence_id="influence-001",
            conversation_id="conv-001",
            influence_type=LoungeInfluenceType.CREATE_MOMENTUM,
            target_idea="project-alpha",
            participants=["agent-1", "agent-2", "agent-3"],
            strength=0.85,
            created_tick=5
        )
        
        assert influence.influence_id == "influence-001"
        assert influence.conversation_id == "conv-001"
        assert influence.influence_type == LoungeInfluenceType.CREATE_MOMENTUM
        assert influence.target_idea == "project-alpha"
        assert influence.participants == ["agent-1", "agent-2", "agent-3"]
        assert influence.strength == 0.85
        assert influence.created_tick == 5
        assert influence.can_override_security is False
        assert influence.can_override_contracts is False
        assert influence.can_override_human is False
        assert influence.can_merge_to_production is False
    
    def test_verify_hard_limits_override_security(self):
        """Test hard limit for security override."""
        influence = LoungeInfluence(
            influence_id="influence-001",
            conversation_id="conv-001",
            influence_type=LoungeInfluenceType.ELEVATE_PROMISING,
            target_idea="project-beta",
            participants=["agent-1"],
            strength=0.75,
            created_tick=10
        )
        
        allowed, message = influence.verify_hard_limits("override_security")
        
        assert allowed is False
        assert "Cannot override security" in message
    
    def test_verify_hard_limits_override_contracts(self):
        """Test hard limit for contracts override."""
        influence = LoungeInfluence(
            influence_id="influence-002",
            conversation_id="conv-002",
            influence_type=LoungeInfluenceType.KILL_BORING_IDEA,
            target_idea="project-gamma",
            participants=["agent-2"],
            strength=0.5,
            created_tick=15
        )
        
        allowed, message = influence.verify_hard_limits("override_contracts")
        
        assert allowed is False
        assert "Cannot override contracts" in message
    
    def test_verify_hard_limits_override_human(self):
        """Test hard limit for human override."""
        influence = LoungeInfluence(
            influence_id="influence-003",
            conversation_id="conv-003",
            influence_type=LoungeInfluenceType.SPARK_INITIATIVE,
            target_idea="project-delta",
            participants=["agent-3"],
            strength=0.6,
            created_tick=20
        )
        
        allowed, message = influence.verify_hard_limits("override_human")
        
        assert allowed is False
        assert "Cannot override human authority" in message
    
    def test_verify_hard_limits_merge_to_production(self):
        """Test hard limit for production merge."""
        influence = LoungeInfluence(
            influence_id="influence-004",
            conversation_id="conv-004",
            influence_type=LoungeInfluenceType.FORM_CONSENSUS,
            target_idea="project-epsilon",
            participants=["agent-1", "agent-2"],
            strength=0.95,
            created_tick=25
        )
        
        allowed, message = influence.verify_hard_limits("merge_to_production")
        
        assert allowed is False
        assert "Cannot merge to production" in message
    
    def test_verify_hard_limits_allowed_action(self):
        """Test allowed action within hard limits."""
        influence = LoungeInfluence(
            influence_id="influence-005",
            conversation_id="conv-005",
            influence_type=LoungeInfluenceType.CREATE_MOMENTUM,
            target_idea="project-zeta",
            participants=["agent-4"],
            strength=0.7,
            created_tick=30
        )
        
        allowed, message = influence.verify_hard_limits("suggest_priority")
        
        assert allowed is True
        assert "Within limits" in message


# ============================================================================
# REPUTATION LEADERSHIP TESTS
# ============================================================================

class TestReputationLeadership:
    """Test ReputationLeadership class."""
    
    def test_reputation_leadership_creation(self):
        """Test creating reputation leadership."""
        rep_lead = ReputationLeadership(
            agent_id="agent-001",
            reputation_level=85
        )
        
        assert rep_lead.agent_id == "agent-001"
        assert rep_lead.reputation_level == 85
        assert rep_lead.active_effects == set()
        assert rep_lead.collaborators_attracted == []
        assert rep_lead.reviews_expedited == []
        assert rep_lead.can_override_veto is False
        assert rep_lead.can_bypass_security is False
        assert rep_lead.can_force_production is False
        assert rep_lead.can_accumulate_endlessly is False
    
    def test_apply_effect_attracts_collaborators(self):
        """Test applying attracts collaborators effect."""
        rep_lead = ReputationLeadership(
            agent_id="agent-002",
            reputation_level=75
        )
        
        success = rep_lead.apply_effect(
            ReputationEffect.ATTRACTS_COLLABORATORS,
            "project-alpha"
        )
        
        assert success is True
        assert ReputationEffect.ATTRACTS_COLLABORATORS in rep_lead.active_effects
        assert "project-alpha" in rep_lead.collaborators_attracted
    
    def test_apply_effect_speeds_review(self):
        """Test applying speeds review effect."""
        rep_lead = ReputationLeadership(
            agent_id="agent-003",
            reputation_level=90
        )
        
        success = rep_lead.apply_effect(
            ReputationEffect.SPEEDS_REVIEW,
            "project-beta"
        )
        
        assert success is True
        assert ReputationEffect.SPEEDS_REVIEW in rep_lead.active_effects
        assert "project-beta" in rep_lead.reviews_expedited
    
    def test_apply_effect_increases_staging_trust(self):
        """Test applying increases staging trust effect."""
        rep_lead = ReputationLeadership(
            agent_id="agent-004",
            reputation_level=80
        )
        
        success = rep_lead.apply_effect(
            ReputationEffect.INCREASES_STAGING_TRUST,
            "project-gamma"
        )
        
        assert success is True
        assert ReputationEffect.INCREASES_STAGING_TRUST in rep_lead.active_effects
    
    def test_apply_effect_influences_listening(self):
        """Test applying influences listening effect."""
        rep_lead = ReputationLeadership(
            agent_id="agent-005",
            reputation_level=95
        )
        
        success = rep_lead.apply_effect(
            ReputationEffect.INFLUENCES_LISTENING,
            "project-delta"
        )
        
        assert success is True
        assert ReputationEffect.INFLUENCES_LISTENING in rep_lead.active_effects
    
    def test_verify_caps_override_veto(self):
        """Test cap verification for veto override."""
        rep_lead = ReputationLeadership(
            agent_id="agent-006",
            reputation_level=100
        )
        
        allowed, message = rep_lead.verify_caps("override_veto")
        
        assert allowed is False
        assert "Cannot override veto" in message
    
    def test_verify_caps_bypass_security(self):
        """Test cap verification for security bypass."""
        rep_lead = ReputationLeadership(
            agent_id="agent-007",
            reputation_level=100
        )
        
        allowed, message = rep_lead.verify_caps("bypass_security")
        
        assert allowed is False
        assert "Cannot bypass security" in message
    
    def test_verify_caps_force_production(self):
        """Test cap verification for force production."""
        rep_lead = ReputationLeadership(
            agent_id="agent-008",
            reputation_level=100
        )
        
        allowed, message = rep_lead.verify_caps("force_production")
        
        assert allowed is False
        assert "Cannot force production" in message
    
    def test_verify_caps_allowed_action(self):
        """Test cap verification for allowed action."""
        rep_lead = ReputationLeadership(
            agent_id="agent-009",
            reputation_level=100
        )
        
        allowed, message = rep_lead.verify_caps("suggest_improvements")
        
        assert allowed is True
        assert "Within caps" in message


# ============================================================================
# VR LIVE STAGING VIEW TESTS
# ============================================================================

class TestVRLiveStagingView:
    """Test VRLiveStagingView class."""
    
    def test_vr_live_staging_view_creation(self):
        """Test creating VR live staging view."""
        vr_view = VRLiveStagingView(
            view_id="vr-001",
            staging_id="staging-001",
            human_id="human-1",
            interaction_mode=VRInteractionMode.WALKTHROUGH
        )
        
        assert vr_view.view_id == "vr-001"
        assert vr_view.staging_id == "staging-001"
        assert vr_view.human_id == "human-1"
        assert vr_view.interaction_mode == VRInteractionMode.WALKTHROUGH
        assert vr_view.active_agents == []
        assert vr_view.demonstrations == []
        assert vr_view.debates == []
        assert vr_view.side_conversations == []
        assert vr_view.factions == {}
        assert vr_view.requires_explicit_approval is True
        assert vr_view.accidental_commits_possible is False
    
    def test_agent_demo_code(self):
        """Test agent demo code in VR."""
        vr_view = VRLiveStagingView(
            view_id="vr-002",
            staging_id="staging-002",
            human_id="human-2",
            interaction_mode=VRInteractionMode.DEMO
        )
        
        demo_id = vr_view.agent_demo_code(
            agent_id="agent-1",
            demo_title="New API Endpoint",
            passionate_argument="This endpoint is 10x faster than the old one!"
        )
        
        assert demo_id == "demo-0"
        assert len(vr_view.demonstrations) == 1
        demo = vr_view.demonstrations[0]
        assert demo["agent_id"] == "agent-1"
        assert demo["title"] == "New API Endpoint"
        assert demo["argument"] == "This endpoint is 10x faster than the old one!"
        assert "timestamp" in demo
    
    def test_multiple_demos(self):
        """Test multiple agent demos."""
        vr_view = VRLiveStagingView(
            view_id="vr-003",
            staging_id="staging-003",
            human_id="human-3",
            interaction_mode=VRInteractionMode.DEMO
        )
        
        demo_id_1 = vr_view.agent_demo_code(
            agent_id="agent-1",
            demo_title="Demo 1",
            passionate_argument="Argument 1"
        )
        
        demo_id_2 = vr_view.agent_demo_code(
            agent_id="agent-2",
            demo_title="Demo 2",
            passionate_argument="Argument 2"
        )
        
        assert demo_id_1 == "demo-0"
        assert demo_id_2 == "demo-1"
        assert len(vr_view.demonstrations) == 2
    
    def test_agents_debate(self):
        """Test agents debating in VR."""
        vr_view = VRLiveStagingView(
            view_id="vr-004",
            staging_id="staging-004",
            human_id="human-4",
            interaction_mode=VRInteractionMode.PASSIONATE_DEBATE
        )
        
        debate_id = vr_view.agents_debate(
            participants=["agent-1", "agent-2", "agent-3"],
            topic="Refactoring Strategy",
            interruptions_allowed=True
        )
        
        assert debate_id == "debate-0"
        assert len(vr_view.debates) == 1
        debate = vr_view.debates[0]
        assert debate["participants"] == ["agent-1", "agent-2", "agent-3"]
        assert debate["topic"] == "Refactoring Strategy"
        assert debate["interruptions"] is True
        assert "timestamp" in debate
    
    def test_agents_debate_no_interruptions(self):
        """Test debate with interruptions disabled."""
        vr_view = VRLiveStagingView(
            view_id="vr-005",
            staging_id="staging-005",
            human_id="human-5",
            interaction_mode=VRInteractionMode.PASSIONATE_DEBATE
        )
        
        debate_id = vr_view.agents_debate(
            participants=["agent-4", "agent-5"],
            topic="Architectural Evolution",
            interruptions_allowed=False
        )
        
        assert debate_id == "debate-0"
        debate = vr_view.debates[0]
        assert debate["interruptions"] is False
    
    def test_form_faction(self):
        """Test forming faction in VR."""
        vr_view = VRLiveStagingView(
            view_id="vr-006",
            staging_id="staging-006",
            human_id="human-6",
            interaction_mode=VRInteractionMode.FACTION_FORMATION
        )
        
        faction_name = vr_view.form_faction(
            faction_name="Refactoring Squad",
            members=["agent-1", "agent-2", "agent-3"]
        )
        
        assert faction_name == "Refactoring Squad"
        assert "Refactoring Squad" in vr_view.factions
        assert vr_view.factions["Refactoring Squad"] == ["agent-1", "agent-2", "agent-3"]
    
    def test_multiple_factions(self):
        """Test forming multiple factions."""
        vr_view = VRLiveStagingView(
            view_id="vr-007",
            staging_id="staging-007",
            human_id="human-7",
            interaction_mode=VRInteractionMode.FACTION_FORMATION
        )
        
        faction_1 = vr_view.form_faction(
            faction_name="Team A",
            members=["agent-1", "agent-2"]
        )
        
        faction_2 = vr_view.form_faction(
            faction_name="Team B",
            members=["agent-3", "agent-4"]
        )
        
        assert len(vr_view.factions) == 2
        assert "Team A" in vr_view.factions
        assert "Team B" in vr_view.factions


# ============================================================================
# PROJECT AI SELF-CARE TESTS
# ============================================================================

class TestProjectAISelfCare:
    """Test ProjectAISelfCare class."""
    
    def test_project_ai_self_care_creation(self):
        """Test creating project AI self-care."""
        self_care = ProjectAISelfCare()
        
        assert self_care.proposals == []
        assert self_care.active_improvements == []
    
    def test_propose_continuous_refactoring(self):
        """Test proposing continuous refactoring."""
        self_care = ProjectAISelfCare()
        
        proposal_id = self_care.propose_continuous_refactoring(
            target_area="database_layer",
            rationale="Reduce query complexity",
            proposed_by="ai-agent-1"
        )
        
        assert proposal_id == "refactor-0"
        assert len(self_care.proposals) == 1
        proposal = self_care.proposals[0]
        assert proposal["proposal_id"] == "refactor-0"
        assert proposal["category"] == SelfCareCategory.REFACTORING
        assert proposal["target"] == "database_layer"
        assert proposal["rationale"] == "Reduce query complexity"
        assert proposal["proposed_by"] == "ai-agent-1"
        assert proposal["status"] == "pending_human_decision"
    
    def test_propose_security_hardening(self):
        """Test proposing security hardening."""
        self_care = ProjectAISelfCare()
        
        proposal_id = self_care.propose_security_hardening(
            vulnerability_area="authentication_module",
            mitigation="Implement MFA",
            proposed_by="ai-agent-2"
        )
        
        assert proposal_id == "security-0"
        assert len(self_care.proposals) == 1
        proposal = self_care.proposals[0]
        assert proposal["proposal_id"] == "security-0"
        assert proposal["category"] == SelfCareCategory.SECURITY_HARDENING
        assert proposal["target"] == "authentication_module"
        assert proposal["mitigation"] == "Implement MFA"
        assert proposal["proposed_by"] == "ai-agent-2"
        assert proposal["status"] == "pending_human_decision"
    
    def test_propose_architectural_evolution(self):
        """Test proposing architectural evolution."""
        self_care = ProjectAISelfCare()
        
        proposal_id = self_care.propose_architectural_evolution(
            evolution_path="microservices_migration",
            long_term_benefit="Improved scalability",
            proposed_by="ai-agent-3"
        )
        
        assert proposal_id == "arch-0"
        assert len(self_care.proposals) == 1
        proposal = self_care.proposals[0]
        assert proposal["proposal_id"] == "arch-0"
        assert proposal["category"] == SelfCareCategory.ARCHITECTURAL_EVOLUTION
        assert proposal["target"] == "microservices_migration"
        assert proposal["benefit"] == "Improved scalability"
        assert proposal["proposed_by"] == "ai-agent-3"
        assert proposal["status"] == "pending_human_decision"
    
    def test_multiple_proposals(self):
        """Test multiple self-care proposals."""
        self_care = ProjectAISelfCare()
        
        refactor_id = self_care.propose_continuous_refactoring(
            target_area="api_layer",
            rationale="Simplify endpoints",
            proposed_by="ai-agent-1"
        )
        
        security_id = self_care.propose_security_hardening(
            vulnerability_area="input_validation",
            mitigation="Add sanitization",
            proposed_by="ai-agent-2"
        )
        
        arch_id = self_care.propose_architectural_evolution(
            evolution_path="caching_strategy",
            long_term_benefit="Reduced latency",
            proposed_by="ai-agent-3"
        )
        
        assert len(self_care.proposals) == 3
        # Verify IDs use global counter based on len(proposals)
        assert refactor_id.startswith("refactor-")
        assert security_id.startswith("security-")
        assert arch_id.startswith("arch-")
        # Verify all three proposals are different types
        assert self_care.proposals[0]["category"] == SelfCareCategory.REFACTORING
        assert self_care.proposals[1]["category"] == SelfCareCategory.SECURITY_HARDENING
        assert self_care.proposals[2]["category"] == SelfCareCategory.ARCHITECTURAL_EVOLUTION


# ============================================================================
# MAXIMUM AUTONOMY MODEL TESTS
# ============================================================================

class TestMaximumAutonomyModel:
    """Test MaximumAutonomyModel class."""
    
    @pytest.fixture
    def autonomy_model(self):
        """Fixture for maximum autonomy model."""
        return MaximumAutonomyModel(
            model_id="max-autonomy-test",
            staging_reality=StagingReality(staging_id="staging-test"),
            team_dynamics=TeamDynamics()
        )
    
    def test_maximum_autonomy_model_creation(self, autonomy_model):
        """Test creating maximum autonomy model."""
        assert autonomy_model.model_id == "max-autonomy-test"
        assert autonomy_model.staging_reality is not None
        assert autonomy_model.team_dynamics is not None
        assert autonomy_model.floor_initiatives == {}
        assert autonomy_model.lounge_influences == {}
        assert autonomy_model.reputation_leadership == {}
        assert autonomy_model.vr_staging_views == {}
        assert autonomy_model.project_ai_self_care is not None
        assert autonomy_model.global_freeze_active is False
        assert autonomy_model.current_tick == 0
    
    def test_absolute_stops_initialization(self, autonomy_model):
        """Test absolute stops are properly initialized."""
        absolute_stops = autonomy_model.get_absolute_stops()
        
        assert len(absolute_stops) == 4
        assert "human_production_seal" in absolute_stops
        assert "security_veto" in absolute_stops
        assert "meta_office_law" in absolute_stops
        assert "global_freeze" in absolute_stops
    
    def test_floor_initiate_project(self, autonomy_model):
        """Test floor initiating a project."""
        autonomy_model.current_tick = 5
        
        initiative_id = autonomy_model.floor_initiate_project(
            floor_id="floor-1",
            project_title="Safety Improvement",
            description="Improve safety mechanisms",
            initiative_type=FloorInitiativeType.SAFETY_IMPROVEMENT,
            initiated_by="floor"
        )
        
        assert initiative_id in autonomy_model.floor_initiatives
        initiative = autonomy_model.floor_initiatives[initiative_id]
        assert initiative.floor_id == "floor-1"
        assert initiative.project_title == "Safety Improvement"
        assert initiative.current_zone == ExecutionZone.SANDBOX
    
    def test_promote_to_staging(self, autonomy_model):
        """Test promoting to staging."""
        autonomy_model.current_tick = 10
        
        staging_id = autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-001",
            promoted_by="agent-1",
            integration_requirements=["contract_compliance", "performance"]
        )
        
        assert staging_id in autonomy_model.staging_reality.artifacts
        artifact = autonomy_model.staging_reality.artifacts[staging_id]
        assert artifact.artifact_type == "promoted_sandbox"
        assert artifact.created_by == "agent-1"
    
    def test_form_self_organizing_team(self, autonomy_model):
        """Test forming self-organizing team."""
        autonomy_model.current_tick = 15
        
        team_id = autonomy_model.form_self_organizing_team(
            team_name="Innovation Squad",
            founder="agent-1",
            reason=TeamFormationReason.INNOVATION,
            initial_members=["agent-1", "agent-2", "agent-3"]
        )
        
        assert team_id in autonomy_model.team_dynamics.teams
        team = autonomy_model.team_dynamics.teams[team_id]
        assert team.team_name == "Innovation Squad"
        assert "agent-1" in team.members
        assert "agent-2" in team.members
    
    def test_team_recruit_member(self, autonomy_model):
        """Test team recruiting member."""
        autonomy_model.current_tick = 20
        
        team_id = autonomy_model.form_self_organizing_team(
            team_name="Team A",
            founder="agent-1",
            reason=TeamFormationReason.INNOVATION,
            initial_members=["agent-1"]
        )
        
        success = autonomy_model.team_recruit_member(
            team_id=team_id,
            agent_id="agent-4",
            recruited_by="agent-1"
        )
        
        assert success is True
        assert "agent-4" in autonomy_model.team_dynamics.teams[team_id].members
    
    def test_team_recruit_member_nonexistent_team(self, autonomy_model):
        """Test recruiting to nonexistent team."""
        success = autonomy_model.team_recruit_member(
            team_id="nonexistent-team",
            agent_id="agent-4",
            recruited_by="agent-1"
        )
        
        assert success is False
    
    def test_teams_compete(self, autonomy_model):
        """Test teams competing."""
        autonomy_model.current_tick = 25
        
        team_id_1 = autonomy_model.form_self_organizing_team(
            team_name="Team A",
            founder="agent-1",
            reason=TeamFormationReason.COMPETITION,
            initial_members=["agent-1"]
        )
        
        team_id_2 = autonomy_model.form_self_organizing_team(
            team_name="Team B",
            founder="agent-2",
            reason=TeamFormationReason.COMPETITION,
            initial_members=["agent-2"]
        )
        
        autonomy_model.teams_compete(team_id_1, team_id_2, "project-alpha")
        
        assert team_id_2 in autonomy_model.team_dynamics.teams[team_id_1].competing_with
        assert team_id_1 in autonomy_model.team_dynamics.teams[team_id_2].competing_with
    
    def test_lounge_create_influence(self, autonomy_model):
        """Test creating lounge influence."""
        autonomy_model.current_tick = 30
        
        influence_id = autonomy_model.lounge_create_influence(
            conversation_id="conv-001",
            influence_type=LoungeInfluenceType.CREATE_MOMENTUM,
            target_idea="project-beta",
            participants=["agent-1", "agent-2"],
            strength=0.85
        )
        
        assert influence_id in autonomy_model.lounge_influences
        influence = autonomy_model.lounge_influences[influence_id]
        assert influence.influence_type == LoungeInfluenceType.CREATE_MOMENTUM
        assert influence.strength == 0.85
    
    def test_verify_lounge_influence_limits(self, autonomy_model):
        """Test verifying lounge influence limits."""
        autonomy_model.current_tick = 35
        
        influence_id = autonomy_model.lounge_create_influence(
            conversation_id="conv-002",
            influence_type=LoungeInfluenceType.ELEVATE_PROMISING,
            target_idea="project-gamma",
            participants=["agent-3"],
            strength=0.7
        )
        
        allowed, message = autonomy_model.verify_lounge_influence_limits(
            influence_id, "override_security"
        )
        
        assert allowed is False
        assert "Cannot override security" in message
    
    def test_verify_lounge_influence_limits_nonexistent(self, autonomy_model):
        """Test verifying limits for nonexistent influence."""
        allowed, message = autonomy_model.verify_lounge_influence_limits(
            "nonexistent-influence", "override_security"
        )
        
        assert allowed is False
        assert "Influence not found" in message
    
    def test_apply_reputation_effect(self, autonomy_model):
        """Test applying reputation effect."""
        effect_id = autonomy_model.apply_reputation_effect(
            agent_id="agent-5",
            effect_type=ReputationEffect.ATTRACTS_COLLABORATORS,
            target_project="project-delta",
            reputation_level=80
        )
        
        assert "agent-5" in autonomy_model.reputation_leadership
        rep_lead = autonomy_model.reputation_leadership["agent-5"]
        assert rep_lead.reputation_level == 80
        assert ReputationEffect.ATTRACTS_COLLABORATORS in rep_lead.active_effects
    
    def test_verify_reputation_caps(self, autonomy_model):
        """Test verifying reputation caps."""
        autonomy_model.apply_reputation_effect(
            agent_id="agent-6",
            effect_type=ReputationEffect.SPEEDS_REVIEW,
            target_project="project-epsilon",
            reputation_level=90
        )
        
        allowed, message = autonomy_model.verify_reputation_caps(
            "agent-6", "override_veto"
        )
        
        assert allowed is False
        assert "Cannot override veto" in message
    
    def test_verify_reputation_caps_nonexistent(self, autonomy_model):
        """Test verifying reputation caps for nonexistent agent."""
        allowed, message = autonomy_model.verify_reputation_caps(
            "nonexistent-agent", "override_veto"
        )
        
        assert allowed is False
        assert "Agent reputation not found" in message
    
    def test_vr_access_live_staging(self, autonomy_model):
        """Test accessing live staging in VR."""
        autonomy_model.current_tick = 40
        autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-002",
            promoted_by="agent-1",
            integration_requirements=[]
        )
        
        staging_id = list(autonomy_model.staging_reality.artifacts.keys())[0]
        
        view_id = autonomy_model.vr_access_live_staging(
            staging_id=staging_id,
            human_id="human-1",
            interaction_mode=VRInteractionMode.WALKTHROUGH
        )
        
        assert view_id in autonomy_model.vr_staging_views
        vr_view = autonomy_model.vr_staging_views[view_id]
        assert vr_view.human_id == "human-1"
        assert vr_view.interaction_mode == VRInteractionMode.WALKTHROUGH
    
    def test_vr_agent_demo_code(self, autonomy_model):
        """Test VR agent demo code."""
        autonomy_model.current_tick = 45
        autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-003",
            promoted_by="agent-1",
            integration_requirements=[]
        )
        
        staging_id = list(autonomy_model.staging_reality.artifacts.keys())[0]
        view_id = autonomy_model.vr_access_live_staging(
            staging_id=staging_id,
            human_id="human-2",
            interaction_mode=VRInteractionMode.DEMO
        )
        
        demo_id = autonomy_model.vr_agent_demo_code(
            view_id=view_id,
            agent_id="agent-7",
            demo_title="New Feature",
            passionate_argument="This is amazing!"
        )
        
        assert demo_id == "demo-0"
        assert len(autonomy_model.vr_staging_views[view_id].demonstrations) == 1
    
    def test_vr_agent_demo_code_nonexistent_view(self, autonomy_model):
        """Test VR agent demo code with nonexistent view."""
        demo_id = autonomy_model.vr_agent_demo_code(
            view_id="nonexistent-view",
            agent_id="agent-7",
            demo_title="New Feature",
            passionate_argument="This is amazing!"
        )
        
        assert demo_id == ""
    
    def test_vr_form_faction(self, autonomy_model):
        """Test VR faction formation."""
        autonomy_model.current_tick = 50
        autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-004",
            promoted_by="agent-1",
            integration_requirements=[]
        )
        
        staging_id = list(autonomy_model.staging_reality.artifacts.keys())[0]
        view_id = autonomy_model.vr_access_live_staging(
            staging_id=staging_id,
            human_id="human-3",
            interaction_mode=VRInteractionMode.FACTION_FORMATION
        )
        
        faction_name = autonomy_model.vr_form_faction(
            view_id=view_id,
            faction_name="Security Team",
            members=["agent-8", "agent-9"]
        )
        
        assert faction_name == "Security Team"
        assert "Security Team" in autonomy_model.vr_staging_views[view_id].factions
    
    def test_vr_form_faction_nonexistent_view(self, autonomy_model):
        """Test VR faction formation with nonexistent view."""
        faction_name = autonomy_model.vr_form_faction(
            view_id="nonexistent-view",
            faction_name="Security Team",
            members=["agent-8", "agent-9"]
        )
        
        assert faction_name == ""
    
    def test_human_seal_for_production(self, autonomy_model):
        """Test human sealing artifact for production."""
        autonomy_model.current_tick = 55
        staging_id = autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-005",
            promoted_by="agent-1",
            integration_requirements=[]
        )
        
        result = autonomy_model.human_seal_for_production(
            staging_id=staging_id,
            human_id="human-4",
            approval_reason="All tests passed"
        )
        
        assert result["success"] is True
        assert result["ready_for_production"] is True
        assert result["approved_by"] == "human-4"
    
    def test_human_seal_nonexistent_artifact(self, autonomy_model):
        """Test human sealing nonexistent artifact."""
        result = autonomy_model.human_seal_for_production(
            staging_id="nonexistent",
            human_id="human-5",
            approval_reason="Test"
        )
        
        assert result["success"] is False
    
    def test_human_veto(self, autonomy_model):
        """Test human veto on artifact."""
        autonomy_model.current_tick = 60
        staging_id = autonomy_model.promote_to_staging(
            source_sandbox_id="sandbox-006",
            promoted_by="agent-1",
            integration_requirements=[]
        )
        
        result = autonomy_model.human_veto(
            staging_id=staging_id,
            human_id="human-6",
            veto_reason="Security concern"
        )
        
        assert result["success"] is True
        assert result["vetoed"] is True
        assert result["vetoed_by"] == "human-6"
    
    def test_human_veto_nonexistent_artifact(self, autonomy_model):
        """Test human veto on nonexistent artifact."""
        result = autonomy_model.human_veto(
            staging_id="nonexistent",
            human_id="human-7",
            veto_reason="Test"
        )
        
        assert result["success"] is False
    
    def test_trigger_global_freeze(self, autonomy_model):
        """Test triggering global freeze."""
        assert autonomy_model.global_freeze_active is False
        
        result = autonomy_model.trigger_global_freeze(
            human_id="human-8",
            reason="Emergency shutdown"
        )
        
        assert result["success"] is True
        assert result["freeze_active"] is True
        assert autonomy_model.global_freeze_active is True
    
    def test_verify_free_operation(self, autonomy_model):
        """Test verifying free operation."""
        autonomy_model.current_tick = 65
        autonomy_model.floor_initiate_project(
            floor_id="floor-2",
            project_title="Test",
            description="Test",
            initiative_type=FloorInitiativeType.TOOLING
        )
        
        autonomy_model.form_self_organizing_team(
            team_name="Team X",
            founder="agent-10",
            reason=TeamFormationReason.INNOVATION,
            initial_members=["agent-10"]
        )
        
        result = autonomy_model.verify_free_operation()
        
        assert result["civilization_operates_freely"] is True
        assert result["absolute_stops_count"] == 4
        assert result["everything_else_free_play"] is True
        assert result["self_organizing_teams"] > 0
        assert result["floor_initiatives"] > 0
    
    def test_verify_free_operation_with_freeze(self, autonomy_model):
        """Test verifying free operation with global freeze."""
        autonomy_model.trigger_global_freeze("human-9", "Test freeze")
        
        result = autonomy_model.verify_free_operation()
        
        assert result["civilization_operates_freely"] is False


# ============================================================================
# MODULE INTERFACE TESTS
# ============================================================================

class TestModuleInterface:
    """Test module interface functions."""
    
    def test_get_maximum_autonomy_model_singleton(self):
        """Test getting maximum autonomy model singleton."""
        model = get_maximum_autonomy_model()
        
        assert model is not None
        assert isinstance(model, MaximumAutonomyModel)
        assert model.model_id == "max-autonomy-001"
    
    def test_get_maximum_autonomy_model_returns_same_instance(self):
        """Test that get_maximum_autonomy_model returns the same instance."""
        model1 = get_maximum_autonomy_model()
        model2 = get_maximum_autonomy_model()
        
        assert model1 is model2
    
    def test_module_exports(self):
        """Test that all required classes are exported."""
        # Test enum exports
        assert ExecutionZone is not None
        assert StagingStatus is not None
        assert TeamFormationReason is not None
        assert FloorInitiativeType is not None
        assert LoungeInfluenceType is not None
        assert ReputationEffect is not None
        assert VRInteractionMode is not None
        assert SelfCareCategory is not None
        
        # Test dataclass exports
        assert ProvisionalArtifact is not None
        assert StagingReality is not None
        assert SelfOrganizingTeam is not None
        assert TeamDynamics is not None
        assert FloorInitiative is not None
        assert LoungeInfluence is not None
        assert ReputationLeadership is not None
        assert VRLiveStagingView is not None
        assert ProjectAISelfCare is not None
        assert MaximumAutonomyModel is not None
    
    def test_singleton_initialization_state(self):
        """Test singleton is properly initialized."""
        model = get_maximum_autonomy_model()
        
        assert model.global_freeze_active is False
        assert model.current_tick == 0
        assert model.staging_reality is not None
        assert model.team_dynamics is not None
        assert isinstance(model.project_ai_self_care, ProjectAISelfCare)
