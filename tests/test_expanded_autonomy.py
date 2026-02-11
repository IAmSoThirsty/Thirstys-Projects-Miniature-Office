"""Unit tests for the expanded autonomy system."""
import pytest
from datetime import datetime, timedelta
from src.core.expanded_autonomy import (
    SandboxStatus, ProjectType, SandboxBranch, SandboxManager,
    ConversationTheme, TechGossipCategory, TechGossipItem, TechGossipBoard,
    LoungeConversation, LoungeWorkBleed, ReputationInfluence, ReputationSystem,
    EmployeeSpotlight, RollbackGuarantee, ExpandedAutonomyModel,
    create_expanded_autonomy_model, get_expanded_autonomy_model
)


class TestSandboxStatus:
    """Test SandboxStatus enum."""
    
    def test_sandbox_status_values(self):
        """Test sandbox status enum values."""
        assert SandboxStatus.ACTIVE.value == "active"
        assert SandboxStatus.PROMOTION_CANDIDATE.value == "promotion_candidate"
        assert SandboxStatus.PROMOTED.value == "promoted"
        assert SandboxStatus.ARCHIVED.value == "archived"
        assert SandboxStatus.KILLED.value == "killed"


class TestProjectType:
    """Test ProjectType enum."""
    
    def test_project_type_values(self):
        """Test project type enum values."""
        assert ProjectType.WIDGET.value == "widget"
        assert ProjectType.UTILITY.value == "utility"
        assert ProjectType.REFACTORING.value == "refactoring"
        assert ProjectType.PROTOTYPE.value == "prototype"
        assert ProjectType.TEST_HELPER.value == "test_helper"
        assert ProjectType.DOC_HELPER.value == "doc_helper"
        assert ProjectType.EXPERIMENT.value == "experiment"


class TestSandboxBranch:
    """Test SandboxBranch class."""
    
    def test_sandbox_creation(self):
        """Test basic sandbox creation."""
        sandbox = SandboxBranch(
            branch_id="sb-001",
            name="Test Sandbox",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="A test sandbox"
        )
        
        assert sandbox.branch_id == "sb-001"
        assert sandbox.name == "Test Sandbox"
        assert sandbox.owner_floor == "floor-1"
        assert sandbox.project_type == ProjectType.WIDGET
        assert sandbox.description == "A test sandbox"
        assert sandbox.status == SandboxStatus.ACTIVE
        assert sandbox.touches_production is False
        assert sandbox.auto_merge_enabled is False
    
    def test_flag_for_promotion(self):
        """Test flagging sandbox for promotion."""
        sandbox = SandboxBranch(
            branch_id="sb-002",
            name="Widget",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.flag_for_promotion("agent-1", "Ready for production")
        
        assert sandbox.status == SandboxStatus.PROMOTION_CANDIDATE
        assert sandbox.promotion_candidate_flagged_by == "agent-1"
        assert sandbox.promotion_reason == "Ready for production"
        assert sandbox.promotion_candidate_flagged_at is not None
    
    def test_promote(self):
        """Test promoting sandbox to production."""
        sandbox = SandboxBranch(
            branch_id="sb-003",
            name="Utility",
            owner_floor="floor-1",
            project_type=ProjectType.UTILITY,
            description="Test"
        )
        
        sandbox.promote("human-1", "directive-001")
        
        assert sandbox.status == SandboxStatus.PROMOTED
        assert sandbox.human_decision == "promote"
        assert sandbox.promoted_to_directive_id == "directive-001"
    
    def test_leave_in_sandbox(self):
        """Test leaving sandbox active."""
        sandbox = SandboxBranch(
            branch_id="sb-004",
            name="Test",
            owner_floor="floor-1",
            project_type=ProjectType.EXPERIMENT,
            description="Test"
        )
        
        sandbox.flag_for_promotion("agent-1", "Test")
        sandbox.leave_in_sandbox("human-1")
        
        assert sandbox.status == SandboxStatus.ACTIVE
        assert sandbox.human_decision == "leave"
        assert sandbox.promotion_candidate_flagged_at is None
    
    def test_archive(self):
        """Test archiving sandbox."""
        sandbox = SandboxBranch(
            branch_id="sb-005",
            name="Old",
            owner_floor="floor-1",
            project_type=ProjectType.PROTOTYPE,
            description="Test"
        )
        
        sandbox.archive("human-1")
        
        assert sandbox.status == SandboxStatus.ARCHIVED
        assert sandbox.human_decision == "archive"
    
    def test_kill(self):
        """Test killing sandbox."""
        sandbox = SandboxBranch(
            branch_id="sb-006",
            name="Bad",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.kill("human-1")
        
        assert sandbox.status == SandboxStatus.KILLED
        assert sandbox.human_decision == "kill"
    
    def test_add_code_file(self):
        """Test adding code files to sandbox."""
        sandbox = SandboxBranch(
            branch_id="sb-007",
            name="Code",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.add_code_file("main.py", "print('hello')")
        sandbox.add_code_file("test.py", "def test(): pass")
        
        assert len(sandbox.code_files) == 2
        assert sandbox.code_files["main.py"] == "print('hello')"
        assert sandbox.code_files["test.py"] == "def test(): pass"
    
    def test_add_code_file_update(self):
        """Test updating existing code file."""
        sandbox = SandboxBranch(
            branch_id="sb-008",
            name="Update",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.add_code_file("main.py", "v1")
        sandbox.add_code_file("main.py", "v2")
        
        assert len(sandbox.code_files) == 1
        assert sandbox.code_files["main.py"] == "v2"
    
    def test_add_collaborator(self):
        """Test adding collaborators."""
        sandbox = SandboxBranch(
            branch_id="sb-009",
            name="Collab",
            owner_floor="floor-1",
            project_type=ProjectType.REFACTORING,
            description="Test"
        )
        
        sandbox.add_collaborator("floor-2")
        sandbox.add_collaborator("floor-3")
        
        assert len(sandbox.collaborators) == 2
        assert "floor-2" in sandbox.collaborators
        assert "floor-3" in sandbox.collaborators
    
    def test_add_collaborator_duplicate(self):
        """Test adding duplicate collaborator."""
        sandbox = SandboxBranch(
            branch_id="sb-010",
            name="Dup",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.add_collaborator("floor-2")
        sandbox.add_collaborator("floor-2")
        
        assert len(sandbox.collaborators) == 1
    
    def test_verify_sandbox_isolation_clean(self):
        """Test verifying clean sandbox isolation."""
        sandbox = SandboxBranch(
            branch_id="sb-011",
            name="Clean",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        is_valid, violations = sandbox.verify_sandbox_isolation()
        
        assert is_valid is True
        assert len(violations) == 0
    
    def test_verify_sandbox_isolation_touches_production(self):
        """Test detecting production touch violation."""
        sandbox = SandboxBranch(
            branch_id="sb-012",
            name="Bad",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.touches_production = True
        is_valid, violations = sandbox.verify_sandbox_isolation()
        
        assert is_valid is False
        assert len(violations) == 1
        assert "touches production" in violations[0]
    
    def test_verify_sandbox_isolation_auto_merge(self):
        """Test detecting auto-merge violation."""
        sandbox = SandboxBranch(
            branch_id="sb-013",
            name="Bad",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.auto_merge_enabled = True
        is_valid, violations = sandbox.verify_sandbox_isolation()
        
        assert is_valid is False
        assert len(violations) == 1
        assert "Auto-merge" in violations[0]
    
    def test_verify_sandbox_isolation_production_resources(self):
        """Test detecting production resource usage."""
        sandbox = SandboxBranch(
            branch_id="sb-014",
            name="Bad",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.sandbox_resources_used["production_database"] = 10.0
        is_valid, violations = sandbox.verify_sandbox_isolation()
        
        assert is_valid is False
        assert len(violations) == 1
        assert "production" in violations[0].lower()
    
    def test_verify_sandbox_isolation_multiple_violations(self):
        """Test detecting multiple violations."""
        sandbox = SandboxBranch(
            branch_id="sb-015",
            name="Very Bad",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="Test"
        )
        
        sandbox.touches_production = True
        sandbox.auto_merge_enabled = True
        sandbox.sandbox_resources_used["production_api"] = 5.0
        
        is_valid, violations = sandbox.verify_sandbox_isolation()
        
        assert is_valid is False
        assert len(violations) == 3


class TestSandboxManager:
    """Test SandboxManager class."""
    
    def test_manager_creation(self):
        """Test sandbox manager creation."""
        manager = SandboxManager(manager_id="sm-001")
        
        assert manager.manager_id == "sm-001"
        assert len(manager.sandboxes) == 0
    
    def test_create_sandbox(self):
        """Test creating a sandbox."""
        manager = SandboxManager(manager_id="sm-002")
        
        sandbox_id = manager.create_sandbox(
            name="Test Widget",
            owner_floor="floor-1",
            project_type=ProjectType.WIDGET,
            description="A test widget"
        )
        
        assert sandbox_id == "sandbox-1"
        assert len(manager.sandboxes) == 1
        assert sandbox_id in manager.sandboxes
        assert manager.sandboxes[sandbox_id].name == "Test Widget"
    
    def test_create_multiple_sandboxes(self):
        """Test creating multiple sandboxes."""
        manager = SandboxManager(manager_id="sm-003")
        
        id1 = manager.create_sandbox("Widget 1", "floor-1", ProjectType.WIDGET, "Test 1")
        id2 = manager.create_sandbox("Widget 2", "floor-2", ProjectType.UTILITY, "Test 2")
        id3 = manager.create_sandbox("Widget 3", "floor-3", ProjectType.PROTOTYPE, "Test 3")
        
        assert len(manager.sandboxes) == 3
        assert id1 == "sandbox-1"
        assert id2 == "sandbox-2"
        assert id3 == "sandbox-3"
    
    def test_get_promotion_candidates(self):
        """Test getting promotion candidates."""
        manager = SandboxManager(manager_id="sm-004")
        
        id1 = manager.create_sandbox("Widget 1", "floor-1", ProjectType.WIDGET, "Test")
        id2 = manager.create_sandbox("Widget 2", "floor-2", ProjectType.WIDGET, "Test")
        id3 = manager.create_sandbox("Widget 3", "floor-3", ProjectType.WIDGET, "Test")
        
        # Flag one for promotion
        manager.sandboxes[id1].flag_for_promotion("agent-1", "Ready")
        manager.sandboxes[id3].flag_for_promotion("agent-2", "Also ready")
        
        candidates = manager.get_promotion_candidates()
        
        assert len(candidates) == 2
        assert manager.sandboxes[id1] in candidates
        assert manager.sandboxes[id3] in candidates
    
    def test_get_promotion_candidates_empty(self):
        """Test getting promotion candidates when none exist."""
        manager = SandboxManager(manager_id="sm-005")
        
        manager.create_sandbox("Widget", "floor-1", ProjectType.WIDGET, "Test")
        
        candidates = manager.get_promotion_candidates()
        
        assert len(candidates) == 0
    
    def test_collapse_all_sandboxes(self):
        """Test collapsing all sandboxes."""
        manager = SandboxManager(manager_id="sm-006")
        
        id1 = manager.create_sandbox("Widget 1", "floor-1", ProjectType.WIDGET, "Test")
        id2 = manager.create_sandbox("Widget 2", "floor-2", ProjectType.WIDGET, "Test")
        id3 = manager.create_sandbox("Widget 3", "floor-3", ProjectType.WIDGET, "Test")
        
        # Flag one for promotion
        manager.sandboxes[id2].flag_for_promotion("agent-1", "Ready")
        
        # Archive one
        manager.sandboxes[id3].archive("human-1")
        
        count = manager.collapse_all_sandboxes("Test rollback")
        
        assert count == 2  # Only active and promotion candidates
        assert manager.sandboxes[id1].status == SandboxStatus.KILLED
        assert manager.sandboxes[id2].status == SandboxStatus.KILLED
        assert manager.sandboxes[id3].status == SandboxStatus.ARCHIVED  # Not changed
    
    def test_collapse_all_sandboxes_empty(self):
        """Test collapsing when no sandboxes exist."""
        manager = SandboxManager(manager_id="sm-007")
        
        count = manager.collapse_all_sandboxes("Test")
        
        assert count == 0
    
    def test_get_active_sandboxes(self):
        """Test getting active sandboxes."""
        manager = SandboxManager(manager_id="sm-008")
        
        id1 = manager.create_sandbox("Widget 1", "floor-1", ProjectType.WIDGET, "Test")
        id2 = manager.create_sandbox("Widget 2", "floor-2", ProjectType.WIDGET, "Test")
        id3 = manager.create_sandbox("Widget 3", "floor-3", ProjectType.WIDGET, "Test")
        
        # Change statuses
        manager.sandboxes[id2].flag_for_promotion("agent-1", "Ready")
        manager.sandboxes[id3].archive("human-1")
        
        active = manager.get_active_sandboxes()
        
        assert len(active) == 1
        assert manager.sandboxes[id1] in active


class TestConversationTheme:
    """Test ConversationTheme enum."""
    
    def test_conversation_theme_values(self):
        """Test conversation theme enum values."""
        assert ConversationTheme.TECHNICAL.value == "technical"
        assert ConversationTheme.PHILOSOPHICAL.value == "philosophical"
        assert ConversationTheme.HUMOROUS.value == "humorous"
        assert ConversationTheme.NARRATIVE.value == "narrative"
        assert ConversationTheme.PROBLEM_SOLVING.value == "problem_solving"
        assert ConversationTheme.BRAINSTORMING.value == "brainstorming"
        assert ConversationTheme.TECH_GOSSIP.value == "tech_gossip"


class TestTechGossipCategory:
    """Test TechGossipCategory enum."""
    
    def test_tech_gossip_category_values(self):
        """Test tech gossip category enum values."""
        assert TechGossipCategory.AI_DEVELOPMENTS.value == "ai_developments"
        assert TechGossipCategory.LANGUAGE_RELEASES.value == "language_releases"
        assert TechGossipCategory.FRAMEWORK_UPDATES.value == "framework_updates"
        assert TechGossipCategory.HARDWARE_BREAKTHROUGHS.value == "hardware_breakthroughs"
        assert TechGossipCategory.SECURITY_INCIDENTS.value == "security_incidents"
        assert TechGossipCategory.INDUSTRY_DRAMA.value == "industry_drama"
        assert TechGossipCategory.RESEARCH_PAPERS.value == "research_papers"
        assert TechGossipCategory.STARTUP_NEWS.value == "startup_news"
        assert TechGossipCategory.OPEN_SOURCE.value == "open_source"
        assert TechGossipCategory.QUANTUM_COMPUTING.value == "quantum_computing"
        assert TechGossipCategory.BLOCKCHAIN_CRYPTO.value == "blockchain_crypto"
        assert TechGossipCategory.CLOUD_INFRASTRUCTURE.value == "cloud_infrastructure"


class TestTechGossipItem:
    """Test TechGossipItem class."""
    
    def test_gossip_item_creation(self):
        """Test creating tech gossip item."""
        gossip = TechGossipItem(
            gossip_id="tg-001",
            category=TechGossipCategory.AI_DEVELOPMENTS,
            topic="GPT-5 rumors",
            speculation="Might have multimodal capabilities",
            gossiped_by=["agent-1", "agent-2"]
        )
        
        assert gossip.gossip_id == "tg-001"
        assert gossip.category == TechGossipCategory.AI_DEVELOPMENTS
        assert gossip.topic == "GPT-5 rumors"
        assert gossip.affects_production is False
        assert gossip.creates_requirements is False
        assert gossip.influences_decisions is False
    
    def test_add_interested_agent(self):
        """Test adding interested agents."""
        gossip = TechGossipItem(
            gossip_id="tg-002",
            category=TechGossipCategory.LANGUAGE_RELEASES,
            topic="Rust 2.0",
            speculation="New features",
            gossiped_by=["agent-1"]
        )
        
        gossip.add_interested_agent("agent-2")
        gossip.add_interested_agent("agent-3")
        
        assert len(gossip.agents_interested) == 2
        assert "agent-2" in gossip.agents_interested
    
    def test_add_interested_agent_duplicate(self):
        """Test adding duplicate interested agent."""
        gossip = TechGossipItem(
            gossip_id="tg-003",
            category=TechGossipCategory.AI_DEVELOPMENTS,
            topic="Test",
            speculation="Test",
            gossiped_by=["agent-1"]
        )
        
        gossip.add_interested_agent("agent-2")
        gossip.add_interested_agent("agent-2")
        
        assert len(gossip.agents_interested) == 1
    
    def test_spark_debate(self):
        """Test sparking debates."""
        gossip = TechGossipItem(
            gossip_id="tg-004",
            category=TechGossipCategory.INDUSTRY_DRAMA,
            topic="Test",
            speculation="Test",
            gossiped_by=["agent-1"]
        )
        
        assert gossip.sparked_debates == 0
        
        gossip.spark_debate()
        gossip.spark_debate()
        
        assert gossip.sparked_debates == 2
    
    def test_water_cooler_moment(self):
        """Test water cooler moments."""
        gossip = TechGossipItem(
            gossip_id="tg-005",
            category=TechGossipCategory.STARTUP_NEWS,
            topic="Test",
            speculation="Test",
            gossiped_by=["agent-1"]
        )
        
        assert gossip.water_cooler_moments == 0
        
        gossip.water_cooler_moment()
        gossip.water_cooler_moment()
        gossip.water_cooler_moment()
        
        assert gossip.water_cooler_moments == 3


class TestTechGossipBoard:
    """Test TechGossipBoard class."""
    
    def test_board_creation(self):
        """Test creating tech gossip board."""
        board = TechGossipBoard(board_id="tgb-001")
        
        assert board.board_id == "tgb-001"
        assert len(board.gossip_items) == 0
        assert len(board.trending_topics) == 0
    
    def test_post_gossip(self):
        """Test posting gossip."""
        board = TechGossipBoard(board_id="tgb-002")
        
        gossip_id = board.post_gossip(
            category=TechGossipCategory.AI_DEVELOPMENTS,
            topic="GPT-5",
            speculation="Coming soon",
            gossiped_by=["agent-1"]
        )
        
        assert gossip_id == "gossip-1"
        assert len(board.gossip_items) == 1
        assert gossip_id in board.gossip_items
        assert "GPT-5" in board.trending_topics
    
    def test_post_multiple_gossip(self):
        """Test posting multiple gossip items."""
        board = TechGossipBoard(board_id="tgb-003")
        
        id1 = board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        id2 = board.post_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust 2.0", "Test", ["a2"])
        
        assert len(board.gossip_items) == 2
        assert id1 == "gossip-1"
        assert id2 == "gossip-2"
    
    def test_get_trending_topics(self):
        """Test getting trending topics."""
        board = TechGossipBoard(board_id="tgb-004")
        
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "Topic 1", "Test", ["a1"])
        board.post_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Topic 2", "Test", ["a2"])
        board.post_gossip(TechGossipCategory.STARTUP_NEWS, "Topic 3", "Test", ["a3"])
        
        trending = board.get_trending_topics(limit=2)
        
        assert len(trending) == 2
        assert "Topic 3" in trending  # Most recent
    
    def test_get_trending_topics_with_repeat(self):
        """Test trending topics with repeated topic."""
        board = TechGossipBoard(board_id="tgb-005")
        
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        board.post_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust", "Test", ["a2"])
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "More news", ["a3"])
        
        trending = board.get_trending_topics()
        
        # GPT-5 should be moved to front
        assert trending[0] == "GPT-5"
        assert len([t for t in trending if t == "GPT-5"]) == 1  # No duplicates
    
    def test_get_gossip_by_category(self):
        """Test getting gossip by category."""
        board = TechGossipBoard(board_id="tgb-006")
        
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "Claude", "Test", ["a2"])
        board.post_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust", "Test", ["a3"])
        
        ai_gossip = board.get_gossip_by_category(TechGossipCategory.AI_DEVELOPMENTS)
        lang_gossip = board.get_gossip_by_category(TechGossipCategory.LANGUAGE_RELEASES)
        
        assert len(ai_gossip) == 2
        assert len(lang_gossip) == 1
    
    def test_get_hot_ai_gossip(self):
        """Test getting hot AI gossip."""
        board = TechGossipBoard(board_id="tgb-007")
        
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "Claude", "Test", ["a2"])
        board.post_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust", "Test", ["a3"])
        
        hot_ai = board.get_hot_ai_gossip()
        
        assert len(hot_ai) == 2
    
    def test_generate_water_cooler_summary_empty(self):
        """Test water cooler summary with no gossip."""
        board = TechGossipBoard(board_id="tgb-008")
        
        summary = board.generate_water_cooler_summary()
        
        assert "quiet" in summary.lower()
    
    def test_generate_water_cooler_summary(self):
        """Test water cooler summary with gossip."""
        board = TechGossipBoard(board_id="tgb-009")
        
        id1 = board.post_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        board.gossip_items[id1].add_interested_agent("a2")
        board.gossip_items[id1].spark_debate()
        
        summary = board.generate_water_cooler_summary()
        
        assert "WATER COOLER" in summary
        assert "GPT-5" in summary
        assert "1 agents interested" in summary
        assert "1 debates sparked" in summary
    
    def test_trending_topics_limited_to_20(self):
        """Test that trending topics are limited to 20."""
        board = TechGossipBoard(board_id="tgb-010")
        
        # Post 25 different topics
        for i in range(25):
            board.post_gossip(
                TechGossipCategory.AI_DEVELOPMENTS,
                f"Topic {i}",
                "Test",
                ["a1"]
            )
        
        assert len(board.trending_topics) == 20


class TestLoungeConversation:
    """Test LoungeConversation class."""
    
    def test_conversation_creation(self):
        """Test creating lounge conversation."""
        conv = LoungeConversation(
            conversation_id="conv-001",
            participants=["agent-1", "agent-2"],
            themes=[ConversationTheme.TECHNICAL, ConversationTheme.BRAINSTORMING]
        )
        
        assert conv.conversation_id == "conv-001"
        assert len(conv.participants) == 2
        assert len(conv.themes) == 2
        assert conv.ended_at is None
    
    def test_spark_initiative(self):
        """Test sparking initiative from conversation."""
        conv = LoungeConversation(
            conversation_id="conv-002",
            participants=["agent-1"],
            themes=[ConversationTheme.BRAINSTORMING]
        )
        
        conv.spark_initiative("New Widget")
        conv.spark_initiative("Better Tests")
        
        assert len(conv.emergent_initiatives) == 2
        assert "New Widget" in conv.emergent_initiatives
    
    def test_link_sandbox(self):
        """Test linking sandbox to conversation."""
        conv = LoungeConversation(
            conversation_id="conv-003",
            participants=["agent-1"],
            themes=[ConversationTheme.TECHNICAL]
        )
        
        conv.link_sandbox("sandbox-1")
        conv.link_sandbox("sandbox-2")
        
        assert len(conv.sparked_sandboxes) == 2
        assert "sandbox-1" in conv.sparked_sandboxes


class TestLoungeWorkBleed:
    """Test LoungeWorkBleed class."""
    
    def test_lounge_work_bleed_creation(self):
        """Test creating lounge work bleed."""
        bleed = LoungeWorkBleed()
        
        assert len(bleed.conversations) == 0
        assert len(bleed.lounge_to_sandbox_transitions) == 0
        assert bleed.tech_gossip_board is not None
    
    def test_start_conversation(self):
        """Test starting a conversation."""
        bleed = LoungeWorkBleed()
        
        conv_id = bleed.start_conversation(
            participants=["agent-1", "agent-2"],
            themes=[ConversationTheme.TECHNICAL]
        )
        
        assert conv_id == "conv-1"
        assert len(bleed.conversations) == 1
        assert conv_id in bleed.conversations
    
    def test_transition_to_sandbox(self):
        """Test lounge to sandbox transition."""
        bleed = LoungeWorkBleed()
        
        conv_id = bleed.start_conversation(["agent-1"], [ConversationTheme.BRAINSTORMING])
        bleed.transition_to_sandbox(conv_id, "sandbox-1", "New Widget")
        
        assert len(bleed.lounge_to_sandbox_transitions) == 1
        
        transition = bleed.lounge_to_sandbox_transitions[0]
        assert transition["conversation_id"] == conv_id
        assert transition["sandbox_id"] == "sandbox-1"
        assert transition["initiative_title"] == "New Widget"
        
        # Check conversation was updated
        conv = bleed.conversations[conv_id]
        assert "New Widget" in conv.emergent_initiatives
        assert "sandbox-1" in conv.sparked_sandboxes
    
    def test_transition_to_sandbox_nonexistent_conversation(self):
        """Test transition with nonexistent conversation."""
        bleed = LoungeWorkBleed()
        
        # Should not raise error
        bleed.transition_to_sandbox("nonexistent", "sandbox-1", "Test")
        
        assert len(bleed.lounge_to_sandbox_transitions) == 0
    
    def test_block_production_transition(self):
        """Test blocking production transition."""
        bleed = LoungeWorkBleed()
        
        bleed.block_production_transition(
            "conv-1",
            "deploy_to_prod",
            "Lounge conversations cannot deploy"
        )
        
        assert len(bleed.lounge_to_production_blocks) == 1
        
        block = bleed.lounge_to_production_blocks[0]
        assert block["conversation_id"] == "conv-1"
        assert block["attempted_action"] == "deploy_to_prod"
        assert block["blocked"] is True
    
    def test_post_tech_gossip(self):
        """Test posting tech gossip."""
        bleed = LoungeWorkBleed()
        
        gossip_id = bleed.post_tech_gossip(
            TechGossipCategory.AI_DEVELOPMENTS,
            "GPT-5",
            "Coming soon",
            ["agent-1", "agent-2"]
        )
        
        assert gossip_id != ""
        assert bleed.tech_gossip_board is not None
        assert len(bleed.tech_gossip_board.gossip_items) == 1
    
    def test_get_water_cooler_buzz(self):
        """Test getting water cooler buzz."""
        bleed = LoungeWorkBleed()
        
        bleed.post_tech_gossip(
            TechGossipCategory.AI_DEVELOPMENTS,
            "GPT-5",
            "Test",
            ["agent-1"]
        )
        
        buzz = bleed.get_water_cooler_buzz()
        
        assert "WATER COOLER" in buzz
        assert "GPT-5" in buzz
    
    def test_get_hot_ai_gossip(self):
        """Test getting hot AI gossip."""
        bleed = LoungeWorkBleed()
        
        bleed.post_tech_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        bleed.post_tech_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust", "Test", ["a2"])
        
        hot_ai = bleed.get_hot_ai_gossip()
        
        assert len(hot_ai) == 1
        assert hot_ai[0].topic == "GPT-5"
    
    def test_get_trending_tech_topics(self):
        """Test getting trending tech topics."""
        bleed = LoungeWorkBleed()
        
        bleed.post_tech_gossip(TechGossipCategory.AI_DEVELOPMENTS, "GPT-5", "Test", ["a1"])
        bleed.post_tech_gossip(TechGossipCategory.LANGUAGE_RELEASES, "Rust", "Test", ["a2"])
        
        trending = bleed.get_trending_tech_topics(limit=5)
        
        assert len(trending) == 2
        assert "Rust" in trending


class TestReputationInfluence:
    """Test ReputationInfluence class."""
    
    def test_reputation_creation(self):
        """Test creating reputation influence."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        assert rep.agent_id == "agent-1"
        assert rep.golden_stars == 0
        assert rep.trust_score == 0.5
        assert rep.attention_multiplier == 1.0
        assert rep.voting_weight == 1.0
        assert rep.authority_level == 0
        assert rep.can_override_security is False
        assert rep.can_force_approval is False
    
    def test_add_contribution(self):
        """Test adding contribution."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        initial_trust = rep.trust_score
        initial_attention = rep.attention_multiplier
        initial_priority = rep.review_priority
        
        rep.add_contribution()
        
        assert rep.contributions == 1
        assert rep.trust_score > initial_trust
        assert rep.attention_multiplier > initial_attention
        assert rep.review_priority > initial_priority
    
    def test_award_golden_star(self):
        """Test awarding golden star."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        initial_trust = rep.trust_score
        
        rep.award_golden_star()
        
        assert rep.golden_stars == 1
        assert rep.trust_score > initial_trust
    
    def test_record_successful_promotion(self):
        """Test recording successful promotion."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        rep.record_successful_promotion()
        
        assert rep.successful_promotions == 1
        assert rep.trust_score > 0.5
    
    def test_record_failed_promotion(self):
        """Test recording failed promotion."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        rep.record_failed_promotion()
        
        assert rep.failed_promotions == 1
        assert rep.trust_score < 0.5
    
    def test_apply_decay(self):
        """Test applying reputation decay."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Boost reputation
        rep.trust_score = 0.9
        rep.attention_multiplier = 1.5
        rep.review_priority = 50
        
        # Set last contribution to 10 days ago
        rep.last_contribution_at = datetime.now() - timedelta(days=10)
        
        rep.apply_decay()
        
        # Should have decayed
        assert rep.trust_score < 0.9
        assert rep.attention_multiplier < 1.5
        assert rep.review_priority < 50
    
    def test_apply_decay_floor_values(self):
        """Test decay doesn't go below minimums."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Set values at minimum
        rep.trust_score = 0.0
        rep.attention_multiplier = 1.0
        rep.review_priority = 0
        
        # Set last contribution to 100 days ago
        rep.last_contribution_at = datetime.now() - timedelta(days=100)
        
        rep.apply_decay()
        
        # Should not go below minimums
        assert rep.trust_score == 0.0
        assert rep.attention_multiplier == 1.0
        assert rep.review_priority == 0
    
    def test_verify_credibility_not_power_clean(self):
        """Test verifying clean reputation."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        is_valid, violations = rep.verify_credibility_not_power()
        
        assert is_valid is True
        assert len(violations) == 0
    
    def test_verify_credibility_not_power_voting_violation(self):
        """Test detecting voting weight violation."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Manually set (shouldn't be possible normally)
        rep.voting_weight = 2.0
        
        is_valid, violations = rep.verify_credibility_not_power()
        
        assert is_valid is False
        assert any("Voting weight" in v for v in violations)
    
    def test_trust_score_max_cap(self):
        """Test trust score doesn't exceed 1.0."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Award many stars
        for _ in range(50):
            rep.award_golden_star()
        
        assert rep.trust_score <= 1.0
    
    def test_attention_multiplier_max_cap(self):
        """Test attention multiplier doesn't exceed 2.0."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Add many contributions
        for _ in range(100):
            rep.add_contribution()
        
        assert rep.attention_multiplier <= 2.0
    
    def test_review_priority_max_cap(self):
        """Test review priority doesn't exceed 100."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Award many stars
        for _ in range(50):
            rep.award_golden_star()
        
        assert rep.review_priority <= 100


class TestReputationSystem:
    """Test ReputationSystem class."""
    
    def test_reputation_system_creation(self):
        """Test creating reputation system."""
        system = ReputationSystem(system_id="rep-001")
        
        assert system.system_id == "rep-001"
        assert len(system.reputations) == 0
    
    def test_get_or_create_reputation_new(self):
        """Test getting/creating new reputation."""
        system = ReputationSystem(system_id="rep-002")
        
        rep = system.get_or_create_reputation("agent-1")
        
        assert rep.agent_id == "agent-1"
        assert len(system.reputations) == 1
    
    def test_get_or_create_reputation_existing(self):
        """Test getting existing reputation."""
        system = ReputationSystem(system_id="rep-003")
        
        rep1 = system.get_or_create_reputation("agent-1")
        rep1.golden_stars = 5
        
        rep2 = system.get_or_create_reputation("agent-1")
        
        assert rep1 is rep2
        assert rep2.golden_stars == 5
    
    def test_apply_decay_to_all(self):
        """Test applying decay to all reputations."""
        system = ReputationSystem(system_id="rep-004")
        
        rep1 = system.get_or_create_reputation("agent-1")
        rep2 = system.get_or_create_reputation("agent-2")
        
        # Boost reputations
        rep1.trust_score = 0.9
        rep2.trust_score = 0.8
        
        # Set last contributions to past
        rep1.last_contribution_at = datetime.now() - timedelta(days=10)
        rep2.last_contribution_at = datetime.now() - timedelta(days=5)
        
        system.apply_decay_to_all()
        
        # Both should have decayed
        assert rep1.trust_score < 0.9
        assert rep2.trust_score < 0.8
    
    def test_reset_all_reputation_weighting(self):
        """Test resetting all reputation weighting."""
        system = ReputationSystem(system_id="rep-005")
        
        rep1 = system.get_or_create_reputation("agent-1")
        rep2 = system.get_or_create_reputation("agent-2")
        
        # Boost reputations
        rep1.trust_score = 0.9
        rep1.attention_multiplier = 1.8
        rep1.review_priority = 80
        
        rep2.trust_score = 0.2
        rep2.attention_multiplier = 1.3
        rep2.review_priority = 20
        
        system.reset_all_reputation_weighting()
        
        # All should be reset to baseline
        assert rep1.trust_score == 0.5
        assert rep1.attention_multiplier == 1.0
        assert rep1.review_priority == 0
        
        assert rep2.trust_score == 0.5
        assert rep2.attention_multiplier == 1.0
        assert rep2.review_priority == 0


class TestEmployeeSpotlight:
    """Test EmployeeSpotlight class."""
    
    def test_spotlight_creation(self):
        """Test creating employee spotlight."""
        spotlight = EmployeeSpotlight(
            spotlight_id="es-001",
            agent_id="agent-1",
            floor_id="floor-1",
            month="2026-01"
        )
        
        assert spotlight.spotlight_id == "es-001"
        assert spotlight.agent_id == "agent-1"
        assert spotlight.grants_authority is False
        assert spotlight.binding_effect is False
        assert spotlight.permanent_elevation is False
    
    def test_add_sandbox_demo(self):
        """Test adding sandbox demos."""
        spotlight = EmployeeSpotlight(
            spotlight_id="es-002",
            agent_id="agent-1",
            floor_id="floor-1",
            month="2026-01"
        )
        
        spotlight.add_sandbox_demo("sandbox-1")
        spotlight.add_sandbox_demo("sandbox-2")
        
        assert len(spotlight.sandbox_demos) == 2
        assert "sandbox-1" in spotlight.sandbox_demos
    
    def test_verify_no_power_granted_clean(self):
        """Test verifying no power granted."""
        spotlight = EmployeeSpotlight(
            spotlight_id="es-003",
            agent_id="agent-1",
            floor_id="floor-1",
            month="2026-01"
        )
        
        is_valid, violations = spotlight.verify_no_power_granted()
        
        assert is_valid is True
        assert len(violations) == 0


class TestRollbackGuarantee:
    """Test RollbackGuarantee class."""
    
    def test_rollback_guarantee_creation(self):
        """Test creating rollback guarantee."""
        rollback = RollbackGuarantee(guarantee_id="rb-001")
        
        assert rollback.guarantee_id == "rb-001"
        assert len(rollback.rollback_history) == 0
        assert rollback.autonomy_locked is False
    
    def test_execute_full_rollback(self):
        """Test executing full rollback."""
        sandbox_manager = SandboxManager(manager_id="sm-001")
        reputation_system = ReputationSystem(system_id="rep-001")
        rollback = RollbackGuarantee(guarantee_id="rb-002")
        
        # Create some sandboxes
        sandbox_manager.create_sandbox("Widget 1", "floor-1", ProjectType.WIDGET, "Test")
        sandbox_manager.create_sandbox("Widget 2", "floor-2", ProjectType.WIDGET, "Test")
        
        # Create some reputations
        rep1 = reputation_system.get_or_create_reputation("agent-1")
        rep1.trust_score = 0.9
        
        result = rollback.execute_full_rollback(
            "human-1",
            "Testing rollback",
            sandbox_manager,
            reputation_system
        )
        
        assert result["sandboxes_collapsed"] == 2
        assert result["reputations_reset"] == 1
        assert rollback.autonomy_locked is True
        assert rollback.lock_reason == "Testing rollback"
        assert len(rollback.rollback_history) == 1
        
        # Check reputation was reset
        assert rep1.trust_score == 0.5
    
    def test_unlock_autonomy(self):
        """Test unlocking autonomy."""
        rollback = RollbackGuarantee(guarantee_id="rb-003")
        
        rollback.autonomy_locked = True
        rollback.lock_reason = "Test"
        
        rollback.unlock_autonomy("human-1")
        
        assert rollback.autonomy_locked is False
        assert rollback.lock_reason is None


class TestExpandedAutonomyModel:
    """Test ExpandedAutonomyModel class."""
    
    def test_model_creation(self):
        """Test creating expanded autonomy model."""
        model = ExpandedAutonomyModel(
            model_id="eam-001",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        assert model.model_id == "eam-001"
        assert model.version == "2.0.0"
        assert model.active is True
        assert model.production_surprises == 0
    
    def test_create_sandbox_from_lounge(self):
        """Test creating sandbox from lounge conversation."""
        model = ExpandedAutonomyModel(
            model_id="eam-002",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Start conversation
        conv_id = model.lounge_work_bleed.start_conversation(
            ["agent-1", "agent-2"],
            [ConversationTheme.BRAINSTORMING]
        )
        
        # Create sandbox from it
        sandbox_id = model.create_sandbox_from_lounge(
            conv_id,
            "New Widget",
            "floor-1",
            ProjectType.WIDGET,
            "A cool widget"
        )
        
        assert sandbox_id in model.sandbox_manager.sandboxes
        assert len(model.lounge_work_bleed.lounge_to_sandbox_transitions) == 1
    
    def test_flag_sandbox_for_promotion(self):
        """Test flagging sandbox for promotion."""
        model = ExpandedAutonomyModel(
            model_id="eam-003",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        sandbox_id = model.sandbox_manager.create_sandbox(
            "Widget",
            "floor-1",
            ProjectType.WIDGET,
            "Test"
        )
        
        model.flag_sandbox_for_promotion(sandbox_id, "agent-1", "Ready for production")
        
        sandbox = model.sandbox_manager.sandboxes[sandbox_id]
        assert sandbox.status == SandboxStatus.PROMOTION_CANDIDATE
        assert model.human_ratifications_required == 1
    
    def test_flag_nonexistent_sandbox(self):
        """Test flagging nonexistent sandbox."""
        model = ExpandedAutonomyModel(
            model_id="eam-004",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Should not raise error
        model.flag_sandbox_for_promotion("nonexistent", "agent-1", "Test")
        
        assert model.human_ratifications_required == 0
    
    def test_human_promote_sandbox(self):
        """Test human promoting sandbox."""
        model = ExpandedAutonomyModel(
            model_id="eam-005",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        sandbox_id = model.sandbox_manager.create_sandbox(
            "Widget",
            "floor-1",
            ProjectType.WIDGET,
            "Test"
        )
        
        model.human_promote_sandbox(sandbox_id, "human-1", "directive-001")
        
        sandbox = model.sandbox_manager.sandboxes[sandbox_id]
        assert sandbox.status == SandboxStatus.PROMOTED
        assert sandbox.promoted_to_directive_id == "directive-001"
        assert model.human_ratifications_given == 1
    
    def test_human_promote_nonexistent_sandbox(self):
        """Test promoting nonexistent sandbox."""
        model = ExpandedAutonomyModel(
            model_id="eam-006",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Should not raise error
        model.human_promote_sandbox("nonexistent", "human-1", "directive-001")
        
        assert model.human_ratifications_given == 0
    
    def test_verify_human_supremacy_clean(self):
        """Test verifying human supremacy with clean state."""
        model = ExpandedAutonomyModel(
            model_id="eam-007",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        is_valid, violations = model.verify_human_supremacy()
        
        assert is_valid is True
        assert len(violations) == 0
    
    def test_verify_human_supremacy_promoted_without_directive(self):
        """Test detecting promoted sandbox without directive."""
        model = ExpandedAutonomyModel(
            model_id="eam-008",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        sandbox_id = model.sandbox_manager.create_sandbox(
            "Widget",
            "floor-1",
            ProjectType.WIDGET,
            "Test"
        )
        
        # Manually set status without directive (shouldn't happen normally)
        sandbox = model.sandbox_manager.sandboxes[sandbox_id]
        sandbox.status = SandboxStatus.PROMOTED
        
        is_valid, violations = model.verify_human_supremacy()
        
        assert is_valid is False
        assert any("without directive" in v for v in violations)
    
    def test_verify_human_supremacy_production_surprises(self):
        """Test detecting production surprises."""
        model = ExpandedAutonomyModel(
            model_id="eam-009",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        model.production_surprises = 3
        
        is_valid, violations = model.verify_human_supremacy()
        
        assert is_valid is False
        assert any("surprises" in v for v in violations)
    
    def test_verify_stop_conditions(self):
        """Test verifying stop conditions."""
        model = ExpandedAutonomyModel(
            model_id="eam-010",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Create active sandbox
        model.sandbox_manager.create_sandbox("Widget", "floor-1", ProjectType.WIDGET, "Test")
        
        # Start conversation
        model.lounge_work_bleed.start_conversation(["agent-1"], [ConversationTheme.TECHNICAL])
        
        conditions = model.verify_stop_conditions()
        
        assert conditions["teams_build_without_asking"] is True
        assert conditions["culture_feels_alive"] is True
        assert conditions["no_production_surprises"] is True
        assert conditions["can_walk_away"] is True
    
    def test_verify_stop_conditions_empty(self):
        """Test stop conditions with no activity."""
        model = ExpandedAutonomyModel(
            model_id="eam-011",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        conditions = model.verify_stop_conditions()
        
        assert conditions["teams_build_without_asking"] is False
        assert conditions["culture_feels_alive"] is False
    
    def test_generate_status_report(self):
        """Test generating status report."""
        model = ExpandedAutonomyModel(
            model_id="eam-012",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Add some activity
        model.sandbox_manager.create_sandbox("Widget", "floor-1", ProjectType.WIDGET, "Test")
        model.lounge_work_bleed.start_conversation(["agent-1"], [ConversationTheme.TECHNICAL])
        
        report = model.generate_status_report()
        
        assert "EXPANDED AUTONOMY MODEL STATUS" in report
        assert "SANDBOX SYSTEM" in report
        assert "LOUNGE → WORK BLEED" in report
        assert "REPUTATION INFLUENCE" in report
        assert "STOP CONDITIONS" in report
        assert model.version in report
    
    def test_generate_status_report_with_reputation(self):
        """Test status report includes reputation stats."""
        model = ExpandedAutonomyModel(
            model_id="eam-013",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Add reputations
        rep1 = model.reputation_system.get_or_create_reputation("agent-1")
        rep1.award_golden_star()
        
        rep2 = model.reputation_system.get_or_create_reputation("agent-2")
        rep2.award_golden_star()
        rep2.award_golden_star()
        
        report = model.generate_status_report()
        
        assert "Agents Tracked: 2" in report
        assert "Total Golden Stars: 3" in report


class TestCreateExpandedAutonomyModel:
    """Test create_expanded_autonomy_model function."""
    
    def test_create_expanded_autonomy_model(self):
        """Test creating expanded autonomy model."""
        model = create_expanded_autonomy_model()
        
        assert model.model_id == "eam-001"
        assert model.version == "2.0.0"
        assert model.sandbox_manager is not None
        assert model.lounge_work_bleed is not None
        assert model.reputation_system is not None
        assert model.rollback_guarantee is not None


class TestGetExpandedAutonomyModel:
    """Test get_expanded_autonomy_model function."""
    
    def test_get_expanded_autonomy_model_creates_instance(self):
        """Test getting expanded autonomy model creates instance."""
        # Reset global
        import src.core.expanded_autonomy as ea
        ea._expanded_autonomy_model = None
        
        model = get_expanded_autonomy_model()
        
        assert model is not None
        assert model.model_id == "eam-001"
    
    def test_get_expanded_autonomy_model_returns_same_instance(self):
        """Test getting model returns same instance."""
        # Reset global
        import src.core.expanded_autonomy as ea
        ea._expanded_autonomy_model = None
        
        model1 = get_expanded_autonomy_model()
        model2 = get_expanded_autonomy_model()
        
        assert model1 is model2


class TestEdgeCases:
    """Test edge cases to achieve 100% coverage."""
    
    def test_lounge_work_bleed_no_gossip_board(self):
        """Test LoungeWorkBleed methods when tech_gossip_board is None."""
        bleed = LoungeWorkBleed()
        bleed.tech_gossip_board = None
        
        # Test post_tech_gossip returns empty string
        result = bleed.post_tech_gossip(
            TechGossipCategory.AI_DEVELOPMENTS,
            "Test",
            "Test",
            ["agent-1"]
        )
        assert result == ""
        
        # Test get_water_cooler_buzz returns fallback message
        buzz = bleed.get_water_cooler_buzz()
        assert buzz == "No gossip board available."
        
        # Test get_hot_ai_gossip returns empty list
        gossip = bleed.get_hot_ai_gossip()
        assert gossip == []
        
        # Test get_trending_tech_topics returns empty list
        trending = bleed.get_trending_tech_topics()
        assert trending == []
    
    def test_reputation_influence_all_violations(self):
        """Test all reputation violation paths."""
        rep = ReputationInfluence(agent_id="agent-1")
        
        # Manually trigger all violations (shouldn't normally happen)
        rep.authority_level = 5
        rep.can_override_security = True
        rep.can_force_approval = True
        
        is_valid, violations = rep.verify_credibility_not_power()
        
        assert is_valid is False
        assert len(violations) == 3
        assert any("Authority level" in v for v in violations)
        assert any("override security" in v for v in violations)
        assert any("force approval" in v for v in violations)
    
    def test_employee_spotlight_all_violations(self):
        """Test all employee spotlight violation paths."""
        spotlight = EmployeeSpotlight(
            spotlight_id="es-001",
            agent_id="agent-1",
            floor_id="floor-1",
            month="2026-01"
        )
        
        # Manually trigger all violations (shouldn't normally happen)
        spotlight.grants_authority = True
        spotlight.binding_effect = True
        spotlight.permanent_elevation = True
        
        is_valid, violations = spotlight.verify_no_power_granted()
        
        assert is_valid is False
        assert len(violations) == 3
        assert any("authority" in v for v in violations)
        assert any("binding" in v for v in violations)
        assert any("elevation" in v for v in violations)
    
    def test_expanded_autonomy_reputation_violations(self):
        """Test expanded autonomy model with reputation violations."""
        model = ExpandedAutonomyModel(
            model_id="eam-001",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Create reputation with violations
        rep = model.reputation_system.get_or_create_reputation("agent-1")
        rep.authority_level = 5  # Violation
        
        is_valid, violations = model.verify_human_supremacy()
        
        assert is_valid is False
        assert len(violations) > 0
    
    def test_expanded_autonomy_locked_autonomy(self):
        """Test expanded autonomy model with locked autonomy."""
        model = ExpandedAutonomyModel(
            model_id="eam-001",
            version="2.0.0",
            sandbox_manager=SandboxManager(manager_id="sm-001"),
            lounge_work_bleed=LoungeWorkBleed(),
            reputation_system=ReputationSystem(system_id="rep-001"),
            rollback_guarantee=RollbackGuarantee(guarantee_id="rb-001")
        )
        
        # Lock autonomy
        model.rollback_guarantee.autonomy_locked = True
        
        # Should still pass verification (it's OK if locked)
        is_valid, violations = model.verify_human_supremacy()
        
        # Should be valid (or only have other violations, not about being locked)
        assert all("locked" not in v.lower() for v in violations)
