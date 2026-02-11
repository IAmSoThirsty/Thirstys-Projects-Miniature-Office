"""Unit tests for the off-duty city system."""
import pytest
from datetime import datetime
from src.core.off_duty_city import (
    CityZone, AgentOffDutyState, PixelColor, PixelAccessory, ReputationVisual,
    PixelRepresentation, AgentEmbodiment, ResidentialDistrict, WorkshopStudio,
    CityLounge, CityArchives, CityPlaza, TransitGate, VisualDecay, CityFirewall,
    CityZoneManager, OffDutyCity, get_off_duty_city
)


# Test constants
FLOAT_COMPARISON_TOLERANCE = 0.01


# ============================================================================
# TEST ENUMERATIONS
# ============================================================================

class TestCityZone:
    """Test CityZone enum."""
    
    def test_city_zone_values(self):
        """Test city zone enum values."""
        assert CityZone.RESIDENTIAL.value == "residential_districts"
        assert CityZone.WORKSHOP.value == "workshops_studios"
        assert CityZone.LOUNGE.value == "city_lounge"
        assert CityZone.ARCHIVES.value == "the_archives"
        assert CityZone.PLAZA.value == "the_plaza"
        assert CityZone.TRANSIT_GATE.value == "transit_gates"


class TestAgentOffDutyState:
    """Test AgentOffDutyState enum."""
    
    def test_agent_off_duty_state_values(self):
        """Test agent off-duty state enum values."""
        assert AgentOffDutyState.RESTING.value == "resting"
        assert AgentOffDutyState.SOCIALIZING.value == "socializing"
        assert AgentOffDutyState.TINKERING.value == "tinkering"
        assert AgentOffDutyState.LEARNING.value == "learning"
        assert AgentOffDutyState.EXPLORING.value == "exploring"
        assert AgentOffDutyState.REFLECTING.value == "reflecting"
        assert AgentOffDutyState.PRACTICING.value == "practicing"
        assert AgentOffDutyState.ATTENDING_CEREMONY.value == "attending_ceremony"


class TestPixelColor:
    """Test PixelColor enum."""
    
    def test_pixel_color_values(self):
        """Test pixel color enum values."""
        assert PixelColor.PYTHON_ORANGE.value == "#FF9F00"
        assert PixelColor.RUST_RED.value == "#CE422B"
        assert PixelColor.JAVASCRIPT_YELLOW.value == "#F7DF1E"
        assert PixelColor.GO_CYAN.value == "#00ADD8"
        assert PixelColor.C_BLUE.value == "#A8B9CC"
        assert PixelColor.SQL_GREEN.value == "#00758F"
        assert PixelColor.SHELL_GRAY.value == "#4EAA25"
        assert PixelColor.GENERIC_WHITE.value == "#FFFFFF"


class TestPixelAccessory:
    """Test PixelAccessory enum."""
    
    def test_pixel_accessory_values(self):
        """Test pixel accessory enum values."""
        assert PixelAccessory.HARD_HAT.value == "hard_hat"
        assert PixelAccessory.MAGNIFYING_GLASS.value == "magnifying_glass"
        assert PixelAccessory.TEST_TUBE.value == "test_tube"
        assert PixelAccessory.SAFETY_GOGGLES.value == "safety_goggles"
        assert PixelAccessory.BLUEPRINT.value == "blueprint"
        assert PixelAccessory.CLIPBOARD.value == "clipboard"
        assert PixelAccessory.WRENCH.value == "wrench"


class TestReputationVisual:
    """Test ReputationVisual enum."""
    
    def test_reputation_visual_values(self):
        """Test reputation visual enum values."""
        assert ReputationVisual.GOLDEN_STAR.value == "golden_star"
        assert ReputationVisual.SILVER_STAR.value == "silver_star"
        assert ReputationVisual.BRONZE_STAR.value == "bronze_star"
        assert ReputationVisual.BLUE_RIBBON.value == "blue_ribbon"
        assert ReputationVisual.RED_EMBLEM.value == "red_emblem"


# ============================================================================
# TEST PIXEL REPRESENTATION
# ============================================================================

class TestPixelRepresentation:
    """Test PixelRepresentation class."""
    
    def test_pixel_creation(self):
        """Test basic pixel creation."""
        pixel = PixelRepresentation(
            representation_id="pixel-001",
            color=PixelColor.PYTHON_ORANGE,
            accessory=PixelAccessory.HARD_HAT
        )
        
        assert pixel.representation_id == "pixel-001"
        assert pixel.color == PixelColor.PYTHON_ORANGE
        assert pixel.accessory == PixelAccessory.HARD_HAT
        assert len(pixel.reputation_markers) == 0
    
    def test_to_visual_description(self):
        """Test visual description generation."""
        pixel = PixelRepresentation(
            representation_id="pixel-002",
            color=PixelColor.RUST_RED,
            accessory=PixelAccessory.WRENCH
        )
        
        description = pixel.to_visual_description()
        assert "rust_red" in description
        assert "wrench" in description
    
    def test_to_visual_description_with_markers(self):
        """Test visual description with reputation markers."""
        pixel = PixelRepresentation(
            representation_id="pixel-003",
            color=PixelColor.GO_CYAN,
            accessory=PixelAccessory.BLUEPRINT,
            reputation_markers=[ReputationVisual.GOLDEN_STAR]
        )
        
        description = pixel.to_visual_description()
        assert "go_cyan" in description
        assert "blueprint" in description
        assert "golden_star" in description
    
    def test_add_reputation_marker(self):
        """Test adding reputation marker."""
        pixel = PixelRepresentation(
            representation_id="pixel-004",
            color=PixelColor.PYTHON_ORANGE,
            accessory=PixelAccessory.HARD_HAT
        )
        
        pixel.add_reputation_marker(ReputationVisual.SILVER_STAR)
        assert ReputationVisual.SILVER_STAR in pixel.reputation_markers
        assert len(pixel.reputation_markers) == 1
    
    def test_add_duplicate_reputation_marker(self):
        """Test that duplicate markers are not added."""
        pixel = PixelRepresentation(
            representation_id="pixel-005",
            color=PixelColor.C_BLUE,
            accessory=PixelAccessory.TEST_TUBE
        )
        
        pixel.add_reputation_marker(ReputationVisual.BRONZE_STAR)
        pixel.add_reputation_marker(ReputationVisual.BRONZE_STAR)
        assert len(pixel.reputation_markers) == 1
    
    def test_remove_reputation_marker(self):
        """Test removing reputation marker."""
        pixel = PixelRepresentation(
            representation_id="pixel-006",
            color=PixelColor.SQL_GREEN,
            accessory=PixelAccessory.MAGNIFYING_GLASS,
            reputation_markers=[ReputationVisual.BLUE_RIBBON]
        )
        
        pixel.remove_reputation_marker(ReputationVisual.BLUE_RIBBON)
        assert ReputationVisual.BLUE_RIBBON not in pixel.reputation_markers
        assert len(pixel.reputation_markers) == 0
    
    def test_remove_nonexistent_marker(self):
        """Test removing marker that doesn't exist."""
        pixel = PixelRepresentation(
            representation_id="pixel-007",
            color=PixelColor.SHELL_GRAY,
            accessory=PixelAccessory.CLIPBOARD
        )
        
        # Should not raise error
        pixel.remove_reputation_marker(ReputationVisual.RED_EMBLEM)
        assert len(pixel.reputation_markers) == 0


# ============================================================================
# TEST AGENT EMBODIMENT
# ============================================================================

class TestAgentEmbodiment:
    """Test AgentEmbodiment class."""
    
    @pytest.fixture
    def sample_pixel(self):
        """Create a sample pixel representation."""
        return PixelRepresentation(
            representation_id="pixel-test",
            color=PixelColor.PYTHON_ORANGE,
            accessory=PixelAccessory.HARD_HAT
        )
    
    @pytest.fixture
    def sample_embodiment(self, sample_pixel):
        """Create a sample agent embodiment."""
        return AgentEmbodiment(
            embodiment_id="emb-001",
            agent_id="agent-001",
            agent_name="Test Agent",
            floor_origin="floor-python",
            role="builder",
            current_state=AgentOffDutyState.RESTING,
            current_zone=CityZone.RESIDENTIAL,
            pixel_representation=sample_pixel
        )
    
    def test_embodiment_creation(self, sample_embodiment):
        """Test basic embodiment creation."""
        assert sample_embodiment.embodiment_id == "emb-001"
        assert sample_embodiment.agent_id == "agent-001"
        assert sample_embodiment.agent_name == "Test Agent"
        assert sample_embodiment.floor_origin == "floor-python"
        assert sample_embodiment.role == "builder"
        assert sample_embodiment.current_state == AgentOffDutyState.RESTING
        assert sample_embodiment.current_zone == CityZone.RESIDENTIAL
        assert sample_embodiment.morale == 1.0
        assert sample_embodiment.burnout_level == 0.0
        assert len(sample_embodiment.recent_activities) == 0
        assert len(sample_embodiment.social_connections) == 0
    
    def test_update_state(self, sample_embodiment):
        """Test updating agent state."""
        sample_embodiment.update_state(AgentOffDutyState.SOCIALIZING, "Chatting in lounge")
        
        assert sample_embodiment.current_state == AgentOffDutyState.SOCIALIZING
        assert len(sample_embodiment.recent_activities) == 1
        assert sample_embodiment.recent_activities[0]["state"] == "socializing"
        assert sample_embodiment.recent_activities[0]["activity"] == "Chatting in lounge"
    
    def test_update_state_activity_limit(self, sample_embodiment):
        """Test that recent activities are limited to 50."""
        # Add 60 activities
        for i in range(60):
            sample_embodiment.update_state(AgentOffDutyState.TINKERING, f"Activity {i}")
        
        # Should only keep 50
        assert len(sample_embodiment.recent_activities) == 50
        # Should keep the most recent ones
        assert sample_embodiment.recent_activities[-1]["activity"] == "Activity 59"
    
    def test_adjust_morale_increase(self, sample_embodiment):
        """Test increasing morale."""
        sample_embodiment.morale = 0.5
        sample_embodiment.adjust_morale(0.3)
        assert sample_embodiment.morale == 0.8
    
    def test_adjust_morale_decrease(self, sample_embodiment):
        """Test decreasing morale."""
        sample_embodiment.morale = 0.7
        sample_embodiment.adjust_morale(-0.2)
        assert abs(sample_embodiment.morale - 0.5) < FLOAT_COMPARISON_TOLERANCE
    
    def test_adjust_morale_clamp_upper(self, sample_embodiment):
        """Test morale clamped at upper bound."""
        sample_embodiment.morale = 0.9
        sample_embodiment.adjust_morale(0.5)
        assert sample_embodiment.morale == 1.0
    
    def test_adjust_morale_clamp_lower(self, sample_embodiment):
        """Test morale clamped at lower bound."""
        sample_embodiment.morale = 0.2
        sample_embodiment.adjust_morale(-0.5)
        assert sample_embodiment.morale == 0.0
    
    def test_adjust_burnout_increase(self, sample_embodiment):
        """Test increasing burnout."""
        sample_embodiment.burnout_level = 0.3
        sample_embodiment.adjust_burnout(0.2)
        assert sample_embodiment.burnout_level == 0.5
    
    def test_adjust_burnout_decrease(self, sample_embodiment):
        """Test decreasing burnout."""
        sample_embodiment.burnout_level = 0.6
        sample_embodiment.adjust_burnout(-0.3)
        assert sample_embodiment.burnout_level == 0.3
    
    def test_adjust_burnout_clamp_upper(self, sample_embodiment):
        """Test burnout clamped at upper bound."""
        sample_embodiment.burnout_level = 0.8
        sample_embodiment.adjust_burnout(0.5)
        assert sample_embodiment.burnout_level == 1.0
    
    def test_adjust_burnout_clamp_lower(self, sample_embodiment):
        """Test burnout clamped at lower bound."""
        sample_embodiment.burnout_level = 0.1
        sample_embodiment.adjust_burnout(-0.5)
        assert sample_embodiment.burnout_level == 0.0


# ============================================================================
# TEST RESIDENTIAL DISTRICT
# ============================================================================

class TestResidentialDistrict:
    """Test ResidentialDistrict class."""
    
    @pytest.fixture
    def sample_district(self):
        """Create a sample residential district."""
        return ResidentialDistrict(district_id="district-001")
    
    @pytest.fixture
    def sample_embodiment(self):
        """Create a sample embodiment for testing."""
        pixel = PixelRepresentation(
            representation_id="pixel-test",
            color=PixelColor.PYTHON_ORANGE,
            accessory=PixelAccessory.HARD_HAT
        )
        return AgentEmbodiment(
            embodiment_id="emb-001",
            agent_id="agent-001",
            agent_name="Test Agent",
            floor_origin="floor-python",
            role="builder",
            current_state=AgentOffDutyState.RESTING,
            current_zone=CityZone.RESIDENTIAL,
            pixel_representation=pixel
        )
    
    def test_district_creation(self, sample_district):
        """Test basic district creation."""
        assert sample_district.district_id == "district-001"
        assert len(sample_district.residents) == 0
    
    def test_add_resident(self, sample_district, sample_embodiment):
        """Test adding resident to district."""
        sample_district.add_resident(sample_embodiment)
        
        assert len(sample_district.residents) == 1
        assert "emb-001" in sample_district.residents
        assert sample_district.residents["emb-001"] == sample_embodiment
    
    def test_remove_resident(self, sample_district, sample_embodiment):
        """Test removing resident from district."""
        sample_district.add_resident(sample_embodiment)
        sample_district.remove_resident("emb-001")
        
        assert len(sample_district.residents) == 0
        assert "emb-001" not in sample_district.residents
    
    def test_remove_nonexistent_resident(self, sample_district):
        """Test removing nonexistent resident."""
        # Should not raise error
        sample_district.remove_resident("emb-999")
        assert len(sample_district.residents) == 0
    
    def test_get_district_status_empty(self, sample_district):
        """Test district status with no residents."""
        status = sample_district.get_district_status()
        
        assert status["district_id"] == "district-001"
        assert status["total_residents"] == 0
        assert status["average_morale"] == 0.0
        assert status["average_burnout"] == 0.0
    
    def test_get_district_status_with_residents(self, sample_district):
        """Test district status with residents."""
        # Add multiple residents with different stats
        for i in range(3):
            pixel = PixelRepresentation(
                representation_id=f"pixel-{i}",
                color=PixelColor.PYTHON_ORANGE,
                accessory=PixelAccessory.HARD_HAT
            )
            embodiment = AgentEmbodiment(
                embodiment_id=f"emb-{i}",
                agent_id=f"agent-{i}",
                agent_name=f"Agent {i}",
                floor_origin="floor-python",
                role="builder",
                current_state=AgentOffDutyState.RESTING,
                current_zone=CityZone.RESIDENTIAL,
                pixel_representation=pixel,
                morale=0.5 + i * 0.2,
                burnout_level=0.1 + i * 0.1
            )
            sample_district.add_resident(embodiment)
        
        status = sample_district.get_district_status()
        
        assert status["total_residents"] == 3
        # Average morale: (0.5 + 0.7 + 0.9) / 3 = 0.7
        assert abs(status["average_morale"] - 0.7) < FLOAT_COMPARISON_TOLERANCE
        # Average burnout: (0.1 + 0.2 + 0.3) / 3 = 0.2
        assert abs(status["average_burnout"] - 0.2) < FLOAT_COMPARISON_TOLERANCE


# ============================================================================
# TEST WORKSHOP STUDIO
# ============================================================================

class TestWorkshopStudio:
    """Test WorkshopStudio class."""
    
    @pytest.fixture
    def sample_workshop(self):
        """Create a sample workshop studio."""
        return WorkshopStudio(workshop_id="workshop-001")
    
    def test_workshop_creation(self, sample_workshop):
        """Test basic workshop creation."""
        assert sample_workshop.workshop_id == "workshop-001"
        assert len(sample_workshop.active_projects) == 0
        assert len(sample_workshop.visitors) == 0
    
    def test_start_project(self, sample_workshop):
        """Test starting a sandbox project."""
        sample_workshop.start_project(
            embodiment_id="emb-001",
            project_type="widget",
            description="Test widget project"
        )
        
        assert len(sample_workshop.active_projects) == 1
        project = sample_workshop.active_projects[0]
        assert project["embodiment_id"] == "emb-001"
        assert project["project_type"] == "widget"
        assert project["description"] == "Test widget project"
        assert project["status"] == "active"
        assert project["sandboxed"] is True
        assert project["binding"] is False
    
    def test_flag_for_promotion(self, sample_workshop):
        """Test flagging project for promotion."""
        sample_workshop.start_project("emb-001", "utility", "Test utility")
        sample_workshop.flag_for_promotion(0)
        
        assert sample_workshop.active_projects[0]["promotion_candidate"] is True
    
    def test_flag_for_promotion_invalid_index(self, sample_workshop):
        """Test flagging invalid project index."""
        # Should not raise error
        sample_workshop.flag_for_promotion(0)
        sample_workshop.flag_for_promotion(-1)
        sample_workshop.flag_for_promotion(999)


# ============================================================================
# TEST CITY LOUNGE
# ============================================================================

class TestCityLounge:
    """Test CityLounge class."""
    
    @pytest.fixture
    def sample_lounge(self):
        """Create a sample city lounge."""
        return CityLounge(lounge_id="lounge-001")
    
    def test_lounge_creation(self, sample_lounge):
        """Test basic lounge creation."""
        assert sample_lounge.lounge_id == "lounge-001"
        assert len(sample_lounge.active_conversations) == 0
        assert len(sample_lounge.cultural_artifacts) == 0
        assert len(sample_lounge.participants) == 0
    
    def test_start_conversation(self, sample_lounge):
        """Test starting a conversation."""
        conv_id = sample_lounge.start_conversation(
            participants=["emb-001", "emb-002"],
            topics=["testing", "automation"]
        )
        
        assert len(sample_lounge.active_conversations) == 1
        conv = sample_lounge.active_conversations[0]
        assert conv["conversation_id"] == conv_id
        assert conv["participants"] == ["emb-001", "emb-002"]
        assert conv["topics"] == ["testing", "automation"]
        assert conv["actionable"] is False
    
    def test_create_cultural_artifact(self, sample_lounge):
        """Test creating cultural artifact."""
        sample_lounge.create_cultural_artifact(
            artifact_type="story",
            title="The Great Refactoring",
            content="Once upon a time..."
        )
        
        assert len(sample_lounge.cultural_artifacts) == 1
        artifact = sample_lounge.cultural_artifacts[0]
        assert artifact["type"] == "story"
        assert artifact["title"] == "The Great Refactoring"
        assert artifact["content"] == "Once upon a time..."
        assert artifact["operational_impact"] is None


# ============================================================================
# TEST CITY ARCHIVES
# ============================================================================

class TestCityArchives:
    """Test CityArchives class."""
    
    @pytest.fixture
    def sample_archives(self):
        """Create a sample city archives."""
        return CityArchives(archives_id="archives-001")
    
    def test_archives_creation(self, sample_archives):
        """Test basic archives creation."""
        assert sample_archives.archives_id == "archives-001"
        assert len(sample_archives.records) == 0
        assert len(sample_archives.visitors) == 0
    
    def test_add_record(self, sample_archives):
        """Test adding historical record."""
        sample_archives.add_record(
            record_type="project",
            title="Legacy System",
            content="Historical project data",
            metadata={"year": 2023, "team": "python"}
        )
        
        assert len(sample_archives.records) == 1
        record = sample_archives.records[0]
        assert record["type"] == "project"
        assert record["title"] == "Legacy System"
        assert record["content"] == "Historical project data"
        assert record["metadata"]["year"] == 2023
    
    def test_visit(self, sample_archives):
        """Test agent visiting archives."""
        sample_archives.visit("emb-001", "Research old patterns")
        
        assert len(sample_archives.visitors) == 1
        visit = sample_archives.visitors[0]
        assert visit["embodiment_id"] == "emb-001"
        assert visit["purpose"] == "Research old patterns"


# ============================================================================
# TEST CITY PLAZA
# ============================================================================

class TestCityPlaza:
    """Test CityPlaza class."""
    
    @pytest.fixture
    def sample_plaza(self):
        """Create a sample city plaza."""
        return CityPlaza(plaza_id="plaza-001")
    
    def test_plaza_creation(self, sample_plaza):
        """Test basic plaza creation."""
        assert sample_plaza.plaza_id == "plaza-001"
        assert len(sample_plaza.ceremonies) == 0
        assert len(sample_plaza.attendees) == 0
    
    def test_host_ceremony(self, sample_plaza):
        """Test hosting a ceremony."""
        sample_plaza.host_ceremony(
            ceremony_type="employee_of_month",
            details="Recognizing outstanding work",
            honorees=["agent-001", "agent-002"]
        )
        
        assert len(sample_plaza.ceremonies) == 1
        ceremony = sample_plaza.ceremonies[0]
        assert ceremony["type"] == "employee_of_month"
        assert ceremony["details"] == "Recognizing outstanding work"
        assert ceremony["honorees"] == ["agent-001", "agent-002"]
        assert ceremony["purely_ceremonial"] is True
        assert ceremony["operational_effect"] is None


# ============================================================================
# TEST TRANSIT GATE
# ============================================================================

class TestTransitGate:
    """Test TransitGate class."""
    
    @pytest.fixture
    def sample_gate(self):
        """Create a sample transit gate."""
        return TransitGate(gate_id="gate-001", gate_type="city_to_office")
    
    def test_gate_creation(self, sample_gate):
        """Test basic gate creation."""
        assert sample_gate.gate_id == "gate-001"
        assert sample_gate.gate_type == "city_to_office"
        assert len(sample_gate.crossing_history) == 0
    
    def test_cross_gate(self, sample_gate):
        """Test crossing gate."""
        result = sample_gate.cross_gate(
            agent_id="agent-001",
            from_zone="residential",
            to_zone="office",
            crossing_by="directive-001"
        )
        
        assert result is True
        assert len(sample_gate.crossing_history) == 1
        crossing = sample_gate.crossing_history[0]
        assert crossing["agent_id"] == "agent-001"
        assert crossing["from_zone"] == "residential"
        assert crossing["to_zone"] == "office"
        assert crossing["crossing_by"] == "directive-001"
        assert crossing["state_changed"] is True
        assert crossing["permissions_updated"] is True


# ============================================================================
# TEST VISUAL DECAY
# ============================================================================

class TestVisualDecay:
    """Test VisualDecay class."""
    
    @pytest.fixture
    def sample_decay(self):
        """Create a sample visual decay system."""
        return VisualDecay(decay_id="decay-001")
    
    def test_decay_creation(self, sample_decay):
        """Test basic decay creation."""
        assert sample_decay.decay_id == "decay-001"
        assert len(sample_decay.decay_records) == 0
    
    def test_decay_reputation_visual_no_age(self, sample_decay):
        """Test decay with no age."""
        factor = sample_decay.decay_reputation_visual(
            agent_id="agent-001",
            visual=ReputationVisual.GOLDEN_STAR,
            awarded_tick=100,
            current_tick=100,
            half_life_ticks=1000
        )
        
        assert factor == 1.0
        assert len(sample_decay.decay_records) == 1
    
    def test_decay_reputation_visual_half_life(self, sample_decay):
        """Test decay at half-life point."""
        factor = sample_decay.decay_reputation_visual(
            agent_id="agent-001",
            visual=ReputationVisual.SILVER_STAR,
            awarded_tick=100,
            current_tick=1100,
            half_life_ticks=1000
        )
        
        assert abs(factor - 0.5) < 0.01
    
    def test_decay_reputation_visual_old(self, sample_decay):
        """Test decay for old visual."""
        factor = sample_decay.decay_reputation_visual(
            agent_id="agent-001",
            visual=ReputationVisual.BRONZE_STAR,
            awarded_tick=100,
            current_tick=5100,
            half_life_ticks=1000
        )
        
        # After 5000 ticks (5 half-lives), factor should be 0.5^5 = 0.03125
        assert factor < 0.05
    
    def test_should_archive_above_threshold(self, sample_decay):
        """Test archival check above threshold."""
        should_archive = sample_decay.should_archive(decay_factor=0.2, threshold=0.1)
        assert should_archive is False
    
    def test_should_archive_below_threshold(self, sample_decay):
        """Test archival check below threshold."""
        should_archive = sample_decay.should_archive(decay_factor=0.05, threshold=0.1)
        assert should_archive is True
    
    def test_should_archive_at_threshold(self, sample_decay):
        """Test archival check at threshold."""
        should_archive = sample_decay.should_archive(decay_factor=0.1, threshold=0.1)
        assert should_archive is False


# ============================================================================
# TEST CITY FIREWALL
# ============================================================================

class TestCityFirewall:
    """Test CityFirewall class."""
    
    @pytest.fixture
    def sample_firewall(self):
        """Create a sample city firewall."""
        return CityFirewall(firewall_id="fw-001")
    
    def test_firewall_creation(self, sample_firewall):
        """Test basic firewall creation."""
        assert sample_firewall.firewall_id == "fw-001"
        assert len(sample_firewall.violations) == 0
    
    def test_block_decision_crossing(self, sample_firewall):
        """Test blocking decision crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="decision",
            source_agent="agent-001",
            attempted_action="Make production decision"
        )
        
        assert blocked is True
        assert "Blocked" in message
        assert "decision" in message
        assert len(sample_firewall.violations) == 1
    
    def test_block_agreement_crossing(self, sample_firewall):
        """Test blocking agreement crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="agreement",
            source_agent="agent-002",
            attempted_action="Form production agreement"
        )
        
        assert blocked is True
        assert "agreement" in message
    
    def test_block_consensus_crossing(self, sample_firewall):
        """Test blocking consensus crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="consensus",
            source_agent="agent-003",
            attempted_action="Build consensus for production"
        )
        
        assert blocked is True
        assert "consensus" in message
    
    def test_block_momentum_crossing(self, sample_firewall):
        """Test blocking momentum crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="momentum",
            source_agent="agent-004",
            attempted_action="Create production momentum"
        )
        
        assert blocked is True
        assert "momentum" in message
    
    def test_block_social_pressure_crossing(self, sample_firewall):
        """Test blocking social pressure crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="social_pressure",
            source_agent="agent-005",
            attempted_action="Apply social pressure"
        )
        
        assert blocked is True
        assert "social_pressure" in message
    
    def test_allow_legitimate_crossing(self, sample_firewall):
        """Test allowing legitimate crossing."""
        blocked, message = sample_firewall.block_city_to_production_crossing(
            crossing_type="user_directive",
            source_agent="agent-006",
            attempted_action="Execute user directive"
        )
        
        assert blocked is False
        assert message == "Allowed"
        assert len(sample_firewall.violations) == 0
    
    def test_record_firewall_violation(self, sample_firewall):
        """Test recording firewall violation."""
        sample_firewall.record_firewall_violation(
            agent_id="agent-007",
            crossing_type="decision",
            description="Attempted unauthorized decision"
        )
        
        assert len(sample_firewall.violations) == 1
        violation = sample_firewall.violations[0]
        assert violation["agent_id"] == "agent-007"
        assert violation["crossing_type"] == "decision"
        assert violation["description"] == "Attempted unauthorized decision"
        assert violation["blocked"] is True
    
    def test_get_recent_violations(self, sample_firewall):
        """Test getting recent violations."""
        # Add multiple violations
        for i in range(15):
            sample_firewall.record_firewall_violation(
                agent_id=f"agent-{i}",
                crossing_type="decision",
                description=f"Violation {i}"
            )
        
        recent = sample_firewall.get_recent_violations(limit=5)
        assert len(recent) == 5
        # Should get the most recent ones
        assert recent[-1]["description"] == "Violation 14"
    
    def test_verify_production_isolation(self, sample_firewall):
        """Test verifying production isolation."""
        result = sample_firewall.verify_production_isolation()
        assert result is True


# ============================================================================
# TEST CITY ZONE MANAGER
# ============================================================================

class TestCityZoneManager:
    """Test CityZoneManager class."""
    
    @pytest.fixture
    def sample_manager(self):
        """Create a sample zone manager."""
        return CityZoneManager(manager_id="zm-001")
    
    def test_manager_creation(self, sample_manager):
        """Test basic manager creation."""
        assert sample_manager.manager_id == "zm-001"
        assert len(sample_manager.residential_districts) == 0
        assert len(sample_manager.workshops) == 0
        assert sample_manager.lounge is None
        assert sample_manager.archives is None
        assert sample_manager.plaza is None
        assert len(sample_manager.transit_gates) == 0
    
    def test_initialize_zones(self, sample_manager):
        """Test initializing zones."""
        sample_manager.initialize_zones()
        
        assert sample_manager.lounge is not None
        assert sample_manager.lounge.lounge_id == "city-lounge-001"
        
        assert sample_manager.archives is not None
        assert sample_manager.archives.archives_id == "city-archives-001"
        
        assert sample_manager.plaza is not None
        assert sample_manager.plaza.plaza_id == "city-plaza-001"
        
        assert len(sample_manager.workshops) == 1
        assert sample_manager.workshops[0].workshop_id == "workshop-001"
        
        assert len(sample_manager.transit_gates) == 2
        assert sample_manager.transit_gates[0].gate_id == "gate-city-office"
        assert sample_manager.transit_gates[1].gate_id == "gate-city-sandbox"


# ============================================================================
# TEST OFF-DUTY CITY
# ============================================================================

class TestOffDutyCity:
    """Test OffDutyCity class."""
    
    @pytest.fixture
    def sample_city(self):
        """Create a sample off-duty city."""
        zone_manager = CityZoneManager(manager_id="zm-test")
        zone_manager.initialize_zones()
        
        city_firewall = CityFirewall(firewall_id="fw-test")
        visual_decay = VisualDecay(decay_id="decay-test")
        
        return OffDutyCity(
            city_id="city-test",
            zone_manager=zone_manager,
            city_firewall=city_firewall,
            visual_decay=visual_decay
        )
    
    def test_city_creation(self, sample_city):
        """Test basic city creation."""
        assert sample_city.city_id == "city-test"
        assert sample_city.zone_manager is not None
        assert sample_city.city_firewall is not None
        assert sample_city.visual_decay is not None
        assert len(sample_city.population) == 0
    
    def test_add_agent_python_builder(self, sample_city):
        """Test adding Python builder agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-001",
            agent_name="Python Builder",
            floor_origin="floor-python",
            role="builder",
            reputation_level=0
        )
        
        assert embodiment_id == "embodiment-agent-001"
        assert len(sample_city.population) == 1
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.agent_name == "Python Builder"
        assert embodiment.pixel_representation.color == PixelColor.PYTHON_ORANGE
        assert embodiment.pixel_representation.accessory == PixelAccessory.HARD_HAT
        assert len(embodiment.pixel_representation.reputation_markers) == 0
    
    def test_add_agent_rust_reviewer(self, sample_city):
        """Test adding Rust reviewer agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-002",
            agent_name="Rust Reviewer",
            floor_origin="floor-rust",
            role="reviewer",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.RUST_RED
        assert embodiment.pixel_representation.accessory == PixelAccessory.MAGNIFYING_GLASS
    
    def test_add_agent_javascript_tester(self, sample_city):
        """Test adding JavaScript tester agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-003",
            agent_name="JS Tester",
            floor_origin="floor-javascript",
            role="tester",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.JAVASCRIPT_YELLOW
        assert embodiment.pixel_representation.accessory == PixelAccessory.TEST_TUBE
    
    def test_add_agent_go_security(self, sample_city):
        """Test adding Go security agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-004",
            agent_name="Go Security",
            floor_origin="floor-go",
            role="security",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.GO_CYAN
        assert embodiment.pixel_representation.accessory == PixelAccessory.SAFETY_GOGGLES
    
    def test_add_agent_c_architect(self, sample_city):
        """Test adding C architect agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-005",
            agent_name="C Architect",
            floor_origin="floor-c",
            role="architect",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.C_BLUE
        assert embodiment.pixel_representation.accessory == PixelAccessory.BLUEPRINT
    
    def test_add_agent_sql_manager(self, sample_city):
        """Test adding SQL manager agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-006",
            agent_name="SQL Manager",
            floor_origin="floor-sql",
            role="manager",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.SQL_GREEN
        assert embodiment.pixel_representation.accessory == PixelAccessory.CLIPBOARD
    
    def test_add_agent_shell_worker(self, sample_city):
        """Test adding Shell worker agent."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-007",
            agent_name="Shell Worker",
            floor_origin="floor-shell",
            role="worker",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.SHELL_GRAY
        assert embodiment.pixel_representation.accessory == PixelAccessory.WRENCH
    
    def test_add_agent_unknown_floor(self, sample_city):
        """Test adding agent from unknown floor."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-008",
            agent_name="Unknown Agent",
            floor_origin="floor-unknown",
            role="builder",
            reputation_level=0
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.pixel_representation.color == PixelColor.GENERIC_WHITE
    
    def test_add_agent_with_bronze_star(self, sample_city):
        """Test adding agent with bronze star reputation."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-009",
            agent_name="Bronze Agent",
            floor_origin="floor-python",
            role="builder",
            reputation_level=1
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert ReputationVisual.BRONZE_STAR in embodiment.pixel_representation.reputation_markers
        assert len(embodiment.pixel_representation.reputation_markers) == 1
    
    def test_add_agent_with_silver_star(self, sample_city):
        """Test adding agent with silver star reputation."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-010",
            agent_name="Silver Agent",
            floor_origin="floor-python",
            role="builder",
            reputation_level=2
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert ReputationVisual.SILVER_STAR in embodiment.pixel_representation.reputation_markers
        assert len(embodiment.pixel_representation.reputation_markers) == 1
    
    def test_add_agent_with_golden_star(self, sample_city):
        """Test adding agent with golden star reputation."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-011",
            agent_name="Golden Agent",
            floor_origin="floor-python",
            role="builder",
            reputation_level=3
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert ReputationVisual.GOLDEN_STAR in embodiment.pixel_representation.reputation_markers
        assert len(embodiment.pixel_representation.reputation_markers) == 1
    
    def test_agent_visit_lounge(self, sample_city):
        """Test agent visiting lounge."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-012",
            agent_name="Social Agent",
            floor_origin="floor-python",
            role="builder"
        )
        
        sample_city.agent_visit_lounge(
            embodiment_id=embodiment_id,
            conversation_topics=["testing", "refactoring"]
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.current_state == AgentOffDutyState.SOCIALIZING
        assert embodiment.current_zone == CityZone.LOUNGE
        assert embodiment_id in sample_city.zone_manager.lounge.participants
    
    def test_agent_visit_lounge_nonexistent(self, sample_city):
        """Test nonexistent agent visiting lounge."""
        # Should not raise error
        sample_city.agent_visit_lounge(
            embodiment_id="emb-999",
            conversation_topics=["testing"]
        )
    
    def test_agent_visit_workshop(self, sample_city):
        """Test agent visiting workshop."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-013",
            agent_name="Tinkering Agent",
            floor_origin="floor-rust",
            role="builder"
        )
        
        sample_city.agent_visit_workshop(
            embodiment_id=embodiment_id,
            project_type="widget",
            project_description="Building a test widget"
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.current_state == AgentOffDutyState.TINKERING
        assert embodiment.current_zone == CityZone.WORKSHOP
        assert len(sample_city.zone_manager.workshops[0].active_projects) == 1
    
    def test_agent_visit_workshop_nonexistent(self, sample_city):
        """Test nonexistent agent visiting workshop."""
        # Should not raise error
        sample_city.agent_visit_workshop(
            embodiment_id="emb-999",
            project_type="widget",
            project_description="Test"
        )
    
    def test_agent_visit_archives(self, sample_city):
        """Test agent visiting archives."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-014",
            agent_name="Historian Agent",
            floor_origin="floor-go",
            role="reviewer"
        )
        
        sample_city.agent_visit_archives(
            embodiment_id=embodiment_id,
            purpose="Research legacy patterns"
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.current_state == AgentOffDutyState.REFLECTING
        assert embodiment.current_zone == CityZone.ARCHIVES
        assert len(sample_city.zone_manager.archives.visitors) == 1
    
    def test_agent_visit_archives_nonexistent(self, sample_city):
        """Test nonexistent agent visiting archives."""
        # Should not raise error
        sample_city.agent_visit_archives(
            embodiment_id="emb-999",
            purpose="Research"
        )
    
    def test_agent_attend_plaza(self, sample_city):
        """Test agent attending plaza ceremony."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-015",
            agent_name="Ceremony Agent",
            floor_origin="floor-c",
            role="manager"
        )
        
        sample_city.agent_attend_plaza(
            embodiment_id=embodiment_id,
            ceremony_type="employee_of_month",
            ceremony_details="Monthly recognition"
        )
        
        embodiment = sample_city.population[embodiment_id]
        assert embodiment.current_state == AgentOffDutyState.ATTENDING_CEREMONY
        assert embodiment.current_zone == CityZone.PLAZA
        assert embodiment_id in sample_city.zone_manager.plaza.attendees
    
    def test_agent_attend_plaza_nonexistent(self, sample_city):
        """Test nonexistent agent attending plaza."""
        # Should not raise error
        sample_city.agent_attend_plaza(
            embodiment_id="emb-999",
            ceremony_type="ceremony",
            ceremony_details="Details"
        )
    
    def test_cross_transit_gate(self, sample_city):
        """Test agent crossing transit gate."""
        embodiment_id = sample_city.add_agent_to_city(
            agent_id="agent-016",
            agent_name="Transit Agent",
            floor_origin="floor-sql",
            role="builder"
        )
        
        result = sample_city.cross_transit_gate(
            embodiment_id=embodiment_id,
            destination="office",
            crossing_by="directive-001",
            reason="Work assignment"
        )
        
        assert result["success"] is True
        assert result["from_zone"] == "residential_districts"
        assert result["to_zone"] == "office"
        assert result["crossing_by"] == "directive-001"
        
        # Check gate history
        assert len(sample_city.zone_manager.transit_gates[0].crossing_history) == 1
    
    def test_cross_transit_gate_nonexistent(self, sample_city):
        """Test nonexistent agent crossing gate."""
        result = sample_city.cross_transit_gate(
            embodiment_id="emb-999",
            destination="office",
            crossing_by="directive-001",
            reason="Test"
        )
        
        assert result["success"] is False
    
    def test_get_city_stats_empty(self, sample_city):
        """Test city stats with empty city."""
        stats = sample_city.get_city_stats()
        
        assert stats["city_id"] == "city-test"
        assert stats["total_population"] == 0
        assert stats["currently_resting"] == 0
        assert stats["currently_socializing"] == 0
        assert stats["in_workshops"] == 0
        assert stats["in_lounge"] == 0
        assert stats["firewall_violations"] == 0
    
    def test_get_city_stats_with_agents(self, sample_city):
        """Test city stats with multiple agents."""
        # Add agents
        emb1 = sample_city.add_agent_to_city("agent-1", "Agent 1", "floor-python", "builder")
        emb2 = sample_city.add_agent_to_city("agent-2", "Agent 2", "floor-rust", "reviewer")
        emb3 = sample_city.add_agent_to_city("agent-3", "Agent 3", "floor-go", "tester")
        
        # Have one visit lounge
        sample_city.agent_visit_lounge(emb1, ["testing"])
        
        # Have one visit workshop
        sample_city.agent_visit_workshop(emb2, "widget", "Test widget")
        
        stats = sample_city.get_city_stats()
        
        assert stats["total_population"] == 3
        assert stats["currently_resting"] == 1  # emb3 still resting
        assert stats["currently_socializing"] == 1  # emb1 in lounge
        assert len(sample_city.zone_manager.lounge.participants) == 1
    
    def test_generate_city_report(self, sample_city):
        """Test generating city report."""
        # Add some agents and activities
        emb1 = sample_city.add_agent_to_city("agent-1", "Agent 1", "floor-python", "builder")
        sample_city.agent_visit_lounge(emb1, ["testing"])
        
        report = sample_city.generate_city_report()
        
        assert "OFF-DUTY AI CITY STATUS" in report
        assert "city-test" in report
        assert "Population:" in report
        assert "ZONES:" in report
        assert "FIREWALL:" in report
        assert "Nothing crosses to production without gates" in report


# ============================================================================
# TEST GLOBAL INSTANCE
# ============================================================================

class TestGetOffDutyCity:
    """Test global off-duty city instance."""
    
    def test_get_off_duty_city_creates_instance(self):
        """Test that get_off_duty_city creates instance."""
        city = get_off_duty_city()
        
        assert city is not None
        assert city.city_id == "city-001"
        assert city.zone_manager is not None
        assert city.city_firewall is not None
        assert city.visual_decay is not None
    
    def test_get_off_duty_city_returns_same_instance(self):
        """Test that get_off_duty_city returns same instance."""
        city1 = get_off_duty_city()
        city2 = get_off_duty_city()
        
        assert city1 is city2
    
    def test_get_off_duty_city_initialized_zones(self):
        """Test that zones are initialized."""
        city = get_off_duty_city()
        
        assert city.zone_manager.lounge is not None
        assert city.zone_manager.archives is not None
        assert city.zone_manager.plaza is not None
        assert len(city.zone_manager.workshops) > 0
        assert len(city.zone_manager.transit_gates) > 0


# ============================================================================
# INTEGRATION TESTS
# ============================================================================

class TestOffDutyCityIntegration:
    """Integration tests for off-duty city system."""
    
    @pytest.fixture
    def populated_city(self):
        """Create a city with multiple agents."""
        zone_manager = CityZoneManager(manager_id="zm-int")
        zone_manager.initialize_zones()
        
        city_firewall = CityFirewall(firewall_id="fw-int")
        visual_decay = VisualDecay(decay_id="decay-int")
        
        city = OffDutyCity(
            city_id="city-int",
            zone_manager=zone_manager,
            city_firewall=city_firewall,
            visual_decay=visual_decay
        )
        
        # Add multiple agents from different floors
        floors_roles = [
            ("floor-python", "builder"),
            ("floor-rust", "reviewer"),
            ("floor-go", "tester"),
            ("floor-javascript", "security"),
            ("floor-c", "architect")
        ]
        
        for i, (floor, role) in enumerate(floors_roles):
            city.add_agent_to_city(
                agent_id=f"agent-int-{i}",
                agent_name=f"Agent {i}",
                floor_origin=floor,
                role=role,
                reputation_level=i % 3
            )
        
        return city
    
    def test_full_agent_lifecycle(self, populated_city):
        """Test full agent lifecycle in city."""
        # Get an agent
        embodiment_id = "embodiment-agent-int-0"
        embodiment = populated_city.population[embodiment_id]
        
        # Initially resting
        assert embodiment.current_state == AgentOffDutyState.RESTING
        
        # Visit lounge
        populated_city.agent_visit_lounge(embodiment_id, ["architecture", "patterns"])
        assert embodiment.current_state == AgentOffDutyState.SOCIALIZING
        assert embodiment.current_zone == CityZone.LOUNGE
        
        # Visit workshop
        populated_city.agent_visit_workshop(embodiment_id, "refactoring", "Code cleanup")
        assert embodiment.current_state == AgentOffDutyState.TINKERING
        assert embodiment.current_zone == CityZone.WORKSHOP
        
        # Visit archives
        populated_city.agent_visit_archives(embodiment_id, "Research history")
        assert embodiment.current_state == AgentOffDutyState.REFLECTING
        assert embodiment.current_zone == CityZone.ARCHIVES
        
        # Attend ceremony
        populated_city.agent_attend_plaza(embodiment_id, "recognition", "Award ceremony")
        assert embodiment.current_state == AgentOffDutyState.ATTENDING_CEREMONY
        assert embodiment.current_zone == CityZone.PLAZA
        
        # Cross gate
        result = populated_city.cross_transit_gate(embodiment_id, "office", "directive-001", "Work")
        assert result["success"] is True
    
    def test_multiple_agents_interacting(self, populated_city):
        """Test multiple agents interacting simultaneously."""
        # Have multiple agents visit lounge
        for i in range(3):
            emb_id = f"embodiment-agent-int-{i}"
            populated_city.agent_visit_lounge(emb_id, ["gossip", "news"])
        
        # Check lounge has all participants
        assert len(populated_city.zone_manager.lounge.participants) == 3
        
        # Start a conversation
        conv_id = populated_city.zone_manager.lounge.start_conversation(
            participants=[f"embodiment-agent-int-{i}" for i in range(3)],
            topics=["testing", "best practices"]
        )
        
        assert conv_id is not None
        assert len(populated_city.zone_manager.lounge.active_conversations) == 1
    
    def test_reputation_decay_lifecycle(self, populated_city):
        """Test reputation decay over time."""
        # Get decay system
        decay = populated_city.visual_decay
        
        # Test decay at different time points
        factor_0 = decay.decay_reputation_visual(
            "agent-int-0", ReputationVisual.GOLDEN_STAR, 0, 0, 1000
        )
        assert factor_0 == 1.0
        
        factor_500 = decay.decay_reputation_visual(
            "agent-int-0", ReputationVisual.GOLDEN_STAR, 0, 500, 1000
        )
        assert 0.6 < factor_500 < 0.8
        
        factor_2000 = decay.decay_reputation_visual(
            "agent-int-0", ReputationVisual.GOLDEN_STAR, 0, 2000, 1000
        )
        assert factor_2000 < 0.3
        assert decay.should_archive(factor_2000, 0.5)
    
    def test_firewall_blocks_all_unauthorized_crossings(self, populated_city):
        """Test firewall blocks all unauthorized crossing types."""
        firewall = populated_city.city_firewall
        
        blocked_types = ["decision", "agreement", "consensus", "momentum", "social_pressure"]
        
        for crossing_type in blocked_types:
            blocked, msg = firewall.block_city_to_production_crossing(
                crossing_type=crossing_type,
                source_agent="agent-int-0",
                attempted_action=f"Attempt {crossing_type}"
            )
            assert blocked is True
            assert crossing_type in msg
        
        assert len(firewall.violations) == len(blocked_types)
    
    def test_workshop_projects_sandboxed(self, populated_city):
        """Test that workshop projects are always sandboxed."""
        emb_id = "embodiment-agent-int-0"
        populated_city.agent_visit_workshop(emb_id, "experiment", "Risky experiment")
        
        workshop = populated_city.zone_manager.workshops[0]
        project = workshop.active_projects[0]
        
        assert project["sandboxed"] is True
        assert project["binding"] is False
    
    def test_lounge_conversations_not_actionable(self, populated_city):
        """Test that lounge conversations are never actionable."""
        lounge = populated_city.zone_manager.lounge
        
        conv_id = lounge.start_conversation(
            participants=["emb-1", "emb-2"],
            topics=["refactoring", "architecture"]
        )
        
        conversation = lounge.active_conversations[0]
        assert conversation["actionable"] is False
    
    def test_plaza_ceremonies_no_operational_effect(self, populated_city):
        """Test that ceremonies have no operational effect."""
        plaza = populated_city.zone_manager.plaza
        
        plaza.host_ceremony(
            ceremony_type="award",
            details="Excellence in testing",
            honorees=["agent-int-0", "agent-int-1"]
        )
        
        ceremony = plaza.ceremonies[0]
        assert ceremony["purely_ceremonial"] is True
        assert ceremony["operational_effect"] is None
    
    def test_city_stats_accuracy(self, populated_city):
        """Test city statistics are accurate."""
        # Perform various activities
        populated_city.agent_visit_lounge("embodiment-agent-int-0", ["topic1"])
        populated_city.agent_visit_lounge("embodiment-agent-int-1", ["topic2"])
        populated_city.agent_visit_workshop("embodiment-agent-int-2", "widget", "Test")
        
        stats = populated_city.get_city_stats()
        
        assert stats["total_population"] == 5
        assert stats["currently_socializing"] == 2
        # Other agents still resting
        assert stats["currently_resting"] == 2
    
    def test_morale_and_burnout_tracking(self, populated_city):
        """Test morale and burnout tracking."""
        emb_id = "embodiment-agent-int-0"
        embodiment = populated_city.population[emb_id]
        
        # Adjust morale and burnout
        embodiment.adjust_morale(-0.3)
        embodiment.adjust_burnout(0.4)
        
        assert embodiment.morale == 0.7
        assert embodiment.burnout_level == 0.4
        
        # Check that adjusted agent is tracked in population
        agents = list(populated_city.population.values())
        assert any(a.morale < 1.0 for a in agents)
        assert any(a.burnout_level > 0.0 for a in agents)
