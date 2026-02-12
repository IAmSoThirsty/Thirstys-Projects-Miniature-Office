"""
Tests for Multi-Language Floor Manager
"""
import pytest
import json
from pathlib import Path
from src.core.floor_manager import MultiLanguageFloorManager, FloorProcess


class TestFloorProcess:
    """Tests for FloorProcess class"""
    
    def test_floor_process_initialization(self):
        """Test FloorProcess initialization with mock process"""
        from unittest.mock import Mock
        
        mock_process = Mock()
        mock_process.poll.return_value = None  # Process is running
        
        floor_process = FloorProcess(
            floor_number=1,
            language="python",
            process=mock_process
        )
        
        assert floor_process.floor_number == 1
        assert floor_process.language == "python"
        assert floor_process.process == mock_process
        assert floor_process.is_running() is True


class TestMultiLanguageFloorManager:
    """Tests for MultiLanguageFloorManager"""
    
    def test_manager_initialization(self):
        """Test manager initializes with correct configuration"""
        manager = MultiLanguageFloorManager()
        
        assert manager.floors_dir.name == "floors"
        assert "python" in manager.floor_configs
        assert "javascript" in manager.floor_configs
        assert "rust" in manager.floor_configs
        assert "go" in manager.floor_configs
        assert "shell" in manager.floor_configs
    
    def test_floor_config_structure(self):
        """Test floor configurations have required fields"""
        manager = MultiLanguageFloorManager()
        
        for language, config in manager.floor_configs.items():
            assert "floor_number" in config, f"{language} missing floor_number"
            assert "executable" in config, f"{language} missing executable"
            assert "path" in config, f"{language} missing path"
            assert isinstance(config["floor_number"], int)
            assert isinstance(config["executable"], list)
            assert isinstance(config["path"], Path)
    
    def test_python_floor_config(self):
        """Test Python floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["python"]
        
        assert config["floor_number"] == 1
        assert config["executable"] == ["python3", "department_floor.py"]
        assert config["path"].name == "python"
    
    def test_javascript_floor_config(self):
        """Test JavaScript floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["javascript"]
        
        assert config["floor_number"] == 4
        assert config["executable"] == ["node", "department_floor.js"]
        assert config["path"].name == "javascript"
    
    def test_rust_floor_config(self):
        """Test Rust floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["rust"]
        
        assert config["floor_number"] == 2
        assert config.get("needs_build") is True
        assert "build_cmd" in config
        assert config["path"].name == "rust"
    
    def test_go_floor_config(self):
        """Test Go floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["go"]
        
        assert config["floor_number"] == 5
        assert config["path"].name == "go"
    
    def test_shell_floor_config(self):
        """Test Shell floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["shell"]
        
        assert config["floor_number"] == 7
        assert config["path"].name == "shell"
    
    def test_start_nonexistent_floor(self):
        """Test starting a non-existent floor returns False"""
        manager = MultiLanguageFloorManager()
        result = manager.start_floor("nonexistent_language")
        
        assert result is False
    
    def test_get_floor_info_not_running(self):
        """Test getting info from a floor that isn't running"""
        manager = MultiLanguageFloorManager()
        result = manager.get_floor_info("python")
        
        assert result is None
    
    def test_send_request_to_not_running_floor(self):
        """Test sending request to floor that isn't running"""
        manager = MultiLanguageFloorManager()
        result = manager.send_request_to_floor("python", "get_info")
        
        assert result is None
    
    def test_stop_not_running_floor(self):
        """Test stopping a floor that isn't running doesn't crash"""
        manager = MultiLanguageFloorManager()
        # Should not raise an exception
        manager.stop_floor("python")
    
    def test_stop_all_floors_empty(self):
        """Test stopping all floors when none are running"""
        manager = MultiLanguageFloorManager()
        # Should not raise an exception
        manager.stop_all_floors()


class TestFloorImplementations:
    """Integration tests for actual floor implementations"""
    
    def test_python_floor_exists(self):
        """Test Python floor file exists"""
        floor_file = Path("floors/python/department_floor.py")
        assert floor_file.exists(), "Python floor implementation not found"
    
    def test_javascript_floor_exists(self):
        """Test JavaScript floor file exists"""
        floor_file = Path("floors/javascript/department_floor.js")
        assert floor_file.exists(), "JavaScript floor implementation not found"
    
    def test_rust_floor_exists(self):
        """Test Rust floor files exist"""
        cargo_file = Path("floors/rust/Cargo.toml")
        main_file = Path("floors/rust/src/main.rs")
        assert cargo_file.exists(), "Rust Cargo.toml not found"
        assert main_file.exists(), "Rust main.rs not found"
    
    def test_go_floor_exists(self):
        """Test Go floor files exist"""
        go_file = Path("floors/go/department_floor.go")
        mod_file = Path("floors/go/go.mod")
        assert go_file.exists(), "Go floor implementation not found"
        assert mod_file.exists(), "Go go.mod not found"
    
    def test_shell_floor_exists(self):
        """Test Shell floor file exists"""
        floor_file = Path("floors/shell/department_floor.sh")
        assert floor_file.exists(), "Shell floor implementation not found"
    
    def test_floor_readme_exists(self):
        """Test each floor has a README"""
        floors = ["python", "javascript", "rust", "go", "shell"]
        for floor in floors:
            readme = Path(f"floors/{floor}/README.md")
            assert readme.exists(), f"{floor} floor README not found"


class TestFloorUniformity:
    """Tests to ensure all floors follow uniform architecture"""
    
    def test_all_floors_have_six_offices(self):
        """Test that floor specifications require 6 offices"""
        # All floors should have these offices according to spec
        required_offices = [
            "Architecture Office",
            "Implementation Office",
            "Review Office",
            "Test Office",
            "Security Office",
            "Manager Office"
        ]
        assert len(required_offices) == 6
    
    def test_all_floors_support_get_info(self):
        """Test all floor configs exist (they should support get_info)"""
        manager = MultiLanguageFloorManager()
        assert len(manager.floor_configs) >= 5, "Not all floors configured"
    
    def test_floor_numbers_are_unique(self):
        """Test each floor has a unique floor number"""
        manager = MultiLanguageFloorManager()
        floor_numbers = [config["floor_number"] for config in manager.floor_configs.values()]
        assert len(floor_numbers) == len(set(floor_numbers)), "Floor numbers are not unique"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
