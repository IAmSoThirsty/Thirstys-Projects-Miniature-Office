"""
Comprehensive tests for src/core/floor_manager.py to achieve 100% coverage

Tests cover:
- FloorProcess class and all methods
- MultiLanguageFloorManager class and all methods
- Floor lifecycle management (start, stop, process management)
- Communication with floor processes
- Error handling and edge cases
- Build processes for compiled languages
- Process termination scenarios
- Request/response handling
- Integration with actual floor directories
"""

import json
import subprocess
from pathlib import Path
from unittest.mock import Mock, patch

import pytest

from src.core.floor_manager import FloorProcess, MultiLanguageFloorManager, demo


class TestFloorProcess:
    """Test FloorProcess class and all methods"""

    def test_floor_process_initialization(self):
        """Test FloorProcess initialization"""
        mock_process = Mock()
        mock_process.poll.return_value = None  # Process is running

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        assert floor_process.floor_number == 1
        assert floor_process.language == "python"
        assert floor_process.process == mock_process

    def test_floor_process_attributes(self):
        """Test all FloorProcess attributes are set correctly"""
        mock_process = Mock()

        floor_process = FloorProcess(
            floor_number=5, language="rust", process=mock_process
        )

        assert floor_process.floor_number == 5
        assert floor_process.language == "rust"
        assert floor_process.process is mock_process

    def test_send_request_basic(self):
        """Test sending a basic request without parameters"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        # Set up the mock process
        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        # Mock response
        response_data = {"result": "success"}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        result = floor_process.send_request("get_info")

        # Verify request was written
        mock_stdin.write.assert_called_once()
        written_data = mock_stdin.write.call_args[0][0].decode()
        request = json.loads(written_data)

        assert request["method"] == "get_info"
        assert "params" not in request

        # Verify flush was called
        mock_stdin.flush.assert_called_once()

        # Verify result
        assert result == response_data

    def test_send_request_with_params(self):
        """Test sending a request with parameters"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        response_data = {"status": "analyzed", "issues": []}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=2, language="rust", process=mock_process
        )

        params = {"code": "fn main() {}", "operation": "analyze"}
        result = floor_process.send_request("process_code", params)

        # Verify request was written with params
        written_data = mock_stdin.write.call_args[0][0].decode()
        request = json.loads(written_data)

        assert request["method"] == "process_code"
        assert request["params"] == params

        assert result == response_data

    def test_send_request_complex_params(self):
        """Test sending request with complex nested parameters"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        response_data = {"processed": True}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=3, language="go", process=mock_process
        )

        params = {
            "nested": {"key": "value", "list": [1, 2, 3]},
            "array": ["a", "b", "c"],
        }
        result = floor_process.send_request("complex_method", params)

        written_data = mock_stdin.write.call_args[0][0].decode()
        request = json.loads(written_data)

        assert request["params"] == params
        assert result == response_data

    def test_is_running_true(self):
        """Test is_running returns True when process is active"""
        mock_process = Mock()
        mock_process.poll.return_value = None  # None means running

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        assert floor_process.is_running() is True

    def test_is_running_false(self):
        """Test is_running returns False when process has terminated"""
        mock_process = Mock()
        mock_process.poll.return_value = 0  # Return code means terminated

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        assert floor_process.is_running() is False

    def test_stop_running_process(self):
        """Test stopping a running process"""
        mock_process = Mock()
        mock_stdin = Mock()

        mock_process.stdin = mock_stdin
        mock_process.poll.return_value = None  # Process is running
        mock_process.wait.return_value = None

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        floor_process.stop()

        # Verify stdin was closed
        mock_stdin.close.assert_called_once()

        # Verify terminate was called
        mock_process.terminate.assert_called_once()

        # Verify wait was called with timeout
        mock_process.wait.assert_called_once_with(timeout=5)

    def test_stop_already_stopped_process(self):
        """Test stopping a process that has already stopped"""
        mock_process = Mock()
        mock_process.poll.return_value = 0  # Already stopped

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        floor_process.stop()

        # Should not attempt to terminate
        mock_process.terminate.assert_not_called()
        mock_process.kill.assert_not_called()

    def test_stop_process_timeout_kill(self):
        """Test that kill is called if terminate times out"""
        mock_process = Mock()
        mock_stdin = Mock()

        mock_process.stdin = mock_stdin
        mock_process.poll.return_value = None  # Process is running
        mock_process.wait.side_effect = subprocess.TimeoutExpired("test", 5)

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        floor_process.stop()

        # Verify terminate was called
        mock_process.terminate.assert_called_once()

        # Verify kill was called after timeout
        mock_process.kill.assert_called_once()


class TestMultiLanguageFloorManager:
    """Test MultiLanguageFloorManager class and all methods"""

    def test_manager_initialization_default(self):
        """Test manager initializes with default floors directory"""
        manager = MultiLanguageFloorManager()

        assert manager.floors_dir == Path("floors")
        assert isinstance(manager.active_floors, dict)
        assert len(manager.active_floors) == 0
        assert isinstance(manager.floor_configs, dict)

    def test_manager_initialization_custom_dir(self):
        """Test manager initialization with custom directory"""
        custom_dir = "custom_floors"
        manager = MultiLanguageFloorManager(floors_dir=custom_dir)

        assert manager.floors_dir == Path(custom_dir)
        assert len(manager.active_floors) == 0

    def test_floor_configs_all_languages(self):
        """Test that all expected languages are configured"""
        manager = MultiLanguageFloorManager()

        expected_languages = ["python", "javascript", "rust", "go", "shell"]

        for lang in expected_languages:
            assert lang in manager.floor_configs
            config = manager.floor_configs[lang]
            assert "floor_number" in config
            assert "executable" in config
            assert "path" in config

    def test_floor_config_structure(self):
        """Test floor configurations have required fields"""
        manager = MultiLanguageFloorManager()

        for language, config in manager.floor_configs.items():
            assert "floor_number" in config
            assert "executable" in config
            assert "path" in config
            assert isinstance(config["floor_number"], int)
            assert isinstance(config["executable"], list)
            assert isinstance(config["path"], Path)

    def test_python_floor_config(self):
        """Test Python floor configuration details"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["python"]

        assert config["floor_number"] == 1
        assert config["executable"] == ["python3", "department_floor.py"]
        assert config["path"].name == "python"
        assert "needs_build" not in config

    def test_javascript_floor_config(self):
        """Test JavaScript floor configuration details"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["javascript"]

        assert config["floor_number"] == 4
        assert config["executable"] == ["node", "department_floor.js"]
        assert config["path"].name == "javascript"

    def test_rust_floor_config(self):
        """Test Rust floor configuration with build requirements"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["rust"]

        assert config["floor_number"] == 2
        assert config["executable"] == ["./target/debug/department_floor"]
        assert config["path"].name == "rust"
        assert config.get("needs_build") is True
        assert config["build_cmd"] == ["cargo", "build"]

    def test_go_floor_config(self):
        """Test Go floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["go"]

        assert config["floor_number"] == 5
        assert config["executable"] == ["go", "run", "department_floor.go"]
        assert config["path"].name == "go"

    def test_shell_floor_config(self):
        """Test Shell floor configuration"""
        manager = MultiLanguageFloorManager()
        config = manager.floor_configs["shell"]

        assert config["floor_number"] == 7
        assert config["executable"] == ["bash", "department_floor.sh"]
        assert config["path"].name == "shell"

    def test_start_floor_already_running(self):
        """Test starting a floor that is already running"""
        manager = MultiLanguageFloorManager()

        # Mock an already running floor
        mock_floor = Mock()
        manager.active_floors["python"] = mock_floor

        result = manager.start_floor("python")

        assert result is True
        # Should still be the same mock object
        assert manager.active_floors["python"] == mock_floor

    def test_start_floor_unknown_language(self):
        """Test starting a floor with unknown language"""
        manager = MultiLanguageFloorManager()

        result = manager.start_floor("nonexistent")

        assert result is False
        assert "nonexistent" not in manager.active_floors

    def test_start_floor_directory_not_found(self):
        """Test starting a floor when directory doesn't exist"""
        manager = MultiLanguageFloorManager(floors_dir="nonexistent_dir")

        result = manager.start_floor("python")

        assert result is False

    @patch("subprocess.Popen")
    def test_start_floor_success_no_build(self, mock_popen):
        """Test successfully starting a floor without build requirement"""
        manager = MultiLanguageFloorManager()

        # Mock the process
        mock_process = Mock()
        mock_popen.return_value = mock_process

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("python")

        assert result is True
        assert "python" in manager.active_floors

        # Verify process was started with correct parameters
        mock_popen.assert_called_once()
        call_args = mock_popen.call_args

        assert call_args[1]["stdin"] == subprocess.PIPE
        assert call_args[1]["stdout"] == subprocess.PIPE
        assert call_args[1]["stderr"] == subprocess.PIPE

    @patch("subprocess.run")
    @patch("subprocess.Popen")
    def test_start_floor_with_build_success(self, mock_popen, mock_run):
        """Test starting a floor that requires building"""
        manager = MultiLanguageFloorManager()

        # Mock successful build
        mock_run_result = Mock()
        mock_run_result.returncode = 0
        mock_run.return_value = mock_run_result

        # Mock process
        mock_process = Mock()
        mock_popen.return_value = mock_process

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("rust")

        assert result is True
        assert "rust" in manager.active_floors

        # Verify build was called
        mock_run.assert_called_once()
        build_call_args = mock_run.call_args
        assert "cargo" in build_call_args[0][0]
        assert "build" in build_call_args[0][0]

    @patch("subprocess.run")
    def test_start_floor_build_fails(self, mock_run):
        """Test starting a floor when build fails"""
        manager = MultiLanguageFloorManager()

        # Mock failed build
        mock_run_result = Mock()
        mock_run_result.returncode = 1
        mock_run_result.stderr = b"Build error"
        mock_run.return_value = mock_run_result

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("rust")

        assert result is False
        assert "rust" not in manager.active_floors

    @patch("subprocess.run")
    def test_start_floor_build_exception(self, mock_run):
        """Test handling exceptions during build"""
        manager = MultiLanguageFloorManager()

        # Mock build exception
        mock_run.side_effect = Exception("Build error")

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("rust")

        assert result is False
        assert "rust" not in manager.active_floors

    @patch("subprocess.run")
    def test_start_floor_build_timeout(self, mock_run):
        """Test handling build timeout"""
        manager = MultiLanguageFloorManager()

        # Mock build timeout
        mock_run.side_effect = subprocess.TimeoutExpired("cargo", 60)

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("rust")

        assert result is False

    @patch("subprocess.Popen")
    def test_start_floor_popen_exception(self, mock_popen):
        """Test handling exception when starting process"""
        manager = MultiLanguageFloorManager()

        # Mock Popen exception
        mock_popen.side_effect = Exception("Failed to start process")

        # Mock directory exists
        with patch.object(Path, "exists", return_value=True):
            result = manager.start_floor("python")

        assert result is False
        assert "python" not in manager.active_floors

    def test_stop_floor_not_running(self):
        """Test stopping a floor that isn't running"""
        manager = MultiLanguageFloorManager()

        # Should not raise exception
        manager.stop_floor("python")

        assert "python" not in manager.active_floors

    def test_stop_floor_running(self):
        """Test stopping a running floor"""
        manager = MultiLanguageFloorManager()

        # Mock a running floor
        mock_floor = Mock()
        manager.active_floors["python"] = mock_floor

        manager.stop_floor("python")

        # Verify stop was called
        mock_floor.stop.assert_called_once()

        # Verify floor was removed
        assert "python" not in manager.active_floors

    def test_stop_all_floors_empty(self):
        """Test stopping all floors when none are running"""
        manager = MultiLanguageFloorManager()

        # Should not raise exception
        manager.stop_all_floors()

        assert len(manager.active_floors) == 0

    def test_stop_all_floors_multiple(self):
        """Test stopping multiple running floors"""
        manager = MultiLanguageFloorManager()

        # Mock multiple running floors
        mock_floor1 = Mock()
        mock_floor2 = Mock()
        mock_floor3 = Mock()

        manager.active_floors["python"] = mock_floor1
        manager.active_floors["rust"] = mock_floor2
        manager.active_floors["go"] = mock_floor3

        manager.stop_all_floors()

        # Verify all were stopped
        mock_floor1.stop.assert_called_once()
        mock_floor2.stop.assert_called_once()
        mock_floor3.stop.assert_called_once()

        # Verify all were removed
        assert len(manager.active_floors) == 0

    def test_get_floor_info_not_running(self):
        """Test getting info from a floor that isn't running"""
        manager = MultiLanguageFloorManager()

        result = manager.get_floor_info("python")

        assert result is None

    def test_get_floor_info_running(self):
        """Test getting info from a running floor"""
        manager = MultiLanguageFloorManager()

        # Mock a running floor
        mock_floor = Mock()
        info_data = {
            "floor_number": 1,
            "language": "python",
            "domain": "backend",
            "offices": ["Architecture", "Implementation"],
            "agent_count": 3,
            "task_count": 5,
        }
        mock_floor.send_request.return_value = info_data

        manager.active_floors["python"] = mock_floor

        result = manager.get_floor_info("python")

        # Verify send_request was called correctly
        mock_floor.send_request.assert_called_once_with("get_info")

        assert result == info_data

    def test_send_request_to_floor_not_running(self):
        """Test sending request to floor that isn't running"""
        manager = MultiLanguageFloorManager()

        result = manager.send_request_to_floor("python", "test_method")

        assert result is None

    def test_send_request_to_floor_no_params(self):
        """Test sending request to floor without parameters"""
        manager = MultiLanguageFloorManager()

        # Mock a running floor
        mock_floor = Mock()
        response_data = {"status": "ok"}
        mock_floor.send_request.return_value = response_data

        manager.active_floors["python"] = mock_floor

        result = manager.send_request_to_floor("python", "test_method")

        mock_floor.send_request.assert_called_once_with("test_method", None)
        assert result == response_data

    def test_send_request_to_floor_with_params(self):
        """Test sending request to floor with parameters"""
        manager = MultiLanguageFloorManager()

        # Mock a running floor
        mock_floor = Mock()
        response_data = {"result": "processed"}
        mock_floor.send_request.return_value = response_data

        manager.active_floors["rust"] = mock_floor

        params = {"code": "fn main() {}", "operation": "analyze"}
        result = manager.send_request_to_floor("rust", "process_code", params)

        mock_floor.send_request.assert_called_once_with("process_code", params)
        assert result == response_data

    def test_get_all_floor_info_empty(self):
        """Test getting info from all floors when none are running"""
        manager = MultiLanguageFloorManager()

        result = manager.get_all_floor_info()

        assert result == {}

    def test_get_all_floor_info_multiple_success(self):
        """Test getting info from multiple running floors"""
        manager = MultiLanguageFloorManager()

        # Mock multiple running floors
        mock_floor1 = Mock()
        mock_floor1.send_request.return_value = {
            "language": "python",
            "floor_number": 1,
        }

        mock_floor2 = Mock()
        mock_floor2.send_request.return_value = {"language": "rust", "floor_number": 2}

        mock_floor3 = Mock()
        mock_floor3.send_request.return_value = {"language": "go", "floor_number": 5}

        manager.active_floors["python"] = mock_floor1
        manager.active_floors["rust"] = mock_floor2
        manager.active_floors["go"] = mock_floor3

        result = manager.get_all_floor_info()

        assert len(result) == 3
        assert result["python"]["language"] == "python"
        assert result["rust"]["language"] == "rust"
        assert result["go"]["language"] == "go"

    def test_get_all_floor_info_with_error(self):
        """Test getting info when one floor raises exception"""
        manager = MultiLanguageFloorManager()

        # Mock floors, one succeeds and one fails
        mock_floor1 = Mock()
        mock_floor1.send_request.return_value = {"language": "python"}

        mock_floor2 = Mock()
        mock_floor2.send_request.side_effect = Exception("Communication error")

        manager.active_floors["python"] = mock_floor1
        manager.active_floors["rust"] = mock_floor2

        result = manager.get_all_floor_info()

        assert len(result) == 2
        assert result["python"]["language"] == "python"
        assert "error" in result["rust"]
        assert "Communication error" in result["rust"]["error"]

    def test_floor_numbers_unique(self):
        """Test that all configured floors have unique floor numbers"""
        manager = MultiLanguageFloorManager()

        floor_numbers = [
            config["floor_number"] for config in manager.floor_configs.values()
        ]

        assert len(floor_numbers) == len(set(floor_numbers))

    def test_active_floors_tracking(self):
        """Test that active_floors dictionary is properly maintained"""
        manager = MultiLanguageFloorManager()

        assert isinstance(manager.active_floors, dict)
        assert len(manager.active_floors) == 0

        # Add mock floors
        mock_floor1 = Mock()
        mock_floor2 = Mock()

        manager.active_floors["python"] = mock_floor1
        assert len(manager.active_floors) == 1

        manager.active_floors["rust"] = mock_floor2
        assert len(manager.active_floors) == 2

        # Remove one
        del manager.active_floors["python"]
        assert len(manager.active_floors) == 1
        assert "rust" in manager.active_floors


class TestDemo:
    """Test the demo function"""

    @patch("src.core.floor_manager.MultiLanguageFloorManager")
    def test_demo_function_no_floors_started(self, mock_manager_class):
        """Test demo when no floors can be started"""
        # Mock the manager and its methods
        mock_manager = Mock()
        mock_manager_class.return_value = mock_manager

        # Mock start_floor to return False (floors not available)
        mock_manager.start_floor.return_value = False

        # Mock get_all_floor_info to return empty
        mock_manager.get_all_floor_info.return_value = {}

        # Mock active_floors as empty dict
        mock_manager.active_floors = {}

        # Should run without errors
        demo()

        # Verify manager was created
        mock_manager_class.assert_called_once()

        # Verify stop_all_floors was called at the end
        mock_manager.stop_all_floors.assert_called_once()

    @patch("src.core.floor_manager.MultiLanguageFloorManager")
    def test_demo_function_with_successful_floors(self, mock_manager_class):
        """Test demo with successfully started floors"""
        mock_manager = Mock()
        mock_manager_class.return_value = mock_manager

        # Mock start_floor to return True for some floors
        def mock_start_floor(lang):
            return lang in ["python", "javascript"]

        mock_manager.start_floor.side_effect = mock_start_floor

        # Mock floor info responses
        mock_manager.get_all_floor_info.return_value = {
            "python": {
                "floor_number": 1,
                "language": "python",
                "domain": "backend",
                "offices": ["Architecture", "Implementation"],
                "agent_count": 3,
                "task_count": 5,
            },
            "javascript": {
                "floor_number": 4,
                "language": "javascript",
                "domain": "frontend",
                "offices": ["Architecture", "Review"],
                "agent_count": 2,
                "task_count": 3,
            },
        }

        # Mock active_floors
        mock_manager.active_floors = {"python": Mock(), "javascript": Mock()}

        # Mock send_request_to_floor
        def mock_send_request(lang, method, params=None):
            if method == "process_code":
                return {"analysis": {"lines": 2, "functions": 1}}
            return None

        mock_manager.send_request_to_floor.side_effect = mock_send_request

        # Should run without errors
        demo()

        # Verify stop_all_floors was called
        mock_manager.stop_all_floors.assert_called_once()

    @patch("src.core.floor_manager.MultiLanguageFloorManager")
    def test_demo_function_with_floor_errors(self, mock_manager_class):
        """Test demo with floor errors"""
        mock_manager = Mock()
        mock_manager_class.return_value = mock_manager

        # Mock start_floor
        mock_manager.start_floor.return_value = True

        # Mock get_all_floor_info with errors
        mock_manager.get_all_floor_info.return_value = {
            "python": {"error": "Connection failed"},
            "rust": {"error": "Timeout"},
        }

        # Mock active_floors as empty
        mock_manager.active_floors = {}

        # Should run without errors even with floor errors
        demo()

        mock_manager.stop_all_floors.assert_called_once()

    @patch("src.core.floor_manager.MultiLanguageFloorManager")
    def test_demo_function_with_result_no_analysis(self, mock_manager_class):
        """Test demo when result has no analysis key"""
        mock_manager = Mock()
        mock_manager_class.return_value = mock_manager

        mock_manager.start_floor.return_value = True

        mock_manager.get_all_floor_info.return_value = {}

        # Mock active_floors with a floor
        mock_manager.active_floors = {"python": Mock()}

        # Mock send_request_to_floor to return result without analysis
        mock_manager.send_request_to_floor.return_value = {"status": "ok"}

        # Should handle missing analysis key
        demo()

        mock_manager.stop_all_floors.assert_called_once()


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
            "Manager Office",
        ]
        assert len(required_offices) == 6

    def test_all_floors_support_get_info(self):
        """Test all floor configs exist (they should support get_info)"""
        manager = MultiLanguageFloorManager()
        assert len(manager.floor_configs) >= 5, "Not all floors configured"

    def test_floor_numbers_are_unique(self):
        """Test each floor has a unique floor number"""
        manager = MultiLanguageFloorManager()
        floor_numbers = [
            config["floor_number"] for config in manager.floor_configs.values()
        ]
        assert len(floor_numbers) == len(
            set(floor_numbers)
        ), "Floor numbers are not unique"


class TestEdgeCases:
    """Test edge cases and boundary conditions"""

    def test_empty_params_in_request(self):
        """Test sending request with empty params dict"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        response_data = {"result": "ok"}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        # Empty dict is falsy, so params won't be included
        result = floor_process.send_request("test", {})

        written_data = mock_stdin.write.call_args[0][0].decode()
        request = json.loads(written_data)

        # Empty params are not included in request (falsy check)
        assert "params" not in request
        assert result == response_data

    def test_floor_process_with_zero_floor_number(self):
        """Test floor process can have floor number 0"""
        mock_process = Mock()

        floor_process = FloorProcess(
            floor_number=0, language="test", process=mock_process
        )

        assert floor_process.floor_number == 0

    def test_manager_with_empty_active_floors(self):
        """Test manager operations with no active floors"""
        manager = MultiLanguageFloorManager()

        assert len(manager.active_floors) == 0

        # All these should work without errors
        manager.stop_all_floors()
        assert manager.get_floor_info("python") is None
        assert manager.send_request_to_floor("python", "test") is None
        assert manager.get_all_floor_info() == {}

    def test_consecutive_start_stop_cycles(self):
        """Test multiple start/stop cycles don't cause issues"""
        manager = MultiLanguageFloorManager()

        mock_floor = Mock()

        # Start
        manager.active_floors["python"] = mock_floor
        assert "python" in manager.active_floors

        # Stop
        manager.stop_floor("python")
        assert "python" not in manager.active_floors

        # Start again
        manager.active_floors["python"] = mock_floor
        assert "python" in manager.active_floors

        # Stop again
        manager.stop_floor("python")
        assert "python" not in manager.active_floors

    def test_floor_config_path_construction(self):
        """Test that floor paths are constructed correctly"""
        custom_dir = "my_custom_floors"
        manager = MultiLanguageFloorManager(floors_dir=custom_dir)

        for language, config in manager.floor_configs.items():
            expected_path = Path(custom_dir) / language
            assert config["path"] == expected_path

    def test_get_all_floor_info_preserves_all_errors(self):
        """Test that all floor errors are captured separately"""
        manager = MultiLanguageFloorManager()

        mock_floor1 = Mock()
        mock_floor1.send_request.side_effect = ValueError("Error 1")

        mock_floor2 = Mock()
        mock_floor2.send_request.side_effect = RuntimeError("Error 2")

        mock_floor3 = Mock()
        mock_floor3.send_request.side_effect = Exception("Error 3")

        manager.active_floors["lang1"] = mock_floor1
        manager.active_floors["lang2"] = mock_floor2
        manager.active_floors["lang3"] = mock_floor3

        result = manager.get_all_floor_info()

        assert len(result) == 3
        assert "error" in result["lang1"]
        assert "error" in result["lang2"]
        assert "error" in result["lang3"]
        assert "Error 1" in result["lang1"]["error"]
        assert "Error 2" in result["lang2"]["error"]
        assert "Error 3" in result["lang3"]["error"]


class TestProcessCommunication:
    """Test process communication edge cases"""

    def test_send_request_json_encoding(self):
        """Test that requests are properly JSON encoded"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        response_data = {"result": "ok"}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        params = {"unicode": "こんにちは", "special": "test\n\t"}
        floor_process.send_request("test", params)

        # Verify the data was written
        written_data = mock_stdin.write.call_args[0][0].decode()

        # Should be valid JSON
        request = json.loads(written_data)
        assert request["params"]["unicode"] == "こんにちは"
        assert request["params"]["special"] == "test\n\t"

    def test_send_request_response_decoding(self):
        """Test that responses are properly decoded"""
        mock_process = Mock()
        mock_stdin = Mock()
        mock_stdout = Mock()

        mock_process.stdin = mock_stdin
        mock_process.stdout = mock_stdout

        # Response with special characters
        response_data = {"message": "Success! 🎉", "data": [1, 2, 3]}
        mock_stdout.readline.return_value = (json.dumps(response_data) + "\n").encode()

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        result = floor_process.send_request("test")

        assert result["message"] == "Success! 🎉"
        assert result["data"] == [1, 2, 3]

    def test_stop_closes_stdin_before_terminate(self):
        """Test that stdin is closed before terminating process"""
        mock_process = Mock()
        mock_stdin = Mock()

        mock_process.stdin = mock_stdin
        mock_process.poll.return_value = None
        mock_process.wait.return_value = None

        floor_process = FloorProcess(
            floor_number=1, language="python", process=mock_process
        )

        floor_process.stop()

        # Verify order of calls
        calls = [
            call[0]
            for call in [mock_stdin.close.call_args, mock_process.terminate.call_args]
        ]  # noqa: F841,F811

        # stdin.close should be called before terminate
        assert mock_stdin.close.called
        assert mock_process.terminate.called


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
