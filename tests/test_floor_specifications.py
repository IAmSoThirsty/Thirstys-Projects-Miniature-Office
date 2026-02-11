"""
Comprehensive tests for floor_specifications.py - 100% coverage
"""
import pytest
from src.core.floor_specifications import (
    ProgrammingLanguage,
    SecurityFocus,
    TestingDoctrine,
    FloorSpecification,
    FLOOR_1_PYTHON,
    FLOOR_2_RUST,
    FLOOR_3_C,
    FLOOR_4_CPP,
    FLOOR_5_JAVASCRIPT,
    FLOOR_6_TYPESCRIPT,
    FLOOR_7_GO,
    FLOOR_8_SQL,
    FLOOR_9_SHELL,
    FLOOR_10_JAVA,
    FLOOR_11_KOTLIN,
    FLOOR_12_SCALA,
    FLOOR_13_SWIFT,
    FLOOR_14_OBJECTIVE_C,
    FLOOR_15_PHP,
    FLOOR_16_RUBY,
    FLOOR_17_PERL,
    FLOOR_18_POWERSHELL,
    FLOOR_19_NOSQL,
    FLOOR_20_HASKELL,
    FLOOR_21_OCAML,
    FLOOR_22_ELIXIR,
    FLOOR_23_ERLANG,
    FLOOR_24_FORTRAN,
    FLOOR_25_MATLAB_OCTAVE,
    FLOOR_26_CUDA_GPU,
    FLOOR_27_WEBASSEMBLY,
    FLOOR_28_RUST_ASYNC,
    ALL_FLOORS,
    get_floor_specification,
    validate_floor_uniformity,
    get_all_floors,
    route_directive_to_floor,
)


class TestProgrammingLanguage:
    """Test ProgrammingLanguage enum"""
    
    def test_all_language_values(self):
        """Test all 28 programming language enum values"""
        assert ProgrammingLanguage.PYTHON.value == "python"
        assert ProgrammingLanguage.RUST.value == "rust"
        assert ProgrammingLanguage.C.value == "c"
        assert ProgrammingLanguage.CPP.value == "cpp"
        assert ProgrammingLanguage.GO.value == "go"
        assert ProgrammingLanguage.JAVASCRIPT.value == "javascript"
        assert ProgrammingLanguage.TYPESCRIPT.value == "typescript"
        assert ProgrammingLanguage.JAVA.value == "java"
        assert ProgrammingLanguage.KOTLIN.value == "kotlin"
        assert ProgrammingLanguage.SCALA.value == "scala"
        assert ProgrammingLanguage.SWIFT.value == "swift"
        assert ProgrammingLanguage.OBJECTIVE_C.value == "objective_c"
        assert ProgrammingLanguage.PHP.value == "php"
        assert ProgrammingLanguage.RUBY.value == "ruby"
        assert ProgrammingLanguage.PERL.value == "perl"
        assert ProgrammingLanguage.SHELL.value == "shell"
        assert ProgrammingLanguage.POWERSHELL.value == "powershell"
        assert ProgrammingLanguage.SQL.value == "sql"
        assert ProgrammingLanguage.NOSQL.value == "nosql"
        assert ProgrammingLanguage.HASKELL.value == "haskell"
        assert ProgrammingLanguage.OCAML.value == "ocaml"
        assert ProgrammingLanguage.ELIXIR.value == "elixir"
        assert ProgrammingLanguage.ERLANG.value == "erlang"
        assert ProgrammingLanguage.FORTRAN.value == "fortran"
        assert ProgrammingLanguage.MATLAB_OCTAVE.value == "matlab_octave"
        assert ProgrammingLanguage.CUDA_GPU.value == "cuda_gpu"
        assert ProgrammingLanguage.WEBASSEMBLY.value == "webassembly"
        assert ProgrammingLanguage.RUST_ASYNC.value == "rust_async"


class TestSecurityFocus:
    """Test SecurityFocus dataclass"""
    
    def test_security_focus_creation(self):
        """Test creating SecurityFocus with data"""
        sf = SecurityFocus(
            primary_risks=["risk1", "risk2"],
            required_checks=["check1", "check2"]
        )
        assert sf.primary_risks == ["risk1", "risk2"]
        assert sf.required_checks == ["check1", "check2"]
    
    def test_security_focus_defaults(self):
        """Test SecurityFocus with default empty lists"""
        sf = SecurityFocus()
        assert sf.primary_risks == []
        assert sf.required_checks == []
    
    def test_security_focus_to_dict(self):
        """Test SecurityFocus to_dict method"""
        sf = SecurityFocus(
            primary_risks=["risk1"],
            required_checks=["check1"]
        )
        result = sf.to_dict()
        assert result == {
            'primary_risks': ["risk1"],
            'required_checks': ["check1"]
        }


class TestTestingDoctrine:
    """Test TestingDoctrine dataclass"""
    
    def test_testing_doctrine_creation(self):
        """Test creating TestingDoctrine with data"""
        td = TestingDoctrine(
            mandatory_tests=["test1"],
            optional_tests=["test2"],
            special_emphasis=["emphasis1"]
        )
        assert td.mandatory_tests == ["test1"]
        assert td.optional_tests == ["test2"]
        assert td.special_emphasis == ["emphasis1"]
    
    def test_testing_doctrine_defaults(self):
        """Test TestingDoctrine with default empty lists"""
        td = TestingDoctrine()
        assert td.mandatory_tests == []
        assert td.optional_tests == []
        assert td.special_emphasis == []
    
    def test_testing_doctrine_to_dict(self):
        """Test TestingDoctrine to_dict method"""
        td = TestingDoctrine(
            mandatory_tests=["test1"],
            optional_tests=["test2"],
            special_emphasis=["emphasis1"]
        )
        result = td.to_dict()
        assert result == {
            'mandatory_tests': ["test1"],
            'optional_tests': ["test2"],
            'special_emphasis': ["emphasis1"]
        }


class TestFloorSpecification:
    """Test FloorSpecification dataclass"""
    
    def test_floor_specification_post_init(self):
        """Test __post_init__ sets jurisdiction correctly"""
        floor = FloorSpecification(
            language=ProgrammingLanguage.PYTHON,
            floor_number=1,
            domain=["test"],
            architectural_constraints=["constraint1"],
            security_focus=SecurityFocus(),
            testing_doctrine=TestingDoctrine()
        )
        
        # Should only emit in own language
        assert floor.can_emit_languages == {ProgrammingLanguage.PYTHON}
        # Should only reason about own language
        assert floor.can_reason_about == {ProgrammingLanguage.PYTHON}
        # Should require contracts for all other languages
        assert ProgrammingLanguage.PYTHON not in floor.requires_contracts_for
        assert ProgrammingLanguage.RUST in floor.requires_contracts_for
        assert len(floor.requires_contracts_for) == 27  # 28 total - 1 (own language)
    
    def test_can_author_in_own_language(self):
        """Test can_author_in returns True for own language"""
        floor = FLOOR_1_PYTHON
        assert floor.can_author_in(ProgrammingLanguage.PYTHON) is True
    
    def test_can_author_in_other_language(self):
        """Test can_author_in returns False for other language"""
        floor = FLOOR_1_PYTHON
        assert floor.can_author_in(ProgrammingLanguage.RUST) is False
    
    def test_can_interpret_own_language(self):
        """Test can_interpret returns True for own language"""
        floor = FLOOR_1_PYTHON
        assert floor.can_interpret(ProgrammingLanguage.PYTHON) is True
    
    def test_can_interpret_other_language(self):
        """Test can_interpret returns False for other language"""
        floor = FLOOR_1_PYTHON
        assert floor.can_interpret(ProgrammingLanguage.JAVASCRIPT) is False
    
    def test_requires_contract_own_language(self):
        """Test requires_contract returns False for own language"""
        floor = FLOOR_1_PYTHON
        assert floor.requires_contract(ProgrammingLanguage.PYTHON) is False
    
    def test_requires_contract_other_language(self):
        """Test requires_contract returns True for other language"""
        floor = FLOOR_1_PYTHON
        assert floor.requires_contract(ProgrammingLanguage.GO) is True
    
    def test_validate_jurisdiction_author_allowed(self):
        """Test validate_jurisdiction for author action on own language"""
        floor = FLOOR_1_PYTHON
        is_legal, reason = floor.validate_jurisdiction("author", ProgrammingLanguage.PYTHON)
        assert is_legal is True
        assert reason is None
    
    def test_validate_jurisdiction_author_denied(self):
        """Test validate_jurisdiction for author action on other language"""
        floor = FLOOR_1_PYTHON
        is_legal, reason = floor.validate_jurisdiction("author", ProgrammingLanguage.RUST)
        assert is_legal is False
        assert "cannot author code in rust" in reason
    
    def test_validate_jurisdiction_interpret_allowed(self):
        """Test validate_jurisdiction for interpret action on own language"""
        floor = FLOOR_2_RUST
        is_legal, reason = floor.validate_jurisdiction("interpret", ProgrammingLanguage.RUST)
        assert is_legal is True
        assert reason is None
    
    def test_validate_jurisdiction_interpret_denied(self):
        """Test validate_jurisdiction for interpret action on other language"""
        floor = FLOOR_2_RUST
        is_legal, reason = floor.validate_jurisdiction("interpret", ProgrammingLanguage.PYTHON)
        assert is_legal is False
        assert "cannot interpret semantics of python" in reason
    
    def test_validate_jurisdiction_cross_language_own(self):
        """Test validate_jurisdiction for cross_language on own language"""
        floor = FLOOR_1_PYTHON
        is_legal, reason = floor.validate_jurisdiction("cross_language", ProgrammingLanguage.PYTHON)
        assert is_legal is True
        assert reason is None
    
    def test_validate_jurisdiction_cross_language_other(self):
        """Test validate_jurisdiction for cross_language on other language"""
        floor = FLOOR_1_PYTHON
        is_legal, reason = floor.validate_jurisdiction("cross_language", ProgrammingLanguage.TYPESCRIPT)
        assert is_legal is False
        assert "requires explicit contract" in reason
    
    def test_validate_jurisdiction_unknown_action(self):
        """Test validate_jurisdiction for unknown action"""
        floor = FLOOR_1_PYTHON
        is_legal, reason = floor.validate_jurisdiction("unknown_action", ProgrammingLanguage.PYTHON)
        assert is_legal is True
        assert reason is None
    
    def test_floor_to_dict(self):
        """Test FloorSpecification to_dict method"""
        floor = FLOOR_1_PYTHON
        result = floor.to_dict()
        
        assert result['language'] == 'python'
        assert result['floor_number'] == 1
        assert isinstance(result['domain'], list)
        assert isinstance(result['architectural_constraints'], list)
        assert isinstance(result['security_focus'], dict)
        assert isinstance(result['testing_doctrine'], dict)
        assert isinstance(result['can_emit_languages'], list)
        assert isinstance(result['can_reason_about'], list)
        assert isinstance(result['requires_contracts_for'], list)


class TestAllFloorSpecifications:
    """Test all 28 floor specifications exist and are valid"""
    
    def test_floor_1_python(self):
        """Test FLOOR_1_PYTHON configuration"""
        floor = FLOOR_1_PYTHON
        assert floor.language == ProgrammingLanguage.PYTHON
        assert floor.floor_number == 1
        assert len(floor.domain) > 0
        assert len(floor.architectural_constraints) > 0
        assert len(floor.security_focus.primary_risks) > 0
        assert len(floor.testing_doctrine.mandatory_tests) > 0
    
    def test_floor_2_rust(self):
        """Test FLOOR_2_RUST configuration"""
        floor = FLOOR_2_RUST
        assert floor.language == ProgrammingLanguage.RUST
        assert floor.floor_number == 2
        assert len(floor.domain) > 0
    
    def test_floor_3_c(self):
        """Test FLOOR_3_C configuration"""
        floor = FLOOR_3_C
        assert floor.language == ProgrammingLanguage.C
        assert floor.floor_number == 3
        assert len(floor.domain) > 0
    
    def test_floor_4_cpp(self):
        """Test FLOOR_4_CPP configuration"""
        floor = FLOOR_4_CPP
        assert floor.language == ProgrammingLanguage.CPP
        assert floor.floor_number == 4
        assert len(floor.domain) > 0
    
    def test_floor_5_javascript(self):
        """Test FLOOR_5_JAVASCRIPT configuration"""
        floor = FLOOR_5_JAVASCRIPT
        assert floor.language == ProgrammingLanguage.JAVASCRIPT
        assert floor.floor_number == 5
        assert len(floor.domain) > 0
    
    def test_floor_6_typescript(self):
        """Test FLOOR_6_TYPESCRIPT configuration"""
        floor = FLOOR_6_TYPESCRIPT
        assert floor.language == ProgrammingLanguage.TYPESCRIPT
        assert floor.floor_number == 6
        assert len(floor.domain) > 0
    
    def test_floor_7_go(self):
        """Test FLOOR_7_GO configuration"""
        floor = FLOOR_7_GO
        assert floor.language == ProgrammingLanguage.GO
        assert floor.floor_number == 7
        assert len(floor.domain) > 0
    
    def test_floor_8_sql(self):
        """Test FLOOR_8_SQL configuration"""
        floor = FLOOR_8_SQL
        assert floor.language == ProgrammingLanguage.SQL
        assert floor.floor_number == 8
        assert len(floor.domain) > 0
    
    def test_floor_9_shell(self):
        """Test FLOOR_9_SHELL configuration"""
        floor = FLOOR_9_SHELL
        assert floor.language == ProgrammingLanguage.SHELL
        assert floor.floor_number == 9
        assert len(floor.domain) > 0
    
    def test_floor_10_java(self):
        """Test FLOOR_10_JAVA configuration"""
        floor = FLOOR_10_JAVA
        assert floor.language == ProgrammingLanguage.JAVA
        assert floor.floor_number == 10
        assert len(floor.domain) > 0
    
    def test_floor_11_kotlin(self):
        """Test FLOOR_11_KOTLIN configuration"""
        floor = FLOOR_11_KOTLIN
        assert floor.language == ProgrammingLanguage.KOTLIN
        assert floor.floor_number == 11
        assert len(floor.domain) > 0
    
    def test_floor_12_scala(self):
        """Test FLOOR_12_SCALA configuration"""
        floor = FLOOR_12_SCALA
        assert floor.language == ProgrammingLanguage.SCALA
        assert floor.floor_number == 12
        assert len(floor.domain) > 0
    
    def test_floor_13_swift(self):
        """Test FLOOR_13_SWIFT configuration"""
        floor = FLOOR_13_SWIFT
        assert floor.language == ProgrammingLanguage.SWIFT
        assert floor.floor_number == 13
        assert len(floor.domain) > 0
    
    def test_floor_14_objective_c(self):
        """Test FLOOR_14_OBJECTIVE_C configuration"""
        floor = FLOOR_14_OBJECTIVE_C
        assert floor.language == ProgrammingLanguage.OBJECTIVE_C
        assert floor.floor_number == 14
        assert len(floor.domain) > 0
    
    def test_floor_15_php(self):
        """Test FLOOR_15_PHP configuration"""
        floor = FLOOR_15_PHP
        assert floor.language == ProgrammingLanguage.PHP
        assert floor.floor_number == 15
        assert len(floor.domain) > 0
    
    def test_floor_16_ruby(self):
        """Test FLOOR_16_RUBY configuration"""
        floor = FLOOR_16_RUBY
        assert floor.language == ProgrammingLanguage.RUBY
        assert floor.floor_number == 16
        assert len(floor.domain) > 0
    
    def test_floor_17_perl(self):
        """Test FLOOR_17_PERL configuration"""
        floor = FLOOR_17_PERL
        assert floor.language == ProgrammingLanguage.PERL
        assert floor.floor_number == 17
        assert len(floor.domain) > 0
    
    def test_floor_18_powershell(self):
        """Test FLOOR_18_POWERSHELL configuration"""
        floor = FLOOR_18_POWERSHELL
        assert floor.language == ProgrammingLanguage.POWERSHELL
        assert floor.floor_number == 18
        assert len(floor.domain) > 0
    
    def test_floor_19_nosql(self):
        """Test FLOOR_19_NOSQL configuration"""
        floor = FLOOR_19_NOSQL
        assert floor.language == ProgrammingLanguage.NOSQL
        assert floor.floor_number == 19
        assert len(floor.domain) > 0
    
    def test_floor_20_haskell(self):
        """Test FLOOR_20_HASKELL configuration"""
        floor = FLOOR_20_HASKELL
        assert floor.language == ProgrammingLanguage.HASKELL
        assert floor.floor_number == 20
        assert len(floor.domain) > 0
    
    def test_floor_21_ocaml(self):
        """Test FLOOR_21_OCAML configuration"""
        floor = FLOOR_21_OCAML
        assert floor.language == ProgrammingLanguage.OCAML
        assert floor.floor_number == 21
        assert len(floor.domain) > 0
    
    def test_floor_22_elixir(self):
        """Test FLOOR_22_ELIXIR configuration"""
        floor = FLOOR_22_ELIXIR
        assert floor.language == ProgrammingLanguage.ELIXIR
        assert floor.floor_number == 22
        assert len(floor.domain) > 0
    
    def test_floor_23_erlang(self):
        """Test FLOOR_23_ERLANG configuration"""
        floor = FLOOR_23_ERLANG
        assert floor.language == ProgrammingLanguage.ERLANG
        assert floor.floor_number == 23
        assert len(floor.domain) > 0
    
    def test_floor_24_fortran(self):
        """Test FLOOR_24_FORTRAN configuration"""
        floor = FLOOR_24_FORTRAN
        assert floor.language == ProgrammingLanguage.FORTRAN
        assert floor.floor_number == 24
        assert len(floor.domain) > 0
    
    def test_floor_25_matlab_octave(self):
        """Test FLOOR_25_MATLAB_OCTAVE configuration"""
        floor = FLOOR_25_MATLAB_OCTAVE
        assert floor.language == ProgrammingLanguage.MATLAB_OCTAVE
        assert floor.floor_number == 25
        assert len(floor.domain) > 0
    
    def test_floor_26_cuda_gpu(self):
        """Test FLOOR_26_CUDA_GPU configuration"""
        floor = FLOOR_26_CUDA_GPU
        assert floor.language == ProgrammingLanguage.CUDA_GPU
        assert floor.floor_number == 26
        assert len(floor.domain) > 0
    
    def test_floor_27_webassembly(self):
        """Test FLOOR_27_WEBASSEMBLY configuration"""
        floor = FLOOR_27_WEBASSEMBLY
        assert floor.language == ProgrammingLanguage.WEBASSEMBLY
        assert floor.floor_number == 27
        assert len(floor.domain) > 0
    
    def test_floor_28_rust_async(self):
        """Test FLOOR_28_RUST_ASYNC configuration"""
        floor = FLOOR_28_RUST_ASYNC
        assert floor.language == ProgrammingLanguage.RUST_ASYNC
        assert floor.floor_number == 28
        assert len(floor.domain) > 0


class TestAllFloorsRegistry:
    """Test ALL_FLOORS registry"""
    
    def test_all_floors_contains_all_languages(self):
        """Test ALL_FLOORS contains all 28 languages"""
        assert len(ALL_FLOORS) == 28
        
        # Verify all languages are present
        for language in ProgrammingLanguage:
            assert language in ALL_FLOORS
    
    def test_all_floors_mappings(self):
        """Test ALL_FLOORS maps languages to correct floors"""
        assert ALL_FLOORS[ProgrammingLanguage.PYTHON] == FLOOR_1_PYTHON
        assert ALL_FLOORS[ProgrammingLanguage.RUST] == FLOOR_2_RUST
        assert ALL_FLOORS[ProgrammingLanguage.C] == FLOOR_3_C
        assert ALL_FLOORS[ProgrammingLanguage.CPP] == FLOOR_4_CPP
        assert ALL_FLOORS[ProgrammingLanguage.JAVASCRIPT] == FLOOR_5_JAVASCRIPT
        assert ALL_FLOORS[ProgrammingLanguage.TYPESCRIPT] == FLOOR_6_TYPESCRIPT
        assert ALL_FLOORS[ProgrammingLanguage.GO] == FLOOR_7_GO
        assert ALL_FLOORS[ProgrammingLanguage.SQL] == FLOOR_8_SQL
        assert ALL_FLOORS[ProgrammingLanguage.SHELL] == FLOOR_9_SHELL
        assert ALL_FLOORS[ProgrammingLanguage.JAVA] == FLOOR_10_JAVA
        assert ALL_FLOORS[ProgrammingLanguage.KOTLIN] == FLOOR_11_KOTLIN
        assert ALL_FLOORS[ProgrammingLanguage.SCALA] == FLOOR_12_SCALA
        assert ALL_FLOORS[ProgrammingLanguage.SWIFT] == FLOOR_13_SWIFT
        assert ALL_FLOORS[ProgrammingLanguage.OBJECTIVE_C] == FLOOR_14_OBJECTIVE_C
        assert ALL_FLOORS[ProgrammingLanguage.PHP] == FLOOR_15_PHP
        assert ALL_FLOORS[ProgrammingLanguage.RUBY] == FLOOR_16_RUBY
        assert ALL_FLOORS[ProgrammingLanguage.PERL] == FLOOR_17_PERL
        assert ALL_FLOORS[ProgrammingLanguage.POWERSHELL] == FLOOR_18_POWERSHELL
        assert ALL_FLOORS[ProgrammingLanguage.NOSQL] == FLOOR_19_NOSQL
        assert ALL_FLOORS[ProgrammingLanguage.HASKELL] == FLOOR_20_HASKELL
        assert ALL_FLOORS[ProgrammingLanguage.OCAML] == FLOOR_21_OCAML
        assert ALL_FLOORS[ProgrammingLanguage.ELIXIR] == FLOOR_22_ELIXIR
        assert ALL_FLOORS[ProgrammingLanguage.ERLANG] == FLOOR_23_ERLANG
        assert ALL_FLOORS[ProgrammingLanguage.FORTRAN] == FLOOR_24_FORTRAN
        assert ALL_FLOORS[ProgrammingLanguage.MATLAB_OCTAVE] == FLOOR_25_MATLAB_OCTAVE
        assert ALL_FLOORS[ProgrammingLanguage.CUDA_GPU] == FLOOR_26_CUDA_GPU
        assert ALL_FLOORS[ProgrammingLanguage.WEBASSEMBLY] == FLOOR_27_WEBASSEMBLY
        assert ALL_FLOORS[ProgrammingLanguage.RUST_ASYNC] == FLOOR_28_RUST_ASYNC


class TestModuleFunctions:
    """Test module-level functions"""
    
    def test_get_floor_specification(self):
        """Test get_floor_specification returns correct floor"""
        floor = get_floor_specification(ProgrammingLanguage.PYTHON)
        assert floor == FLOOR_1_PYTHON
        assert floor.language == ProgrammingLanguage.PYTHON
    
    def test_get_floor_specification_all_languages(self):
        """Test get_floor_specification for all languages"""
        for language in ProgrammingLanguage:
            floor = get_floor_specification(language)
            assert floor.language == language
    
    def test_validate_floor_uniformity(self):
        """Test validate_floor_uniformity returns True"""
        assert validate_floor_uniformity() is True
    
    def test_get_all_floors(self):
        """Test get_all_floors returns all 28 floors"""
        floors = get_all_floors()
        assert len(floors) == 28
        assert isinstance(floors, list)
        
        # Verify all are FloorSpecification instances
        for floor in floors:
            assert isinstance(floor, FloorSpecification)
    
    def test_get_all_floors_contains_all_languages(self):
        """Test get_all_floors contains all languages"""
        floors = get_all_floors()
        languages = {floor.language for floor in floors}
        assert languages == set(ProgrammingLanguage)
    
    def test_route_directive_to_floor(self):
        """Test route_directive_to_floor routes to correct floor"""
        floor = route_directive_to_floor(ProgrammingLanguage.RUST)
        assert floor == FLOOR_2_RUST
        assert floor.language == ProgrammingLanguage.RUST
    
    def test_route_directive_to_floor_all_languages(self):
        """Test route_directive_to_floor for all languages"""
        for language in ProgrammingLanguage:
            floor = route_directive_to_floor(language)
            assert floor.language == language
