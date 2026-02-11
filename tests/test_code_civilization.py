"""
Tests for Code-Authoring Civilization pipeline
"""
import pytest
from src.core.code_civilization import (
    CodeAuthoringCivilization,
    CodeDirective,
    ProgrammingLanguage,
    InputType,
    RequestedOutcome,
    get_code_civilization
)


class TestArchitecturalPass:
    """Test Step 2: Architectural Pass"""
    
    def test_python_extend_directive(self):
        """Test architectural analysis for Python EXTEND directive"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create a function to validate email addresses",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        
        assert arch.approved is True
        assert "Type hints" in arch.invariants
        assert "PEP 8 compliance" in arch.invariants
        assert "Backward compatibility" in arch.invariants
        assert arch.rejected_reason is None
    
    def test_conflicting_constraints(self):
        """Test rejection of impossible requirements"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-002",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="import numpy as np\ndata = np.array([1, 2, 3])",
            requested_outcome=RequestedOutcome.FIX,
            constraints=["no external dependencies"]
        )
        
        arch = civilization._architectural_pass(directive)
        
        assert arch.approved is False
        assert arch.rejected_reason is not None
        assert "no external dependencies" in arch.rejected_reason.lower()
    
    def test_empty_spec(self):
        """Test rejection of empty specification"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-003",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        
        assert arch.approved is False
        assert "empty" in arch.rejected_reason.lower()


class TestImplementationSprint:
    """Test Step 3: Implementation Sprint"""
    
    def test_python_function_generation(self):
        """Test Python function generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-004",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create a function validate_email that checks if email is valid",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert impl.code is not None
        assert len(impl.code) > 50  # Not just a stub
        assert "def " in impl.code  # Contains function definition
        assert '"""' in impl.code  # Contains docstring
        assert len(impl.files_modified) > 0
        assert impl.files_modified[0].endswith('.py')
    
    def test_javascript_generation(self):
        """Test JavaScript code generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-005",
            language=ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT,
            input_type=InputType.SPEC,
            source="Create a function to validate user input",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert impl.code is not None
        assert "function" in impl.code
        assert impl.files_modified[0].endswith('.js')
    
    def test_fix_outcome(self):
        """Test FIX outcome generates appropriate code"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-006",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="def broken_func():\n    return 1/0",
            requested_outcome=RequestedOutcome.FIX,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "try:" in impl.code or "except" in impl.code
        assert "error" in impl.code.lower() or "Error" in impl.code


class TestInternalReview:
    """Test Step 4: Internal Review"""
    
    def test_approve_good_code(self):
        """Test that good code passes review"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def validate_email(email: str) -> bool:
    """
    Validate email address format.
    
    Args:
        email: Email address to validate
    
    Returns:
        True if valid, False otherwise
    """
    if email is None:
        raise ValueError("Email cannot be None")
    return "@" in email and "." in email
''',
            files_modified=["validator.py"]
        )
        
        review = civilization._internal_review(impl)
        
        assert review.approved is True
        assert len(review.violations) == 0
    
    def test_reject_empty_code(self):
        """Test that empty code is rejected"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code="",
            files_modified=["empty.py"]
        )
        
        review = civilization._internal_review(impl)
        
        assert review.approved is False
        assert len(review.violations) > 0
    
    def test_detect_missing_docstrings(self):
        """Test detection of missing docstrings"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def some_function(x):
    return x * 2
''',
            files_modified=["func.py"]
        )
        
        review = civilization._internal_review(impl)
        
        assert review.approved is False
        assert any("docstring" in v.lower() or "documentation" in v.lower() 
                   for v in review.violations)


class TestTestingMandate:
    """Test Step 5: Testing Mandate"""
    
    def test_generate_python_tests(self):
        """Test generation of Python unit tests"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def calculate_sum(a: int, b: int) -> int:
    """Calculate sum of two numbers."""
    return a + b

def validate_input(data: str) -> bool:
    """Validate input data."""
    return len(data) > 0
''',
            files_modified=["calculator.py"]
        )
        
        tests = civilization._testing_mandate(impl)
        
        assert tests.test_count > 0
        assert "pytest" in tests.test_code or "assert" in tests.test_code
        assert "test_" in tests.test_code
        assert tests.coverage_percent > 0
        assert tests.all_pass is True
    
    def test_generate_edge_case_tests(self):
        """Test that edge case tests are generated"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def process_data(data):
    """Process the input data."""
    if data is None:
        raise ValueError("Data cannot be None")
    return data
''',
            files_modified=["processor.py"]
        )
        
        tests = civilization._testing_mandate(impl)
        
        assert "edge" in tests.test_code.lower() or "None" in tests.test_code
        assert "pytest.raises" in tests.test_code or "ValueError" in tests.test_code


class TestEndToEndPipeline:
    """Test complete pipeline from directive to output"""
    
    def test_full_pipeline_python_extend(self):
        """Test complete pipeline for Python EXTEND"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="e2e-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create a function to calculate factorial of a number",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=["no recursion"]
        )
        
        # Submit and process
        directive_id = civilization.submit_directive(directive)
        output = civilization.process_directive(directive_id)
        
        # Verify output
        assert output.final_code is not None
        assert len(output.final_code) > 0
        assert output.test_suite is not None
        assert output.test_suite.test_count > 0
        assert output.change_log is not None
        
        # Check if pipeline completed
        assert output.manager_seal is not None
        
        # Format delivery
        delivery = output.format_delivery()
        assert "✔" in delivery or "FINAL CODE" in delivery
    
    def test_pipeline_with_rejection(self):
        """Test that impossible requirements are rejected"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="e2e-002",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="",  # Empty source
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        directive_id = civilization.submit_directive(directive)
        
        # Should raise error for rejected directives
        with pytest.raises(ValueError) as exc_info:
            output = civilization.process_directive(directive_id)
        
        # Verify error message mentions rejection
        assert "rejection" in str(exc_info.value).lower() or "empty" in str(exc_info.value).lower()


class TestPurposeConstitution:
    """Test Purpose Constitution validation"""
    
    def test_is_action_legal_forbidden_actions(self):
        """Test detection of forbidden actions"""
        from src.core.code_civilization import PurposeConstitution
        
        # Test forbidden actions
        assert not PurposeConstitution.is_action_legal("Speculative features", True)
        assert not PurposeConstitution.is_action_legal("Self-initiated projects", True)
        assert not PurposeConstitution.is_action_legal("Adding helpful side quests", True)
        assert not PurposeConstitution.is_action_legal("Unrelated optimizations", True)
    
    def test_is_action_legal_not_advancing_correctness(self):
        """Test that actions not advancing correctness are illegal"""
        from src.core.code_civilization import PurposeConstitution
        
        assert not PurposeConstitution.is_action_legal("Write code", False)
        assert PurposeConstitution.is_action_legal("Fix bug", True)
    
    def test_validate_purpose_compliance_missing_directive(self):
        """Test validation fails without code directive"""
        from src.core.code_civilization import PurposeConstitution
        
        context = {'target_artifact': 'code'}
        assert not PurposeConstitution.validate_purpose_compliance(context)
    
    def test_validate_purpose_compliance_missing_artifact(self):
        """Test validation fails without target artifact"""
        from src.core.code_civilization import PurposeConstitution
        
        directive = CodeDirective(
            directive_id="test",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="test",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        context = {'code_directive': directive}
        assert not PurposeConstitution.validate_purpose_compliance(context)
    
    def test_validate_purpose_compliance_forbidden_action(self):
        """Test validation fails with forbidden actions"""
        from src.core.code_civilization import PurposeConstitution
        
        directive = CodeDirective(
            directive_id="test",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="test",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        context = {
            'code_directive': directive,
            'target_artifact': 'code',
            'action': 'Speculative features'
        }
        assert not PurposeConstitution.validate_purpose_compliance(context)


class TestLanguageFloor:
    """Test Language Floor functionality"""
    
    def test_can_author_same_language(self):
        """Test floor can author its own language"""
        from src.core.code_civilization import LanguageFloor
        
        floor = LanguageFloor(1, ProgrammingLanguage.PYTHON, "Runtime")
        assert floor.can_author(ProgrammingLanguage.PYTHON)
    
    def test_can_author_different_language(self):
        """Test floor cannot author different language"""
        from src.core.code_civilization import LanguageFloor
        
        floor = LanguageFloor(1, ProgrammingLanguage.PYTHON, "Runtime")
        assert not floor.can_author(ProgrammingLanguage.RUST)
    
    def test_can_interpret_same_language(self):
        """Test floor can interpret its own language"""
        from src.core.code_civilization import LanguageFloor
        
        floor = LanguageFloor(1, ProgrammingLanguage.PYTHON, "Runtime")
        assert floor.can_interpret(ProgrammingLanguage.PYTHON)
    
    def test_can_interpret_different_language(self):
        """Test floor cannot interpret different language"""
        from src.core.code_civilization import LanguageFloor
        
        floor = LanguageFloor(1, ProgrammingLanguage.PYTHON, "Runtime")
        assert not floor.can_interpret(ProgrammingLanguage.RUST)
    
    def test_to_dict(self):
        """Test floor serialization"""
        from src.core.code_civilization import LanguageFloor
        
        floor = LanguageFloor(1, ProgrammingLanguage.PYTHON, "Runtime")
        data = floor.to_dict()
        assert data['floor_number'] == 1
        assert data['language'] == 'python'
        assert data['jurisdiction'] == 'Runtime'


class TestDataclassSerialization:
    """Test serialization methods for dataclasses"""
    
    def test_code_directive_to_dict(self):
        """Test CodeDirective serialization"""
        directive = CodeDirective(
            directive_id="test-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="test source",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=["constraint1"]
        )
        data = directive.to_dict()
        assert data['directive_id'] == "test-001"
        assert data['language'] == 'python'
        assert data['inputType'] == 'spec'
        assert data['source'] == "test source"
        assert data['requestedOutcome'] == 'extend'
        assert data['constraints'] == ["constraint1"]
    
    def test_architectural_decision_to_dict(self):
        """Test ArchitecturalDecision serialization"""
        from src.core.code_civilization import ArchitecturalDecision
        
        arch = ArchitecturalDecision(
            invariants=["Type hints"],
            rejected_reason=None,
            approved=True
        )
        data = arch.to_dict()
        assert data['invariants'] == ["Type hints"]
        assert data['rejected_reason'] is None
        assert data['approved'] is True
    
    def test_implementation_output_to_dict(self):
        """Test ImplementationOutput serialization"""
        from src.core.code_civilization import ImplementationOutput
        
        impl = ImplementationOutput(
            code="def test(): pass",
            files_modified=["test.py"]
        )
        data = impl.to_dict()
        assert data['code'] == "def test(): pass"
        assert data['files_modified'] == ["test.py"]
    
    def test_review_decision_to_dict(self):
        """Test ReviewDecision serialization"""
        from src.core.code_civilization import ReviewDecision
        
        review = ReviewDecision(
            approved=True,
            violations=[],
            recommendations=["Add tests"]
        )
        data = review.to_dict()
        assert data['approved'] is True
        assert data['violations'] == []
        assert data['recommendations'] == ["Add tests"]
    
    def test_test_suite_to_dict(self):
        """Test TestSuite serialization"""
        from src.core.code_civilization import TestSuite
        
        suite = TestSuite(
            test_code="def test(): pass",
            test_count=5,
            coverage_percent=85.5,
            all_pass=True
        )
        data = suite.to_dict()
        assert data['test_code'] == "def test(): pass"
        assert data['test_count'] == 5
        assert data['coverage_percent'] == 85.5
        assert data['all_pass'] is True
    
    def test_manager_seal_to_dict(self):
        """Test ManagerSeal serialization"""
        from src.core.code_civilization import ManagerSeal
        
        seal = ManagerSeal(
            contract_satisfied=True,
            tests_pass=True,
            no_unresolved_dissent=True
        )
        seal.seal()
        data = seal.to_dict()
        assert data['contract_satisfied'] is True
        assert data['tests_pass'] is True
        assert data['no_unresolved_dissent'] is True
        assert data['sealed'] is True
    
    def test_code_output_to_dict(self):
        """Test CodeOutput serialization"""
        from src.core.code_civilization import (
            CodeOutput, TestSuite, ManagerSeal
        )
        
        suite = TestSuite(
            test_code="def test(): pass",
            test_count=1,
            coverage_percent=100.0,
            all_pass=True
        )
        seal = ManagerSeal(
            contract_satisfied=True,
            tests_pass=True,
            no_unresolved_dissent=True
        )
        output = CodeOutput(
            directive_id="test",
            final_code="def func(): pass",
            test_suite=suite,
            change_log=["Added function"],
            manager_seal=seal
        )
        data = output.to_dict()
        assert data['directive_id'] == "test"
        assert data['final_code'] == "def func(): pass"
        assert 'test_suite' in data
        assert 'manager_seal' in data
        assert data['change_log'] == ["Added function"]


class TestArchitecturalPassLanguages:
    """Test architectural pass for different languages"""
    
    def test_rust_architectural_pass(self):
        """Test Rust-specific architectural decisions"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="rust-001",
            language=ProgrammingLanguage.RUST,
            input_type=InputType.SPEC,
            source="Create a safe memory function",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        assert arch.approved is True
        assert "Memory safety" in arch.invariants
        assert "Ownership rules" in arch.invariants
    
    def test_javascript_typescript_architectural_pass(self):
        """Test JavaScript/TypeScript architectural decisions"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="js-001",
            language=ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT,
            input_type=InputType.SPEC,
            source="Create a validation function",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        assert arch.approved is True
        assert "Type safety (TypeScript)" in arch.invariants
    
    def test_other_language_architectural_pass(self):
        """Test architectural pass for other languages"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="go-001",
            language=ProgrammingLanguage.GO,
            input_type=InputType.SPEC,
            source="Create a function",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        assert arch.approved is True
        assert "Language-specific best practices" in arch.invariants
    
    def test_refactor_outcome_invariants(self):
        """Test REFACTOR outcome invariants"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="refactor-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="def old_func(): pass",
            requested_outcome=RequestedOutcome.REFACTOR,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        assert arch.approved is True
        assert "No behavioral changes" in arch.invariants
        assert "Improve structure or readability" in arch.invariants
    
    def test_audit_outcome_invariants(self):
        """Test AUDIT outcome invariants"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="audit-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="def func(): pass",
            requested_outcome=RequestedOutcome.AUDIT,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        assert arch.approved is True
        assert "No code modifications" in arch.invariants
        assert "Report issues only" in arch.invariants


class TestImplementationSprintLanguages:
    """Test implementation sprint for different languages"""
    
    def test_rust_code_generation(self):
        """Test Rust code generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="rust-impl-001",
            language=ProgrammingLanguage.RUST,
            input_type=InputType.SPEC,
            source="Create a safe function",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert impl.code is not None
        assert "pub fn" in impl.code or "fn " in impl.code
        assert "Result" in impl.code
        assert impl.files_modified[0].endswith('.rs')
    
    def test_rust_non_extend_generation(self):
        """Test Rust code generation for non-EXTEND outcome"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="rust-fix-001",
            language=ProgrammingLanguage.RUST,
            input_type=InputType.EXISTING_CODE,
            source="fn broken() {}",
            requested_outcome=RequestedOutcome.FIX,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert impl.code is not None
        assert "fn implementation" in impl.code
    
    def test_other_language_generation(self):
        """Test generic code generation for unsupported languages"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="java-001",
            language=ProgrammingLanguage.JAVA,
            input_type=InputType.SPEC,
            source="Create a class",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert impl.code is not None
        assert "//" in impl.code
        assert "java" in impl.code


class TestPythonCodeGeneration:
    """Test Python code generation paths"""
    
    def test_python_class_generation(self):
        """Test Python class generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="class-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create a data validator class",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "class " in impl.code
        assert "__init__" in impl.code
    
    def test_python_refactor_generation(self):
        """Test Python refactor code generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="refactor-002",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="def messy_code(): pass",
            requested_outcome=RequestedOutcome.REFACTOR,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "refactored" in impl.code.lower()
        assert "_validate_input" in impl.code
        assert "_process_data" in impl.code
    
    def test_python_audit_generation(self):
        """Test Python audit report generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="audit-002",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.EXISTING_CODE,
            source="def code_to_audit(): pass",
            requested_outcome=RequestedOutcome.AUDIT,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "AUDIT_REPORT" in impl.code
        assert "status" in impl.code
        assert "recommendations" in impl.code
    
    def test_python_translate_generation(self):
        """Test Python translate/default generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="translate-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Translate this code",
            requested_outcome=RequestedOutcome.TRANSLATE,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "def implementation" in impl.code
        assert "pass" in impl.code


class TestJavaScriptCodeGeneration:
    """Test JavaScript code generation paths"""
    
    def test_javascript_non_extend(self):
        """Test JavaScript non-EXTEND generation"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="js-fix-001",
            language=ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT,
            input_type=InputType.EXISTING_CODE,
            source="function broken() {}",
            requested_outcome=RequestedOutcome.FIX,
            constraints=[]
        )
        
        arch = civilization._architectural_pass(directive)
        impl = civilization._implementation_sprint(directive, arch)
        
        assert "function implementation" in impl.code
        assert "return true" in impl.code


class TestFunctionExtraction:
    """Test function and class name extraction"""
    
    def test_extract_function_name_from_def(self):
        """Test extracting function name from def statement"""
        civilization = get_code_civilization()
        name = civilization._extract_function_name("def my_function():")
        assert name == "my_function"
    
    def test_extract_function_name_from_text(self):
        """Test extracting function name from natural language"""
        civilization = get_code_civilization()
        name = civilization._extract_function_name("Create a function validate_email for emails")
        assert name == "validate_email"
    
    def test_extract_function_name_returns_none(self):
        """Test function extraction returns None when not found"""
        civilization = get_code_civilization()
        name = civilization._extract_function_name("Some random text")
        assert name is None
    
    def test_extract_class_name_from_class_statement(self):
        """Test extracting class name from class statement"""
        civilization = get_code_civilization()
        name = civilization._extract_class_name("class MyClass:")
        assert name == "MyClass"
    
    def test_extract_class_name_returns_none(self):
        """Test class extraction returns None when not found"""
        civilization = get_code_civilization()
        name = civilization._extract_class_name("Some random text")
        assert name is None


class TestInternalReviewAdvanced:
    """Test advanced internal review scenarios"""
    
    def test_review_with_todo_comments(self):
        """Test review detects TODO comments"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def some_function(x):
    """Function with TODO."""
    # TODO: Need to add more logic here
    return x
''',
            files_modified=["func.py"]
        )
        
        review = civilization._internal_review(impl)
        # Should have recommendations for TODO
        assert any("TODO" in r for r in review.recommendations)
    
    def test_review_recommends_error_handling(self):
        """Test review recommends error handling for non-trivial code"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def complex_function(data):
    """
    A complex function without error handling.
    """
    result = []
    for item in data:
        result.append(item * 2)
    return result
''',
            files_modified=["func.py"]
        )
        
        review = civilization._internal_review(impl)
        assert any("error handling" in r.lower() for r in review.recommendations)
    
    def test_review_detects_camelcase_python(self):
        """Test review detects camelCase in Python"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
def myBadFunction(x):
    """Bad naming convention."""
    return x
''',
            files_modified=["bad.py"]
        )
        
        review = civilization._internal_review(impl)
        assert any("snake_case" in v for v in review.violations)
    
    def test_review_javascript_var_usage(self):
        """Test review detects var in JavaScript"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
function oldStyle() {
    var x = 10;
    return x;
}
''',
            files_modified=["old.js"]
        )
        
        review = civilization._internal_review(impl)
        assert any("var" in r.lower() for r in review.recommendations)
    
    def test_review_detects_long_lines(self):
        """Test review detects overly long lines"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        long_line = "x = " + "a" * 130
        impl = ImplementationOutput(
            code=f'''
def func():
    """Function with long line."""
    {long_line}
    return x
''',
            files_modified=["long.py"]
        )
        
        review = civilization._internal_review(impl)
        assert any("too long" in r.lower() for r in review.recommendations)
    
    def test_review_detects_high_complexity(self):
        """Test review detects high cyclomatic complexity"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        # Create code with many if/for/while statements
        complex_code = "def complex():\n"
        for i in range(12):
            complex_code += f"    if x{i}: pass\n"
        
        impl = ImplementationOutput(
            code=complex_code,
            files_modified=["complex.py"]
        )
        
        review = civilization._internal_review(impl)
        assert any("complexity" in r.lower() for r in review.recommendations)


class TestTestingMandateAdvanced:
    """Test advanced testing mandate scenarios"""
    
    def test_generate_class_tests(self):
        """Test generation of class instantiation tests"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
class DataProcessor:
    """Process data."""
    def __init__(self):
        self.data = []
''',
            files_modified=["processor.py"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert "test_dataprocessor_instantiation" in tests.test_code
        assert "DataProcessor()" in tests.test_code
    
    def test_generate_tests_no_functions(self):
        """Test generation when no functions are found"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
# Just some constants
CONSTANT_VALUE = 42
ANOTHER_CONSTANT = "test"
''',
            files_modified=["constants.py"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert tests.test_count >= 1
        assert "test_implementation_exists" in tests.test_code
    
    def test_generate_javascript_tests(self):
        """Test JavaScript test generation"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
function processData(data) {
    return data;
}

function validateInput(input) {
    return input !== null;
}
''',
            files_modified=["processor.js"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert "describe" in tests.test_code
        assert "expect" in tests.test_code
        assert tests.test_count >= 2
    
    def test_generate_javascript_tests_no_functions(self):
        """Test JavaScript test generation with no functions"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
const CONSTANT = 42;
const data = [1, 2, 3];
''',
            files_modified=["data.js"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert tests.test_count >= 1
        assert "describe" in tests.test_code
        assert "should exist" in tests.test_code
    
    def test_generate_generic_tests(self):
        """Test generic test generation for other languages"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code='''
public class Test {
    public void method() {}
}
''',
            files_modified=["Test.java"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert tests.test_count >= 1
        assert "test_implementation" in tests.test_code
    
    def test_coverage_calculation_empty_code(self):
        """Test coverage calculation with empty code"""
        from src.core.code_civilization import ImplementationOutput
        
        civilization = get_code_civilization()
        
        impl = ImplementationOutput(
            code="# Just a comment\n",
            files_modified=["empty.py"]
        )
        
        tests = civilization._testing_mandate(impl)
        assert tests.coverage_percent == 0.0


class TestProcessDirectiveErrors:
    """Test error handling in process_directive"""
    
    def test_directive_not_found(self):
        """Test error when directive not found"""
        civilization = get_code_civilization()
        
        with pytest.raises(ValueError) as exc_info:
            civilization.process_directive("nonexistent-id")
        
        assert "not found" in str(exc_info.value).lower()
    
    def test_no_floor_for_language(self):
        """Test error when no floor exists for language"""
        civilization = get_code_civilization()
        
        # Create a directive with an unsupported language
        directive = CodeDirective(
            directive_id="unsupported-001",
            language=ProgrammingLanguage.PHP,  # Not in initialized floors
            input_type=InputType.SPEC,
            source="Create a function",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        civilization.active_directives[directive.directive_id] = directive
        
        with pytest.raises(ValueError) as exc_info:
            civilization.process_directive(directive.directive_id)
        
        assert "no floor" in str(exc_info.value).lower()
    
    def test_review_failure(self):
        """Test error when review fails"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="review-fail-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create function with_no_docs",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        civilization.active_directives[directive.directive_id] = directive
        
        # Temporarily patch to simulate review failure
        original_review = civilization._internal_review
        
        def failing_review(impl):
            from src.core.code_civilization import ReviewDecision
            return ReviewDecision(
                approved=False,
                violations=["Critical violation"],
                recommendations=[]
            )
        
        civilization._internal_review = failing_review
        
        with pytest.raises(ValueError) as exc_info:
            civilization.process_directive(directive.directive_id)
        
        civilization._internal_review = original_review
        assert "review failed" in str(exc_info.value).lower()
    
    def test_tests_failure(self):
        """Test error when tests fail"""
        civilization = get_code_civilization()
        
        directive = CodeDirective(
            directive_id="test-fail-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Create a function that validates emails",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        civilization.active_directives[directive.directive_id] = directive
        
        # Temporarily patch to simulate test failure
        original_testing = civilization._testing_mandate
        
        def failing_tests(impl):
            from src.core.code_civilization import TestSuite
            return TestSuite(
                test_code="def test_fail(): assert False",
                test_count=1,
                coverage_percent=100.0,
                all_pass=False  # Tests fail
            )
        
        civilization._testing_mandate = failing_tests
        
        with pytest.raises(ValueError) as exc_info:
            civilization.process_directive(directive.directive_id)
        
        civilization._testing_mandate = original_testing
        assert "tests failed" in str(exc_info.value).lower()


class TestSubmitDirective:
    """Test directive submission"""
    
    def test_submit_violates_purpose(self):
        """Test submitting directive that violates purpose"""
        civilization = get_code_civilization()
        
        # Temporarily patch validate_purpose_compliance to return False
        from src.core.code_civilization import PurposeConstitution
        original_validate = PurposeConstitution.validate_purpose_compliance
        
        def always_fails(context):
            return False
        
        PurposeConstitution.validate_purpose_compliance = staticmethod(always_fails)
        
        directive = CodeDirective(
            directive_id="invalid-001",
            language=ProgrammingLanguage.PYTHON,
            input_type=InputType.SPEC,
            source="Some code",
            requested_outcome=RequestedOutcome.EXTEND,
            constraints=[]
        )
        
        with pytest.raises(ValueError) as exc_info:
            civilization.submit_directive(directive)
        
        PurposeConstitution.validate_purpose_compliance = staticmethod(original_validate)
        assert "violates Purpose Constitution" in str(exc_info.value)


class TestCodeOutputFormatting:
    """Test CodeOutput formatting"""
    
    def test_format_delivery_failed_checkboxes(self):
        """Test formatting when checkboxes fail"""
        from src.core.code_civilization import (
            CodeOutput, TestSuite, ManagerSeal
        )
        
        suite = TestSuite(
            test_code="def test(): pass",
            test_count=1,
            coverage_percent=100.0,
            all_pass=False  # Tests don't pass
        )
        seal = ManagerSeal(
            contract_satisfied=True,
            tests_pass=False,  # Fails
            no_unresolved_dissent=True
        )
        output = CodeOutput(
            directive_id="fail-001",
            final_code="def func(): pass",
            test_suite=suite,
            change_log=["Added function"],
            manager_seal=seal
        )
        
        delivery = output.format_delivery()
        assert "✗" in delivery
        assert "CODE NOT RELEASED" in delivery
