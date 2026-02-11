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
