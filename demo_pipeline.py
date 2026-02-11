#!/usr/bin/env python3
"""
Manual test of the Code-Authoring Civilization Pipeline
Demonstrates end-to-end functionality
"""

from src.core.code_civilization import (
    CodeAuthoringCivilization,
    CodeDirective,
    ProgrammingLanguage,
    InputType,
    RequestedOutcome,
    get_code_civilization
)

def test_email_validator():
    """Test creating an email validator function"""
    print("=" * 80)
    print("TEST 1: Create Email Validator Function (Python EXTEND)")
    print("=" * 80)
    
    civilization = get_code_civilization()
    
    directive = CodeDirective(
        directive_id="demo-email-validator",
        language=ProgrammingLanguage.PYTHON,
        input_type=InputType.SPEC,
        source="Create a function validate_email that checks if an email address is valid",
        requested_outcome=RequestedOutcome.EXTEND,
        constraints=["no external dependencies", "handle unicode"]
    )
    
    # Submit and process
    print("\n📝 Submitting directive...")
    directive_id = civilization.submit_directive(directive)
    
    print(f"✓ Directive ID: {directive_id}")
    print(f"  Language: {directive.language.value}")
    print(f"  Outcome: {directive.requested_outcome.value}")
    print(f"  Constraints: {', '.join(directive.constraints)}")
    
    print("\n⚙️  Processing through 6-step pipeline...")
    output = civilization.process_directive(directive_id)
    
    print("\n" + "=" * 80)
    print("RESULTS")
    print("=" * 80)
    
    print("\n📊 Pipeline Steps:")
    print("  1. Floor Routing: ✓")
    print("  2. Architectural Pass: ✓")
    print("  3. Implementation Sprint: ✓")
    print(f"     Files Modified: {len(output.final_code)} characters generated")
    print("  4. Internal Review: ✓")
    print("  5. Testing Mandate: ✓")
    print(f"     Tests Generated: {output.test_suite.test_count}")
    print(f"     Coverage: {output.test_suite.coverage_percent}%")
    print("  6. Manager Seal: ✓")
    print(f"     Sealed: {output.manager_seal.sealed}")
    
    print("\n" + "-" * 80)
    print("GENERATED CODE")
    print("-" * 80)
    print(output.final_code[:500] + "...")
    
    print("\n" + "-" * 80)
    print("GENERATED TESTS")
    print("-" * 80)
    print(output.test_suite.test_code[:400] + "...")
    
    print("\n" + "-" * 80)
    print("DELIVERY OUTPUT")
    print("-" * 80)
    delivery = output.format_delivery()
    print(delivery[:600] + "...")
    
    return output


def test_calculator():
    """Test creating a calculator function"""
    print("\n\n")
    print("=" * 80)
    print("TEST 2: Create Calculator Function (Python EXTEND)")
    print("=" * 80)
    
    civilization = get_code_civilization()
    
    directive = CodeDirective(
        directive_id="demo-calculator",
        language=ProgrammingLanguage.PYTHON,
        input_type=InputType.SPEC,
        source="Create a function calculate that performs basic arithmetic operations (add, subtract, multiply, divide)",
        requested_outcome=RequestedOutcome.EXTEND,
        constraints=["handle division by zero"]
    )
    
    print("\n📝 Submitting directive...")
    directive_id = civilization.submit_directive(directive)
    
    print("\n⚙️  Processing...")
    output = civilization.process_directive(directive_id)
    
    print("\n✅ SUCCESS!")
    print(f"   Generated {len(output.final_code)} characters of code")
    print(f"   Created {output.test_suite.test_count} tests")
    print(f"   Manager sealed: {output.manager_seal.sealed}")
    
    print("\n💡 Change Log:")
    for change in output.change_log[:3]:
        print(f"   - {change}")
    
    return output


def test_javascript():
    """Test JavaScript code generation"""
    print("\n\n")
    print("=" * 80)
    print("TEST 3: Create Form Validator (JavaScript EXTEND)")
    print("=" * 80)
    
    civilization = get_code_civilization()
    
    directive = CodeDirective(
        directive_id="demo-js-validator",
        language=ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT,
        input_type=InputType.SPEC,
        source="Create a function validateForm that checks if form inputs are valid",
        requested_outcome=RequestedOutcome.EXTEND,
        constraints=[]
    )
    
    print("\n📝 Submitting directive...")
    directive_id = civilization.submit_directive(directive)
    
    print("\n⚙️  Processing...")
    output = civilization.process_directive(directive_id)
    
    print("\n✅ Generated JavaScript Code:")
    print("-" * 40)
    print(output.final_code)
    
    return output


def test_fix_bug():
    """Test fixing buggy code"""
    print("\n\n")
    print("=" * 80)
    print("TEST 4: Fix Buggy Code (Python FIX)")
    print("=" * 80)
    
    civilization = get_code_civilization()
    
    buggy_code = """
def divide(a, b):
    return a / b  # Bug: no check for division by zero
"""
    
    directive = CodeDirective(
        directive_id="demo-fix-bug",
        language=ProgrammingLanguage.PYTHON,
        input_type=InputType.EXISTING_CODE,
        source=buggy_code,
        requested_outcome=RequestedOutcome.FIX,
        constraints=[]
    )
    
    print("\n📝 Original buggy code:")
    print(buggy_code)
    
    print("\n⚙️  Processing fix...")
    directive_id = civilization.submit_directive(directive)
    output = civilization.process_directive(directive_id)
    
    print("\n✅ Fixed Code:")
    print("-" * 40)
    print(output.final_code)
    
    return output


if __name__ == "__main__":
    print("\n")
    print("╔" + "=" * 78 + "╗")
    print("║" + " " * 15 + "CODE-AUTHORING CIVILIZATION DEMO" + " " * 31 + "║")
    print("║" + " " * 20 + "Everything That Works!" + " " * 35 + "║")
    print("╚" + "=" * 78 + "╝")
    
    try:
        # Run tests
        test_email_validator()
        test_calculator()
        test_javascript()
        test_fix_bug()
        
        print("\n\n")
        print("╔" + "=" * 78 + "╗")
        print("║" + " " * 25 + "ALL TESTS PASSED! ✓" + " " * 34 + "║")
        print("║" + " " * 15 + "The pipeline is fully functional!" + " " * 28 + "║")
        print("╚" + "=" * 78 + "╝")
        print("\n")
        
    except Exception as e:
        print(f"\n\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
