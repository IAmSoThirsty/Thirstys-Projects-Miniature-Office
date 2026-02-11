"""
Purpose Constitution - Code-Authoring Civilization
HARD BINDING: The system's exclusive purpose

This is law ABOVE all other laws.
"""
from enum import Enum
from typing import Dict, List, Optional
from dataclasses import dataclass, field


class PurposeConstitution:
    """
    Purpose Constitution (Hard Binding)
    
    Primary Civilizational Purpose:
    The system exists EXCLUSIVELY to transform user-supplied code directives 
    into correct, tested, auditable code artifacts.
    
    Consequences:
    - No speculative features
    - No self-initiated projects
    - No autonomous goal formation
    - No "helpful" side quests
    
    If an action does not advance code correctness or completeness, it is ILLEGAL.
    """
    
    PRIMARY_PURPOSE = "Transform user code directives into correct, tested, auditable code artifacts"
    
    FORBIDDEN_ACTIONS = [
        "Speculative features",
        "Self-initiated projects",
        "Autonomous goal formation",
        "Helpful side quests",
        "Unrelated optimizations",
        "Feature suggestions",
        "Architecture improvements beyond scope"
    ]
    
    @staticmethod
    def is_action_legal(action_description: str, advances_code_correctness: bool) -> bool:
        """
        Validate if an action is legal under the Purpose Constitution.
        
        If an action does not advance code correctness or completeness, it is illegal.
        """
        # Check if action is in forbidden list
        for forbidden in PurposeConstitution.FORBIDDEN_ACTIONS:
            if forbidden.lower() in action_description.lower():
                return False
        
        # Action must advance code correctness
        return advances_code_correctness
    
    @staticmethod
    def validate_purpose_compliance(context: Dict) -> bool:
        """
        Validate that the system is operating within its constitutional purpose.
        """
        # Must have code directive
        if 'code_directive' not in context:
            return False
        
        # Must be working toward code artifact
        if 'target_artifact' not in context:
            return False
        
        # Must not be doing forbidden actions
        action = context.get('action', '')
        for forbidden in PurposeConstitution.FORBIDDEN_ACTIONS:
            if forbidden.lower() in action.lower():
                return False
        
        return True


class ProgrammingLanguage(Enum):
    """
    Programming Language Floors
    Each floor has ONE-TO-ONE correspondence with a language
    """
    PYTHON = "python"
    RUST = "rust"
    C_CPP = "c_cpp"
    JAVASCRIPT_TYPESCRIPT = "javascript_typescript"
    GO = "go"
    SQL = "sql"
    JAVA = "java"
    CSHARP = "csharp"
    PHP = "php"
    RUBY = "ruby"


@dataclass
class LanguageFloor:
    """
    Floor = Programming Language (ONE-TO-ONE)
    
    The building is the codebase.
    
    No floor may:
    - Author code outside its language
    - Interpret semantics of another language
    
    Cross-language work must go through contracts.
    """
    floor_number: int
    language: ProgrammingLanguage
    jurisdiction: str  # e.g., "Runtime logic", "Performance/safety"
    
    def can_author(self, target_language: ProgrammingLanguage) -> bool:
        """Check if this floor can author code in target language"""
        return self.language == target_language
    
    def can_interpret(self, target_language: ProgrammingLanguage) -> bool:
        """Check if this floor can interpret semantics of target language"""
        return self.language == target_language
    
    def to_dict(self) -> Dict:
        return {
            'floor_number': self.floor_number,
            'language': self.language.value,
            'jurisdiction': self.jurisdiction
        }


class OfficeRole(Enum):
    """
    Office Internal Structure (Minimum, Fixed)
    
    Every floor has identical internal offices.
    This uniformity is intentional.
    
    No role is optional.
    No role may merge responsibilities.
    """
    ARCHITECT = "architect"  # Shapes structure from user intent
    IMPLEMENTER = "implementer"  # Writes code
    REVIEWER = "reviewer"  # Enforces correctness & idioms
    TESTER = "tester"  # Produces executable tests
    SECURITY = "security"  # Language-specific risks
    MANAGER = "manager"  # Arbitration & completion authority


class InputType(Enum):
    """Type of code input"""
    EXISTING_CODE = "existing_code"
    SPEC = "spec"
    DIFF = "diff"


class RequestedOutcome(Enum):
    """What the user wants done"""
    FIX = "fix"
    EXTEND = "extend"
    REFACTOR = "refactor"
    AUDIT = "audit"
    TRANSLATE = "translate"


@dataclass
class CodeDirective:
    """
    Code Directive (Canonical Input Format)
    
    You do not give "tasks". You give Code Directives.
    
    This directive becomes a Cognitive Contract automatically.
    """
    directive_id: str
    language: ProgrammingLanguage
    input_type: InputType
    source: str  # User provided code
    requested_outcome: RequestedOutcome
    constraints: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'directive_id': self.directive_id,
            'language': self.language.value,
            'inputType': self.input_type.value,
            'source': self.source,
            'requestedOutcome': self.requested_outcome.value,
            'constraints': self.constraints
        }
    
    def to_cognitive_contract(self):
        """Convert to Cognitive Contract automatically"""
        from src.core.cognitive_contract import (
            CognitiveContract, Intent, BindingLevel
        )
        
        intent = Intent(
            goal=f"{self.requested_outcome.value} code in {self.language.value}",
            constraints=self.constraints,
            non_goals=[
                "Improve beyond request",
                "Add unrelated features",
                "Introduce new architecture"
            ]
        )
        
        contract = CognitiveContract(
            contract_id=self.directive_id,
            issued_at_tick=0,
            issuer="Human",
            intent=intent,
            binding_level=BindingLevel.MANDATORY
        )
        
        return contract


class CodePipelineStep(Enum):
    """
    How Code Is Actually Written (No Magic)
    
    Strict 6-step pipeline.
    """
    FLOOR_ROUTING = 1  # Route to exact language floor
    ARCHITECTURAL_PASS = 2  # Interpret intent, declare invariants
    IMPLEMENTATION_SPRINT = 3  # Write code only
    INTERNAL_REVIEW = 4  # Enforce idioms, style, complexity
    TESTING_MANDATE = 5  # Write tests, no tests = no delivery
    MANAGER_SEAL = 6  # Verify contract, tests, dissent


@dataclass
class ArchitecturalDecision:
    """
    Architectural Pass output (Step 2)
    """
    invariants: List[str] = field(default_factory=list)
    rejected_reason: Optional[str] = None  # If request is impossible
    approved: bool = False
    
    def to_dict(self) -> Dict:
        return {
            'invariants': self.invariants,
            'rejected_reason': self.rejected_reason,
            'approved': self.approved
        }


@dataclass
class ImplementationOutput:
    """
    Implementation Sprint output (Step 3)
    
    Implementers:
    - Write code only
    - No discussion
    - No testing
    - No refactoring beyond scope
    """
    code: str
    files_modified: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'code': self.code,
            'files_modified': self.files_modified
        }


@dataclass
class ReviewDecision:
    """
    Internal Review output (Step 4)
    
    Reviewer enforces:
    - Language idioms
    - Style
    - Complexity limits
    
    May reject entire implementation.
    """
    approved: bool
    violations: List[str] = field(default_factory=list)
    recommendations: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'approved': self.approved,
            'violations': self.violations,
            'recommendations': self.recommendations
        }


@dataclass
class TestSuite:
    """
    Testing Mandate output (Step 5)
    
    Tester writes:
    - Unit tests
    - Edge cases
    
    No tests → no delivery
    """
    test_code: str
    test_count: int
    coverage_percent: float
    all_pass: bool
    
    def to_dict(self) -> Dict:
        return {
            'test_code': self.test_code,
            'test_count': self.test_count,
            'coverage_percent': self.coverage_percent,
            'all_pass': self.all_pass
        }


@dataclass
class ManagerSeal:
    """
    Manager Seal output (Step 6)
    
    Only the Manager can declare DONE.
    
    Manager verifies:
    - Contract satisfied
    - Tests pass
    - No unresolved dissent
    """
    contract_satisfied: bool
    tests_pass: bool
    no_unresolved_dissent: bool
    sealed: bool = False
    
    def is_ready_for_delivery(self) -> bool:
        """Check if all checkboxes pass"""
        return (
            self.contract_satisfied and
            self.tests_pass and
            self.no_unresolved_dissent
        )
    
    def seal(self):
        """Seal the output for delivery"""
        if self.is_ready_for_delivery():
            self.sealed = True
    
    def to_dict(self) -> Dict:
        return {
            'contract_satisfied': self.contract_satisfied,
            'tests_pass': self.tests_pass,
            'no_unresolved_dissent': self.no_unresolved_dissent,
            'sealed': self.sealed
        }


@dataclass
class CodeOutput:
    """
    Output Format (Non-Negotiable)
    
    Every output includes:
    - Final Code
    - Test Suite
    - Change Log
    - Contract Fulfillment Statement
    """
    directive_id: str
    final_code: str
    test_suite: TestSuite
    change_log: List[str]
    manager_seal: ManagerSeal
    
    def format_delivery(self) -> str:
        """
        Format output with verification checkboxes.
        
        If any checkbox fails, no code is released.
        """
        checkmark = "✔" if self.manager_seal.contract_satisfied else "✗"
        result = f"{checkmark} Contract satisfied\n"
        
        checkmark = "✔" if self.manager_seal.tests_pass else "✗"
        result += f"{checkmark} All tests pass\n"
        
        checkmark = "✔" if self.manager_seal.no_unresolved_dissent else "✗"
        result += f"{checkmark} No unresolved dissent\n"
        
        checkmark = "✔" if not self._has_scope_violations() else "✗"
        result += f"{checkmark} No scope violations\n"
        
        if self.manager_seal.is_ready_for_delivery():
            result += "\n--- FINAL CODE ---\n"
            result += self.final_code
            result += "\n\n--- TEST SUITE ---\n"
            result += self.test_suite.test_code
            result += "\n\n--- CHANGE LOG ---\n"
            result += "\n".join(f"- {change}" for change in self.change_log)
        else:
            result += "\n❌ CODE NOT RELEASED - Checkboxes failed\n"
        
        return result
    
    def _has_scope_violations(self) -> bool:
        """Check for scope violations"""
        # Would implement actual scope checking
        return False
    
    def to_dict(self) -> Dict:
        return {
            'directive_id': self.directive_id,
            'final_code': self.final_code,
            'test_suite': self.test_suite.to_dict(),
            'change_log': self.change_log,
            'manager_seal': self.manager_seal.to_dict()
        }


class HumanRole(Enum):
    """
    Human Role (You are not "the user")
    
    You are:
    - The Legislature (you issue directives)
    - The Supreme Authority (you can freeze or override)
    - The Auditor (you can inspect any decision)
    
    But:
    - All overrides are logged
    - All overrides incur cost
    - You cannot silently change outcomes
    
    This preserves trust.
    """
    LEGISLATURE = "legislature"  # Issues directives
    SUPREME_AUTHORITY = "supreme_authority"  # Can freeze or override
    AUDITOR = "auditor"  # Can inspect any decision


class CodeAuthoringCivilization:
    """
    Code-Authoring Civilization Engine
    
    What This Is:
    - Writes code
    - Reviews itself
    - Tests itself
    - Explains itself
    - Obeys scope
    - Respects language boundaries
    - Never hallucinates intent
    
    What This Is NOT:
    - Creative
    - Exploratory
    
    It is RELIABLE. That's why it's valuable.
    """
    
    def __init__(self):
        self.purpose = PurposeConstitution()
        self.floors: Dict[ProgrammingLanguage, LanguageFloor] = {}
        self.active_directives: Dict[str, CodeDirective] = {}
        self.outputs: Dict[str, CodeOutput] = {}
        
        # Initialize language floors
        self._initialize_floors()
    
    def _initialize_floors(self):
        """Initialize the building with language floors"""
        floor_config = [
            (1, ProgrammingLanguage.PYTHON, "Runtime logic"),
            (2, ProgrammingLanguage.RUST, "Performance / safety"),
            (3, ProgrammingLanguage.C_CPP, "Low-level"),
            (4, ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT, "Frontend / tooling"),
            (5, ProgrammingLanguage.GO, "Services"),
            (6, ProgrammingLanguage.SQL, "Data"),
        ]
        
        for floor_num, lang, jurisdiction in floor_config:
            floor = LanguageFloor(floor_num, lang, jurisdiction)
            self.floors[lang] = floor
    
    def submit_directive(self, directive: CodeDirective) -> str:
        """
        Submit a code directive (Input Pipeline).
        
        This becomes a Cognitive Contract automatically.
        """
        # Validate against Purpose Constitution
        context = {
            'code_directive': directive,
            'target_artifact': 'code',
            'action': f'{directive.requested_outcome.value} code'
        }
        
        if not self.purpose.validate_purpose_compliance(context):
            raise ValueError("Directive violates Purpose Constitution")
        
        # Store directive
        self.active_directives[directive.directive_id] = directive
        
        # Convert to Cognitive Contract (for audit trail)
        _ = directive.to_cognitive_contract()
        
        return directive.directive_id
    
    def process_directive(self, directive_id: str) -> CodeOutput:
        """
        Process a directive through the 6-step pipeline.
        
        Step 1 — Floor Routing
        Step 2 — Architectural Pass
        Step 3 — Implementation Sprint
        Step 4 — Internal Review
        Step 5 — Testing Mandate
        Step 6 — Manager Seal
        """
        directive = self.active_directives.get(directive_id)
        if not directive:
            raise ValueError(f"Directive {directive_id} not found")
        
        # Step 1: Floor Routing
        floor = self.floors.get(directive.language)
        if not floor:
            raise ValueError(f"No floor for language {directive.language.value}")
        
        # Step 2: Architectural Pass
        arch_decision = self._architectural_pass(directive)
        if not arch_decision.approved:
            raise ValueError(f"Architectural rejection: {arch_decision.rejected_reason}")
        
        # Step 3: Implementation Sprint
        impl_output = self._implementation_sprint(directive, arch_decision)
        
        # Step 4: Internal Review
        review = self._internal_review(impl_output)
        if not review.approved:
            raise ValueError(f"Review failed: {review.violations}")
        
        # Step 5: Testing Mandate
        test_suite = self._testing_mandate(impl_output)
        if not test_suite.all_pass:
            raise ValueError("Tests failed - no delivery")
        
        # Step 6: Manager Seal
        manager_seal = self._manager_seal(directive, test_suite, review)
        
        # Create output
        output = CodeOutput(
            directive_id=directive_id,
            final_code=impl_output.code,
            test_suite=test_suite,
            change_log=["Implementation complete"],
            manager_seal=manager_seal
        )
        
        self.outputs[directive_id] = output
        return output
    
    def _architectural_pass(self, directive: CodeDirective) -> ArchitecturalDecision:
        """Step 2: Architect interprets intent, declares invariants"""
        # Mock implementation
        return ArchitecturalDecision(
            invariants=["Type safety", "Error handling"],
            approved=True
        )
    
    def _implementation_sprint(
        self,
        directive: CodeDirective,
        arch: ArchitecturalDecision
    ) -> ImplementationOutput:
        """Step 3: Implementers write code only"""
        # Mock implementation
        return ImplementationOutput(
            code=f"# {directive.requested_outcome.value} implementation\n# Language: {directive.language.value}\n",
            files_modified=["main.py"]
        )
    
    def _internal_review(self, impl: ImplementationOutput) -> ReviewDecision:
        """Step 4: Reviewer enforces correctness & idioms"""
        # Mock implementation
        return ReviewDecision(approved=True)
    
    def _testing_mandate(self, impl: ImplementationOutput) -> TestSuite:
        """Step 5: Tester produces executable tests"""
        # Mock implementation
        return TestSuite(
            test_code="# Test suite\ndef test_implementation():\n    assert True",
            test_count=1,
            coverage_percent=100.0,
            all_pass=True
        )
    
    def _manager_seal(
        self,
        directive: CodeDirective,
        tests: TestSuite,
        review: ReviewDecision
    ) -> ManagerSeal:
        """Step 6: Manager verifies and seals"""
        seal = ManagerSeal(
            contract_satisfied=True,
            tests_pass=tests.all_pass,
            no_unresolved_dissent=review.approved
        )
        seal.seal()
        return seal


# Global instance
_code_civilization = CodeAuthoringCivilization()


def get_code_civilization() -> CodeAuthoringCivilization:
    """Get the global Code-Authoring Civilization instance"""
    return _code_civilization
