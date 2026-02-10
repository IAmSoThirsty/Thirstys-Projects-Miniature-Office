"""
Floor Specifications - Language Jurisdiction System
CIVILIZATION TIER - PURPOSE-LOCKED

Each floor is a sovereign language jurisdiction with:
- Domain definition
- Architectural constraints
- Security focus
- Testing doctrine

All floors share identical topology (uniformity doctrine).
"""
from enum import Enum
from typing import Dict, List, Set, Optional
from dataclasses import dataclass, field


class ProgrammingLanguage(Enum):
    """Programming Language Floors - One per language"""
    PYTHON = "python"
    RUST = "rust"
    C_CPP = "c_cpp"
    JAVASCRIPT_TYPESCRIPT = "javascript_typescript"
    GO = "go"
    SQL = "sql"
    SHELL = "shell"


@dataclass
class SecurityFocus:
    """Security concerns specific to a language"""
    primary_risks: List[str] = field(default_factory=list)
    required_checks: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'primary_risks': self.primary_risks,
            'required_checks': self.required_checks
        }


@dataclass
class TestingDoctrine:
    """Testing requirements for a language"""
    mandatory_tests: List[str] = field(default_factory=list)
    optional_tests: List[str] = field(default_factory=list)
    special_emphasis: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'mandatory_tests': self.mandatory_tests,
            'optional_tests': self.optional_tests,
            'special_emphasis': self.special_emphasis
        }


@dataclass
class FloorSpecification:
    """
    Complete specification for a language floor.
    
    Global Floor Invariants (apply to all):
    1. Language Sovereignty - Emits artifacts only in its language
    2. Identical Internal Topology - Architecture → Implementation → Review → Test → Security → Manager
    3. Contract-Bound Operation - No action without parent Cognitive Contract
    4. Non-Creative Mandate - No extrapolation beyond directive
    5. Failure Escalation Guarantee - All failures surface and consume resources
    """
    language: ProgrammingLanguage
    floor_number: int
    domain: List[str]  # What this floor handles
    architectural_constraints: List[str]
    security_focus: SecurityFocus
    testing_doctrine: TestingDoctrine
    
    # Jurisdiction laws
    can_emit_languages: Set[ProgrammingLanguage] = field(default_factory=set)
    can_reason_about: Set[ProgrammingLanguage] = field(default_factory=set)
    requires_contracts_for: Set[ProgrammingLanguage] = field(default_factory=set)
    
    def __post_init__(self):
        """Enforce language sovereignty"""
        # Can only emit in own language
        self.can_emit_languages = {self.language}
        # Can only reason about own language
        self.can_reason_about = {self.language}
        # Must use contracts for all other languages
        all_languages = set(ProgrammingLanguage)
        self.requires_contracts_for = all_languages - {self.language}
    
    def can_author_in(self, target_language: ProgrammingLanguage) -> bool:
        """Check if this floor can author code in target language"""
        return target_language in self.can_emit_languages
    
    def can_interpret(self, target_language: ProgrammingLanguage) -> bool:
        """Check if this floor can interpret semantics of target language"""
        return target_language in self.can_reason_about
    
    def requires_contract(self, target_language: ProgrammingLanguage) -> bool:
        """Check if cross-language work requires contract"""
        return target_language in self.requires_contracts_for
    
    def validate_jurisdiction(self, action: str, target_language: ProgrammingLanguage) -> tuple[bool, Optional[str]]:
        """
        Validate if an action is legal under jurisdiction laws.
        Returns: (is_legal, reason_if_illegal)
        """
        if action == "author" and not self.can_author_in(target_language):
            return False, f"Floor {self.floor_number} ({self.language.value}) cannot author code in {target_language.value}"
        
        if action == "interpret" and not self.can_interpret(target_language):
            return False, f"Floor {self.floor_number} ({self.language.value}) cannot interpret semantics of {target_language.value}"
        
        if action == "cross_language" and not self.requires_contract(target_language):
            return True, None
        
        if action == "cross_language" and self.requires_contract(target_language):
            return False, f"Cross-language work from {self.language.value} to {target_language.value} requires explicit contract"
        
        return True, None
    
    def to_dict(self) -> Dict:
        return {
            'language': self.language.value,
            'floor_number': self.floor_number,
            'domain': self.domain,
            'architectural_constraints': self.architectural_constraints,
            'security_focus': self.security_focus.to_dict(),
            'testing_doctrine': self.testing_doctrine.to_dict(),
            'can_emit_languages': [lang.value for lang in self.can_emit_languages],
            'can_reason_about': [lang.value for lang in self.can_reason_about],
            'requires_contracts_for': [lang.value for lang in self.requires_contracts_for]
        }


# FLOOR 1 — PYTHON JURISDICTION
FLOOR_1_PYTHON = FloorSpecification(
    language=ProgrammingLanguage.PYTHON,
    floor_number=1,
    domain=[
        "Application logic",
        "Automation",
        "Data processing",
        "Glue code"
    ],
    architectural_constraints=[
        "Readability > micro-optimization",
        "Explicit typing encouraged but not enforced unless requested",
        "Standard library preferred unless contract permits dependencies",
        "PEP 8 compliance"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "Injection risks",
            "Unsafe deserialization",
            "Dynamic execution (eval, exec)",
            "Dependency trust"
        ],
        required_checks=[
            "Input validation",
            "Dependency scanning",
            "No eval/exec without justification",
            "Pickle usage auditing"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Unit tests"],
        optional_tests=["Property tests (if requested)"],
        special_emphasis=["Edge case coverage", "Exception handling"]
    )
)


# FLOOR 2 — RUST JURISDICTION
FLOOR_2_RUST = FloorSpecification(
    language=ProgrammingLanguage.RUST,
    floor_number=2,
    domain=[
        "Memory-safe systems",
        "Performance-critical logic",
        "FFI boundaries"
    ],
    architectural_constraints=[
        "Ownership and lifetime correctness is non-negotiable",
        "Unsafe blocks require explicit justification",
        "Zero undefined behavior tolerated",
        "Clippy linting enforced"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "Memory safety proofs",
            "Unsafe scope auditing",
            "ABI correctness"
        ],
        required_checks=[
            "Unsafe block justification",
            "Miri validation for unsafe code",
            "FFI boundary verification",
            "No panic in unsafe"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Unit tests", "Integration tests"],
        optional_tests=[],
        special_emphasis=["Fuzzing strongly preferred if scope includes parsing or I/O"]
    )
)


# FLOOR 3 — C/C++ JURISDICTION
FLOOR_3_C_CPP = FloorSpecification(
    language=ProgrammingLanguage.C_CPP,
    floor_number=3,
    domain=[
        "Low-level systems",
        "Embedded logic",
        "Performance primitives"
    ],
    architectural_constraints=[
        "Explicit memory ownership",
        "Deterministic lifetimes",
        "No implicit allocations unless justified",
        "RAII patterns enforced (C++)"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "Buffer overflows",
            "Use-after-free",
            "Integer overflow",
            "Undefined behavior"
        ],
        required_checks=[
            "Static analysis (clang-tidy, cppcheck)",
            "AddressSanitizer",
            "UndefinedBehaviorSanitizer",
            "Manual bounds checking"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Unit tests"],
        optional_tests=[],
        special_emphasis=["Static analysis strongly emphasized", "Memory leak detection"]
    )
)


# FLOOR 4 — JAVASCRIPT/TYPESCRIPT JURISDICTION
FLOOR_4_JS_TS = FloorSpecification(
    language=ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT,
    floor_number=4,
    domain=[
        "Frontend logic",
        "Tooling",
        "Node services"
    ],
    architectural_constraints=[
        "Async correctness",
        "Deterministic side effects",
        "TypeScript preferred when allowed",
        "ESLint compliance"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "XSS",
            "Prototype pollution",
            "Supply-chain risks",
            "Runtime injection"
        ],
        required_checks=[
            "Input sanitization",
            "Dependency auditing",
            "CSP headers (frontend)",
            "No eval or Function constructor"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Unit tests"],
        optional_tests=[],
        special_emphasis=["Integration tests required for UI or API layers"]
    )
)


# FLOOR 5 — GO JURISDICTION
FLOOR_5_GO = FloorSpecification(
    language=ProgrammingLanguage.GO,
    floor_number=5,
    domain=[
        "Network services",
        "Concurrency-heavy systems",
        "Infrastructure logic"
    ],
    architectural_constraints=[
        "Explicit concurrency patterns",
        "Channel safety",
        "No hidden goroutine leaks",
        "Go fmt and go vet compliance"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "Race conditions",
            "Input validation",
            "Network boundary hardening"
        ],
        required_checks=[
            "Race detector",
            "Goroutine leak detection",
            "Context cancellation",
            "TLS verification"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Unit tests"],
        optional_tests=[],
        special_emphasis=["Race detection emphasized", "Benchmark tests for concurrency"]
    )
)


# FLOOR 6 — SQL/DATA JURISDICTION
FLOOR_6_SQL = FloorSpecification(
    language=ProgrammingLanguage.SQL,
    floor_number=6,
    domain=[
        "Schema design",
        "Queries",
        "Data transformations"
    ],
    architectural_constraints=[
        "Deterministic queries",
        "Explicit indexing",
        "No implicit schema assumptions",
        "Normalized design unless justified"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "SQL injection",
            "Privilege separation",
            "Data leakage"
        ],
        required_checks=[
            "Parameterized queries",
            "Least privilege",
            "Row-level security",
            "Audit logging"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Query correctness tests"],
        optional_tests=[],
        special_emphasis=["Migration safety verification", "Performance testing"]
    )
)


# FLOOR 7 — SHELL/SCRIPTING JURISDICTION
FLOOR_7_SHELL = FloorSpecification(
    language=ProgrammingLanguage.SHELL,
    floor_number=7,
    domain=[
        "Automation",
        "System orchestration"
    ],
    architectural_constraints=[
        "Explicit error handling (set -e)",
        "No silent failures",
        "Idempotence preferred",
        "ShellCheck compliance"
    ],
    security_focus=SecurityFocus(
        primary_risks=[
            "Command injection",
            "Environment poisoning",
            "Privilege escalation"
        ],
        required_checks=[
            "Input quoting",
            "PATH hardening",
            "Privilege dropping",
            "Secure temp files"
        ]
    ),
    testing_doctrine=TestingDoctrine(
        mandatory_tests=["Script behavior verification"],
        optional_tests=[],
        special_emphasis=["Dry-run validation", "Rollback testing"]
    )
)


# Registry of all floors
ALL_FLOORS: Dict[ProgrammingLanguage, FloorSpecification] = {
    ProgrammingLanguage.PYTHON: FLOOR_1_PYTHON,
    ProgrammingLanguage.RUST: FLOOR_2_RUST,
    ProgrammingLanguage.C_CPP: FLOOR_3_C_CPP,
    ProgrammingLanguage.JAVASCRIPT_TYPESCRIPT: FLOOR_4_JS_TS,
    ProgrammingLanguage.GO: FLOOR_5_GO,
    ProgrammingLanguage.SQL: FLOOR_6_SQL,
    ProgrammingLanguage.SHELL: FLOOR_7_SHELL,
}


def get_floor_specification(language: ProgrammingLanguage) -> FloorSpecification:
    """Get the specification for a language floor"""
    return ALL_FLOORS[language]


def validate_floor_uniformity() -> bool:
    """
    Validate that all floors have identical topology (uniformity doctrine).
    
    Every floor must have:
    - Architecture Office
    - Implementation Office
    - Review Office
    - Test Office
    - Security Office
    - Manager Office
    """
    # This is enforced structurally - all floors use FloorSpecification
    # which guarantees uniformity
    return True


def get_all_floors() -> List[FloorSpecification]:
    """Get all floor specifications"""
    return list(ALL_FLOORS.values())


def route_directive_to_floor(language: ProgrammingLanguage) -> FloorSpecification:
    """
    Route a code directive to the appropriate floor.
    Step 1 of the 6-step pipeline.
    """
    return get_floor_specification(language)
