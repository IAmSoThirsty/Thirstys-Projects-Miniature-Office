# Floor Specifications - Language Jurisdictions

**PURPOSE-LOCKED CIVILIZATION TIER ARCHITECTURE**

## Overview

Each floor is a **sovereign language jurisdiction** with exclusive authority over its programming language. This document specifies all 7 floors and their jurisdictional rules.

## Global Floor Invariants

All floors are bound by these five laws:

1. **Language Sovereignty** - Emits artifacts only in its language
2. **Identical Internal Topology** - Architecture → Implementation → Review → Test → Security → Manager
3. **Contract-Bound Operation** - No action without a parent Cognitive Contract
4. **Non-Creative Mandate** - No extrapolation, optimization, or improvement beyond directive
5. **Failure Escalation Guarantee** - All failures surface, halt progress, and consume resources

**Floors differ only in technical jurisdiction, not authority.**

---

## Floor 1: Python Jurisdiction

**Floor Number:** 1  
**Language:** Python

### Domain
- Application logic
- Automation
- Data processing
- Glue code

### Architectural Constraints
- Readability > micro-optimization
- Explicit typing encouraged but not enforced unless requested
- Standard library preferred unless contract permits dependencies
- PEP 8 compliance

### Security Focus

**Primary Risks:**
- Injection risks
- Unsafe deserialization
- Dynamic execution (eval, exec)
- Dependency trust

**Required Checks:**
- Input validation
- Dependency scanning
- No eval/exec without justification
- Pickle usage auditing

### Testing Doctrine

**Mandatory:**
- Unit tests

**Optional:**
- Property tests (if requested)

**Special Emphasis:**
- Edge case coverage
- Exception handling

---

## Floor 2: Rust Jurisdiction

**Floor Number:** 2  
**Language:** Rust

### Domain
- Memory-safe systems
- Performance-critical logic
- FFI boundaries

### Architectural Constraints
- Ownership and lifetime correctness is **non-negotiable**
- Unsafe blocks require explicit justification
- Zero undefined behavior tolerated
- Clippy linting enforced

### Security Focus

**Primary Risks:**
- Memory safety proofs
- Unsafe scope auditing
- ABI correctness

**Required Checks:**
- Unsafe block justification
- Miri validation for unsafe code
- FFI boundary verification
- No panic in unsafe

### Testing Doctrine

**Mandatory:**
- Unit tests
- Integration tests

**Special Emphasis:**
- Fuzzing strongly preferred if scope includes parsing or I/O

---

## Floor 3: C/C++ Jurisdiction

**Floor Number:** 3  
**Language:** C/C++

### Domain
- Low-level systems
- Embedded logic
- Performance primitives

### Architectural Constraints
- Explicit memory ownership
- Deterministic lifetimes
- No implicit allocations unless justified
- RAII patterns enforced (C++)

### Security Focus

**Primary Risks:**
- Buffer overflows
- Use-after-free
- Integer overflow
- Undefined behavior

**Required Checks:**
- Static analysis (clang-tidy, cppcheck)
- AddressSanitizer
- UndefinedBehaviorSanitizer
- Manual bounds checking

### Testing Doctrine

**Mandatory:**
- Unit tests

**Special Emphasis:**
- Static analysis strongly emphasized
- Memory leak detection

---

## Floor 4: JavaScript/TypeScript Jurisdiction

**Floor Number:** 4  
**Language:** JavaScript/TypeScript

### Domain
- Frontend logic
- Tooling
- Node services

### Architectural Constraints
- Async correctness
- Deterministic side effects
- TypeScript preferred when allowed
- ESLint compliance

### Security Focus

**Primary Risks:**
- XSS (Cross-Site Scripting)
- Prototype pollution
- Supply-chain risks
- Runtime injection

**Required Checks:**
- Input sanitization
- Dependency auditing
- CSP headers (frontend)
- No eval or Function constructor

### Testing Doctrine

**Mandatory:**
- Unit tests

**Special Emphasis:**
- Integration tests required for UI or API layers

---

## Floor 5: Go Jurisdiction

**Floor Number:** 5  
**Language:** Go

### Domain
- Network services
- Concurrency-heavy systems
- Infrastructure logic

### Architectural Constraints
- Explicit concurrency patterns
- Channel safety
- No hidden goroutine leaks
- Go fmt and go vet compliance

### Security Focus

**Primary Risks:**
- Race conditions
- Input validation
- Network boundary hardening

**Required Checks:**
- Race detector
- Goroutine leak detection
- Context cancellation
- TLS verification

### Testing Doctrine

**Mandatory:**
- Unit tests

**Special Emphasis:**
- Race detection emphasized
- Benchmark tests for concurrency

---

## Floor 6: SQL/Data Jurisdiction

**Floor Number:** 6  
**Language:** SQL

### Domain
- Schema design
- Queries
- Data transformations

### Architectural Constraints
- Deterministic queries
- Explicit indexing
- No implicit schema assumptions
- Normalized design unless justified

### Security Focus

**Primary Risks:**
- SQL injection
- Privilege separation
- Data leakage

**Required Checks:**
- Parameterized queries
- Least privilege
- Row-level security
- Audit logging

### Testing Doctrine

**Mandatory:**
- Query correctness tests

**Special Emphasis:**
- Migration safety verification
- Performance testing

---

## Floor 7: Shell/Scripting Jurisdiction

**Floor Number:** 7  
**Language:** Bash, PowerShell, etc.

### Domain
- Automation
- System orchestration

### Architectural Constraints
- Explicit error handling (set -e)
- No silent failures
- Idempotence preferred
- ShellCheck compliance

### Security Focus

**Primary Risks:**
- Command injection
- Environment poisoning
- Privilege escalation

**Required Checks:**
- Input quoting
- PATH hardening
- Privilege dropping
- Secure temp files

### Testing Doctrine

**Mandatory:**
- Script behavior verification

**Special Emphasis:**
- Dry-run validation
- Rollback testing

---

## Jurisdiction Laws

### Language Sovereignty

Each floor:
- **CAN** emit artifacts only in its own language
- **CAN** reason only about its own language semantics
- **MUST** use contracts for all cross-language work
- **CANNOT** author code in another language
- **CANNOT** interpret semantics of another language

### Cross-Language Work

When user code spans multiple languages:

1. **Primary floor** owns the intent
2. **Secondary floors** operate under explicit contracts
3. **Contracts define:**
   - Data formats
   - ABI / FFI boundaries
   - Failure modes
4. **No implicit glue code**
5. **No assumptions**

### Uniformity Doctrine

Every floor contains identical office topology:

- Architecture Office
- Implementation Office
- Review Office
- Test Office
- Security Office
- Manager Office

**This uniformity is mandatory** to prevent:
- Privileged languages
- Informal authority
- Emergent bias

---

## API Access

### List All Floors
```bash
GET /api/floors
```

Returns all 7 floor specifications with complete jurisdictional rules.

### Get Specific Floor
```bash
GET /api/floors/<language>
```

Examples:
```bash
GET /api/floors/python
GET /api/floors/rust
GET /api/floors/javascript_typescript
```

---

## Validation

### Check Jurisdiction
```python
from src.core.floor_specifications import get_floor_specification, ProgrammingLanguage

# Get Python floor
python_floor = get_floor_specification(ProgrammingLanguage.PYTHON)

# Check if can author in Rust (should be False)
can_author = python_floor.can_author_in(ProgrammingLanguage.RUST)
# Returns: False

# Check if requires contract for Rust (should be True)
requires_contract = python_floor.requires_contract(ProgrammingLanguage.RUST)
# Returns: True
```

### Validate Action
```python
# Validate if action is legal
is_legal, reason = python_floor.validate_jurisdiction(
    action="author",
    target_language=ProgrammingLanguage.RUST
)
# Returns: (False, "Floor 1 (python) cannot author code in rust")
```

---

## Constitutional Binding

These floor specifications are **constitutionally bound** by the Density Codex:

- **Axiom: Language Sovereignty** - Each floor has exclusive jurisdiction
- **Layer 2: Actors** - Floors are sovereign actors
- **Law Class B: Structural** - Specifications are amendable via constitutional mutation
- **Economic Reality** - Cross-language work has higher cost

**Violation of floor jurisdiction is a civilization fault, not a task error.**
