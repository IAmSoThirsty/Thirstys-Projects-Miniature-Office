# CODE-AUTHORING CIVILIZATION
## Purpose-Locked Architecture (Final)

This document defines the **exclusive purpose** and operational boundaries of the Miniature Office system as a **Code-Authoring Civilization**.

---

## I. PURPOSE CONSTITUTION (HARD BINDING)

**This is law ABOVE all other laws.**

### Primary Civilizational Purpose

> The system exists EXCLUSIVELY to transform user-supplied code directives into correct, tested, auditable code artifacts.

### Consequences

✅ **Allowed:**
- Transform code directives into code artifacts
- Review code for correctness
- Generate tests
- Audit code quality

❌ **FORBIDDEN:**
- Speculative features
- Self-initiated projects
- Autonomous goal formation
- "Helpful" side quests
- Unrelated optimizations
- Feature suggestions beyond scope
- Architecture improvements not requested

### Constitutional Rule

**If an action does not advance code correctness or completeness, it is ILLEGAL.**

---

## II. WORLD STRUCTURE (Office Building, Not Metaphor)

**The building IS the codebase.**

### Floor = Programming Language (ONE-TO-ONE)

| Floor | Language | Jurisdiction |
|-------|----------|--------------|
| L1 | Python | Runtime logic |
| L2 | Rust | Performance / safety |
| L3 | C/C++ | Low-level |
| L4 | JS/TS | Frontend / tooling |
| L5 | Go | Services |
| L6 | SQL | Data |
| ... | ... | ... |

### Floor Constraints

**No floor may:**
- ❌ Author code outside its language
- ❌ Interpret semantics of another language

**Cross-language work MUST go through contracts.**

---

## III. OFFICE INTERNAL STRUCTURE (Minimum, Fixed)

**Every floor has identical internal offices. This uniformity is intentional.**

### Required Roles (No role is optional, no role may merge responsibilities)

| Role | Function |
|------|----------|
| **Architect** | Shapes structure from user intent |
| **Implementer** | Writes code |
| **Reviewer** | Enforces correctness & idioms |
| **Tester** | Produces executable tests |
| **Security** | Language-specific risks |
| **Manager** | Arbitration & completion authority |

---

## IV. INPUT PIPELINE (What You Give It)

**You do not give "tasks". You give Code Directives.**

### Code Directive (Canonical Format)

```json
{
  "language": "rust",
  "inputType": "existing_code | spec | diff",
  "source": "...user provided code...",
  "requestedOutcome": "fix | extend | refactor | audit | translate",
  "constraints": [
    "no new dependencies",
    "preserve API",
    "optimize for memory"
  ]
}
```

**This directive becomes a Cognitive Contract automatically.**

### Input Types
- `existing_code` - Modify/fix existing code
- `spec` - Implement from specification
- `diff` - Apply or review changes

### Requested Outcomes
- `fix` - Repair bugs or errors
- `extend` - Add functionality
- `refactor` - Restructure without changing behavior
- `audit` - Review for issues
- `translate` - Convert to another language (via contract)

---

## V. HOW CODE IS ACTUALLY WRITTEN (No Magic)

### Step 1 — Floor Routing
- Directive routed to exact language floor
- Other floors locked unless invoked by contract

### Step 2 — Architectural Pass

**Architect:**
- Interprets intent
- Declares invariants
- **Rejects impossible requests EARLY**

Output: `ArchitecturalDecision`
```json
{
  "invariants": ["Type safety", "Error handling"],
  "rejected_reason": null,
  "approved": true
}
```

### Step 3 — Implementation Sprint

**Implementers:**
- ✅ Write code only
- ❌ No discussion
- ❌ No testing
- ❌ No refactoring beyond scope

Output: `ImplementationOutput`
```json
{
  "code": "...",
  "files_modified": ["main.rs"]
}
```

### Step 4 — Internal Review

**Reviewer enforces:**
- Language idioms
- Style
- Complexity limits

**May reject entire implementation.**

Output: `ReviewDecision`
```json
{
  "approved": true,
  "violations": [],
  "recommendations": []
}
```

### Step 5 — Testing Mandate

**Tester writes:**
- Unit tests
- Edge cases

**No tests → no delivery**

Output: `TestSuite`
```json
{
  "test_code": "...",
  "test_count": 15,
  "coverage_percent": 95.0,
  "all_pass": true
}
```

### Step 6 — Manager Seal

**Only the Manager can declare DONE.**

Manager verifies:
- ✅ Contract satisfied
- ✅ Tests pass
- ✅ No unresolved dissent

Output: `ManagerSeal`
```json
{
  "contract_satisfied": true,
  "tests_pass": true,
  "no_unresolved_dissent": true,
  "sealed": true
}
```

---

## VI. CROSS-LANGUAGE WORK (Strictly Governed)

### If user code spans languages:

1. **Primary floor owns intent**
2. **Secondary floors operate under explicit contracts**

### Contracts Must Define:
- Data formats
- ABI / FFI boundaries
- Failure modes

### Rules:
- ❌ No implicit glue code
- ❌ No assumptions
- ✅ Everything explicit

---

## VII. WHAT THE SYSTEM IS FORBIDDEN TO DO

### These are ILLEGAL actions:

1. ❌ "Improve" code beyond request
2. ❌ Introduce new architecture
3. ❌ Add dependencies unless authorized
4. ❌ Rewrite for elegance
5. ❌ Suggest unrelated optimizations
6. ❌ Speculate about future features

**This is not an advisor. It is a code civilization under orders.**

---

## VIII. OUTPUT FORMAT (Non-Negotiable)

### Every output includes:
1. Final Code
2. Test Suite
3. Change Log
4. Contract Fulfillment Statement

### Example Output:

```
✔ Contract satisfied
✔ All tests pass
✔ No scope violations
✔ No unresolved dissent

--- FINAL CODE ---
[code here]

--- TEST SUITE ---
[tests here]

--- CHANGE LOG ---
- Implemented authentication module
- Added input validation
- Fixed edge case in error handling
```

### Rule:
**If any checkbox fails, no code is released.**

---

## IX. HUMAN ROLE (You)

**You are not "the user".**

### You are:
1. **The Legislature** - You issue directives
2. **The Supreme Authority** - You can freeze or override
3. **The Auditor** - You can inspect any decision

### But:
- ⚠️ All overrides are logged
- ⚠️ All overrides incur cost
- ⚠️ You cannot silently change outcomes

**This preserves trust.**

---

## X. WHAT YOU HAVE NOW (Clear Statement)

### You now have a Civilization-Tier Code Authoring Engine that:

✅ **Writes code** - Implements exactly what you request
✅ **Reviews itself** - Internal review before delivery
✅ **Tests itself** - Generates and runs tests
✅ **Explains itself** - Full change log and reasoning
✅ **Obeys scope** - Never exceeds directive bounds
✅ **Respects language boundaries** - One floor per language
✅ **Never hallucinates intent** - Works only from your directives

### What it is NOT:
❌ Creative
❌ Exploratory
❌ Advisory
❌ Speculative

### What it IS:
✅ **Reliable**

**That's why it's valuable.**

---

## USAGE EXAMPLE

### 1. Submit a Code Directive

```python
from src.core.code_civilization import (
    CodeDirective,
    ProgrammingLanguage,
    InputType,
    RequestedOutcome,
    get_code_civilization
)

directive = CodeDirective(
    directive_id="dir-001",
    language=ProgrammingLanguage.PYTHON,
    input_type=InputType.SPEC,
    source="Create a function to validate email addresses",
    requested_outcome=RequestedOutcome.EXTEND,
    constraints=[
        "no external dependencies",
        "handle unicode",
        "return detailed error messages"
    ]
)

civilization = get_code_civilization()
directive_id = civilization.submit_directive(directive)
```

### 2. Process Through Pipeline

```python
output = civilization.process_directive(directive_id)
```

### 3. Receive Output

```python
delivery = output.format_delivery()
print(delivery)
```

**Output:**
```
✔ Contract satisfied
✔ All tests pass
✔ No scope violations
✔ No unresolved dissent

--- FINAL CODE ---
[validated, tested code]

--- TEST SUITE ---
[comprehensive tests]

--- CHANGE LOG ---
[all changes documented]
```

---

## VERIFICATION

### To verify Purpose Compliance:

```python
from src.core.code_civilization import PurposeConstitution

context = {
    'code_directive': directive,
    'target_artifact': 'code',
    'action': 'implement authentication'
}

is_compliant = PurposeConstitution.validate_purpose_compliance(context)
```

Returns `True` only if action advances code correctness/completeness.

---

## SUMMARY

This system is:
- **Purpose-locked** to code authoring
- **Language-partitioned** by floors
- **Role-segregated** by office structure
- **Pipeline-enforced** through 6 steps
- **Contract-governed** for cross-language work
- **Human-directed** with logged authority
- **Output-verified** with mandatory checks

**It does ONE thing: Transform directives into correct, tested code.**

**It does it RELIABLY.**

**That's the value.**

---

*"This is not an advisor. It is a code civilization under orders."*
