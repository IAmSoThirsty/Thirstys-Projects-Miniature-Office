# Executive Interfaces - The Consigliere & Head of Security

**PURPOSE-LOCKED CIVILIZATION TIER ARCHITECTURE**

## Overview

The system provides two **executive interfaces** for human interaction:

1. **The Consigliere** - Your Right-Hand Executive (operational authority on your instruction)
2. **Head of Security** - Safety Authority (executive power, independent)

These replace traditional micromanagement with **governed interaction**.

---

## User Interaction Model

You interact with **three entities only**:

1. **Consigliere** - Your right-hand man who executes YOUR commands
2. **Head of Security** - Safety authority who can override for security
3. **Formal Directives** - Binding execution (when you want direct control)

**You never:**
- Micromanage individual agents (tell Consigliere instead)
- Bypass governance
- Chase implementation details (unless inspecting)

---

# The Consigliere

**Role Classification:** Chief Operating Executive — Operational Authority (On Your Instruction Only)

## Purpose

Your right-hand executive who commands the civilization **on your behalf**.

**Key Point:** He has the AUTHORITY to command, but ONLY EXERCISES it when you explicitly tell him to.

He doesn't act autonomously. He waits for your direction.

**You say:** "Consigliere, tell Manager X to do Y"  
**He does:** Tells Manager X to do Y

**You don't say anything?**  
**He doesn't do anything** (except advise when you ask)

## Authority vs Autonomy

- **Authority:** CAN issue commands to managers and agents
- **Autonomy:** ZERO - only acts on your explicit instruction

This is the distinction between a trusted executive (has power) and an autonomous agent (makes own decisions). The Consigliere is the former, not the latter.

## Capabilities

### 1. EXPLAIN

#### Why Blocked
```bash
POST /api/consigliere/explain
{
  "type": "why_blocked",
  "entity_id": "task-001"
}
```

Explains:
- Why is task X blocked?
- What preconditions are unmet?
- When can it proceed?

#### Why Decision
```bash
POST /api/consigliere/explain
{
  "type": "why_decision",
  "entity_id": "decision-042"
}
```

Explains:
- Why was implementation X rejected?
- Why did consensus vote Y?
- Why did manager approve Z?

#### What Options
```bash
POST /api/consigliere/explain
{
  "type": "what_options",
  "situation": "blocked task requiring cross-language work"
}
```

Explains:
- What can I do about this situation?
- What are valid next steps?
- What options exist within scope?

### 2. TRANSLATE

#### Civilization → Human Language
```bash
POST /api/consigliere/translate
{
  "text": "Task state: BLOCKED, preconditions unmet. Consensus threshold: 0.67"
}
```

Translates technical civilization language into plain human language:
- Cognitive Contract → "Work agreement with clear goals"
- Consensus Band → "Voting system for disagreements"
- Meta-Office → "Supreme authority that judges"

Includes glossary of terms.

### 3. PREVIEW

#### Consequences of Actions
```bash
POST /api/consigliere/preview
{
  "action": "override manager decision",
  "context": {}
}
```

Returns:
- **Consequences** - What will happen
- **Resource Costs** - What it will cost
- **Risks** - What could go wrong
- **Alternatives** - What else you could do

Example response:
```json
{
  "action": "override manager decision",
  "consequences": [
    "Decision will be overridden with justification logged",
    "Original decision-makers will be notified",
    "Override will be in audit trail permanently"
  ],
  "resource_costs": {
    "manager_attention": 5,
    "audit_entries": 1
  },
  "risks": [
    "May undermine agent autonomy",
    "Could set precedent for future overrides"
  ],
  "alternatives": [
    "Request clarification instead",
    "Allow appeal process to complete",
    "Modify directive to avoid conflict"
  ]
}
```

### 4. PREPARE

#### Draft Directive
```bash
POST /api/consigliere/draft
{
  "goal": "Create user authentication module",
  "language": "python",
  "constraints": ["no external dependencies", "preserve API"]
}
```

Returns a complete draft directive ready for human approval.

**The user still must issue it** - the Consigliere prepares, you decide.

---

### 5. COMMAND (On Your Explicit Instruction)

#### Issue Directive to Manager
```bash
POST /api/consigliere/command/manager
{
  "manager_id": "mgr-001",
  "directive": "Prioritize authentication module completion",
  "priority": "high"
}
```

**Use when:** You want to tell a manager what to do, but through your Consigliere.

**Result:** Consigliere issues the command on your behalf (logged as "on behalf of human").

#### Update Agent Directive
```bash
POST /api/consigliere/command/agent
{
  "agent_id": "agent-005",
  "new_directive": "Focus on input validation first",
  "justification": "Security concern identified"
}
```

**Use when:** You want to redirect an agent's work.

**Result:** Consigliere updates the directive (you're still in control).

#### Coordinate Cross-Floor Work
```bash
POST /api/consigliere/coordinate
{
  "floor_ids": ["floor-python", "floor-rust"],
  "coordination_plan": "Python calls Rust FFI for crypto operations"
}
```

**Use when:** Work spans multiple language floors.

**Result:** Consigliere coordinates the integration (on your instruction).

#### Assess Feasibility
```bash
POST /api/consigliere/assess
{
  "request": "Implement quantum-resistant encryption in 1 day"
}
```

**Use when:** You want to know if something is possible.

**Result:**
```json
{
  "feasible": false,
  "reason": "Quantum-resistant encryption requires extensive testing and review",
  "alternatives": ["Use existing proven algorithms", "Allocate 2 weeks", "Hire specialist"],
  "note": "You're the boss - you can override this if you want (it's a you problem now 😉)"
}
```

The Consigliere tells you it's impossible.  
You can say "that's a you problem" and he'll try anyway (because you're the boss).

---

## Hard Limits

The Consigliere **CANNOT**:
- ❌ Act on his own initiative (no autonomy)
- ❌ Override your decisions (you're the boss)
- ❌ Write code directly (that's what agents do)
- ❌ Suppress audit logs
- ❌ Delete history

**He has authority, but zero autonomy.**

## API Endpoints

### Get State
```bash
GET /api/consigliere
```

Returns:
```json
{
  "role": "Chief Operating Executive",
  "authority_level": "Operational (when instructed by human)",
  "can_issue_commands": true,
  "can_manage_agents": true,
  "can_override_human": false,
  "autonomy": "None - only acts on explicit human instruction",
  "relationship": "Your right-hand executive who executes YOUR commands (not his own)"
}
```

---

# Head of Security

**Role Classification:** Executive Authority — Security Sovereign

## Purpose

Guarantee that **no artifact, decision, or action compromises safety, legality, or system integrity** — regardless of productivity cost.

**Security always outranks delivery speed.**

## Jurisdiction

The Head of Security has authority over:
- Safety policies
- Threat models
- Tool permissions
- Emergency halts
- Audit depth
- Red-line decisions

## Capabilities

### 1. GRANT/REVOKE

#### Grant Tool Access
```bash
POST /api/security/grant
{
  "entity_id": "agent-005",
  "resource": "unsafe_compiler_flag",
  "justification": "Required for low-level optimization in performance-critical path",
  "expires_in_hours": 24
}
```

Grants permission with:
- **Justification** (required)
- **Expiration** (optional)
- **Audit logging** (automatic)

#### Revoke Access
```bash
POST /api/security/revoke
{
  "permission_id": "perm_agent-005_1234567890",
  "reason": "Security vulnerability discovered in feature"
}
```

Revokes immediately with logged reason.

#### Approve Unsafe Operation
```python
from src.core.head_of_security import get_head_of_security

security = get_head_of_security()
permission = security.approve_unsafe_operation(
    entity_id="agent-rust-01",
    operation="use_unsafe_block",
    justification="FFI boundary requires unsafe",
    mitigations=["Manual review", "Miri validation", "Documented invariants"]
)
```

Approves with **required mitigations**.

### 2. TRIGGER

#### Full System Audit
```bash
POST /api/security/audit
{
  "type": "full_system",
  "reason": "Pre-release security review"
}
```

Triggers comprehensive security audit of entire building.

#### Floor Lockdown
```bash
POST /api/security/lockdown
{
  "scope": "floor:floor-rust",
  "reason": "Memory safety violation detected",
  "threat_level": "high"
}
```

Locks down a specific floor - **all work stops**.

#### Cross-Floor Security Review
```bash
POST /api/security/audit
{
  "type": "cross_floor",
  "scope": ["floor-python", "floor-rust"],
  "reason": "FFI boundary audit"
}
```

Reviews security of cross-language integration.

### 3. ABSOLUTE POWERS

#### Block Delivery
```bash
POST /api/security/block
{
  "artifact_id": "artifact-auth-module",
  "reason": "SQL injection vulnerability in authentication",
  "required_mitigations": [
    "Parameterized queries",
    "Input validation",
    "Penetration testing"
  ]
}
```

Blocks artifact delivery **indefinitely** until mitigations are applied.

**No delivery happens until Head of Security approves.**

#### Invalidate Artifact
```python
security.invalidate_artifact(
    artifact_id="artifact-042",
    security_reason="Cryptographic weakness - uses MD5 for passwords"
)
```

Declares artifact invalid due to security.

#### Freeze Building
```bash
POST /api/security/lockdown
{
  "scope": "building",
  "reason": "Critical security incident - potential data breach",
  "threat_level": "critical"
}
```

**Emergency halt** - entire building stops.

#### Force Re-Architecture
```python
security.force_rearchitecture(
    artifact_id="artifact-network-service",
    safety_issue="Missing authentication on admin endpoints",
    required_changes=[
        "Add authentication layer",
        "Implement authorization",
        "Add audit logging"
    ]
)
```

Forces redesign if current architecture is unsafe.

## Absolute Limits

Even the Head of Security **CANNOT**:
- ❌ Change user intent
- ❌ Modify code directly
- ❌ Suppress audit logs
- ❌ Override constitutional laws

**Security is powerful, but still governed.**

## API Endpoints

### Get State
```bash
GET /api/security
```

Returns:
```json
{
  "role": "Executive Authority - Security Sovereign",
  "can_block_delivery": true,
  "can_freeze_building": true,
  "can_force_rearchitecture": true,
  "can_change_user_intent": false,
  "can_modify_code_directly": false,
  "can_suppress_audit_logs": false,
  "can_override_constitutional_laws": false,
  "active_lockdowns": 0,
  "blocked_deliveries": 1,
  "active_permissions": 3,
  "policies": 3,
  "audits": 2
}
```

### Query Active Issues
```bash
GET /api/security/lockdowns       # Active lockdowns
GET /api/security/blocked          # Blocked deliveries
GET /api/security/permissions      # Active permissions
```

---

## Security Policies

Three core policies are **always enforced**:

### 1. Memory Safety Policy
**Enforcement:** CRITICAL

Rules:
- No buffer overflows
- No use-after-free
- No double-free
- Bounds checking required

### 2. Injection Prevention Policy
**Enforcement:** CRITICAL

Rules:
- All inputs must be validated
- Use parameterized queries
- No eval/exec on untrusted input
- Sanitize all outputs

### 3. Least Privilege Policy
**Enforcement:** MANDATORY

Rules:
- Grant minimum necessary permissions
- Time-limit elevated privileges
- Audit all privilege escalations
- Separate duties

---

## Example Workflows

### Scenario 1: Agent Needs Unsafe Operation

1. **Agent requests** unsafe block in Rust
2. **Manager escalates** to Head of Security
3. **Head of Security**:
   - Reviews justification
   - Requires mitigations (Miri, manual review)
   - Grants time-limited permission
   - Logs to audit
4. **Agent proceeds** with oversight
5. **Permission expires** automatically

### Scenario 2: Security Vulnerability Discovered

1. **Head of Security** discovers SQL injection risk
2. **Blocks delivery** immediately
3. **Specifies mitigations** required
4. **Agent implements** fixes
5. **Security reviews** again
6. **Unblocks** when safe

### Scenario 3: Emergency Security Incident

1. **Head of Security** detects critical vulnerability
2. **Freezes building** (all work stops)
3. **Triggers full audit**
4. **Coordinates remediation**
5. **Lifts freeze** when safe
6. **Complete post-mortem** logged

---

## Constitutional Binding

These executive interfaces are **constitutionally bound**:

### Consigliere
- **Authority Level:** Advisory only
- **Constitutional Role:** Translator between civilization and human
- **Hard Limits:** Cannot execute, cannot govern

### Head of Security
- **Authority Level:** Executive (security domain only)
- **Constitutional Role:** Safety guarantor
- **Absolute Limits:** Cannot change intent, code, or constitution

Both operate under **Density Codex** Layer 2 (Actors) and Layer 4 (Governance).

---

## Integration with Civilization

### Three-Entity Model

```
Human (Legislature)
  │
  ├─── Consigliere (Advice)
  │      └─── Explains, Translates, Previews, Prepares
  │
  ├─── Head of Security (Safety)
  │      └─── Grants, Blocks, Audits, Freezes
  │
  └─── Formal Directives (Law)
         └─── Creates Cognitive Contracts
```

### Authority Flow

```
Human
  ↓ issues directive
Consigliere (prepares)
  ↓ drafts
Human approves
  ↓ becomes binding
Cognitive Contract
  ↓ routes to floor
Floor processes
  ↓ may trigger
Head of Security (reviews)
  ↓ grants/blocks
Delivery
```

**All interactions are logged, audited, and governed.**
