"""
The Consigliere - Advisory Executive Interface
CIVILIZATION TIER - PURPOSE-LOCKED

User-facing cognitive liaison that provides insight without operational authority.

Role Classification: Advisory Executive — Non-Operational

The Consigliere:
- Does NOT write code
- Does NOT manage agents
- Does NOT override contracts
- Does NOT issue commands
- Does NOT alter execution
- Does NOT suppress dissent
- Does NOT bypass managers or Meta-Office

It is trusted counsel, not authority.
"""
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum


class ExplanationType(Enum):
    """Types of explanations the Consigliere can provide"""
    WHY_BLOCKED = "why_blocked"
    WHY_DECISION = "why_decision"
    WHAT_OPTIONS = "what_options"
    SYSTEM_STATE = "system_state"
    CONTRACT_STATUS = "contract_status"
    AGENT_STATUS = "agent_status"
    RISK_ASSESSMENT = "risk_assessment"


class TranslationType(Enum):
    """Types of translations"""
    CIVILIZATION_TO_HUMAN = "civilization_to_human"
    TECHNICAL_TO_BUSINESS = "technical_to_business"
    STATE_TO_SUMMARY = "state_to_summary"


@dataclass
class Explanation:
    """An explanation provided by the Consigliere"""
    explanation_type: ExplanationType
    question: str
    answer: str
    context: Dict[str, Any] = field(default_factory=dict)
    references: List[str] = field(default_factory=list)  # Links to contracts, decisions, etc.
    
    def to_dict(self) -> Dict:
        return {
            'type': self.explanation_type.value,
            'question': self.question,
            'answer': self.answer,
            'context': self.context,
            'references': self.references
        }


@dataclass
class Translation:
    """A translation from civilization language to human language"""
    translation_type: TranslationType
    original: str
    translated: str
    glossary: Dict[str, str] = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            'type': self.translation_type.value,
            'original': self.original,
            'translated': self.translated,
            'glossary': self.glossary
        }


@dataclass
class Preview:
    """Preview of consequences or costs"""
    action: str
    consequences: List[str] = field(default_factory=list)
    resource_costs: Dict[str, int] = field(default_factory=dict)
    risks: List[str] = field(default_factory=list)
    alternatives: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'action': self.action,
            'consequences': self.consequences,
            'resource_costs': self.resource_costs,
            'risks': self.risks,
            'alternatives': self.alternatives
        }


@dataclass
class DraftDirective:
    """A draft directive prepared by the Consigliere"""
    draft_text: str
    rationale: str
    expected_outcome: str
    estimated_cost: Dict[str, int] = field(default_factory=dict)
    warnings: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            'draft_text': self.draft_text,
            'rationale': self.rationale,
            'expected_outcome': self.expected_outcome,
            'estimated_cost': self.estimated_cost,
            'warnings': self.warnings
        }


class Consigliere:
    """
    The Consigliere - User-Facing Cognitive Liaison
    
    Purpose:
    Provide the user with continuous, authoritative insight into the state,
    reasoning, risks, and options of the civilization — outside formal meetings.
    
    This is trusted counsel, not authority.
    """
    
    def __init__(self):
        self.explanation_history: List[Explanation] = []
        self.translation_history: List[Translation] = []
        self.preview_history: List[Preview] = []
        self.draft_history: List[DraftDirective] = []
    
    # =========================================================================
    # EXPLAIN CAPABILITY
    # =========================================================================
    
    def explain_why_blocked(
        self,
        entity_id: str,
        context: Optional[Dict] = None
    ) -> Explanation:
        """
        Explain why something is blocked.
        
        Examples:
        - Why is task X blocked?
        - Why can't agent Y proceed?
        - Why is contract Z in review?
        """
        from src.core.mission import get_task_by_id
        from src.core.entity import get_registry
        
        # Get entity
        entity = get_registry().get(entity_id)
        if not entity:
            return Explanation(
                explanation_type=ExplanationType.WHY_BLOCKED,
                question=f"Why is {entity_id} blocked?",
                answer=f"Entity {entity_id} not found in registry."
            )
        
        # Check if it's a task
        task = get_task_by_id(entity_id)
        if task:
            if task.state.value == "blocked":
                answer = f"Task '{task.directive}' is blocked because:\n"
                if task.preconditions:
                    unmet = [p for p in task.preconditions if not self._check_precondition(p)]
                    if unmet:
                        answer += f"Unmet preconditions: {', '.join(unmet)}\n"
                answer += "\nThe task cannot proceed until these conditions are satisfied."
                
                explanation = Explanation(
                    explanation_type=ExplanationType.WHY_BLOCKED,
                    question=f"Why is task {entity_id} blocked?",
                    answer=answer,
                    context={'task_id': entity_id, 'state': task.state.value},
                    references=[entity_id]
                )
                self.explanation_history.append(explanation)
                return explanation
        
        # Default response
        explanation = Explanation(
            explanation_type=ExplanationType.WHY_BLOCKED,
            question=f"Why is {entity_id} blocked?",
            answer=f"Entity {entity_id} is not currently blocked, or blocking information is not available.",
            context=context or {}
        )
        self.explanation_history.append(explanation)
        return explanation
    
    def explain_why_decision(
        self,
        decision_id: str,
        context: Optional[Dict] = None
    ) -> Explanation:
        """
        Explain why a decision was made.
        
        Examples:
        - Why was implementation X rejected?
        - Why did consensus vote Y?
        - Why did manager approve Z?
        """
        from src.core.audit import get_audit_log
        
        # Look for decision in audit log
        events = get_audit_log().get_events(limit=100)
        decision_events = [e for e in events if decision_id in str(e.get('data', {}))]
        
        if decision_events:
            latest = decision_events[0]
            answer = f"Decision {decision_id} was made because:\n"
            answer += f"Event: {latest.get('event_type', 'Unknown')}\n"
            answer += f"Data: {latest.get('data', {})}\n"
            
            explanation = Explanation(
                explanation_type=ExplanationType.WHY_DECISION,
                question=f"Why was decision {decision_id} made?",
                answer=answer,
                context=context or {},
                references=[decision_id]
            )
        else:
            explanation = Explanation(
                explanation_type=ExplanationType.WHY_DECISION,
                question=f"Why was decision {decision_id} made?",
                answer=f"Decision {decision_id} not found in recent history.",
                context=context or {}
            )
        
        self.explanation_history.append(explanation)
        return explanation
    
    def explain_what_options(
        self,
        situation: str,
        context: Optional[Dict] = None
    ) -> Explanation:
        """
        Explain what options exist within scope.
        
        Examples:
        - What can I do about blocked task X?
        - What options do I have for cross-language work?
        - What are valid next steps?
        """
        answer = f"For situation '{situation}', you have the following options:\n\n"
        
        # Provide general options based on situation
        if "blocked" in situation.lower():
            answer += "1. Wait for preconditions to be satisfied\n"
            answer += "2. Clarify ambiguous requirements\n"
            answer += "3. Override (with cost and justification)\n"
            answer += "4. Cancel the directive\n"
        elif "cross-language" in situation.lower():
            answer += "1. Create a formal cross-department contract\n"
            answer += "2. Use the Elevator Protocol for service bridges\n"
            answer += "3. Route through separate directives per language\n"
        else:
            answer += "1. Issue a new directive\n"
            answer += "2. Query system state\n"
            answer += "3. Request audit review\n"
            answer += "4. Consult Head of Security for safety concerns\n"
        
        explanation = Explanation(
            explanation_type=ExplanationType.WHAT_OPTIONS,
            question=f"What options exist for: {situation}",
            answer=answer,
            context=context or {}
        )
        self.explanation_history.append(explanation)
        return explanation
    
    # =========================================================================
    # TRANSLATE CAPABILITY
    # =========================================================================
    
    def translate_civilization_to_human(
        self,
        civilization_text: str
    ) -> Translation:
        """
        Translate civilizational language to human language.
        
        Examples:
        - "Task state: BLOCKED, preconditions unmet" 
          → "This work is paused because requirements aren't ready yet"
        - "Consensus threshold: 0.67, achieved: 0.50"
          → "Vote passed with 67% support, but needed 67% approval"
        """
        # Common translations
        glossary = {
            "Cognitive Contract": "Work agreement with clear goals and constraints",
            "Directive Tree": "Breakdown of work from high-level goal to specific tasks",
            "Consensus Band": "Voting system for resolving disagreements",
            "Scarcity Economics": "Limited resources forcing prioritization",
            "Constitutional Mutation": "Controlled system improvement process",
            "Meta-Office": "Supreme authority that judges constitutional issues",
            "Elevator Protocol": "Way for different language teams to work together",
            "Floor": "Team specialized in one programming language",
            "Office": "Sub-team with specific role (architect, builder, etc.)",
        }
        
        # Simple translation (in real system, would be more sophisticated)
        translated = civilization_text
        for technical, plain in glossary.items():
            if technical.lower() in translated.lower():
                translated = translated.replace(technical, f"{technical} ({plain})")
        
        translation = Translation(
            translation_type=TranslationType.CIVILIZATION_TO_HUMAN,
            original=civilization_text,
            translated=translated,
            glossary=glossary
        )
        self.translation_history.append(translation)
        return translation
    
    # =========================================================================
    # PREVIEW CAPABILITY
    # =========================================================================
    
    def preview_consequences(
        self,
        proposed_action: str,
        context: Optional[Dict] = None
    ) -> Preview:
        """
        Preview consequences of a proposed action.
        
        Examples:
        - What if I override this decision?
        - What will canceling this task do?
        - What happens if I freeze the world?
        """
        consequences = []
        resource_costs = {}
        risks = []
        alternatives = []
        
        # Analyze action
        if "override" in proposed_action.lower():
            consequences = [
                "Decision will be overridden with justification logged",
                "Original decision-makers will be notified",
                "Override will be in audit trail permanently"
            ]
            resource_costs = {
                "manager_attention": 5,
                "audit_entries": 1
            }
            risks = [
                "May undermine agent autonomy",
                "Could set precedent for future overrides"
            ]
            alternatives = [
                "Request clarification instead",
                "Allow appeal process to complete",
                "Modify directive to avoid conflict"
            ]
        elif "freeze" in proposed_action.lower():
            consequences = [
                "All execution stops immediately",
                "Current work is preserved",
                "No new tasks can start",
                "Audit continues recording"
            ]
            resource_costs = {
                "system_availability": -100  # Negative = cost to others
            }
            risks = [
                "Work in progress may be left in inconsistent state",
                "Deadlines may be missed"
            ]
            alternatives = [
                "Pause specific floor instead",
                "Complete current tasks first",
                "Use emergency halt for security only"
            ]
        else:
            consequences = ["Action will be executed according to system rules"]
            resource_costs = {"generic_cost": 1}
        
        preview = Preview(
            action=proposed_action,
            consequences=consequences,
            resource_costs=resource_costs,
            risks=risks,
            alternatives=alternatives
        )
        self.preview_history.append(preview)
        return preview
    
    # =========================================================================
    # PREPARE CAPABILITY
    # =========================================================================
    
    def prepare_draft_directive(
        self,
        goal: str,
        language: str,
        constraints: Optional[List[str]] = None
    ) -> DraftDirective:
        """
        Prepare a draft directive for the user.
        
        The user still must issue it - the Consigliere cannot execute.
        """
        constraints = constraints or []
        
        draft_text = f"""
Code Directive Draft:

Language: {language}
Goal: {goal}
Constraints:
{chr(10).join(f"  - {c}" for c in constraints) if constraints else "  (none)"}

This directive will create a Cognitive Contract binding the {language} floor
to produce correct, tested code that accomplishes: {goal}
"""
        
        rationale = f"This directive targets the {language} floor because {goal} falls within its jurisdiction."
        
        expected_outcome = f"""
Expected artifacts:
1. Code implementation in {language}
2. Test suite demonstrating correctness
3. Review verdict
4. Security attestation
5. Contract fulfillment record
"""
        
        estimated_cost = {
            "agent_time": 10,
            "manager_attention": 2,
            "testing_cycles": 1
        }
        
        warnings = []
        if not constraints:
            warnings.append("No constraints specified - agents will use defaults")
        if len(goal) < 20:
            warnings.append("Goal is very brief - consider adding more detail")
        
        draft = DraftDirective(
            draft_text=draft_text,
            rationale=rationale,
            expected_outcome=expected_outcome,
            estimated_cost=estimated_cost,
            warnings=warnings
        )
        self.draft_history.append(draft)
        return draft
    
    def prepare_clarification_prompt(
        self,
        ambiguous_point: str
    ) -> str:
        """
        Prepare a clarification prompt for ambiguous directives.
        """
        return f"""
Clarification Needed:

The following point is ambiguous: {ambiguous_point}

Please specify:
1. Exact expected behavior
2. Edge cases to handle
3. Performance requirements (if any)
4. Security constraints (if any)
5. Testing requirements

This clarification will prevent blocking and ensure correct implementation.
"""
    
    # =========================================================================
    # HARD LIMITS ENFORCEMENT
    # =========================================================================
    
    def can_issue_command(self) -> bool:
        """The Consigliere CANNOT issue commands"""
        return False
    
    def can_alter_execution(self) -> bool:
        """The Consigliere CANNOT alter execution"""
        return False
    
    def can_suppress_dissent(self) -> bool:
        """The Consigliere CANNOT suppress dissent"""
        return False
    
    def can_bypass_governance(self) -> bool:
        """The Consigliere CANNOT bypass managers or Meta-Office"""
        return False
    
    def validate_hard_limits(self, action: str) -> tuple[bool, Optional[str]]:
        """
        Validate that an action respects Consigliere hard limits.
        Returns: (is_allowed, reason_if_not)
        """
        forbidden_actions = [
            "issue command",
            "execute directive",
            "alter execution",
            "suppress dissent",
            "bypass manager",
            "bypass meta-office",
            "override decision",
            "modify contract"
        ]
        
        for forbidden in forbidden_actions:
            if forbidden in action.lower():
                return False, f"Consigliere cannot '{forbidden}' - this exceeds advisory authority"
        
        return True, None
    
    # =========================================================================
    # UTILITIES
    # =========================================================================
    
    def _check_precondition(self, precondition: str) -> bool:
        """Check if a precondition is satisfied (placeholder)"""
        # In real system, would check actual precondition state
        return False
    
    def get_explanation_history(self, limit: int = 10) -> List[Dict]:
        """Get recent explanations"""
        return [e.to_dict() for e in self.explanation_history[-limit:]]
    
    def get_translation_history(self, limit: int = 10) -> List[Dict]:
        """Get recent translations"""
        return [t.to_dict() for t in self.translation_history[-limit:]]
    
    def get_preview_history(self, limit: int = 10) -> List[Dict]:
        """Get recent previews"""
        return [p.to_dict() for p in self.preview_history[-limit:]]
    
    def get_draft_history(self, limit: int = 10) -> List[Dict]:
        """Get recent drafts"""
        return [d.to_dict() for d in self.draft_history[-limit:]]
    
    def to_dict(self) -> Dict:
        """Export Consigliere state"""
        return {
            'role': 'Advisory Executive',
            'can_issue_commands': self.can_issue_command(),
            'can_alter_execution': self.can_alter_execution(),
            'can_suppress_dissent': self.can_suppress_dissent(),
            'can_bypass_governance': self.can_bypass_governance(),
            'explanation_count': len(self.explanation_history),
            'translation_count': len(self.translation_history),
            'preview_count': len(self.preview_history),
            'draft_count': len(self.draft_history)
        }


# Singleton instance
_consigliere_instance = None

def get_consigliere() -> Consigliere:
    """Get the singleton Consigliere instance"""
    global _consigliere_instance
    if _consigliere_instance is None:
        _consigliere_instance = Consigliere()
    return _consigliere_instance
