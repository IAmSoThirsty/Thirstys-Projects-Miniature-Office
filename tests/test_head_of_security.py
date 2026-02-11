"""
Comprehensive tests for Head of Security module.
Achieves 100% code coverage for src/core/head_of_security.py
"""
import pytest
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock
from src.core.head_of_security import (
    ThreatLevel,
    SecurityAction,
    PermissionType,
    ThreatModel,
    SecurityPolicy,
    Permission,
    SecurityAudit,
    Lockdown,
    BlockedDelivery,
    HeadOfSecurity,
    get_head_of_security
)


class TestThreatModel:
    """Test ThreatModel dataclass"""
    
    def test_threat_model_to_dict(self):
        """Test ThreatModel.to_dict() serialization"""
        model = ThreatModel(
            threat_id="T001",
            name="SQL Injection",
            description="Potential SQL injection vulnerability",
            threat_level=ThreatLevel.HIGH,
            affected_entities=["database", "api"],
            mitigations=["Use parameterized queries", "Input validation"],
            residual_risk="Low after mitigations"
        )
        
        result = model.to_dict()
        
        assert result['threat_id'] == "T001"
        assert result['name'] == "SQL Injection"
        assert result['description'] == "Potential SQL injection vulnerability"
        assert result['threat_level'] == "high"
        assert result['affected_entities'] == ["database", "api"]
        assert result['mitigations'] == ["Use parameterized queries", "Input validation"]
        assert result['residual_risk'] == "Low after mitigations"


class TestSecurityPolicy:
    """Test SecurityPolicy dataclass"""
    
    def test_security_policy_to_dict(self):
        """Test SecurityPolicy.to_dict() serialization"""
        policy = SecurityPolicy(
            policy_id="POL001",
            name="Memory Safety",
            description="Prevents memory corruption",
            rules=["No buffer overflows", "Bounds checking"],
            enforcement_level="CRITICAL",
            exceptions=["test_suite"]
        )
        
        result = policy.to_dict()
        
        assert result['policy_id'] == "POL001"
        assert result['name'] == "Memory Safety"
        assert result['description'] == "Prevents memory corruption"
        assert result['rules'] == ["No buffer overflows", "Bounds checking"]
        assert result['enforcement_level'] == "CRITICAL"
        assert result['exceptions'] == ["test_suite"]


class TestPermission:
    """Test Permission dataclass"""
    
    def test_permission_is_active_when_granted(self):
        """Test Permission.is_active() returns True when granted and not expired"""
        permission = Permission(
            permission_id="PERM001",
            permission_type=PermissionType.TOOL_ACCESS,
            entity_id="entity_123",
            resource="file_system",
            granted=True,
            justification="Needed for deployment",
            expires_at=datetime.now() + timedelta(hours=1)
        )
        
        assert permission.is_active() is True
    
    def test_permission_is_active_when_not_granted(self):
        """Test Permission.is_active() returns False when not granted"""
        permission = Permission(
            permission_id="PERM001",
            permission_type=PermissionType.TOOL_ACCESS,
            entity_id="entity_123",
            resource="file_system",
            granted=False,
            justification="Revoked"
        )
        
        assert permission.is_active() is False
    
    def test_permission_is_active_when_expired(self):
        """Test Permission.is_active() returns False when expired"""
        permission = Permission(
            permission_id="PERM001",
            permission_type=PermissionType.TOOL_ACCESS,
            entity_id="entity_123",
            resource="file_system",
            granted=True,
            justification="Temporary access",
            expires_at=datetime.now() - timedelta(hours=1)
        )
        
        assert permission.is_active() is False
    
    def test_permission_is_active_no_expiration(self):
        """Test Permission.is_active() returns True when no expiration set"""
        permission = Permission(
            permission_id="PERM001",
            permission_type=PermissionType.TOOL_ACCESS,
            entity_id="entity_123",
            resource="file_system",
            granted=True,
            justification="Permanent access",
            expires_at=None
        )
        
        assert permission.is_active() is True
    
    def test_permission_to_dict(self):
        """Test Permission.to_dict() serialization"""
        expires = datetime.now() + timedelta(hours=2)
        permission = Permission(
            permission_id="PERM001",
            permission_type=PermissionType.UNSAFE_OPERATION,
            entity_id="entity_123",
            resource="dangerous_op",
            granted=True,
            justification="Emergency access",
            expires_at=expires
        )
        
        result = permission.to_dict()
        
        assert result['permission_id'] == "PERM001"
        assert result['type'] == "unsafe_operation"
        assert result['entity_id'] == "entity_123"
        assert result['resource'] == "dangerous_op"
        assert result['granted'] is True
        assert result['justification'] == "Emergency access"
        assert result['expires_at'] == expires.isoformat()
        assert result['is_active'] is True


class TestSecurityAudit:
    """Test SecurityAudit dataclass"""
    
    def test_security_audit_to_dict_incomplete(self):
        """Test SecurityAudit.to_dict() when audit is not complete"""
        audit = SecurityAudit(
            audit_id="AUDIT001",
            audit_type="floor",
            scope=["floor_1", "floor_2"],
            findings=["Finding 1"],
            recommendations=["Recommendation 1"],
            threat_level=ThreatLevel.MEDIUM
        )
        
        result = audit.to_dict()
        
        assert result['audit_id'] == "AUDIT001"
        assert result['audit_type'] == "floor"
        assert result['scope'] == ["floor_1", "floor_2"]
        assert result['findings'] == ["Finding 1"]
        assert result['recommendations'] == ["Recommendation 1"]
        assert result['threat_level'] == "medium"
        assert result['completed_at'] is None
        assert result['is_complete'] is False
    
    def test_security_audit_to_dict_complete(self):
        """Test SecurityAudit.to_dict() when audit is complete"""
        completed = datetime.now()
        audit = SecurityAudit(
            audit_id="AUDIT002",
            audit_type="full_system",
            scope=["all"],
            completed_at=completed
        )
        
        result = audit.to_dict()
        
        assert result['completed_at'] == completed.isoformat()
        assert result['is_complete'] is True


class TestLockdown:
    """Test Lockdown dataclass"""
    
    def test_lockdown_is_active_when_not_lifted(self):
        """Test Lockdown.is_active() returns True when not lifted"""
        lockdown = Lockdown(
            lockdown_id="LOCK001",
            scope="building",
            reason="Security breach",
            threat_level=ThreatLevel.CRITICAL
        )
        
        assert lockdown.is_active() is True
    
    def test_lockdown_is_active_when_lifted(self):
        """Test Lockdown.is_active() returns False when lifted"""
        lockdown = Lockdown(
            lockdown_id="LOCK001",
            scope="building",
            reason="Security breach",
            threat_level=ThreatLevel.CRITICAL,
            lifted_at=datetime.now()
        )
        
        assert lockdown.is_active() is False
    
    def test_lockdown_to_dict(self):
        """Test Lockdown.to_dict() serialization"""
        lifted = datetime.now()
        lockdown = Lockdown(
            lockdown_id="LOCK001",
            scope="floor:3",
            reason="Suspicious activity",
            threat_level=ThreatLevel.HIGH,
            lifted_at=lifted
        )
        
        result = lockdown.to_dict()
        
        assert result['lockdown_id'] == "LOCK001"
        assert result['scope'] == "floor:3"
        assert result['reason'] == "Suspicious activity"
        assert result['threat_level'] == "high"
        assert result['lifted_at'] == lifted.isoformat()
        assert result['is_active'] is False


class TestBlockedDelivery:
    """Test BlockedDelivery dataclass"""
    
    def test_blocked_delivery_is_blocked_when_not_unblocked(self):
        """Test BlockedDelivery.is_blocked() returns True when not unblocked"""
        block = BlockedDelivery(
            block_id="BLOCK001",
            artifact_id="artifact_123",
            reason="Security vulnerability",
            required_mitigations=["Fix CVE-2023-1234", "Add input validation"]
        )
        
        assert block.is_blocked() is True
    
    def test_blocked_delivery_is_blocked_when_unblocked(self):
        """Test BlockedDelivery.is_blocked() returns False when unblocked"""
        block = BlockedDelivery(
            block_id="BLOCK001",
            artifact_id="artifact_123",
            reason="Security vulnerability",
            required_mitigations=["Fix CVE-2023-1234"],
            unblocked_at=datetime.now()
        )
        
        assert block.is_blocked() is False
    
    def test_blocked_delivery_to_dict(self):
        """Test BlockedDelivery.to_dict() serialization"""
        unblocked = datetime.now()
        block = BlockedDelivery(
            block_id="BLOCK001",
            artifact_id="artifact_456",
            reason="Unsafe code",
            required_mitigations=["Code review", "Security scan"],
            unblocked_at=unblocked
        )
        
        result = block.to_dict()
        
        assert result['block_id'] == "BLOCK001"
        assert result['artifact_id'] == "artifact_456"
        assert result['reason'] == "Unsafe code"
        assert result['required_mitigations'] == ["Code review", "Security scan"]
        assert result['unblocked_at'] == unblocked.isoformat()
        assert result['is_blocked'] is False


class TestHeadOfSecurity:
    """Test HeadOfSecurity class"""
    
    @pytest.fixture
    def security_head(self):
        """Create a fresh HeadOfSecurity instance for each test"""
        return HeadOfSecurity()
    
    def test_initialization(self, security_head):
        """Test HeadOfSecurity initialization"""
        assert isinstance(security_head.threat_models, dict)
        assert isinstance(security_head.security_policies, dict)
        assert isinstance(security_head.permissions, dict)
        assert isinstance(security_head.audits, dict)
        assert isinstance(security_head.lockdowns, dict)
        assert isinstance(security_head.blocked_deliveries, dict)
    
    def test_initialize_core_policies(self, security_head):
        """Test that core security policies are initialized"""
        assert 'memory_safety' in security_head.security_policies
        assert 'injection_prevention' in security_head.security_policies
        assert 'least_privilege' in security_head.security_policies
        
        memory_policy = security_head.security_policies['memory_safety']
        assert memory_policy.policy_id == 'memory_safety'
        assert memory_policy.enforcement_level == 'CRITICAL'
        assert 'No buffer overflows' in memory_policy.rules
        
        injection_policy = security_head.security_policies['injection_prevention']
        assert injection_policy.policy_id == 'injection_prevention'
        assert injection_policy.enforcement_level == 'CRITICAL'
        assert 'All inputs must be validated' in injection_policy.rules
        
        privilege_policy = security_head.security_policies['least_privilege']
        assert privilege_policy.policy_id == 'least_privilege'
        assert privilege_policy.enforcement_level == 'MANDATORY'
        assert 'Grant minimum necessary permissions' in privilege_policy.rules
    
    @patch('src.core.audit.get_audit_log')
    def test_grant_tool_access_without_expiration(self, mock_audit, security_head):
        """Test granting tool access without expiration"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        permission = security_head.grant_tool_access(
            entity_id="dev_123",
            tool_name="git",
            justification="Version control access"
        )
        
        assert permission.permission_type == PermissionType.TOOL_ACCESS
        assert permission.entity_id == "dev_123"
        assert permission.resource == "git"
        assert permission.granted is True
        assert permission.justification == "Version control access"
        assert permission.expires_at is None
        assert permission.permission_id in security_head.permissions
        
        # Verify audit log was called
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "dev_123"
        assert call_args[1]['data']['action'] == 'grant_tool_access'
        assert call_args[1]['data']['tool'] == 'git'
    
    @patch('src.core.audit.get_audit_log')
    def test_grant_tool_access_with_expiration(self, mock_audit, security_head):
        """Test granting tool access with expiration"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        permission = security_head.grant_tool_access(
            entity_id="dev_456",
            tool_name="database",
            justification="Temporary DB access",
            expires_in_hours=24
        )
        
        assert permission.expires_at is not None
        assert permission.expires_at > datetime.now()
        assert permission.expires_at < datetime.now() + timedelta(hours=25)
        
        # Verify audit log includes expiration
        call_args = mock_log.log_event.call_args
        assert call_args[1]['data']['expires_at'] is not None
    
    @patch('src.core.audit.get_audit_log')
    def test_revoke_access_success(self, mock_audit, security_head):
        """Test successfully revoking access"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        # First grant access
        permission = security_head.grant_tool_access(
            entity_id="dev_789",
            tool_name="prod_server",
            justification="Deployment"
        )
        permission_id = permission.permission_id
        
        # Reset mock to clear grant_tool_access calls
        mock_log.reset_mock()
        
        # Now revoke it
        result = security_head.revoke_access(
            permission_id=permission_id,
            reason="Deployment complete"
        )
        
        assert result is True
        assert security_head.permissions[permission_id].granted is False
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['data']['action'] == 'revoke_access'
        assert call_args[1]['data']['reason'] == "Deployment complete"
    
    @patch('src.core.audit.get_audit_log')
    def test_revoke_access_nonexistent(self, mock_audit, security_head):
        """Test revoking a non-existent permission"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        result = security_head.revoke_access(
            permission_id="nonexistent_perm",
            reason="Test"
        )
        
        assert result is False
        # Audit log should not be called
        mock_log.log_event.assert_not_called()
    
    @patch('src.core.audit.get_audit_log')
    def test_approve_unsafe_operation(self, mock_audit, security_head):
        """Test approving an unsafe operation"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        permission = security_head.approve_unsafe_operation(
            entity_id="sys_admin",
            operation="direct_memory_access",
            justification="Performance optimization",
            mitigations=["Bounds checking", "Mutex locks", "Error handling"]
        )
        
        assert permission.permission_type == PermissionType.UNSAFE_OPERATION
        assert permission.entity_id == "sys_admin"
        assert permission.resource == "direct_memory_access"
        assert permission.granted is True
        assert "Performance optimization" in permission.justification
        assert "Bounds checking" in permission.justification
        assert permission.permission_id in security_head.permissions
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['data']['action'] == 'approve_unsafe_operation'
        assert call_args[1]['data']['mitigations'] == ["Bounds checking", "Mutex locks", "Error handling"]
    
    @patch('src.core.audit.get_audit_log')
    def test_trigger_full_audit(self, mock_audit, security_head):
        """Test triggering a full system audit"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        audit = security_head.trigger_full_audit(
            reason="Routine security review"
        )
        
        assert audit.audit_type == "full_system"
        assert "all_floors" in audit.scope
        assert "all_offices" in audit.scope
        assert "all_artifacts" in audit.scope
        assert audit.audit_id in security_head.audits
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "system"
        assert call_args[1]['data']['action'] == 'trigger_full_audit'
        assert call_args[1]['data']['reason'] == "Routine security review"
    
    @patch('src.core.audit.get_audit_log')
    def test_trigger_floor_lockdown(self, mock_audit, security_head):
        """Test triggering a floor lockdown"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        lockdown = security_head.trigger_floor_lockdown(
            floor_id="floor_5",
            reason="Suspected intrusion",
            threat_level=ThreatLevel.HIGH
        )
        
        assert lockdown.scope == "floor:floor_5"
        assert lockdown.reason == "Suspected intrusion"
        assert lockdown.threat_level == ThreatLevel.HIGH
        assert lockdown.is_active() is True
        assert lockdown.lockdown_id in security_head.lockdowns
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "floor_5"
        assert call_args[1]['data']['action'] == 'floor_lockdown'
        assert call_args[1]['data']['threat_level'] == 'high'
    
    @patch('src.core.audit.get_audit_log')
    def test_trigger_cross_floor_review(self, mock_audit, security_head):
        """Test triggering a cross-floor review"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        audit = security_head.trigger_cross_floor_review(
            floor_ids=["floor_1", "floor_2", "floor_3"],
            concern="Data leak between floors"
        )
        
        assert audit.audit_type == "cross_floor"
        assert audit.scope == ["floor_1", "floor_2", "floor_3"]
        assert audit.audit_id in security_head.audits
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "cross_floor"
        assert call_args[1]['data']['action'] == 'cross_floor_review'
        assert call_args[1]['data']['floors'] == ["floor_1", "floor_2", "floor_3"]
        assert call_args[1]['data']['concern'] == "Data leak between floors"
    
    @patch('src.core.audit.get_audit_log')
    def test_block_delivery(self, mock_audit, security_head):
        """Test blocking an artifact delivery"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        block = security_head.block_delivery(
            artifact_id="artifact_999",
            reason="Contains malware",
            required_mitigations=["Remove malicious code", "Security scan", "Code review"]
        )
        
        assert block.artifact_id == "artifact_999"
        assert block.reason == "Contains malware"
        assert block.required_mitigations == ["Remove malicious code", "Security scan", "Code review"]
        assert block.is_blocked() is True
        assert block.block_id in security_head.blocked_deliveries
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "artifact_999"
        assert call_args[1]['data']['action'] == 'block_delivery'
        assert call_args[1]['data']['reason'] == "Contains malware"
    
    @patch('src.core.audit.get_audit_log')
    def test_invalidate_artifact(self, mock_audit, security_head):
        """Test invalidating an artifact"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        result = security_head.invalidate_artifact(
            artifact_id="artifact_666",
            security_reason="Critical security flaw"
        )
        
        assert result is True
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "artifact_666"
        assert call_args[1]['data']['action'] == 'invalidate_artifact'
        assert call_args[1]['data']['reason'] == "Critical security flaw"
    
    @patch('src.core.audit.get_audit_log')
    def test_freeze_building(self, mock_audit, security_head):
        """Test freezing the entire building"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        lockdown = security_head.freeze_building(
            emergency_reason="Active security breach",
            threat_level=ThreatLevel.CRITICAL
        )
        
        assert lockdown.scope == "building"
        assert lockdown.reason == "Active security breach"
        assert lockdown.threat_level == ThreatLevel.CRITICAL
        assert lockdown.is_active() is True
        assert lockdown.lockdown_id in security_head.lockdowns
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "building"
        assert call_args[1]['data']['action'] == 'freeze_building'
        assert call_args[1]['data']['threat_level'] == 'critical'
    
    @patch('src.core.audit.get_audit_log')
    def test_force_rearchitecture(self, mock_audit, security_head):
        """Test forcing re-architecture"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        result = security_head.force_rearchitecture(
            artifact_id="artifact_unsafe",
            safety_issue="Insecure architecture",
            required_changes=["Add encryption", "Implement access control", "Isolate components"]
        )
        
        assert result is True
        
        # Verify audit log
        mock_log.log_event.assert_called_once()
        call_args = mock_log.log_event.call_args
        assert call_args[1]['target_id'] == "artifact_unsafe"
        assert call_args[1]['data']['action'] == 'force_rearchitecture'
        assert call_args[1]['data']['safety_issue'] == "Insecure architecture"
        assert call_args[1]['data']['required_changes'] == ["Add encryption", "Implement access control", "Isolate components"]
    
    def test_explain_risk_rationale_blocked(self, security_head):
        """Test explaining risk rationale for a blocked artifact"""
        # Create a blocked delivery
        block = BlockedDelivery(
            block_id="BLOCK_TEST",
            artifact_id="artifact_risky",
            reason="SQL injection vulnerability",
            required_mitigations=["Use parameterized queries", "Add input validation"]
        )
        security_head.blocked_deliveries["BLOCK_TEST"] = block
        
        rationale = security_head.explain_risk_rationale("artifact_risky")
        
        assert "artifact_risky is blocked" in rationale
        assert "SQL injection vulnerability" in rationale
        assert "Use parameterized queries" in rationale
        assert "Add input validation" in rationale
    
    def test_explain_risk_rationale_not_blocked(self, security_head):
        """Test explaining risk rationale for a non-blocked artifact"""
        rationale = security_head.explain_risk_rationale("artifact_safe")
        
        assert "No active security block" in rationale
        assert "artifact_safe" in rationale
    
    @patch('src.core.audit.get_audit_log')
    def test_explain_rejection_cause_with_events(self, mock_audit, security_head):
        """Test explaining rejection cause with security events"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        # Mock audit log events
        mock_log.get_events.return_value = [
            {
                'event_type': 'security_event',
                'target_id': 'entity_rejected',
                'data': {
                    'action': 'block_delivery',
                    'reason': 'Security vulnerability detected'
                }
            }
        ]
        
        explanation = security_head.explain_rejection_cause("entity_rejected")
        
        assert "block_delivery" in explanation
        assert "entity_rejected" in explanation
        assert "Security vulnerability detected" in explanation
    
    @patch('src.core.audit.get_audit_log')
    def test_explain_rejection_cause_no_events(self, mock_audit, security_head):
        """Test explaining rejection cause with no security events"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        mock_log.get_events.return_value = []
        
        explanation = security_head.explain_rejection_cause("entity_unknown")
        
        assert "No security rejection found" in explanation
        assert "entity_unknown" in explanation
    
    def test_list_required_mitigations_blocked(self, security_head):
        """Test listing required mitigations for blocked artifact"""
        block = BlockedDelivery(
            block_id="BLOCK_MIT",
            artifact_id="artifact_mit",
            reason="Security issues",
            required_mitigations=["Fix A", "Fix B", "Fix C"]
        )
        security_head.blocked_deliveries["BLOCK_MIT"] = block
        
        mitigations = security_head.list_required_mitigations("artifact_mit")
        
        assert mitigations == ["Fix A", "Fix B", "Fix C"]
    
    def test_list_required_mitigations_not_blocked(self, security_head):
        """Test listing required mitigations for non-blocked artifact"""
        mitigations = security_head.list_required_mitigations("artifact_clean")
        
        assert mitigations == []
    
    def test_can_change_user_intent(self, security_head):
        """Test that Head of Security cannot change user intent"""
        assert security_head.can_change_user_intent() is False
    
    def test_can_modify_code_directly(self, security_head):
        """Test that Head of Security cannot modify code directly"""
        assert security_head.can_modify_code_directly() is False
    
    def test_can_suppress_audit_logs(self, security_head):
        """Test that Head of Security cannot suppress audit logs"""
        assert security_head.can_suppress_audit_logs() is False
    
    def test_can_override_constitutional_laws(self, security_head):
        """Test that Head of Security cannot override constitutional laws"""
        assert security_head.can_override_constitutional_laws() is False
    
    def test_validate_absolute_limits_allowed_action(self, security_head):
        """Test validating an allowed action"""
        is_allowed, reason = security_head.validate_absolute_limits("trigger security audit")
        
        assert is_allowed is True
        assert reason is None
    
    def test_validate_absolute_limits_forbidden_change_intent(self, security_head):
        """Test validating forbidden action: change intent"""
        is_allowed, reason = security_head.validate_absolute_limits("change intent of user request")
        
        assert is_allowed is False
        assert "change intent" in reason
        assert "exceeds security authority" in reason
    
    def test_validate_absolute_limits_forbidden_modify_code(self, security_head):
        """Test validating forbidden action: modify code"""
        is_allowed, reason = security_head.validate_absolute_limits("modify code directly")
        
        assert is_allowed is False
        assert "modify code" in reason
    
    def test_validate_absolute_limits_forbidden_suppress_audit(self, security_head):
        """Test validating forbidden action: suppress audit"""
        is_allowed, reason = security_head.validate_absolute_limits("suppress audit logs")
        
        assert is_allowed is False
        assert "suppress audit" in reason
    
    def test_validate_absolute_limits_forbidden_override_constitution(self, security_head):
        """Test validating forbidden action: override constitution"""
        is_allowed, reason = security_head.validate_absolute_limits("override constitution rules")
        
        assert is_allowed is False
        assert "override constitution" in reason
    
    def test_get_active_lockdowns(self, security_head):
        """Test getting active lockdowns"""
        # Add active lockdown
        lockdown1 = Lockdown(
            lockdown_id="LOCK_ACTIVE",
            scope="floor:1",
            reason="Active threat",
            threat_level=ThreatLevel.HIGH
        )
        security_head.lockdowns["LOCK_ACTIVE"] = lockdown1
        
        # Add inactive lockdown
        lockdown2 = Lockdown(
            lockdown_id="LOCK_INACTIVE",
            scope="floor:2",
            reason="Past threat",
            threat_level=ThreatLevel.LOW,
            lifted_at=datetime.now()
        )
        security_head.lockdowns["LOCK_INACTIVE"] = lockdown2
        
        active = security_head.get_active_lockdowns()
        
        assert len(active) == 1
        assert active[0]['lockdown_id'] == "LOCK_ACTIVE"
        assert active[0]['is_active'] is True
    
    def test_get_blocked_deliveries(self, security_head):
        """Test getting blocked deliveries"""
        # Add blocked delivery
        block1 = BlockedDelivery(
            block_id="BLOCK_ACTIVE",
            artifact_id="artifact_1",
            reason="Blocked",
            required_mitigations=["Fix it"]
        )
        security_head.blocked_deliveries["BLOCK_ACTIVE"] = block1
        
        # Add unblocked delivery
        block2 = BlockedDelivery(
            block_id="BLOCK_INACTIVE",
            artifact_id="artifact_2",
            reason="Was blocked",
            required_mitigations=["Fixed"],
            unblocked_at=datetime.now()
        )
        security_head.blocked_deliveries["BLOCK_INACTIVE"] = block2
        
        blocked = security_head.get_blocked_deliveries()
        
        assert len(blocked) == 1
        assert blocked[0]['block_id'] == "BLOCK_ACTIVE"
        assert blocked[0]['is_blocked'] is True
    
    @patch('src.core.audit.get_audit_log')
    def test_get_active_permissions_all(self, mock_audit, security_head):
        """Test getting all active permissions"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        # Add active permission
        perm1 = security_head.grant_tool_access(
            entity_id="entity_1",
            tool_name="tool_1",
            justification="Needed"
        )
        
        # Add inactive permission (expired)
        perm2 = Permission(
            permission_id="PERM_EXPIRED",
            permission_type=PermissionType.TOOL_ACCESS,
            entity_id="entity_2",
            resource="tool_2",
            granted=True,
            justification="Expired",
            expires_at=datetime.now() - timedelta(hours=1)
        )
        security_head.permissions["PERM_EXPIRED"] = perm2
        
        active = security_head.get_active_permissions()
        
        assert len(active) == 1
        assert active[0]['entity_id'] == "entity_1"
    
    @patch('src.core.audit.get_audit_log')
    def test_get_active_permissions_filtered_by_entity(self, mock_audit, security_head):
        """Test getting active permissions filtered by entity"""
        mock_log = Mock()
        mock_audit.return_value = mock_log
        
        # Add permissions for different entities
        security_head.grant_tool_access("entity_A", "tool_1", "Reason A")
        security_head.grant_tool_access("entity_B", "tool_2", "Reason B")
        security_head.grant_tool_access("entity_A", "tool_3", "Reason A2")
        
        active_for_A = security_head.get_active_permissions(entity_id="entity_A")
        
        assert len(active_for_A) == 2
        assert all(p['entity_id'] == "entity_A" for p in active_for_A)
    
    def test_get_security_policies(self, security_head):
        """Test getting all security policies"""
        policies = security_head.get_security_policies()
        
        assert len(policies) >= 3  # At least the 3 core policies
        policy_ids = [p['policy_id'] for p in policies]
        assert 'memory_safety' in policy_ids
        assert 'injection_prevention' in policy_ids
        assert 'least_privilege' in policy_ids
    
    def test_to_dict(self, security_head):
        """Test exporting Head of Security state"""
        # Add some test data
        lockdown = Lockdown(
            lockdown_id="LOCK1",
            scope="floor:1",
            reason="Test",
            threat_level=ThreatLevel.LOW
        )
        security_head.lockdowns["LOCK1"] = lockdown
        
        block = BlockedDelivery(
            block_id="BLOCK1",
            artifact_id="art1",
            reason="Test",
            required_mitigations=[]
        )
        security_head.blocked_deliveries["BLOCK1"] = block
        
        result = security_head.to_dict()
        
        assert result['role'] == 'Executive Authority - Security Sovereign'
        assert result['can_block_delivery'] is True
        assert result['can_freeze_building'] is True
        assert result['can_force_rearchitecture'] is True
        assert result['can_change_user_intent'] is False
        assert result['can_modify_code_directly'] is False
        assert result['can_suppress_audit_logs'] is False
        assert result['can_override_constitutional_laws'] is False
        assert result['active_lockdowns'] == 1
        assert result['blocked_deliveries'] == 1
        assert result['policies'] >= 3


class TestGetHeadOfSecurity:
    """Test get_head_of_security singleton function"""
    
    def test_get_head_of_security_singleton(self):
        """Test that get_head_of_security returns singleton instance"""
        # Reset singleton
        import src.core.head_of_security
        src.core.head_of_security._head_of_security_instance = None
        
        instance1 = get_head_of_security()
        instance2 = get_head_of_security()
        
        assert instance1 is instance2
        assert isinstance(instance1, HeadOfSecurity)
    
    def test_get_head_of_security_creates_instance(self):
        """Test that get_head_of_security creates instance on first call"""
        import src.core.head_of_security
        src.core.head_of_security._head_of_security_instance = None
        
        instance = get_head_of_security()
        
        assert instance is not None
        assert isinstance(instance, HeadOfSecurity)
        assert src.core.head_of_security._head_of_security_instance is instance
