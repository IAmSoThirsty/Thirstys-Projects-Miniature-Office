"""Comprehensive tests for the canonical bundle module."""
import pytest
from datetime import datetime
from src.core.canonical_bundle import (
    # I. Foundational Legitimacy Pack
    CivilizationCharter,
    AuthorityGrant,
    AuthorityRoleLedger,
    PurposeLockCheck,
    PurposeLockAttestation,
    # II. Directive & Governance Records
    BoardResolution,
    BoardResolutionArchive,
    DirectivePrecedent,
    DirectivePrecedentCorpus,
    MetaOfficeRuling,
    MetaOfficeRulingsLedger,
    # III. Verification & Proof Pack
    FailureResponse,
    LawFailureResponseMatrix,
    FormalInvariant,
    FormalLawVerificationModels,
    ViolationPlaybook,
    InvariantViolationPlaybooks,
    # IV. Execution Evidence
    CanonicalExecutionKernel,
    SimulationTrace,
    SimulationTraceCorpus,
    ReproducibilityPacket,
    ReproducibilityPackets,
    # V. Floor & Contract Artifacts
    FloorRuntimeProfile,
    FloorRuntimeProfiles,
    CrossFloorContract,
    CrossFloorContractRegistry,
    ContractDriftReport,
    ContractDriftReports,
    # VI. Toolchain & Supply-Chain Trust
    ToolProvenanceRecord,
    ToolProvenanceTrustLedger,
    UnsafeCapabilityException,
    UnsafeCapabilityExceptionRecords,
    # VII. Human-in-the-Loop Records
    ConsigliereInteraction,
    ConsigliereInteractionLogs,
    SecurityDecision,
    SecurityDecisionDossiers,
    Override,
    OverrideCostLedger,
    # VIII. Evolution & Change Control
    ConstitutionalAmendment,
    ConstitutionalAmendmentRegistry,
    RejectedProposal,
    DormantRejectedProposalArchive,
    # IX. Audit & External Trust
    IndependentAuditInterface,
    ComplianceReport,
    ComplianceCertificationReports,
    # X. Termination & Continuity
    CivilizationFreezeProtocol,
    CivilizationShutdownSuccession,
    # XI. Meta-Evaluation
    SuccessFailureMetricsCanon,
    # Bundle Manager
    NonDesignCanonicalBundle,
    create_canonical_bundle,
    get_canonical_bundle,
)


class TestCivilizationCharter:
    """Test CivilizationCharter class."""
    
    def test_charter_creation(self):
        """Test basic charter creation."""
        charter = CivilizationCharter(
            charter_id="charter-001",
            version="1.0.0",
            issued_date=datetime.now(),
            axioms=["Axiom 1", "Axiom 2"],
            constitutional_laws=[
                {
                    "class": "foundation",
                    "name": "Law 1",
                    "statement": "All agents must obey",
                    "enforcement": "automatic"
                }
            ],
            role_definitions={
                "agent": {
                    "powers": ["execute", "report"],
                    "prohibitions": ["self-modify"]
                }
            },
            limits_and_prohibitions={
                "general": ["No autonomous goals"]
            },
            supersession_rules=["Rule 1"],
            digital_signature="sig-001"
        )
        
        assert charter.charter_id == "charter-001"
        assert charter.version == "1.0.0"
        assert charter.is_immutable is True
        assert len(charter.axioms) == 2
    
    def test_to_human_readable(self):
        """Test converting charter to human-readable format."""
        charter = CivilizationCharter(
            charter_id="charter-001",
            version="1.0.0",
            issued_date=datetime(2024, 1, 1),
            axioms=["Axiom 1", "Axiom 2"],
            constitutional_laws=[
                {
                    "class": "foundation",
                    "name": "Law 1",
                    "statement": "All agents must obey",
                    "enforcement": "automatic"
                }
            ],
            role_definitions={
                "agent": {
                    "powers": ["execute"],
                    "prohibitions": ["self-modify"]
                }
            },
            limits_and_prohibitions={},
            supersession_rules=[],
            digital_signature="sig-001"
        )
        
        text = charter.to_human_readable()
        assert "CIVILIZATION CHARTER" in text
        assert "Version: 1.0.0" in text
        assert "Axiom 1" in text
        assert "Law 1" in text
        assert "agent:" in text
        assert "Digital Signature: sig-001" in text
        assert "Immutable: True" in text
    
    def test_verify_signature(self):
        """Test signature verification."""
        charter = CivilizationCharter(
            charter_id="charter-001",
            version="1.0.0",
            issued_date=datetime.now(),
            axioms=[],
            constitutional_laws=[],
            role_definitions={},
            limits_and_prohibitions={},
            supersession_rules=[],
            digital_signature="sig-001"
        )
        
        # Placeholder implementation returns True
        assert charter.verify_signature("public-key") is True


class TestAuthorityRoleLedger:
    """Test AuthorityRoleLedger class."""
    
    def test_grant_authority(self):
        """Test granting authority."""
        ledger = AuthorityRoleLedger(
            ledger_id="ledger-001",
            created_at=datetime.now(),
            roles={},
            grants=[]
        )
        
        grant_id = ledger.grant_authority(
            role="admin",
            authority="deploy",
            to_entity="entity-001",
            by_entity="entity-000",
            justification="Initial setup"
        )
        
        assert grant_id == "grant-1"
        assert len(ledger.grants) == 1
        assert ledger.grants[0].role == "admin"
        assert ledger.grants[0].authority == "deploy"
        assert ledger.grants[0].revoked is False
    
    def test_revoke_authority(self):
        """Test revoking authority."""
        ledger = AuthorityRoleLedger(
            ledger_id="ledger-001",
            created_at=datetime.now(),
            roles={},
            grants=[]
        )
        
        grant_id = ledger.grant_authority(
            "admin", "deploy", "entity-001", "entity-000", "Setup"
        )
        
        result = ledger.revoke_authority(grant_id, "entity-000", "No longer needed")
        
        assert result is True
        assert ledger.grants[0].revoked is True
        assert ledger.grants[0].revoked_by == "entity-000"
        assert "REVOKED" in ledger.grants[0].justification
    
    def test_revoke_nonexistent_authority(self):
        """Test revoking non-existent authority."""
        ledger = AuthorityRoleLedger(
            ledger_id="ledger-001",
            created_at=datetime.now(),
            roles={},
            grants=[]
        )
        
        result = ledger.revoke_authority("grant-999", "entity-000", "Reason")
        assert result is False
    
    def test_get_active_authorities(self):
        """Test getting active authorities for an entity."""
        ledger = AuthorityRoleLedger(
            ledger_id="ledger-001",
            created_at=datetime.now(),
            roles={},
            grants=[]
        )
        
        ledger.grant_authority("admin", "deploy", "entity-001", "entity-000", "Setup")
        ledger.grant_authority("admin", "monitor", "entity-001", "entity-000", "Setup")
        grant_id = ledger.grant_authority("admin", "delete", "entity-001", "entity-000", "Setup")
        ledger.revoke_authority(grant_id, "entity-000", "Removed")
        
        authorities = ledger.get_active_authorities("entity-001")
        
        assert len(authorities) == 2
        assert "deploy" in authorities
        assert "monitor" in authorities
        assert "delete" not in authorities
    
    def test_has_authority(self):
        """Test checking if entity has authority."""
        ledger = AuthorityRoleLedger(
            ledger_id="ledger-001",
            created_at=datetime.now(),
            roles={},
            grants=[]
        )
        
        ledger.grant_authority("admin", "deploy", "entity-001", "entity-000", "Setup")
        
        assert ledger.has_authority("entity-001", "deploy") is True
        assert ledger.has_authority("entity-001", "delete") is False


class TestPurposeLockAttestation:
    """Test PurposeLockAttestation class."""
    
    def test_verify_purpose_lock(self):
        """Test verifying purpose lock for a subsystem."""
        attestation = PurposeLockAttestation(
            attestation_id="attest-001",
            version="1.0.0",
            timestamp=datetime.now(),
            subsystems_checked=["subsystem1"],
            checks=[],
            overall_locked=True,
            attestation_signature="sig-001"
        )
        
        is_locked, violations = attestation.verify_purpose_lock("subsystem2")
        
        assert is_locked is True
        assert len(violations) == 0
        assert len(attestation.checks) == 1
        assert attestation.checks[0].subsystem == "subsystem2"
    
    def test_generate_attestation_report(self):
        """Test generating attestation report."""
        attestation = PurposeLockAttestation(
            attestation_id="attest-001",
            version="1.0.0",
            timestamp=datetime(2024, 1, 1),
            subsystems_checked=[],
            checks=[],
            overall_locked=True,
            attestation_signature="sig-001"
        )
        
        attestation.verify_purpose_lock("subsystem1")
        attestation.verify_purpose_lock("subsystem2")
        
        report = attestation.generate_attestation_report()
        
        assert "PURPOSE LOCK ATTESTATION" in report
        assert "Version: 1.0.0" in report
        assert "subsystem1: ✓ LOCKED" in report
        assert "subsystem2: ✓ LOCKED" in report
        assert "Overall Status: LOCKED" in report
        assert "Signature: sig-001" in report
    
    def test_generate_attestation_report_with_violations(self):
        """Test generating attestation report with violations."""
        attestation = PurposeLockAttestation(
            attestation_id="attest-001",
            version="1.0.0",
            timestamp=datetime(2024, 1, 1),
            subsystems_checked=[],
            checks=[],
            overall_locked=False,
            attestation_signature="sig-001"
        )
        
        # Add a check with violations
        check = PurposeLockCheck(
            check_id="check-1",
            subsystem="subsystem3",
            timestamp=datetime.now(),
            is_locked=False,
            violations=["Autonomous goal formation detected", "Exploratory behavior found"],
            evidence={"details": "test"}
        )
        attestation.checks.append(check)
        
        report = attestation.generate_attestation_report()
        
        assert "subsystem3: ✗ UNLOCKED" in report
        assert "Violations: Autonomous goal formation detected, Exploratory behavior found" in report
        assert "Overall Status: UNLOCKED" in report


class TestBoardResolutionArchive:
    """Test BoardResolutionArchive class."""
    
    def test_record_resolution(self):
        """Test recording a board resolution."""
        archive = BoardResolutionArchive(
            archive_id="archive-001",
            created_at=datetime.now(),
            resolutions=[]
        )
        
        resolution_id = archive.record_resolution(
            directive_id="dir-001",
            decision="accepted",
            language="python",
            rationale="Best fit for requirements",
            votes={"member-1": "yes", "member-2": "yes"}
        )
        
        assert resolution_id == "res-1"
        assert len(archive.resolutions) == 1
        assert archive.resolutions[0].decision == "accepted"
        assert archive.resolutions[0].language_selected == "python"
    
    def test_get_resolutions_for_directive(self):
        """Test getting resolutions for a directive."""
        archive = BoardResolutionArchive(
            archive_id="archive-001",
            created_at=datetime.now(),
            resolutions=[]
        )
        
        archive.record_resolution("dir-001", "accepted", "python", "Good", {})
        archive.record_resolution("dir-002", "rejected", None, "Bad", {})
        archive.record_resolution("dir-001", "superseded", "rust", "Better", {})
        
        resolutions = archive.get_resolutions_for_directive("dir-001")
        
        assert len(resolutions) == 2
        assert all(r.directive_id == "dir-001" for r in resolutions)


class TestDirectivePrecedentCorpus:
    """Test DirectivePrecedentCorpus class."""
    
    def test_add_precedent(self):
        """Test adding a precedent."""
        corpus = DirectivePrecedentCorpus(
            corpus_id="corpus-001",
            precedents=[],
            index={}
        )
        
        precedent_id = corpus.add_precedent(
            directive_id="dir-001",
            resolution_id="res-001",
            scenario="API endpoint creation",
            outcome="success",
            rationale="Clear requirements",
            tags=["api", "python", "success"]
        )
        
        assert precedent_id == "prec-1"
        assert len(corpus.precedents) == 1
        assert "api" in corpus.index
        assert "python" in corpus.index
        assert precedent_id in corpus.index["api"]
    
    def test_search_precedents(self):
        """Test searching precedents by tags."""
        corpus = DirectivePrecedentCorpus(
            corpus_id="corpus-001",
            precedents=[],
            index={}
        )
        
        corpus.add_precedent("dir-001", "res-001", "Scenario 1", "success", "Good", ["api", "python"])
        corpus.add_precedent("dir-002", "res-002", "Scenario 2", "failure", "Bad", ["api", "rust"])
        corpus.add_precedent("dir-003", "res-003", "Scenario 3", "success", "Good", ["database"])
        
        results = corpus.search_precedents(["api"])
        assert len(results) == 2
        
        results = corpus.search_precedents(["python"])
        assert len(results) == 1
        
        results = corpus.search_precedents(["api", "python"])
        assert len(results) == 2


class TestMetaOfficeRulingsLedger:
    """Test MetaOfficeRulingsLedger class."""
    
    def test_record_ruling(self):
        """Test recording a Meta-Office ruling."""
        ledger = MetaOfficeRulingsLedger(
            ledger_id="ledger-001",
            rulings=[]
        )
        
        ruling_id = ledger.record_ruling(
            escalation_id="esc-001",
            ruling_type="constitutional",
            affected_entities=["entity-001", "entity-002"],
            decision="Uphold complaint",
            rationale="Constitutional violation confirmed",
            sanctions=["warning"]
        )
        
        assert ruling_id == "ruling-1"
        assert len(ledger.rulings) == 1
        assert ledger.rulings[0].ruling_type == "constitutional"
        assert len(ledger.rulings[0].affected_entities) == 2


class TestLawFailureResponseMatrix:
    """Test LawFailureResponseMatrix class."""
    
    def test_get_response(self):
        """Test getting response for law-failure combination."""
        response = FailureResponse(
            failure_type="timeout",
            law_violated="execution-time-limit",
            response_action="halt-and-report",
            escalation_path=["supervisor", "meta-office"],
            resource_cost={"time": 0},
            is_automatic=True
        )
        
        matrix = LawFailureResponseMatrix(
            matrix_id="matrix-001",
            version="1.0.0",
            responses={("law-1", "failure-1"): response}
        )
        
        result = matrix.get_response("law-1", "failure-1")
        assert result is not None
        assert result.failure_type == "timeout"
        
        result = matrix.get_response("law-1", "failure-2")
        assert result is None
    
    def test_verify_completeness(self):
        """Test verifying matrix completeness."""
        response1 = FailureResponse("timeout", "law-1", "halt", [], {}, True)
        response2 = FailureResponse("error", "law-1", "retry", [], {}, True)
        
        matrix = LawFailureResponseMatrix(
            matrix_id="matrix-001",
            version="1.0.0",
            responses={
                ("law-1", "failure-1"): response1,
                ("law-2", "failure-1"): response2
            }
        )
        
        is_complete, missing = matrix.verify_completeness(
            ["law-1", "law-2"],
            ["failure-1", "failure-2"]
        )
        
        assert is_complete is False
        assert len(missing) == 2
        assert "law-1 × failure-2" in missing
        assert "law-2 × failure-2" in missing


class TestFormalLawVerificationModels:
    """Test FormalLawVerificationModels class."""
    
    def test_add_invariant(self):
        """Test adding an invariant."""
        models = FormalLawVerificationModels(
            model_id="model-001",
            version="1.0.0",
            invariants=[]
        )
        
        inv_id = models.add_invariant(
            name="Safety Property",
            statement="G(safe -> X safe)",
            proof_type="model_checking",
            proof_artifact="proof.txt",
            verified=True
        )
        
        assert inv_id == "inv-1"
        assert len(models.invariants) == 1
        assert models.invariants[0].verified is True
    
    def test_verify_all_invariants(self):
        """Test verifying all invariants."""
        models = FormalLawVerificationModels(
            model_id="model-001",
            version="1.0.0",
            invariants=[]
        )
        
        models.add_invariant("Inv1", "stmt1", "check", "proof1", True)
        models.add_invariant("Inv2", "stmt2", "check", "proof2", False)
        models.add_invariant("Inv3", "stmt3", "check", "proof3", True)
        
        all_verified, unverified = models.verify_all_invariants()
        
        assert all_verified is False
        assert len(unverified) == 1
        assert "Inv2" in unverified


class TestInvariantViolationPlaybooks:
    """Test InvariantViolationPlaybooks class."""
    
    def test_get_playbook(self):
        """Test getting a playbook."""
        playbook = ViolationPlaybook(
            playbook_id="pb-001",
            invariant_violated="safety-property",
            detection_method="runtime-check",
            automatic_halt_behavior="immediate-halt",
            human_intervention_protocol="notify-security",
            recovery_constraints=["verify-state"],
            escalation_path=["security", "meta-office"]
        )
        
        playbooks = InvariantViolationPlaybooks(
            playbooks_id="playbooks-001",
            playbooks={"safety-property": playbook}
        )
        
        result = playbooks.get_playbook("safety-property")
        assert result is not None
        assert result.detection_method == "runtime-check"
        
        result = playbooks.get_playbook("unknown")
        assert result is None
    
    def test_execute_playbook(self):
        """Test executing a playbook."""
        playbook = ViolationPlaybook(
            playbook_id="pb-001",
            invariant_violated="safety-property",
            detection_method="runtime-check",
            automatic_halt_behavior="immediate-halt",
            human_intervention_protocol="notify-security",
            recovery_constraints=["verify-state"],
            escalation_path=["security", "meta-office"]
        )
        
        playbooks = InvariantViolationPlaybooks(
            playbooks_id="playbooks-001",
            playbooks={"safety-property": playbook}
        )
        
        result = playbooks.execute_playbook("safety-property")
        assert "invariant" in result
        assert result["invariant"] == "safety-property"
        assert result["halt_behavior"] == "immediate-halt"
        assert result["intervention"] == "notify-security"
        
        result = playbooks.execute_playbook("unknown")
        assert "error" in result


class TestCanonicalExecutionKernel:
    """Test CanonicalExecutionKernel class."""
    
    def test_verify_conformance(self):
        """Test verifying implementation conformance."""
        kernel = CanonicalExecutionKernel(
            kernel_id="kernel-001",
            version="1.0.0",
            kernel_code="def run(): pass",
            test_suite="test.py",
            conformance_criteria=["deterministic", "bounded"]
        )
        
        conforms, failures = kernel.verify_conformance("implementation-code")
        
        # Placeholder returns True with empty failures
        assert conforms is True
        assert len(failures) == 0


class TestSimulationTraceCorpus:
    """Test SimulationTraceCorpus class."""
    
    def test_add_trace(self):
        """Test adding a simulation trace."""
        corpus = SimulationTraceCorpus(
            corpus_id="corpus-001",
            traces=[]
        )
        
        trace_id = corpus.add_trace(
            scenario="basic-execution",
            start_state={"tick": 0, "agents": []},
            ticks=[{"tick": 1, "event": "start"}],
            end_state={"tick": 1, "agents": []}
        )
        
        assert trace_id == "trace-1"
        assert len(corpus.traces) == 1
        assert corpus.traces[0].scenario == "basic-execution"
        assert corpus.traces[0].trace_hash is not None
    
    def test_replay_trace(self):
        """Test replaying a trace."""
        corpus = SimulationTraceCorpus(
            corpus_id="corpus-001",
            traces=[]
        )
        
        trace_id = corpus.add_trace(
            "scenario",
            {"tick": 0},
            [{"tick": 1}],
            {"tick": 1}
        )
        
        # Placeholder returns True
        result = corpus.replay_trace(trace_id)
        assert result is True


class TestReproducibilityPackets:
    """Test ReproducibilityPackets class."""
    
    def test_create_packet(self):
        """Test creating a reproducibility packet."""
        packets = ReproducibilityPackets(
            packets_id="packets-001",
            packets=[]
        )
        
        packet_id = packets.create_packet(
            directive_id="dir-001",
            resolution_id="res-001",
            environment={"os": "linux", "python": "3.10"},
            contracts={"contract-1": "v1.0"},
            tools={"pytest": "7.0"},
            input_data={"param": "value"},
            output_data={"result": "success"}
        )
        
        assert packet_id == "packet-1"
        assert len(packets.packets) == 1
        assert packets.packets[0].directive_id == "dir-001"
        assert packets.packets[0].packet_hash is not None


class TestFloorRuntimeProfiles:
    """Test FloorRuntimeProfiles class."""
    
    def test_get_profile(self):
        """Test getting a runtime profile."""
        profile = FloorRuntimeProfile(
            floor_id="floor-python",
            language="python",
            resource_budgets={"cpu": 100, "memory": 1024},
            tool_permissions=["pytest", "black"],
            execution_ceilings={"time": 300},
            security_constraints=["no-network"]
        )
        
        profiles = FloorRuntimeProfiles(
            profiles_id="profiles-001",
            profiles={"python": profile}
        )
        
        result = profiles.get_profile("python")
        assert result is not None
        assert result.language == "python"
        
        result = profiles.get_profile("rust")
        assert result is None
    
    def test_verify_uniformity(self):
        """Test verifying profile uniformity."""
        profiles = FloorRuntimeProfiles(
            profiles_id="profiles-001",
            profiles={}
        )
        
        # Placeholder returns True with empty violations
        is_uniform, violations = profiles.verify_uniformity()
        assert is_uniform is True
        assert len(violations) == 0


class TestCrossFloorContractRegistry:
    """Test CrossFloorContractRegistry class."""
    
    def test_register_contract(self):
        """Test registering a cross-floor contract."""
        registry = CrossFloorContractRegistry(
            registry_id="registry-001",
            contracts=[]
        )
        
        contract_id = registry.register_contract(
            from_floor="python",
            to_floor="rust",
            version="1.0",
            directional=True,
            resolution_id="res-001",
            data_formats=["json"],
            failure_modes=["timeout", "invalid-data"]
        )
        
        assert contract_id == "contract-1"
        assert len(registry.contracts) == 1
        assert registry.contracts[0].from_floor == "python"
        assert registry.contracts[0].is_active is True
    
    def test_get_active_contracts(self):
        """Test getting active contracts for a floor."""
        registry = CrossFloorContractRegistry(
            registry_id="registry-001",
            contracts=[]
        )
        
        registry.register_contract("python", "rust", "1.0", True, "res-001", [], [])
        registry.register_contract("rust", "go", "1.0", True, "res-002", [], [])
        c_id = registry.register_contract("python", "go", "1.0", True, "res-003", [], [])
        
        # Deactivate one contract
        for contract in registry.contracts:
            if contract.contract_id == c_id:
                contract.is_active = False
        
        contracts = registry.get_active_contracts("python")
        assert len(contracts) == 1  # Only active contracts
        
        contracts = registry.get_active_contracts("rust")
        assert len(contracts) == 2


class TestContractDriftReports:
    """Test ContractDriftReports class."""
    
    def test_detect_drift(self):
        """Test detecting contract drift."""
        reports = ContractDriftReports(
            reports_id="reports-001",
            reports=[]
        )
        
        report = reports.detect_drift("contract-001")
        
        assert report.contract_id == "contract-001"
        assert report.report_id == "drift-1"
        assert len(reports.reports) == 1


class TestToolProvenanceTrustLedger:
    """Test ToolProvenanceTrustLedger class."""
    
    def test_register_tool(self):
        """Test registering a tool."""
        ledger = ToolProvenanceTrustLedger(
            ledger_id="ledger-001",
            tools={}
        )
        
        tool_id = ledger.register_tool(
            tool_name="pytest",
            version="7.0.0",
            checksum="abc123",
            trust_score=0.95
        )
        
        assert tool_id == "tool-1"
        assert len(ledger.tools) == 1
        assert ledger.tools[tool_id].tool_name == "pytest"
    
    def test_verify_tool_integrity(self):
        """Test verifying tool integrity."""
        ledger = ToolProvenanceTrustLedger(
            ledger_id="ledger-001",
            tools={}
        )
        
        tool_id = ledger.register_tool("pytest", "7.0.0", "abc123", 0.95)
        
        assert ledger.verify_tool_integrity(tool_id, "abc123") is True
        assert ledger.verify_tool_integrity(tool_id, "wrong") is False
        assert ledger.verify_tool_integrity("tool-999", "abc123") is False


class TestUnsafeCapabilityExceptionRecords:
    """Test UnsafeCapabilityExceptionRecords class."""
    
    def test_grant_exception(self):
        """Test granting an unsafe capability exception."""
        records = UnsafeCapabilityExceptionRecords(
            records_id="records-001",
            exceptions=[]
        )
        
        exc_id = records.grant_exception(
            capability="network-access",
            authorized_by="security-001",
            authorized_for="agent-001",
            justification="Required for API testing",
            duration=100
        )
        
        assert exc_id == "unsafe-1"
        assert len(records.exceptions) == 1
        assert records.exceptions[0].capability == "network-access"
    
    def test_get_active_exceptions(self):
        """Test getting active exceptions."""
        from datetime import timedelta
        
        records = UnsafeCapabilityExceptionRecords(
            records_id="records-001",
            exceptions=[]
        )
        
        records.grant_exception("network", "sec-001", "agent-001", "Reason", 100)
        records.grant_exception("file-write", "sec-001", "agent-002", "Reason", 100)
        
        # Set future expiry dates so exceptions are active
        future = datetime.now() + timedelta(hours=1)
        records.exceptions[0].expires_at = future
        records.exceptions[1].expires_at = future
        
        # Mark one as revoked
        records.exceptions[0].revoked = True
        
        # Should not include revoked exception
        active = records.get_active_exceptions("agent-001")
        assert len(active) == 0  # agent-001's exception is revoked
        
        # agent-002's exception should be active
        active = records.get_active_exceptions("agent-002")
        assert len(active) == 1
        assert active[0].capability == "file-write"


class TestConsigliereInteractionLogs:
    """Test ConsigliereInteractionLogs class."""
    
    def test_log_interaction(self):
        """Test logging a Consigliere interaction."""
        logs = ConsigliereInteractionLogs(
            logs_id="logs-001",
            interactions=[]
        )
        
        interaction_id = logs.log_interaction(
            interaction_type="explanation",
            content="This is how it works...",
            human_id="human-001",
            response="Understood"
        )
        
        assert interaction_id == "cons-1"
        assert len(logs.interactions) == 1
        assert logs.interactions[0].interaction_type == "explanation"
        assert logs.interactions[0].response == "Understood"


class TestSecurityDecisionDossiers:
    """Test SecurityDecisionDossiers class."""
    
    def test_record_decision(self):
        """Test recording a security decision."""
        dossiers = SecurityDecisionDossiers(
            dossiers_id="dossiers-001",
            decisions=[]
        )
        
        decision_id = dossiers.record_decision(
            threat_model="unauthorized-access",
            risk_analysis="High risk of data breach",
            decision="deny",
            conditions=["require-2fa"],
            decided_by="security-001"
        )
        
        assert decision_id == "sec-1"
        assert len(dossiers.decisions) == 1
        assert dossiers.decisions[0].decision == "deny"


class TestOverrideCostLedger:
    """Test OverrideCostLedger class."""
    
    def test_record_override(self):
        """Test recording an override."""
        ledger = OverrideCostLedger(
            ledger_id="ledger-001",
            overrides=[]
        )
        
        override_id = ledger.record_override(
            what_bypassed="safety-check",
            cost={"time": 10, "trust": 5},
            risk="Medium",
            justification="Emergency fix required",
            human_id="human-001"
        )
        
        assert override_id == "override-1"
        assert len(ledger.overrides) == 1
        assert ledger.overrides[0].what_bypassed == "safety-check"


class TestConstitutionalAmendmentRegistry:
    """Test ConstitutionalAmendmentRegistry class."""
    
    def test_propose_amendment(self):
        """Test proposing an amendment."""
        registry = ConstitutionalAmendmentRegistry(
            registry_id="registry-001",
            amendments=[]
        )
        
        amendment_id = registry.propose_amendment(
            proposal_id="prop-001",
            proposed_change="Add new role: auditor",
            simulations=[{"outcome": "stable"}],
            votes={"member-1": "yes", "member-2": "yes"},
            outcome="approved"
        )
        
        assert amendment_id == "amend-1"
        assert len(registry.amendments) == 1
        assert registry.amendments[0].outcome == "approved"


class TestDormantRejectedProposalArchive:
    """Test DormantRejectedProposalArchive class."""
    
    def test_archive_proposal(self):
        """Test archiving a rejected proposal."""
        archive = DormantRejectedProposalArchive(
            archive_id="archive-001",
            proposals=[]
        )
        
        proposal_id = archive.archive_proposal(
            proposal_id="prop-001",
            proposed_change="Remove safety checks",
            rejection_reason="Violates core principles",
            lessons="Never compromise safety"
        )
        
        assert proposal_id == "prop-001"
        assert len(archive.proposals) == 1
        assert archive.proposals[0].rejection_reason == "Violates core principles"


class TestIndependentAuditInterface:
    """Test IndependentAuditInterface class."""
    
    def test_grant_audit_access(self):
        """Test granting audit access."""
        interface = IndependentAuditInterface(
            interface_id="interface-001",
            accessible_artifacts=["charter", "ledger"],
            access_log=[]
        )
        
        token = interface.grant_audit_access(
            auditor_id="auditor-001",
            artifacts=["charter", "ledger"]
        )
        
        assert token == "audit-1"
        assert len(interface.access_log) == 1
        assert interface.access_log[0]["auditor_id"] == "auditor-001"
    
    def test_get_artifact(self):
        """Test getting an artifact."""
        interface = IndependentAuditInterface(
            interface_id="interface-001",
            accessible_artifacts=["charter"],
            access_log=[]
        )
        
        # Placeholder returns None
        result = interface.get_artifact("charter", "token-001")
        assert result is None


class TestComplianceCertificationReports:
    """Test ComplianceCertificationReports class."""
    
    def test_generate_report(self):
        """Test generating a compliance report."""
        reports = ComplianceCertificationReports(
            reports_id="reports-001",
            reports=[]
        )
        
        report_id = reports.generate_report(
            standard="ISO-27001",
            evidence=["doc-1", "doc-2"],
            status="compliant",
            gaps=[]
        )
        
        assert report_id == "comp-1"
        assert len(reports.reports) == 1
        assert reports.reports[0].standard == "ISO-27001"


class TestCivilizationFreezeProtocol:
    """Test CivilizationFreezeProtocol class."""
    
    def test_freeze(self):
        """Test freezing the civilization."""
        protocol = CivilizationFreezeProtocol(
            protocol_id="freeze-001",
            is_frozen=False,
            frozen_at=None,
            frozen_by=None,
            freeze_reason="",
            sealed_state_hash=None,
            access_locked=False
        )
        
        state_hash = protocol.freeze(
            frozen_by="security-001",
            reason="Critical bug detected",
            state_snapshot={"agents": [], "tick": 100}
        )
        
        assert protocol.is_frozen is True
        assert protocol.frozen_by == "security-001"
        assert protocol.freeze_reason == "Critical bug detected"
        assert protocol.sealed_state_hash is not None
        assert protocol.access_locked is True
        assert state_hash == protocol.sealed_state_hash
    
    def test_unfreeze(self):
        """Test unfreezing the civilization."""
        protocol = CivilizationFreezeProtocol(
            protocol_id="freeze-001",
            is_frozen=True,
            frozen_at=datetime.now(),
            frozen_by="security-001",
            freeze_reason="Bug",
            sealed_state_hash="hash123",
            access_locked=True
        )
        
        result = protocol.unfreeze("admin-001", "Bug fixed")
        
        assert result is True
        assert protocol.is_frozen is False
        assert protocol.access_locked is False
    
    def test_unfreeze_when_not_frozen(self):
        """Test unfreezing when not frozen."""
        protocol = CivilizationFreezeProtocol(
            protocol_id="freeze-001",
            is_frozen=False,
            frozen_at=None,
            frozen_by=None,
            freeze_reason="",
            sealed_state_hash=None,
            access_locked=False
        )
        
        result = protocol.unfreeze("admin-001", "Not needed")
        assert result is False


class TestCivilizationShutdownSuccession:
    """Test CivilizationShutdownSuccession class."""
    
    def test_shutdown(self):
        """Test shutting down the civilization."""
        protocol = CivilizationShutdownSuccession(
            protocol_id="shutdown-001",
            is_shutdown=False,
            shutdown_at=None,
            final_archive_hash=None,
            successor_steward=None,
            cryptographic_seal=None
        )
        
        seal = protocol.shutdown(
            initiated_by="admin-001",
            archive_data={"artifacts": 27, "complete": True},
            successor="steward-002"
        )
        
        assert protocol.is_shutdown is True
        assert protocol.successor_steward == "steward-002"
        assert protocol.final_archive_hash is not None
        assert protocol.cryptographic_seal is not None
        assert seal == protocol.cryptographic_seal


class TestSuccessFailureMetricsCanon:
    """Test SuccessFailureMetricsCanon class."""
    
    def test_evaluate_correctness(self):
        """Test evaluating correctness."""
        canon = SuccessFailureMetricsCanon(
            canon_id="canon-001",
            version="1.0.0",
            correctness_metrics={"tests_pass": "All tests must pass"},
            completeness_metrics={},
            trustworthiness_metrics={},
            forbidden_metrics=[]
        )
        
        is_correct, message = canon.evaluate_correctness({})
        
        # Placeholder returns True
        assert is_correct is True
        assert "correctness" in message.lower()
    
    def test_evaluate_completeness(self):
        """Test evaluating completeness."""
        canon = SuccessFailureMetricsCanon(
            canon_id="canon-001",
            version="1.0.0",
            correctness_metrics={},
            completeness_metrics={"all_requirements": "Must be met"},
            trustworthiness_metrics={},
            forbidden_metrics=[]
        )
        
        is_complete, message = canon.evaluate_completeness({})
        
        assert is_complete is True
        assert "completeness" in message.lower()
    
    def test_evaluate_trustworthiness(self):
        """Test evaluating trustworthiness."""
        canon = SuccessFailureMetricsCanon(
            canon_id="canon-001",
            version="1.0.0",
            correctness_metrics={},
            completeness_metrics={},
            trustworthiness_metrics={"reproducible": "Must be reproducible"},
            forbidden_metrics=["lines_of_code"]
        )
        
        is_trustworthy, message = canon.evaluate_trustworthiness({})
        
        assert is_trustworthy is True
        assert "trustworthiness" in message.lower()


class TestNonDesignCanonicalBundle:
    """Test NonDesignCanonicalBundle class."""
    
    def test_verify_bundle_completeness(self):
        """Test verifying bundle completeness."""
        bundle = create_canonical_bundle()
        
        is_complete, missing = bundle.verify_bundle_completeness()
        
        assert is_complete is True
        assert len(missing) == 0
    
    def test_verify_bundle_with_missing_artifacts(self):
        """Test verifying bundle with missing artifacts."""
        bundle = create_canonical_bundle()
        # Make an artifact None
        bundle.charter = None
        
        is_complete, missing = bundle.verify_bundle_completeness()
        
        assert is_complete is False
        assert "charter" in missing
    
    def test_generate_bundle_report(self):
        """Test generating bundle report."""
        bundle = create_canonical_bundle()
        
        report = bundle.generate_bundle_report()
        
        assert "NON-DESIGN CANONICAL BUNDLE" in report
        assert "Version: 1.0.0" in report
        assert "I. FOUNDATIONAL LEGITIMACY PACK" in report
        assert "1. Civilization Charter" in report
        assert "27. Success & Failure Metrics Canon" in report
        assert "Complete: Yes" in report
        assert "CIVILIZATION LAYER: FINISHED" in report


class TestCanonicalBundleCreation:
    """Test canonical bundle creation and retrieval."""
    
    def test_create_canonical_bundle(self):
        """Test creating a canonical bundle."""
        bundle = create_canonical_bundle()
        
        assert bundle is not None
        assert bundle.bundle_id == "bundle-001"
        assert bundle.version == "1.0.0"
        assert bundle.charter is not None
        assert bundle.authority_ledger is not None
        assert bundle.purpose_lock is not None
        assert bundle.metrics_canon is not None
    
    def test_get_canonical_bundle(self):
        """Test getting the global canonical bundle."""
        # Reset global instance
        import src.core.canonical_bundle
        src.core.canonical_bundle._canonical_bundle = None
        
        bundle1 = get_canonical_bundle()
        bundle2 = get_canonical_bundle()
        
        # Should return same instance
        assert bundle1 is bundle2
        assert bundle1.bundle_id == "bundle-001"
    
    def test_bundle_all_27_artifacts(self):
        """Test that bundle contains all 27 artifacts."""
        bundle = create_canonical_bundle()
        
        # Verify all artifact types are present
        assert isinstance(bundle.charter, CivilizationCharter)
        assert isinstance(bundle.authority_ledger, AuthorityRoleLedger)
        assert isinstance(bundle.purpose_lock, PurposeLockAttestation)
        assert isinstance(bundle.board_resolutions, BoardResolutionArchive)
        assert isinstance(bundle.precedent_corpus, DirectivePrecedentCorpus)
        assert isinstance(bundle.meta_office_rulings, MetaOfficeRulingsLedger)
        assert isinstance(bundle.law_failure_matrix, LawFailureResponseMatrix)
        assert isinstance(bundle.formal_verification, FormalLawVerificationModels)
        assert isinstance(bundle.violation_playbooks, InvariantViolationPlaybooks)
        assert isinstance(bundle.execution_kernel, CanonicalExecutionKernel)
        assert isinstance(bundle.simulation_traces, SimulationTraceCorpus)
        assert isinstance(bundle.reproducibility_packets, ReproducibilityPackets)
        assert isinstance(bundle.floor_profiles, FloorRuntimeProfiles)
        assert isinstance(bundle.contract_registry, CrossFloorContractRegistry)
        assert isinstance(bundle.contract_drift, ContractDriftReports)
        assert isinstance(bundle.tool_provenance, ToolProvenanceTrustLedger)
        assert isinstance(bundle.unsafe_exceptions, UnsafeCapabilityExceptionRecords)
        assert isinstance(bundle.consigliere_logs, ConsigliereInteractionLogs)
        assert isinstance(bundle.security_dossiers, SecurityDecisionDossiers)
        assert isinstance(bundle.override_ledger, OverrideCostLedger)
        assert isinstance(bundle.amendment_registry, ConstitutionalAmendmentRegistry)
        assert isinstance(bundle.rejected_proposals, DormantRejectedProposalArchive)
        assert isinstance(bundle.audit_interface, IndependentAuditInterface)
        assert isinstance(bundle.compliance_reports, ComplianceCertificationReports)
        assert isinstance(bundle.freeze_protocol, CivilizationFreezeProtocol)
        assert isinstance(bundle.shutdown_protocol, CivilizationShutdownSuccession)
        assert isinstance(bundle.metrics_canon, SuccessFailureMetricsCanon)
