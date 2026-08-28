import os

from src.core.audit import GENESIS_HASH, AuditEvent, AuditLog, EventType, sign_hash


def test_chain_links_previous_hash():
    log = AuditLog()
    first = log.log_event(EventType.AGENT_ACTION, "a", "t1", {})
    second = log.log_event(EventType.AGENT_ACTION, "a", "t2", {})
    assert first.prev_hash == GENESIS_HASH
    assert second.prev_hash == first._hash
    assert log.verify_integrity()


def test_persist_and_reload(tmp_path):
    path = tmp_path / "audit.jsonl"
    log = AuditLog(persist_path=path)
    log.log_event(EventType.WORKSPACE_WRITE, "operator", "f.txt", {"size": 1})
    log.log_event(EventType.TERMINAL_RUN, "operator", ".", {"argv": ["python3"]})
    reloaded = AuditLog(persist_path=path)
    assert len(reloaded.graph.events) == 2
    assert reloaded.verify_integrity()


def test_hmac_present_when_key_set(monkeypatch):
    monkeypatch.setenv("MO_AUDIT_HMAC_KEY", "unit-test-hmac-key-not-a-placeholder")
    monkeypatch.delenv("SECRET_KEY", raising=False)
    log = AuditLog()
    event = log.log_event(EventType.AGENT_ACTION, "a", "t", {})
    assert event.hmac_sha256
    assert event.hmac_sha256 == sign_hash(event._hash)
    assert event.verify_integrity()
    assert log.signed() is True


def test_hmac_rejects_tampered_tag(monkeypatch):
    monkeypatch.setenv("MO_AUDIT_HMAC_KEY", "unit-test-hmac-key-not-a-placeholder")
    event = AuditEvent(event_type=EventType.AGENT_ACTION, actor_id="a", target_id="t")
    event.hmac_sha256 = "0" * 64
    assert event.verify_integrity() is False
