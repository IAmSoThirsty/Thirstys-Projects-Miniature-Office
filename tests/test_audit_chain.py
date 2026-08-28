from src.core.audit import GENESIS_HASH, AuditLog, EventType


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
