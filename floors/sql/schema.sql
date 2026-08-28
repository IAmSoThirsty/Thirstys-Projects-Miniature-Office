-- SQL floor sample schema. This file is SQL, not Python.
-- Toy inventory: floors/README.md

CREATE TABLE IF NOT EXISTS office_floors (
    floor_id TEXT PRIMARY KEY,
    language TEXT NOT NULL,
    toy BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TABLE IF NOT EXISTS audit_events (
    event_id TEXT PRIMARY KEY,
    event_type TEXT NOT NULL,
    actor_id TEXT,
    target_id TEXT,
    prev_hash TEXT NOT NULL,
    content_hash TEXT NOT NULL,
    hmac_sha256 TEXT,
    created_at TIMESTAMP NOT NULL
);
