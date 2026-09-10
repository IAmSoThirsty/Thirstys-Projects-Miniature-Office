> **Historical document — not current status.**
> This file was written as a completion certificate. Canonical measured status is [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) (audited 28 Aug 2026, updated when the tree changed). Do not cite this file as evidence that a feature shipped.

# Deployment

This is **not** a production deploy guide. The tree is an experimental Flask prototype with in-memory world state. Operator how-to: [INSTALL.md](INSTALL.md), [GETTING_STARTED.md](GETTING_STARTED.md). Limitations: [LIMITATIONS.md](LIMITATIONS.md).

## What actually exists

| Path | Reality |
| --- | --- |
| `python3 run.py` | Flask + Flask-SocketIO on port 5000 |
| `docker compose up --build` | gunicorn (`src.server.app:app`, **4** eventlet workers) in a container. Each worker has its own in-memory `simulation` global. STEP on one worker is not visible to REFRESH on another. There is no Socket.IO message queue. `SECRET_KEY` is interpolated with **no default**. Production refuses placeholders. |
| `GET /health` | Liveness probe. **Always HTTP 200** if this process can serve HTTP. It does **not** return 503 at startup. Tests: `test_health_without_simulation_is_liveness`. Body `"simulation": "running"` means the global object exists (lazy init), **not** that `POST /api/world/start` ran. Independent `GET /api/world/state` `"is_running"` is false until START. `"status": "healthy"` is the liveness string. |
| `GET /metrics` | Prometheus text of in-memory counts. **503** while `simulation` is `None`. After `/health` lazy-init: HTTP 200. HELP `minioffice_floors_total` says “Total number of floors”; the value is `len(world.floors)` (**2** `World.Floor` objects: Python, JavaScript), not 28 language floors. `minioffice_agents_total` is 11. `minioffice_artifacts_total` is 0. |
| `GET /api/world/state` | **HTTP 500** `Simulation not initialized` until `/health` (or another lazy-init) has run. After that: `is_running` is still **false** until START. |
| `GET /api/ide/health` | IDE-core liveness used by compose `healthcheck` |
| Kubernetes | **No `k8s/` directory.** No in-tree manifests. |
| systemd unit | **Not in the tree.** |
| GHCR | CD may push `ghcr.io/iamsothirsty/thirstys-projects-miniature-office`. That image is the same in-memory prototype, not a hardened service. |

World / registries restart to empty. Workspace files persist under `MO_WORKSPACE`. Audit JSONL persists only when `MO_DATA_DIR` is set. HMAC-tagged when a real key is set; rotating that key cannot verify an existing `audit.jsonl`.

## Docker (the only packaged path)

Compose snippets are bash / WSL / Git Bash unless a PowerShell block is shown. Generate `SECRET_KEY` **once** and reuse it.

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
# chmod 777 is the CD bind-mount workaround, not a hardened default
chmod 777 user_workspace data logs
docker compose up --build
```

Then open `http://127.0.0.1:5000`. Stop with `Ctrl+C` or `docker compose down`.

PowerShell: `$env:SECRET_KEY = python -c "import secrets; print(secrets.token_hex(32))"`.

CD `test-docker` curls `/health` **and** `/api/ide/health`. Observed green on code pin `fdd9762` and on later docs-only commits. Docker stays Partial: in-memory world, gunicorn `--workers 4` (split-brain), `chmod 777`.

## Do not

- Do not treat `docker compose up -d` as production.
- Do not expect `/health` to return 503 while the simulation starts. It is liveness 200.
- Do not `kubectl apply` a `k8s/deployment.yaml` from this repo. That file does not exist.
- Do not point Prometheus at this process as if it were a production SLO. `/metrics` is a prototype exposition of in-memory counters.
- Do not rotate `SECRET_KEY` / `MO_AUDIT_HMAC_KEY` if `./data/audit.jsonl` already exists.

Canonical status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Production ready remains false.
