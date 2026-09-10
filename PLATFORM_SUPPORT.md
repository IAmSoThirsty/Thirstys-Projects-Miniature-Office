# Platform support

**Status: experimental Flask prototype — not production-ready.** Measured status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

Miniature Office is a **local Flask app**. You start a Python (or Docker) process and open it in a browser. That is the entire platform story.

There is a small PWA shell (`manifest.json` + `sw.js`). There is no native desktop app, no store client, and no WebXR. The service worker installs only from a secure context (`https` or `http://localhost` / `http://127.0.0.1`). A LAN `http://IP:5000` origin is not a secure context.

## What actually runs

| Path | What it is |
| --- | --- |
| `python3 run.py` | Flask + Flask-SocketIO on port 5000 |
| `./install.sh` / `install.ps1` | `pip install -r requirements.txt` into the **current** Python (no venv). Requires Python **3.10+** (`pytest==9.0.3` does not install on 3.9) |
| `./start.sh` / `start.bat` / `start.command` | Runs `python3 run.py` (Unix) or `python run.py` (Windows) in the **current** interpreter. Does not create or activate a venv |
| `docker compose up --build` | gunicorn in a container, port 5000. Dockerfile CMD is `--workers 4 --worker-class eventlet`. Each worker has its own in-memory `simulation`. `SECRET_KEY` is interpolated with **no default**. Production refuses placeholders |

`install.sh` is not a native OS installer. `start.command` is a shell wrapper, not a signed macOS app.

CD `test-docker` curls `/health` and `/api/ide/health` and `chmod 777`s `user_workspace` / `data` / `logs` before compose up. On **code pin** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a): CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) **succeeded**, CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) **succeeded**. Later docs-only commits are identical in `src/`/`tests/` and also green, including [`1a103bf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/1a103bf198ebb4b795b36d04cdc081d3a1fa4687) ([CI 33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008), [CD 33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012)) and `32a70dc` ([CI 33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458), [CD 33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461)). Later docs-only `a0910d4` is also green ([CI 33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717), [CD 33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743)), as is `32b08d8` ([CI 33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624), [CD 33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630)). Docker is still not a hardened stack (in-memory world, `chmod 777`). Compose snippets in operator how-to are bash unless a PowerShell block is shown. Generate `SECRET_KEY` once and reuse it; a rotated key cannot verify an HMAC-tagged `audit.jsonl` already in `./data`.

## Browsers

A modern desktop or mobile browser on the same machine or LAN can load `http://HOST:5000`. A headset browser can do the same. You get the Flask HTML UI (world canvas, file tree, editor, terminal), not a VR product. The PWA shell can be installed from a supporting browser on a secure context; it is not a native app. Bookmarking a LAN HTTP URL still works.

## Not included

- Electron / packaged desktop app
- App Store / Play Store clients
- WebXR, VR controllers, or hand tracking
- Production hardening (in-memory world state)

Canonical status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Runbook: [INSTALL.md](INSTALL.md).
