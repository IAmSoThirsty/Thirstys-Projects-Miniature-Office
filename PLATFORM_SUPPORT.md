# Platform support

Miniature Office is a **local Flask app**. You start a Python (or Docker) process and open it in a browser. That is the entire platform story.

There is no native desktop app, no mobile app, no PWA (`manifest.json` / service worker are absent), and no WebXR.

## What actually runs

| Path | What it is |
| --- | --- |
| `python3 run.py` | Flask + Flask-SocketIO on port 5000 |
| `./install.sh` / `install.ps1` | Creates a venv and `pip install -r requirements.txt` |
| `./start.sh` / `start.bat` / `start.command` | Activates the venv and runs `run.py` |
| `docker compose up --build` | gunicorn in a container, port 5000. `SECRET_KEY` is interpolated with **no default**. Production refuses placeholders |

`install.sh` is not a native OS installer. `start.command` is a shell wrapper, not a signed macOS app.

The CD compose probe currently fails: it curls `/health` (503) while the compose healthcheck uses `/api/ide/health`, and the image user cannot write `./user_workspace`.

## Browsers

A modern desktop or mobile browser on the same machine or LAN can load `http://HOST:5000`. A headset browser can do the same. You get the Flask HTML UI (world canvas, file tree, editor, terminal), not a VR product.

## Not included

- Electron / packaged desktop app
- App Store / Play Store clients
- Installable PWA
- WebXR, VR controllers, or hand tracking
- Production hardening (in-memory state, no auth, CD probe red)

Canonical status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Runbook: [INSTALL.md](INSTALL.md).
