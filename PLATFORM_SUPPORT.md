# Platform support

Miniature Office is a **local Flask app**. You start a Python (or Docker) process and open it in a browser. That is the entire platform story.

There is a small PWA shell (`manifest.json` + `sw.js`). There is no native desktop app, no store client, and no WebXR.

## What actually runs

| Path | What it is |
| --- | --- |
| `python3 run.py` | Flask + Flask-SocketIO on port 5000 |
| `./install.sh` / `install.ps1` | Creates a venv and `pip install -r requirements.txt` |
| `./start.sh` / `start.bat` / `start.command` | Activates the venv and runs `run.py` |
| `docker compose up --build` | gunicorn in a container, port 5000. `SECRET_KEY` is interpolated with **no default**. Production refuses placeholders |

`install.sh` is not a native OS installer. `start.command` is a shell wrapper, not a signed macOS app.

CD `test-docker` curls `/health` and `/api/ide/health` and `chmod 777`s `user_workspace` / `data` / `logs` before compose up. On [`1a103bf`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/1a103bf198ebb4b795b36d04cdc081d3a1fa4687): CI [33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008) **succeeded**, CD [33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012) **succeeded**. Docker is still not a hardened stack (in-memory world, `chmod 777`).

## Browsers

A modern desktop or mobile browser on the same machine or LAN can load `http://HOST:5000`. A headset browser can do the same. You get the Flask HTML UI (world canvas, file tree, editor, terminal), not a VR product. The PWA shell can be installed from a supporting browser; it is not a native app.

## Not included

- Electron / packaged desktop app
- App Store / Play Store clients
- WebXR, VR controllers, or hand tracking
- Production hardening (in-memory world state)

Canonical status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). Runbook: [INSTALL.md](INSTALL.md).
