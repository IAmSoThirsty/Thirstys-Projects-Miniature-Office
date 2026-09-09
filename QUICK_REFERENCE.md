# Quick Reference - Installation Commands

Experimental Flask prototype. Not production-ready. Status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).


## Windows

```powershell
# Installation (one-time)
Right-click install.ps1 → "Run with PowerShell"

# Starting the app
Double-click: start.bat
```

## macOS

```bash
# Installation (one-time)
./install.sh

# Starting the app
Double-click: start.command
# OR
./start.sh
```

## Linux

```bash
# Installation (one-time)
./install.sh

# Starting the app
./start.sh
```

## Docker (bash / WSL / Git Bash)

Not Windows cmd.exe. PowerShell snippet is below.

Generate `SECRET_KEY` **once** and reuse it. A new key cannot verify an HMAC-tagged `audit.jsonl` already in `./data`.

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
# chmod 777 is the CD bind-mount workaround, not a hardened default
chmod 777 user_workspace data logs
docker compose up --build

# Stop
docker compose down
```

Compose interpolates `SECRET_KEY` with **no default**. Production refuses placeholders.

### Docker on Windows (PowerShell)

```powershell
$env:SECRET_KEY = python -c "import secrets; print(secrets.token_hex(32))"
New-Item -ItemType Directory -Force -Path user_workspace, data, logs | Out-Null
docker compose up --build
```

Reuse the same `$env:SECRET_KEY` on later runs if `./data` already holds an HMAC-tagged audit log.

## Access URLs

| Device | URL |
|--------|-----|
| Same computer | `http://localhost:5000` |
| Other devices on network | `http://YOUR_IP:5000` |
| Example | `http://192.168.1.100:5000` |

## Mobile Quick Steps

1. Start server on computer (see above)
2. Find computer's IP address
3. Open phone browser → `http://YOUR_IP:5000`
4. Optional: bookmark it. “Add to Home Screen” can pin a shortcut. The PWA shell (`manifest.json` + `sw.js`) installs only from a **secure context** (`https`, or `http://localhost` / `http://127.0.0.1`). A plain `http://LAN_IP:5000` origin is not a secure context, so the service worker will not register there. That is still the Flask HTML UI, not a native app.


## Headsets

There is no WebXR client. A headset browser can load `http://YOUR_IP:5000` the same way a phone can. That is still the Flask HTML UI.

## Find Your IP Address

**Windows:**
```cmd
ipconfig
```
Look for: IPv4 Address

**Mac/Linux:**
```bash
ifconfig | grep inet
# OR
ip addr show
```
Look for: inet 192.168.x.x

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Python not found | Install Python 3.10+ from python.org |
| Port 5000 in use | Edit `run.py`, change port to 8080. `HOST` / `PORT` env vars are **not** read |
| Can't connect from phone | Same WiFi? Firewall off? |
| Permission denied | Run: `chmod +x install.sh start.sh start.command` |

## Need Help?

- 📖 Full guide: [INSTALL.md](INSTALL.md)
- 🎯 Step-by-step: [GETTING_STARTED.md](GETTING_STARTED.md)
- 📚 Quick start: [QUICKSTART.md](QUICKSTART.md)
- 🐛 Issues: [GitHub Issues](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues)

---

**Remember**: LAN devices can open `http://LAN_IP:5000` if the host firewall allows port 5000. That is the Flask HTML UI, not a native app.
