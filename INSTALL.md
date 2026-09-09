# Installation Guide

**Status: experimental Flask prototype — not production-ready.** Measured status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

Access: local Flask app in a browser (desktop and mobile). There is a small PWA shell. There is no native VR client. `HOST` / `PORT` environment variables are **not** read.

## Easiest Option: Use Your Web Browser (All Platforms)

The Miniature Office is a Flask HTML UI. A modern desktop, phone, tablet, or headset browser can load it. That is not a native app and not WebXR.

1. Someone starts the server (see options below)
2. Open your browser to: `http://localhost:5000` (or the server's IP address)
3. Works in a modern browser on the same machine or LAN. A headset browser can load the page; that is not a VR product (no WebXR).

This means:
- **Desktop**: Chrome, Firefox, Safari, Edge on Windows/Mac/Linux
- **Mobile / tablet**: any phone or tablet browser pointed at the Flask server
- **PWA**: `manifest.json` + `sw.js` ship with the Flask client. Supporting browsers can install the shell from a **secure context** (`https`, or `http://localhost` / `http://127.0.0.1`). A plain `http://LAN_IP:5000` origin is not a secure context; bookmarking still works, the service worker will not register there. That is still the Flask HTML UI, not a native app.
- **Not included**: a store client, Electron package, or WebXR session

---

## Desktop Installation (Windows, macOS, Linux)

### Option 1: Installer scripts

#### Windows
1. Download the repository or clone it
2. Right-click `install.ps1` and select "Run with PowerShell"
3. Follow the prompts
4. Double-click `start.bat` to launch the application
5. Open your browser to `http://localhost:5000`

#### macOS
1. Download the repository or clone it
2. Open Terminal in the project folder
3. Run: `./install.sh`
4. Double-click `start.command` to launch the application
5. Open your browser to `http://localhost:5000`

#### Linux
1. Download the repository or clone it
2. Open Terminal in the project folder
3. Run: `./install.sh`
4. Run: `./start.sh` to launch the application
5. Open your browser to `http://localhost:5000`

### Option 2: Docker

Compose interpolates `SECRET_KEY` with **no default**. Production refuses placeholders. Generate the key once and reuse it; a new key cannot verify an HMAC-tagged `audit.jsonl` already in `./data`. `chmod 777` is the CD bind-mount workaround, not a hardened default.

**bash / WSL / Git Bash** (not Windows cmd.exe):

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
chmod 777 user_workspace data logs
docker compose up --build
```

**Windows PowerShell:**

```powershell
$env:SECRET_KEY = python -c "import secrets; print(secrets.token_hex(32))"
New-Item -ItemType Directory -Force -Path user_workspace, data, logs | Out-Null
docker compose up --build
```

Then open: `http://localhost:5000`

**Prerequisites**: Install [Docker Desktop](https://www.docker.com/products/docker-desktop).

### Option 3: Manual Installation

**Prerequisites**: Python 3.10 or higher (`pytest==9.0.3` in `requirements.txt` does not install on 3.9)

```bash
# Install dependencies
pip install -r requirements.txt

# Run the application
python3 run.py
```

Then open: `http://localhost:5000`

---

## Mobile Access (Android & iOS)

The Miniature Office is a **web-based application** that works on mobile devices:

### Method 1: Connect to Local Server
1. Start the server on your computer (see Desktop Installation above)
2. Find your computer's IP address:
   - Windows: `ipconfig` (look for IPv4 Address)
   - macOS/Linux: `ifconfig` or `ip addr` (look for inet address)
3. On your mobile device, open the browser
4. Navigate to: `http://YOUR_COMPUTER_IP:5000`

**Example**: If your computer's IP is `192.168.1.100`, go to `http://192.168.1.100:5000`

### Method 2: Shortcut / PWA shell (optional)

Bookmarking or “Add to Home Screen” on `http://LAN_IP:5000` pins a shortcut. That origin is **not** a secure context, so the service worker in `src/client/index.html` will not register. The PWA shell (`manifest.json` / `sw.js`) installs from `https` or `http://localhost`. That is the same HTML UI, not a native app and not WebXR.

### Method 3: Run on Your Phone (Advanced)

**Android (Termux)**:
This path is untested and is not a supported platform. The same Python **3.10+** requirement applies (`pytest==9.0.3` does not install on 3.9). Confirm `python --version` before `pip install`.

```bash
# Install Termux from F-Droid or Play Store
pkg install python git
python --version   # must be 3.10 or newer
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
pip install -r requirements.txt
python run.py
# Open browser to http://localhost:5000
```

**iOS**: Not directly supported, but you can access a server running elsewhere

---

## Headset browsers (not a VR product)

A Quest (or other) browser can load `http://YOUR_COMPUTER_IP:5000` the same way a phone can. That is the Flask HTML UI. There is no WebXR session, no controller support, and no immersive office.

---

## Network Access

### Make it Accessible on Your Network

`run.py` already binds `0.0.0.0:5000`. `HOST` / `PORT` environment variables are **not** read (see `.env.example`). To change the port, edit `run.py`:

```python
run_server(host='0.0.0.0', port=8080)
```

Anyone on the same network can then open `http://YOUR_IP:PORT`.

### Security Note
When opening to your network:
1. Set a strong `SECRET_KEY` (compose has **no** default). Reuse it if `./data` already has an HMAC-tagged audit log.
2. `/api/ide/*` is open unless `MO_IDE_TOKEN` is set (required when `FLASK_ENV=production`)
3. Restarting the process drops in-memory world state and audit events
4. Use firewall rules to limit access. This is a local prototype, not a hardened service.

---

## Quick Start Commands

### Windows
```cmd
install.ps1          # First time setup
start.bat            # Start the application
```

### macOS
```bash
./install.sh         # First time setup
./start.command      # Start (double-clickable)
# or
./start.sh          # Start (command line)
```

### Linux
```bash
./install.sh         # First time setup
./start.sh          # Start the application
```

### Docker (bash / WSL / Git Bash)

```bash
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
chmod 777 user_workspace data logs
docker compose up --build    # Start everything
docker compose down          # Stop everything
```

PowerShell: `$env:SECRET_KEY = python -c "import secrets; print(secrets.token_hex(32))"` then `docker compose up --build`. Reuse the key across restarts.

---

## Pre-built Packages (Future)

We're working on pre-built installers:
- [ ] Windows: `.exe` installer with automatic Python bundling
- [ ] macOS: `.dmg` or `.app` bundle
- [ ] Linux: `.deb` and `.rpm` packages
- [ ] Snap package for Linux
- [ ] Electron-based desktop app (Windows, macOS, Linux)

Those packages do not exist yet.

---

## Troubleshooting

### Port Already in Use
If port 5000 is taken, edit `run.py` and change the port:
```python
run_server(host='0.0.0.0', port=8080)
```

### Python Not Found
- Windows: Install from [python.org](https://www.python.org/downloads/) (3.10+; `pytest==9.0.3` does not install on 3.9)
- macOS: `brew install python@3.12` (need 3.10 or newer; `brew install python3` may still be 3.9 on older machines)
- Linux: `sudo apt install python3 python3-pip` — confirm `python3 --version` is 3.10+

### Permission Denied on Scripts
```bash
chmod +x install.sh start.sh start.command

```

### Mobile Can't Connect
- Ensure your computer and mobile device are on the same WiFi network
- Check your firewall isn't blocking port 5000
- Try disabling VPN temporarily

### Headset browser cannot load the page
- Same as a phone: same Wi-Fi, use the PC’s LAN IP, not `localhost`
- You still get the Flask HTML UI. There is no WebXR client to update.

---

## Tips

1. **Bookmark it**: Save `http://localhost:5000` on the machine that runs the server
2. **LAN only**: Other devices on the same network can open `http://LAN_IP:5000`. There is no account system.
3. **Keep the process running**: Restarting drops in-memory world state
4. **Docker**: `docker compose up --build` if you have Docker; you must export `SECRET_KEY`

---

## Additional Resources

- **[README.md](README.md)** - Project overview and measured metrics
- **[CLAIMS_AUDIT.md](CLAIMS_AUDIT.md)** - What is actually implemented
- **[QUICKSTART.md](QUICKSTART.md)** - Quick usage guide
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Design notes (intent, not a completion certificate)

---

## Need Help?

- Check [GitHub Issues](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues)
- Read the troubleshooting section above
- Review the logs in the console where you started the server
