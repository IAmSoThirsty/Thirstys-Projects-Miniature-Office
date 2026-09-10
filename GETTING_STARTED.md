# Getting Started

**Status: experimental Flask prototype — not production-ready.** Measured status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md).

This guide is how to run the Flask HTML UI in a browser. A phone, tablet, or headset browser can load that page. There is no native app and no WebXR.

## Quick Decision Tree

```
What device do you have?
│
├─ 🖥️  Desktop Computer (Windows/Mac/Linux)
│   │
│   ├─ Have Docker installed?
│   │   └─ YES → Set SECRET_KEY once, then: docker compose up --build
│   │   └─ NO  → Run the installer script (install.sh or install.ps1)
│   │
│   └─ Then open browser to: http://localhost:5000
│
├─ 📱 Phone or Tablet (Android/iOS)
│   │
│   ├─ Someone else running the server?
│   │   └─ YES → Open browser to: http://[server-ip]:5000
│   │   └─ NO  → Need someone to start the server first
│   │
│   └─ Optional: bookmark the URL. “Add to Home Screen” pins a shortcut.
│       The PWA shell (manifest.json + sw.js) installs only from a
│       secure context (https or localhost). http://LAN_IP:5000 is not one.

│
└─ Headset browser
    │
    └─ Same as a phone: open the headset’s web browser to http://[server-ip]:5000
       There is no WebXR / native VR client. You get the Flask HTML UI.
```

---

## 🖥️ Desktop - Step by Step

### Windows Users

1. **Download the Project**
   - Click the green "Code" button on GitHub
   - Select "Download ZIP"
   - Extract the ZIP file

2. **Run the Installer**
   - Find the file: `install.ps1`
   - Right-click it
   - Select "Run with PowerShell"
   - Wait for installation to complete

3. **Start the Application**
   - Double-click: `start.bat`
   - A console window will open
   - Wait a few seconds

4. **Open Your Browser**
   - Open Chrome, Firefox, or Edge
   - Go to: `http://localhost:5000`
   - 🎉 You're in!

### Mac Users

1. **Download the Project**
   - Click the green "Code" button on GitHub
   - Select "Download ZIP"
   - Extract the ZIP file

2. **Run the Installer**
   - Open Terminal (Applications → Utilities → Terminal)
   - Drag the project folder into Terminal
   - Type: `cd ` then drag the folder again, press Enter
   - Type: `./install.sh` and press Enter
   - Wait for installation to complete

3. **Start the Application**
   - Find the file: `start.command`
   - Double-click it
   - A Terminal window will open

4. **Open Your Browser**
   - Open Safari, Chrome, or Firefox
   - Go to: `http://localhost:5000`
   - 🎉 You're in!

### Linux Users

1. **Clone or Download the Project**
   ```bash
   git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
   cd Thirstys-Projects-Miniature-Office
   ```

2. **Run the Installer**
   ```bash
   ./install.sh
   ```

3. **Start the Application**
   ```bash
   ./start.sh
   ```

4. **Open Your Browser**
   - Go to: `http://localhost:5000`
   - 🎉 You're in!

---

## 📱 Mobile - Step by Step

### Prerequisites
Someone needs to start the server on a computer first (see Desktop steps above).

### Step 1: Find the Server IP Address

**On the computer running the server:**

Windows:
```cmd
ipconfig
```
Look for "IPv4 Address" - something like `192.168.1.100`

Mac/Linux:
```bash
ifconfig | grep inet
```
Look for an address like `192.168.1.100`

### Step 2: Connect from Mobile

1. **Make sure your phone is on the same WiFi** as the computer
2. Open your phone's web browser (Chrome, Safari, etc.)
3. Type in the address bar: `http://192.168.1.100:5000`
   (Replace `192.168.1.100` with your actual computer's IP)
4. 🎉 You're in!

### Step 3: Add to Home Screen (Optional)

Bookmarking and “Add to Home Screen” can pin a shortcut on `http://LAN_IP:5000`. That origin is **not** a secure context, so `navigator.serviceWorker` will not register there. The PWA shell (`manifest.json` + `sw.js`) installs from `https` or `http://localhost` / `http://127.0.0.1`. Either way this is still the Flask HTML UI, not a native app.

**iPhone/iPad:**
1. Tap the Share button (square with arrow)
2. Scroll down and tap "Add to Home Screen"
3. Give it a name: "Miniature Office"
4. Tap "Add"

**Android:**
1. Tap the menu button (⋮) in the browser
2. Tap "Add to Home screen"
3. Give it a name: "Miniature Office"
4. Tap "Add"

---

## Headset browsers

There is no WebXR session and no VR-native UI. A Quest (or other) browser can load `http://[server-ip]:5000` the same way a phone can. That is still the Flask HTML page.

---

## 🐳 Docker

Compose interpolates `SECRET_KEY` with **no default**. Production refuses placeholders. Generate the key once and reuse it; a new key cannot verify an HMAC-tagged `audit.jsonl` already in `./data`.

### bash / WSL / Git Bash

Not Windows cmd.exe.

```bash
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
export SECRET_KEY=$(python3 -c 'import secrets; print(secrets.token_hex(32))')
mkdir -p user_workspace data logs
# chmod 777 is the CD bind-mount workaround, not a hardened default
chmod 777 user_workspace data logs
docker compose up --build
```

Then open `http://localhost:5000`. Stop with `Ctrl+C` or `docker compose down`.

Dockerfile CMD is gunicorn `--workers 4 --worker-class eventlet`. Each worker has its own in-memory simulation. Compose is **not** the same as `python3 run.py`. STEP / REFRESH can hit different workers.

**Prerequisites**: [Docker Desktop](https://www.docker.com/products/docker-desktop)


### Windows PowerShell

```powershell
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
$env:SECRET_KEY = python -c "import secrets; print(secrets.token_hex(32))"
New-Item -ItemType Directory -Force -Path user_workspace, data, logs | Out-Null
docker compose up --build
```

---

## ❓ Troubleshooting

### "Python not found"
- **Windows**: Download from python.org (3.10+, check "Add to PATH")
- **Mac**: `brew install python@3.12` (3.10+ required; `pytest==9.0.3` does not install on 3.9)
- **Linux**: `sudo apt install python3 python3-pip` — confirm `python3 --version` is 3.10+

### "Port 5000 already in use"
- Another program is using port 5000
- Edit `run.py` and change `port=5000` to `port=8080`
- `HOST` / `PORT` environment variables are **not** read
- Then use `http://localhost:8080` instead

### "Can't connect from phone"
- Make sure phone and computer are on the **same WiFi**
- Check if firewall is blocking port 5000
- Try temporarily disabling firewall/VPN

### "Permission denied" on Mac/Linux
```bash
chmod +x install.sh start.sh start.command
```

### Still having issues?
- Check the [INSTALL.md](INSTALL.md) for detailed troubleshooting
- Open an issue on GitHub
- Check that all prerequisites are installed

---

## 🎮 What Now?

Once you're in:

1. **Explore the Interface**: green-on-navy Flask HTML UI (canvas world view, textarea editor, terminal). Not pixel-art sprites and not a Vault-Tec product.
2. **Check Metrics**: the right panel heading is **Metrics**. Labels are Floors, Agents, Tasks, Tools.
3. **View Agents**: the heading is **Agents**. Default-seed assistants stay idle — they are on the department, not in `office-1.agents`, so the tick loop does not process them.
4. **STEP / START / STOP / REFRESH**: those are the simulation button labels (not “STEP (+1 Tick)” or “REFRESH STATE”).
5. **Read the Docs**: Check out QUICKSTART.md for the API and the actual default seed.
---

## 💡 Pro Tips

- **Bookmark it**: Save the URL for quick access
- **Keep the process running**: Restarting drops in-memory world state
- **LAN access**: Other devices on the same network can open `http://LAN_IP:5000`. There is no account system.
- **Docker**: `docker compose up --build` if you have Docker; you must export `SECRET_KEY` (compose has **no** default). Reuse that key across restarts if `./data` already has an HMAC-tagged audit log. The image runs gunicorn `--workers 4` (split in-memory world), not `run.py`.

---

This is a Flask prototype, not a production IDE.
