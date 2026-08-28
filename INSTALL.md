# Installation Guide - Miniature Office

**Access: local Flask app in a browser (desktop and mobile). There is no native VR client.**

## 📱 Easiest Option: Use Your Web Browser (All Platforms)

The Miniature Office runs as a web application, accessible from **any device with a modern web browser**:

1. Someone starts the server (see options below)
2. Open your browser to: `http://localhost:5000` (or the server's IP address)
3. Works in a modern browser on the same machine or LAN. A headset browser can load the page; that is not a VR product (no WebXR).

This means:
- **Desktop**: Chrome, Firefox, Safari, Edge on Windows/Mac/Linux
- **Mobile / tablet**: any phone or tablet browser pointed at the Flask server
- **Not included**: a native app or WebXR session
- **Included**: a small PWA shell (`manifest.json` + `sw.js`). “Add to Home Screen” installs that shell, not a native app

---

## 🖥️ Desktop Installation (Windows, macOS, Linux)

### Option 1: One-Click Installer (Recommended)

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

### Option 2: Docker (Easiest, All Platforms)

```bash
# One command to run everything
docker compose up --build
```

Then open: `http://localhost:5000`

**Prerequisites**: Install [Docker Desktop](https://www.docker.com/products/docker-desktop)

### Option 3: Manual Installation

**Prerequisites**: Python 3.9 or higher

```bash
# Install dependencies
pip install -r requirements.txt

# Run the application
python3 run.py
```

Then open: `http://localhost:5000`

---

## 📱 Mobile Access (Android & iOS)

The Miniature Office is a **web-based application** that works perfectly on mobile devices:

### Method 1: Connect to Local Server
1. Start the server on your computer (see Desktop Installation above)
2. Find your computer's IP address:
   - Windows: `ipconfig` (look for IPv4 Address)
   - macOS/Linux: `ifconfig` or `ip addr` (look for inet address)
3. On your mobile device, open the browser
4. Navigate to: `http://YOUR_COMPUTER_IP:5000`

**Example**: If your computer's IP is `192.168.1.100`, go to `http://192.168.1.100:5000`

### Method 2: Browser shortcut (optional)

You can bookmark `http://YOUR_COMPUTER_IP:5000` or use the browser’s “Add to Home Screen.” That is a shortcut to the Flask page. This repo does not ship a PWA (`manifest.json` and a service worker are absent).

### Method 3: Run on Your Phone (Advanced)

**Android (Termux)**:
```bash
# Install Termux from F-Droid or Play Store
pkg install python git
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

## 🌐 Network Access

### Make it Accessible on Your Network

**Edit the run.py file** or use environment variables:
```python
run_server(host='0.0.0.0', port=5000)  # Already set to accept connections
```

Now anyone on your network can access it at: `http://YOUR_IP:5000`

### Security Note
When opening to your network:
1. Set a strong `SECRET_KEY` (compose default is a placeholder)
2. There is no authentication on the API
3. Restarting the process drops in-memory world state and audit events
4. Use firewall rules to limit access. This is a local prototype, not a hardened service.

---

## 🚀 Quick Start Commands

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

### Docker (All Platforms)
```bash
docker compose up --build    # Start everything
docker compose down          # Stop everything
```

---

## 📦 Pre-built Packages (Future)

We're working on pre-built installers:
- [ ] Windows: `.exe` installer with automatic Python bundling
- [ ] macOS: `.dmg` or `.app` bundle
- [ ] Linux: `.deb` and `.rpm` packages
- [ ] Snap package for Linux
- [ ] Electron-based desktop app (Windows, macOS, Linux)

---

## 🔧 Troubleshooting

### Port Already in Use
If port 5000 is taken, edit `run.py` and change the port:
```python
run_server(host='0.0.0.0', port=8080)
```

### Python Not Found
- Windows: Install from [python.org](https://www.python.org/downloads/)
- macOS: `brew install python3`
- Linux: `sudo apt install python3 python3-pip` (Ubuntu/Debian)

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

## 💡 Tips

1. **Bookmark it**: Save `http://localhost:5000` on the machine that runs the server
2. **LAN only**: Other devices on the same network can open `http://LAN_IP:5000`. There is no account system.
3. **Keep the process running**: Restarting drops in-memory world state
4. **Docker**: `docker compose up --build` if you have Docker; default `SECRET_KEY` is a placeholder

---

## 📚 Additional Resources

- **[README.md](README.md)** - Project overview and measured metrics
- **[CLAIMS_AUDIT.md](CLAIMS_AUDIT.md)** - What is actually implemented
- **[QUICKSTART.md](QUICKSTART.md)** - Quick usage guide
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Design notes (intent, not a completion certificate)

---

## ❓ Need Help?

- Check [GitHub Issues](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues)
- Read the troubleshooting section above
- Review the logs in the console where you started the server
