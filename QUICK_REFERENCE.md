# 🚀 Quick Reference - Installation Commands

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

## Docker (All Platforms)

```bash
# One command - no installation needed
docker compose up --build


# Stop
docker compose down

```

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
4. Optional: bookmark it. “Add to Home Screen” is a browser shortcut, not a PWA.


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
| Python not found | Install Python 3.9+ from python.org |
| Port 5000 in use | Edit run.py, change port to 8080 |
| Can't connect from phone | Same WiFi? Firewall off? |
| Permission denied | Run: `chmod +x install.sh start.sh start.command` |

## Need Help?

- 📖 Full guide: [INSTALL.md](INSTALL.md)
- 🎯 Step-by-step: [GETTING_STARTED.md](GETTING_STARTED.md)
- 📚 Quick start: [QUICKSTART.md](QUICKSTART.md)
- 🐛 Issues: [GitHub Issues](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues)

---

**Remember**: Once the server is running, you can access it from ANY device on your network! 🌐
