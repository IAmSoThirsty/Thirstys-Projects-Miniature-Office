# Installation Guide - Miniature Office

**Quick access across all platforms: Desktop, Mobile, and VR**

## 📱 Easiest Option: Use Your Web Browser (All Platforms)

The Miniature Office runs as a web application, accessible from **any device with a modern web browser**:

1. Someone starts the server (see options below)
2. Open your browser to: `http://localhost:5000` (or the server's IP address)
3. Works on: Windows, macOS, Linux, Android, iOS, VR headsets with browsers

This means:
- ✅ **Desktop**: Chrome, Firefox, Safari, Edge on Windows/Mac/Linux
- ✅ **Mobile**: Any phone or tablet with a web browser
- ✅ **VR**: Quest Browser, Firefox Reality, Wolvic on VR headsets
- ✅ **Tablets**: iPad, Android tablets, Surface devices

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
docker-compose up
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

### Method 2: Progressive Web App (PWA)
1. Open the application in your mobile browser
2. **iOS Safari**: Tap the Share button → "Add to Home Screen"
3. **Android Chrome**: Tap the menu (⋮) → "Add to Home Screen"
4. Now you have an app icon that launches Miniature Office like a native app!

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

## 🥽 VR Access (Quest, PSVR, PC VR)

The Miniature Office works in VR through web browsers!

### Meta Quest (Quest 2, Quest 3, Quest Pro)
1. Start the Miniature Office server on your PC
2. On your Quest, open the **Browser** app
3. Navigate to your PC's IP address: `http://192.168.1.XXX:5000`
4. The web interface is fully functional in VR!

### PC VR (Steam VR, Virtual Desktop)
1. Run the Miniature Office on your PC
2. In VR, open Steam's Desktop view or Virtual Desktop
3. Open any browser to `http://localhost:5000`
4. Interact with the interface in VR space

### WebXR Support (Future)
The application uses web technologies that are compatible with WebXR. Future updates may include immersive VR mode with:
- 3D spatial visualization of the office
- VR controller interaction
- Hand tracking support

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
1. Set a strong `SECRET_KEY` in `.env`
2. Configure CORS if needed
3. Consider using HTTPS (see DEPLOYMENT.md)
4. Use firewall rules to limit access

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
docker-compose up    # Start everything
docker-compose down  # Stop everything
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
chmod +x install.sh start.sh
```

### Mobile Can't Connect
- Ensure your computer and mobile device are on the same WiFi network
- Check your firewall isn't blocking port 5000
- Try disabling VPN temporarily

### VR Browser Issues
- Ensure your VR headset and PC are on the same network
- Try using the IP address instead of `localhost`
- Update your VR browser to the latest version

---

## 💡 Tips

1. **Bookmark it**: Save `http://localhost:5000` as a bookmark on all devices
2. **Mobile home screen**: Add to home screen for quick access
3. **Always accessible**: Keep the server running and access from any device
4. **Share with others**: Start the server and share your IP for collaborative access
5. **Use Docker**: For the easiest setup with no Python installation needed

---

## 📚 Additional Resources

- **[README.md](README.md)** - Project overview
- **[QUICKSTART.md](QUICKSTART.md)** - Quick usage guide
- **[DEPLOYMENT.md](DEPLOYMENT.md)** - Production deployment
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - System architecture

---

## ❓ Need Help?

- Check [GitHub Issues](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues)
- Read the troubleshooting section above
- Review the logs in the console where you started the server
