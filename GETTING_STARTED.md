# 🎯 Getting Started - Choose Your Platform

Welcome to Miniature Office! This guide will help you get started no matter what device you're using.

## Quick Decision Tree

```
What device do you have?
│
├─ 🖥️  Desktop Computer (Windows/Mac/Linux)
│   │
│   ├─ Have Docker installed? 
│   │   └─ YES → Run: docker-compose up ✅ (Easiest!)
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
│   └─ Want app-like experience?
│       └─ Add to Home Screen from browser! (Acts like an app)
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

**iPhone/iPad:**
1. Tap the Share button (square with arrow)
2. Scroll down and tap "Add to Home Screen"
3. Give it a name: "Miniature Office"
4. Tap "Add"
5. Now you have an app icon! Tap it anytime to launch

**Android:**
1. Tap the menu button (⋮) in the browser
2. Tap "Add to Home screen"
3. Give it a name: "Miniature Office"
4. Tap "Add"
5. Now you have an app icon! Tap it anytime to launch

---

## Headset browsers

There is no WebXR session and no VR-native UI. A Quest (or other) browser can load `http://[server-ip]:5000` the same way a phone can. That is still the Flask HTML page.

---

## 🐳 Docker - Universal Method

Works on Windows, Mac, and Linux!

### Prerequisites
Install Docker Desktop: https://www.docker.com/products/docker-desktop

### Steps

1. **Open Terminal/Command Prompt**

2. **Clone the project** (or download and extract):
   ```bash
   git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
   cd Thirstys-Projects-Miniature-Office
   ```

3. **Start with one command**:
   ```bash
   docker-compose up
   ```

4. **Wait for it to start** (10-30 seconds)

5. **Open browser to**: `http://localhost:5000`

6. **To stop**: Press `Ctrl+C` or run `docker-compose down`

---

## ❓ Troubleshooting

### "Python not found"
- **Windows**: Download from python.org (check "Add to PATH"!)
- **Mac**: Run `brew install python3`
- **Linux**: Run `sudo apt install python3 python3-pip`

### "Port 5000 already in use"
- Another program is using port 5000
- Edit `run.py` and change `port=5000` to `port=8080`
- Then use `http://localhost:8080` instead

### "Can't connect from phone"
- Make sure phone and computer are on the **same WiFi**
- Check if firewall is blocking port 5000
- Try temporarily disabling firewall/VPN

### "Permission denied" on Mac/Linux
```bash
chmod +x install.sh start.sh
```

### Still having issues?
- Check the [INSTALL.md](INSTALL.md) for detailed troubleshooting
- Open an issue on GitHub
- Check that all prerequisites are installed

---

## 🎮 What Now?

Once you're in:

1. **Explore the Interface**: Vault-Tec pixel art design
2. **Check World Metrics**: See floors, agents, tasks
3. **View Active Agents**: Watch the simulation
4. **Step Through Time**: Use simulation controls
5. **Read the Docs**: Check out QUICKSTART.md for features

---

## 💡 Pro Tips

- **Bookmark it**: Save the URL for quick access
- **Keep server running**: Access from multiple devices simultaneously
- **Share access**: Give others your IP on the same LAN
- **Use Docker**: Easiest setup with zero configuration

---

This is a Flask prototype, not a production IDE.

