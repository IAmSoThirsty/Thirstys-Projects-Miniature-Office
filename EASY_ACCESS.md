# Easy access

Miniature Office is a local Flask app you open in a browser.

There is no phone-native installer, no PWA package, and no VR/WebXR client. A phone, tablet, or headset browser on the same network can load the Flask URL. That is still the HTML UI.

## Desktop

| OS | Install | Start |
| --- | --- | --- |
| Windows | `install.ps1` | `start.bat` |
| macOS | `install.sh` | `start.command` or `./start.sh` |
| Linux | `install.sh` | `./start.sh` |
| Any with Docker | — | `docker compose up --build` |

Then open `http://127.0.0.1:5000`.

## Other devices on the LAN

1. Start the server on a computer.
2. Find that computer’s LAN IP (`ipconfig` / `ifconfig` / `ip addr`).
3. On the other device, open `http://THE_IP:5000`.

“Add to Home Screen” in a mobile browser is a bookmark. This repo does not ship a web app manifest or service worker.

## More detail

- Commands: [QUICK_REFERENCE.md](QUICK_REFERENCE.md)
- Steps: [GETTING_STARTED.md](GETTING_STARTED.md)
- What “runs everywhere” means: [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md)
- Status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md)
