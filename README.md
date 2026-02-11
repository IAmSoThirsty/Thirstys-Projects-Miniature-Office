# Thirstys-Projects-Miniature-Office

[![CI Tests](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CI%20-%20Test%20and%20Lint/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![Docker](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CD%20-%20Build%20and%20Deploy/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python 3.9+](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)

**A Civilization-Tier Cognitive IDE: Where Software Development Becomes Spatial Simulation**

🚀 **BETA STATUS** - Core pipeline now functional! ([see what's new](LIMITATIONS.md))

💻 **RUNS EVERYWHERE** - Desktop, Mobile, VR, Tablets - Access from any device!

It's a spatialized, agent-orchestrated development environment where code synthesis, review, and integration are modeled as a living organization inside a simulated world.

## 🎯 Super Easy Installation

### One-Click Installers

**Windows**: Right-click `install.ps1` → "Run with PowerShell" → Double-click `start.bat`

**macOS**: Run `./install.sh` in Terminal → Double-click `start.command`

**Linux**: Run `./install.sh` in Terminal → Run `./start.sh`

**Docker** (All Platforms): `docker-compose up` - Done! ✅

Then open your browser to `http://localhost:5000`

📱 **Works on phones & tablets too!** See **[INSTALL.md](INSTALL.md)** for mobile & VR setup.

## 🌐 Access From Anywhere

- 🖥️ **Desktop**: Windows, macOS, Linux
- 📱 **Mobile**: Android, iOS (via browser or PWA)
- 🥽 **VR**: Quest Browser, Firefox Reality, any VR web browser
- 💻 **Tablets**: iPad, Android tablets, Surface devices

**It's a web app**, so once the server is running, access it from **any device** on your network!

## Core Concept

**IDE × RTS × Organizational Simulation**

Less "write code" — More "run a software company in miniature"

## What Makes This "Civilization-Tier"?

This isn't just an IDE or agent framework. It's a **Cognitive Operating Environment for Software Civilization** with:

### 🧠 Cognitive Contracts
Intent, design, and responsibility as first-class objects:
- Why does this code exist?
- Who agreed to this?
- What assumptions does this rest on?
- Who is accountable if it fails?

### 💰 Scarcity Economics
Resources are finite, forcing meaningful decisions:
- Agent time budgets
- Manager attention limits
- Tool slot constraints
- No infinite retries or endless consensus

### 🏛️ Constitutional Mutation
Controlled system evolution with safeguards:
- Formal mutation proposals
- Impact simulations required
- Delayed activation (never immediate)
- Mandatory rollback paths
- Core laws cannot be self-removed

## Quick Start

### 🚀 Super Easy (Recommended for Everyone)

**One-Click Installation:**

**Windows** 
```cmd
# Right-click install.ps1 → "Run with PowerShell"
# Then double-click start.bat
```

**macOS**
```bash
./install.sh          # One-time setup
./start.command       # Double-click to start!
```

**Linux**
```bash
./install.sh          # One-time setup  
./start.sh           # Run to start
```

Then open your browser to `http://localhost:5000`

### 🐳 Docker (Works on All Platforms)

```bash
# Clone and run with Docker Compose
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
docker-compose up
```

Then open your browser to `http://localhost:5000`

### 🐍 Python Development (Manual)

```bash
# Prerequisites: Python 3.9+
pip install -r requirements.txt
python3 run.py
```

### 📱 Mobile & VR Access

See **[INSTALL.md](INSTALL.md)** for complete instructions on:
- Mobile phones (Android & iOS)
- Tablets (iPad, Android tablets)
- VR headsets (Quest, PSVR, PC VR)
- Creating a Progressive Web App (PWA)
- Network access from other devices

## Production Deployment

See **[DEPLOYMENT.md](DEPLOYMENT.md)** for comprehensive production deployment guide including:
- Docker deployment
- Kubernetes manifests
- Systemd service configuration
- Security hardening
- Monitoring setup
- Load balancing

## Documentation

- **[LIMITATIONS.md](LIMITATIONS.md)** - ⚠️ **Current implementation status and known limitations**
- **[INSTALL.md](INSTALL.md)** - 📱 **Complete installation guide** for Desktop, Mobile, VR, and all platforms
- **[GETTING_STARTED.md](GETTING_STARTED.md)** - 🎯 **Step-by-step visual guide** with decision tree
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - ⚡ **Quick commands** cheat sheet
- **[QUICKSTART.md](QUICKSTART.md)** - Basic usage and API examples
- **[DEPLOYMENT.md](DEPLOYMENT.md)** - Production deployment guide
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Complete system design (11 layers)

## Features

### Implemented ✅
- Formal ontology with entity relationships
- Immutable audit log with causality graphs
- Task lifecycle with state machines
- Agent system with capability profiles
- Department auto-staffing
- Supply store with tool checkout
- Contract system for integration
- Tick-based simulation engine
- RESTful API with WebSocket
- Pixel-art Vault-Tec interface
- **Cognitive Contracts** with enforcement laws
- **Scarcity Economics** with resource ledgers
- **Constitutional Mutation** engine with safeguards
- **🎉 NEW: Functional Code Generation Pipeline!**
  - Architectural analysis with conflict detection
  - Template-based code generation (Python, JS, Rust)
  - Automated code review with style checking
  - Test generation with coverage estimation

### Production Features ✅
- ✅ Comprehensive test suite (35 tests, 39% coverage - **up from 32%!**)
- ✅ Docker containerization with multi-stage builds
- ✅ CI/CD with GitHub Actions
- ✅ Health check & Prometheus metrics endpoints
- ✅ Security headers & CORS configuration
- ✅ Production WSGI server (Gunicorn + eventlet)
- ✅ Environment-based configuration
- ✅ Non-root Docker container
- ✅ Automated security scanning

## Quick Demo

Try the code generation pipeline:

```bash
python demo_pipeline.py
```

This demonstrates:
- Python function generation with docstrings and type hints
- JavaScript function generation with validation
- Automated test generation (pytest/chai format)
- Code review with style enforcement
- End-to-end pipeline processing

## API Endpoints

- `GET /health` - Health check for monitoring
- `GET /metrics` - Prometheus metrics
- `GET /api/world/state` - Current world state
- `POST /api/world/step` - Advance simulation
- `GET /api/agents` - List all agents
- `GET /api/departments` - List departments
- `GET /api/audit/events` - Audit log events
- See full API documentation at `/api`

## Testing

```bash
# Run tests
pytest tests/

# Run with coverage
pytest tests/ --cov=src --cov-report=html

# Run linting
flake8 src/
black --check src/
isort --check-only src/
```

## What You've Built

At this tier, the system is no longer just an IDE, agent framework, or simulator.

It is: **A Cognitive Operating Environment for Software Civilization**

Where:
- Code is governed
- Decisions are recorded  
- Authority is bounded
- History is immutable
- Evolution is controlled

This is the same conceptual tier as legal systems, constitutional governments, and safety-critical control rooms.

## License

See [LICENSE](LICENSE) for details.
