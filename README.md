# Thirstys-Projects-Miniature-Office

[![CI Tests](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CI%20-%20Test%20and%20Lint/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![Docker](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/workflows/CD%20-%20Build%20and%20Deploy/badge.svg)](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Python 3.9+](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/)

**A Civilization-Tier Cognitive IDE: Where Software Development Becomes Spatial Simulation**

🚀 **PRODUCTION READY** - Fully deployed, tested, and production-hardened

It's a spatialized, agent-orchestrated development environment where code synthesis, review, and integration are modeled as a living organization inside a simulated world.

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

### 🐳 Docker (Recommended)

```bash
# Clone and run with Docker Compose
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
docker-compose up
```

Then open your browser to `http://localhost:5000`

### 🐍 Python Development

```bash
# Prerequisites: Python 3.9+
pip install -r requirements.txt
python3 run.py
```

## Production Deployment

See **[DEPLOYMENT.md](DEPLOYMENT.md)** for comprehensive production deployment guide including:
- Docker deployment
- Kubernetes manifests
- Systemd service configuration
- Security hardening
- Monitoring setup
- Load balancing

## Documentation

- **[QUICKSTART.md](QUICKSTART.md)** - Installation and basic usage
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

### Production Features ✅
- ✅ Comprehensive test suite (22 tests, 32% coverage)
- ✅ Docker containerization with multi-stage builds
- ✅ CI/CD with GitHub Actions
- ✅ Health check & Prometheus metrics endpoints
- ✅ Security headers & CORS configuration
- ✅ Production WSGI server (Gunicorn + eventlet)
- ✅ Environment-based configuration
- ✅ Non-root Docker container
- ✅ Automated security scanning

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
