# Deployment Guide

## Production Deployment Options

### Option 1: Docker Deployment (Recommended)

#### Prerequisites
- Docker 20.10+
- Docker Compose 2.0+

#### Quick Start

1. **Clone the repository:**
```bash
git clone https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office.git
cd Thirstys-Projects-Miniature-Office
```

2. **Configure environment variables:**
```bash
cp .env.example .env
# Edit .env and set your SECRET_KEY
nano .env
```

3. **Build and run with Docker Compose:**
```bash
docker-compose up -d
```

4. **Verify deployment:**
```bash
curl http://localhost:5000/health
```

5. **View logs:**
```bash
docker-compose logs -f
```

6. **Stop the service:**
```bash
docker-compose down
```

#### Production Docker Deployment

For production, use a proper secret key:

```bash
# Generate a secure random secret
python3 -c "import secrets; print(secrets.token_hex(32))"

# Set environment variable
export SECRET_KEY="your-generated-secret-here"

# Deploy
docker-compose up -d
```

### Option 2: Direct Python Deployment

#### Prerequisites
- Python 3.9+
- pip
- systemd (for service management)

#### Installation Steps

1. **Install dependencies:**
```bash
pip install -r requirements.txt
```

2. **Configure environment:**
```bash
cp .env.example .env
# Edit .env with your configuration
```

3. **Run with Gunicorn:**
```bash
gunicorn --bind 0.0.0.0:5000 \
         --workers 4 \
         --worker-class eventlet \
         --timeout 120 \
         --access-logfile /var/log/miniature-office/access.log \
         --error-logfile /var/log/miniature-office/error.log \
         src.server.app:app
```

#### Systemd Service

Create `/etc/systemd/system/miniature-office.service`:

```ini
[Unit]
Description=Miniature Office - Cognitive IDE
After=network.target

[Service]
Type=notify
User=minioffice
Group=minioffice
WorkingDirectory=/opt/miniature-office
Environment="PATH=/opt/miniature-office/venv/bin"
EnvironmentFile=/opt/miniature-office/.env
ExecStart=/opt/miniature-office/venv/bin/gunicorn \
    --bind 0.0.0.0:5000 \
    --workers 4 \
    --worker-class eventlet \
    --timeout 120 \
    --access-logfile /var/log/miniature-office/access.log \
    --error-logfile /var/log/miniature-office/error.log \
    src.server.app:app
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Enable and start the service:

```bash
sudo systemctl daemon-reload
sudo systemctl enable miniature-office
sudo systemctl start miniature-office
sudo systemctl status miniature-office
```

### Option 3: Kubernetes Deployment

#### Prerequisites
- Kubernetes cluster
- kubectl configured
- Container registry access

#### Deployment Manifests

Create `k8s/deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: miniature-office
  labels:
    app: miniature-office
spec:
  replicas: 3
  selector:
    matchLabels:
      app: miniature-office
  template:
    metadata:
      labels:
        app: miniature-office
    spec:
      containers:
      - name: miniature-office
        image: ghcr.io/iamsothirsty/thirstys-projects-miniature-office:latest
        ports:
        - containerPort: 5000
        env:
        - name: SECRET_KEY
          valueFrom:
            secretKeyRef:
              name: minioffice-secrets
              key: secret-key
        - name: FLASK_ENV
          value: "production"
        - name: WORKERS
          value: "4"
        livenessProbe:
          httpGet:
            path: /health
            port: 5000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 5000
          initialDelaySeconds: 10
          periodSeconds: 5
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
---
apiVersion: v1
kind: Service
metadata:
  name: miniature-office
spec:
  selector:
    app: miniature-office
  ports:
  - protocol: TCP
    port: 80
    targetPort: 5000
  type: LoadBalancer
```

Deploy to Kubernetes:

```bash
# Create secret
kubectl create secret generic minioffice-secrets \
  --from-literal=secret-key=$(python3 -c "import secrets; print(secrets.token_hex(32))")

# Deploy application
kubectl apply -f k8s/deployment.yaml

# Check status
kubectl get pods
kubectl get services

# View logs
kubectl logs -f deployment/miniature-office
```

## Monitoring and Operations

### Health Checks

The application provides two monitoring endpoints:

1. **Health Check:** `GET /health`
   - Returns 200 when healthy
   - Returns 503 when starting up
   - Use for load balancer health checks

2. **Metrics:** `GET /metrics`
   - Prometheus-compatible metrics
   - World time, agents, tasks, audit events
   - Use with Prometheus/Grafana

### Prometheus Configuration

Add to `prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'miniature-office'
    static_configs:
      - targets: ['localhost:5000']
    metrics_path: '/metrics'
    scrape_interval: 15s
```

### Logging

Application logs are written to:
- Docker: `stdout/stderr` (view with `docker-compose logs`)
- Systemd: `/var/log/miniature-office/`
- Kubernetes: Container logs (view with `kubectl logs`)

Log levels can be configured via the `LOG_LEVEL` environment variable:
- `DEBUG`: Detailed debugging information
- `INFO`: General informational messages (default)
- `WARNING`: Warning messages
- `ERROR`: Error messages
- `CRITICAL`: Critical errors

## Security Best Practices

### 1. Secret Key Management

**Never** commit the `.env` file or expose `SECRET_KEY`. Use:
- Environment variables
- Kubernetes secrets
- Docker secrets
- Cloud provider secret managers (AWS Secrets Manager, Azure Key Vault, etc.)

### 2. Network Security

- Run behind a reverse proxy (nginx, Traefik, etc.)
- Enable TLS/SSL certificates
- Configure firewall rules
- Use private networks when possible

### 3. CORS Configuration

In production, restrict CORS origins:

```bash
# In .env
CORS_ORIGINS=https://yourdomain.com,https://app.yourdomain.com
```

### 4. Rate Limiting

Use a reverse proxy for rate limiting:

**Nginx example:**

```nginx
limit_req_zone $binary_remote_addr zone=minioffice:10m rate=10r/s;

server {
    listen 80;
    server_name minioffice.yourdomain.com;

    location / {
        limit_req zone=minioffice burst=20 nodelay;
        proxy_pass http://localhost:5000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

## Scaling Considerations

### Horizontal Scaling

The application can be scaled horizontally:
- Multiple worker processes (via `WORKERS` env var)
- Multiple container instances
- Load balancer in front

**Note:** Current implementation uses in-memory state. For true horizontal scaling across multiple instances, consider:
- Redis for shared state
- PostgreSQL for persistent storage
- Message queue for task distribution

### Vertical Scaling

Adjust resources based on load:
- CPU: 250m-1000m per worker
- Memory: 256Mi-1Gi per worker
- Workers: 2-8 per instance (2-4 × CPU cores)

## Backup and Disaster Recovery

### State Persistence

The audit log and world state are currently in-memory. For production:

1. **Implement database persistence** (planned feature)
2. **Regular snapshots** of world state
3. **Audit log export** to permanent storage

### Backup Script Example

```bash
#!/bin/bash
# backup-minioffice.sh

DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backups/minioffice"

mkdir -p "$BACKUP_DIR"

# Export audit log
curl http://localhost:5000/api/audit/events?limit=10000 > \
    "$BACKUP_DIR/audit_log_$DATE.json"

# Export world state
curl http://localhost:5000/api/world/state > \
    "$BACKUP_DIR/world_state_$DATE.json"

# Compress and retain last 30 days
find "$BACKUP_DIR" -name "*.json" -mtime +30 -delete
```

## Troubleshooting

### Application Won't Start

1. Check logs: `docker-compose logs` or `journalctl -u miniature-office`
2. Verify port 5000 is available: `lsof -i :5000`
3. Check Python version: `python3 --version` (needs 3.9+)
4. Verify all dependencies: `pip install -r requirements.txt`

### Health Check Fails

1. Check if application is running: `curl http://localhost:5000/health`
2. Verify simulation initialized: Check logs for "Simulation initialized"
3. Check resource limits: Ensure adequate CPU/memory

### Performance Issues

1. Increase worker count: Set `WORKERS=8` in environment
2. Check metrics endpoint: `curl http://localhost:5000/metrics`
3. Monitor resource usage: `docker stats` or `kubectl top pods`
4. Review logs for errors or warnings

### WebSocket Connection Issues

1. Ensure eventlet worker class: `--worker-class eventlet`
2. Check WebSocket protocol support in reverse proxy
3. Verify CORS configuration allows WebSocket upgrades

## Support and Maintenance

### Updating the Application

```bash
# Docker
git pull
docker-compose build
docker-compose up -d

# Systemd
git pull
sudo systemctl restart miniature-office

# Kubernetes
docker build -t your-registry/miniature-office:new-version .
docker push your-registry/miniature-office:new-version
kubectl set image deployment/miniature-office \
    miniature-office=your-registry/miniature-office:new-version
```

### Monitoring Checklist

- [ ] Health endpoint returns 200
- [ ] Metrics endpoint accessible
- [ ] CPU usage < 80%
- [ ] Memory usage < 80%
- [ ] No error logs
- [ ] Simulation advancing (check world time)
- [ ] Agents operational
- [ ] Audit log growing

## Production Checklist

Before going to production:

- [ ] Generate and set secure `SECRET_KEY`
- [ ] Configure proper `CORS_ORIGINS`
- [ ] Set `FLASK_ENV=production` and `FLASK_DEBUG=0`
- [ ] Enable TLS/SSL certificates
- [ ] Configure reverse proxy with rate limiting
- [ ] Set up monitoring (Prometheus/Grafana)
- [ ] Configure log aggregation
- [ ] Set up backup automation
- [ ] Test health checks
- [ ] Load test the application
- [ ] Document incident response procedures
- [ ] Configure alerting rules

## Contact

For issues and questions:
- GitHub Issues: https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/issues
- Documentation: See README.md and ARCHITECTURE.md
