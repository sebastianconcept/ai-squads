---
description: Feature Solution - Deployment Automation
type: feature-solution
status: planned
---

# Solution: Deployment Automation

## Solution Overview

Implement a comprehensive CI/CD deployment system using GitHub Actions that automatically deploys BankFlow to Digital Ocean droplets with separate staging and production environments, automated server setup scripts, and emergency deployment capabilities.

## Technical Approach

### Microservices Architecture

**Complete Containerized Services:**
```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Site Service  │    │   SaaS Service   │    │   API Service   │
│   (Port 3000)   │    │   (Port 3001)    │    │   (Port 8080)   │
│   Marketing     │    │   User App       │    │   Backend API   │
└─────────────────┘    └──────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 ▼
                    ┌──────────────────┐
                    │   Data Services  │
                    │   PostgreSQL     │
                    │   (Port 5432)    │
                    │   Redis          │
                    │   (Port 6379)    │
                    └──────────────────┘
                                 │
                                 ▼
                    ┌──────────────────┐
                    │   Load Balancer  │
                    │   Nginx          │
                    │   (Port 80/443)  │
                    └──────────────────┘
```

**Service Independence:**
- Each service runs in its own container
- Independent scaling and resource allocation
- Individual health checks and monitoring
- Isolated deployments and rollbacks
- Service-to-service communication via internal networking

### GitHub Actions CI/CD Pipeline

**Pipeline Structure:**
```yaml
# .github/workflows/deploy.yml
name: Deploy BankFlow

on:
  push:
    branches: [main, staging]
  pull_request:
    branches: [main, staging]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run tests
        run: cargo test
      - name: Run security audit
        run: cargo audit
      - name: Check formatting
        run: cargo fmt -- --check
      - name: Run clippy
        run: cargo clippy -- -D warnings

  deploy-staging:
    if: github.ref == 'refs/heads/staging'
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Deploy to staging
        run: ./scripts/deploy.sh staging
        env:
          DROPLET_IP: ${{ secrets.DROPLET_IP }}
          SSH_KEY: ${{ secrets.SSH_KEY }}

  deploy-production:
    if: github.ref == 'refs/heads/main'
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Deploy to production
        run: ./scripts/deploy.sh production
        env:
          DROPLET_IP: ${{ secrets.DROPLET_IP }}
          SSH_KEY: ${{ secrets.SSH_KEY }}
```

### Digital Ocean Droplet Configuration

**Server Setup Script:**
```bash
#!/bin/bash
# scripts/setup-droplet.sh

set -e

echo "🚀 Setting up BankFlow droplet..."

# Update system
echo "📦 Updating system packages..."
apt update && apt upgrade -y

# Check if Docker is already installed
if command -v docker &> /dev/null; then
    echo "✅ Docker is already installed (version: $(docker --version))"
    
    # Check if Docker Compose plugin is available
    if docker compose version &> /dev/null; then
        echo "✅ Docker Compose plugin is already available (version: $(docker compose version))"
    else
        echo "📦 Installing Docker Compose plugin..."
        apt install docker-compose-plugin -y
    fi
else
    echo "📦 Installing Docker and Docker Compose..."
    curl -fsSL https://get.docker.com -o get-docker.sh
    sh get-docker.sh
    apt install docker-compose-plugin -y
    echo "✅ Docker installation completed"
fi

# Check if application user exists
if id "bankflow" &>/dev/null; then
    echo "✅ Application user 'bankflow' already exists"
else
    echo "👤 Creating application user..."
    useradd -m -s /bin/bash bankflow
    echo "✅ Application user created"
fi

# Add user to docker group (safe to run multiple times)
usermod -aG docker bankflow
echo "✅ User added to docker group"

# Create application directory
if [ -d "/home/bankflow/app" ]; then
    echo "✅ Application directory already exists"
else
    echo "📁 Creating application directory..."
    mkdir -p /home/bankflow/app
    echo "✅ Application directory created"
fi

chown bankflow:bankflow /home/bankflow/app

# Install monitoring tools and SSL certificate management
echo "📊 Installing monitoring tools and SSL certificate management..."
apt install -y htop curl certbot python3-certbot-nginx

# Configure firewall (check current status)
echo "🔥 Configuring firewall..."
if ufw status | grep -q "Status: active"; then
    echo "✅ Firewall is already active"
else
    ufw allow 22
    ufw allow 80
    ufw allow 443
    ufw --force enable
    echo "✅ Firewall configured and enabled"
fi

# Setup log rotation (check if already exists)
if [ -f "/etc/logrotate.d/bankflow" ]; then
    echo "✅ Log rotation already configured"
else
    echo "📝 Setting up log rotation..."
    cat > /etc/logrotate.d/bankflow << EOF
/home/bankflow/app/logs/*.log {
    daily
    missingok
    rotate 7
    compress
    delaycompress
    notifempty
    create 644 bankflow bankflow
}
EOF
    echo "✅ Log rotation configured"
fi

# Verify Docker is working
echo "🔍 Verifying Docker installation..."
if docker run --rm hello-world &> /dev/null; then
    echo "✅ Docker is working correctly"
else
    echo "❌ Docker verification failed"
    exit 1
fi

# Verify Docker Compose is working
echo "🔍 Verifying Docker Compose..."
if docker compose version &> /dev/null; then
    echo "✅ Docker Compose is working correctly"
else
    echo "❌ Docker Compose verification failed"
    exit 1
fi

echo "🎉 Droplet setup completed successfully!"
echo ""
echo "📋 Setup Summary:"
echo "  - Docker: $(docker --version)"
echo "  - Docker Compose: $(docker compose version)"
echo "  - User: bankflow (in docker group)"
echo "  - Directory: /home/bankflow/app"
echo "  - Firewall: Active (ports 22, 80, 443)"
echo "  - Log rotation: Configured"
echo ""
echo "🚀 Ready for BankFlow deployment!"
```

### Environment-Specific Configuration

**Staging Environment (Microservices Architecture):**
```yaml
# docker-compose.staging.yml
version: '3.8'
services:
  # Site Service - Landing page and marketing
  site:
    build: 
      context: .
      dockerfile: Dockerfile.site
    ports: ["3000:3000"]
    environment:
      - RUST_LOG=info
      - ENVIRONMENT=staging
      - DOMAIN=qa.conciliaextrato.com
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3000/health"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 256M, cpus: '0.25' }

  # SaaS Service - Main application
  saas:
    build:
      context: .
      dockerfile: Dockerfile.saas
    ports: ["3001:3001"]
    environment:
      - RUST_LOG=info
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=staging
      - DOMAIN=app.qa.conciliaextrato.com
      - API_BASE_URL=http://api:8080
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [postgres, redis, api]
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3001/health"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 512M, cpus: '0.5' }

  # API Service - Backend API
  api:
    build:
      context: .
      dockerfile: Dockerfile.api
    ports: ["8080:8080"]
    environment:
      - RUST_LOG=info
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=staging
      - DOMAIN=api.qa.conciliaextrato.com
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [postgres, redis]
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 1G, cpus: '1.0' }

  # Database Service
  postgres:
    image: postgres:15-alpine
    ports: ["5432:5432"]
    environment:
      - POSTGRES_DB=${POSTGRES_DB}
      - POSTGRES_USER=${POSTGRES_USER}
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${POSTGRES_USER} -d ${POSTGRES_DB}"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 1G, cpus: '1.0' }

  # Cache/Queue Service
  redis:
    image: redis:7-alpine
    ports: ["6379:6379"]
    volumes: [redis_data:/data]
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 256M, cpus: '0.25' }

  # Load Balancer/Reverse Proxy
  nginx:
    image: nginx:alpine
    ports: ["80:80", "443:443"]
    volumes:
      - ./nginx/nginx.staging.conf:/etc/nginx/nginx.conf
    depends_on: [site, saas, api]
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost/health"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  postgres_data:
  redis_data:
```

**Production Environment (Microservices Architecture):**
```yaml
# docker-compose.prod.yml
version: '3.8'
services:
  # Site Service - Landing page and marketing
  site:
    build: 
      context: .
      dockerfile: Dockerfile.site
    ports: ["3000:3000"]
    environment:
      - RUST_LOG=warn
      - ENVIRONMENT=production
      - DOMAIN=conciliaextrato.com
    restart: always
    deploy:
      resources:
        limits: { memory: 512M, cpus: '0.5' }
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3000/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  # SaaS Service - Main application
  saas:
    build:
      context: .
      dockerfile: Dockerfile.saas
    ports: ["3001:3001"]
    environment:
      - RUST_LOG=warn
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=production
      - DOMAIN=app.conciliaextrato.com
      - API_BASE_URL=http://api:8080
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [postgres, redis, api]
    restart: always
    deploy:
      resources:
        limits: { memory: 1G, cpus: '1.0' }
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3001/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  # API Service - Backend API
  api:
    build:
      context: .
      dockerfile: Dockerfile.api
    ports: ["8080:8080"]
    environment:
      - RUST_LOG=warn
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=production
      - DOMAIN=api.conciliaextrato.com
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [postgres, redis]
    restart: always
    deploy:
      resources:
        limits: { memory: 2G, cpus: '2.0' }
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  # Database Service
  postgres:
    image: postgres:15-alpine
    ports: ["5432:5432"]
    environment:
      - POSTGRES_DB=${POSTGRES_DB}
      - POSTGRES_USER=${POSTGRES_USER}
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${POSTGRES_USER} -d ${POSTGRES_DB}"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 2G, cpus: '2.0' }

  # Cache/Queue Service
  redis:
    image: redis:7-alpine
    ports: ["6379:6379"]
    volumes: [redis_data:/data]
    restart: always
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 30s
      timeout: 10s
      retries: 3
    deploy:
      resources:
        limits: { memory: 512M, cpus: '0.5' }

  # Load Balancer/Reverse Proxy
  nginx:
    image: nginx:alpine
    ports: ["80:80", "443:443"]
    volumes:
      - ./nginx/nginx.prod.conf:/etc/nginx/nginx.conf
      - ./nginx/ssl:/etc/nginx/ssl
    depends_on: [site, saas, api]
    restart: always
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost/health"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  postgres_data:
  redis_data:
```

### Deployment Script

**Automated Deployment Script:**
```bash
#!/bin/bash
# scripts/deploy.sh

set -e

ENVIRONMENT=$1
DROPLET_IP=${DROPLET_IP}
SSH_KEY=${SSH_KEY}

if [ "$ENVIRONMENT" = "staging" ]; then
    DOMAIN="qa.conciliaextrato.com"
    COMPOSE_FILE="docker-compose.staging.yml"
elif [ "$ENVIRONMENT" = "production" ]; then
    DOMAIN="conciliaextrato.com"
    COMPOSE_FILE="docker-compose.prod.yml"
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "Deploying to $ENVIRONMENT environment ($DOMAIN)..."

# Deploy to droplet (build images on droplet)
ssh -i $SSH_KEY bankflow@$DROPLET_IP << EOF
    cd /home/bankflow/app
    
    # Pull latest code
    echo "📥 Pulling latest code from $ENVIRONMENT branch..."
    git pull origin $ENVIRONMENT
    
    # Stop current services
    echo "🛑 Stopping current services..."
    docker-compose -f $COMPOSE_FILE down
    
    # Build images on droplet
    echo "🔨 Building Docker images on droplet..."
    docker-compose -f $COMPOSE_FILE build --no-cache
    
    # Start services with new images
    echo "🚀 Starting services with new images..."
    docker-compose -f $COMPOSE_FILE up -d
    
    # Clean up old images
    echo "🧹 Cleaning up old images..."
    docker system prune -f
    
    # Verify deployment
    echo "✅ Verifying deployment..."
    docker-compose -f $COMPOSE_FILE ps
EOF

echo "Deployment to $ENVIRONMENT completed successfully!"
```

### Additional Required Scripts

**Backup Script:**
```bash
#!/bin/bash
# scripts/backup.sh

set -e

BACKUP_DIR="/home/bankflow/backups"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="bankflow_backup_${DATE}.sql"

echo "Creating database backup..."

# Create backup directory
mkdir -p $BACKUP_DIR

# Create PostgreSQL backup
docker exec bankflow-postgres pg_dump -U ${POSTGRES_USER} -d ${POSTGRES_DB} > $BACKUP_DIR/$BACKUP_FILE

# Compress backup
gzip $BACKUP_DIR/$BACKUP_FILE

# Keep only last 7 days of backups
find $BACKUP_DIR -name "bankflow_backup_*.sql.gz" -mtime +7 -delete

echo "Backup completed: $BACKUP_DIR/${BACKUP_FILE}.gz"
```

**Emergency Deployment Script:**
```bash
#!/bin/bash
# scripts/emergency-deploy.sh

set -e

echo "🚨 EMERGENCY DEPLOYMENT MODE 🚨"
echo "This script will deploy to a new droplet quickly"
echo "Use only for critical fixes!"

read -p "Are you sure? (yes/no): " confirm
if [ "$confirm" != "yes" ]; then
    echo "Emergency deployment cancelled"
    exit 1
fi

ENVIRONMENT=$1
NEW_DROPLET_IP=$2

if [ -z "$NEW_DROPLET_IP" ]; then
    echo "Usage: $0 <environment> <new_droplet_ip>"
    exit 1
fi

echo "Setting up new droplet: $NEW_DROPLET_IP"

# Run server setup on new droplet
ssh -i $SSH_KEY root@$NEW_DROPLET_IP < scripts/setup-droplet.sh

# Deploy to new droplet
ssh -i $SSH_KEY bankflow@$NEW_DROPLET_IP << EOF
    cd /home/bankflow/app
    git clone https://github.com/your-repo/bankflow.git .
    ./scripts/deploy.sh $ENVIRONMENT
EOF

echo "Emergency deployment completed!"
```

**Health Check Script:**
```bash
#!/bin/bash
# scripts/health-check.sh

set -e

ENVIRONMENT=$1
DROPLET_IP=${DROPLET_IP}

if [ "$ENVIRONMENT" = "staging" ]; then
    DOMAINS=("qa.conciliaextrato.com" "app.qa.conciliaextrato.com" "api.qa.conciliaextrato.com")
elif [ "$ENVIRONMENT" = "production" ]; then
    DOMAINS=("conciliaextrato.com" "app.conciliaextrato.com" "api.conciliaextrato.com")
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "Checking health of $ENVIRONMENT environment..."

# Check each domain
for domain in "${DOMAINS[@]}"; do
    echo "Checking $domain..."
    if curl -f -s "https://$domain/health" > /dev/null; then
        echo "✅ $domain is healthy"
    else
        echo "❌ $domain is unhealthy"
        exit 1
    fi
done

# Check Docker containers
ssh -i $SSH_KEY bankflow@$DROPLET_IP << EOF
    cd /home/bankflow/app
    docker-compose ps
    docker-compose logs --tail=10
EOF

echo "All health checks passed!"
```

**Rollback Script:**
```bash
#!/bin/bash
# scripts/rollback.sh

set -e

ENVIRONMENT=$1
DROPLET_IP=${DROPLET_IP}

if [ "$ENVIRONMENT" = "staging" ]; then
    COMPOSE_FILE="docker-compose.staging.yml"
elif [ "$ENVIRONMENT" = "production" ]; then
    COMPOSE_FILE="docker-compose.prod.yml"
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "Rolling back $ENVIRONMENT environment..."

ssh -i $SSH_KEY bankflow@$DROPLET_IP << EOF
    cd /home/bankflow/app
    
    # Stop current services
    docker-compose -f $COMPOSE_FILE down
    
    # Get previous image
    docker images | grep bankflow | head -2
    
    # Start with previous image (you might need to adjust this)
    docker-compose -f $COMPOSE_FILE up -d
    
    # Verify rollback
    docker-compose -f $COMPOSE_FILE ps
EOF

echo "Rollback completed!"
```

**Log Management Script:**
```bash
#!/bin/bash
# scripts/logs.sh

set -e

ENVIRONMENT=$1
SERVICE=$2
DROPLET_IP=${DROPLET_IP}

if [ "$ENVIRONMENT" = "staging" ]; then
    COMPOSE_FILE="docker-compose.staging.yml"
elif [ "$ENVIRONMENT" = "production" ]; then
    COMPOSE_FILE="docker-compose.prod.yml"
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "Fetching logs for $ENVIRONMENT environment..."

ssh -i $SSH_KEY bankflow@$DROPLET_IP << EOF
    cd /home/bankflow/app
    
    if [ -n "$SERVICE" ]; then
        echo "Logs for $SERVICE service:"
        docker-compose -f $COMPOSE_FILE logs --tail=100 $SERVICE
    else
        echo "Logs for all services:"
        docker-compose -f $COMPOSE_FILE logs --tail=50
    fi
EOF
```

**Database Migration Script:**
```bash
#!/bin/bash
# scripts/migrate.sh

set -e

ENVIRONMENT=$1
DROPLET_IP=${DROPLET_IP}

if [ "$ENVIRONMENT" = "staging" ]; then
    COMPOSE_FILE="docker-compose.staging.yml"
elif [ "$ENVIRONMENT" = "production" ]; then
    COMPOSE_FILE="docker-compose.prod.yml"
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "Running database migrations for $ENVIRONMENT..."

ssh -i $SSH_KEY bankflow@$DROPLET_IP << EOF
    cd /home/bankflow/app
    
    # Run migrations
    docker-compose -f $COMPOSE_FILE exec api cargo run --bin migrate
    
    # Verify migration
    docker-compose -f $COMPOSE_FILE exec postgres psql -U \${POSTGRES_USER} -d \${POSTGRES_DB} -c "\\dt"
EOF

echo "Migrations completed!"
```

**SSL Certificate Management Script:**
```bash
#!/bin/bash
# scripts/ssl-setup.sh

set -e

ENVIRONMENT=$1
EMAIL=${SSL_EMAIL:-admin@conciliaextrato.com}

if [ "$ENVIRONMENT" = "staging" ]; then
    DOMAINS=("qa.conciliaextrato.com" "app.qa.conciliaextrato.com" "api.qa.conciliaextrato.com")
    STAGING_FLAG="--staging"
elif [ "$ENVIRONMENT" = "production" ]; then
    DOMAINS=("conciliaextrato.com" "www.conciliaextrato.com" "app.conciliaextrato.com" "api.conciliaextrato.com")
    STAGING_FLAG=""
else
    echo "Invalid environment. Use 'staging' or 'production'"
    exit 1
fi

echo "🔒 Setting up SSL certificates for $ENVIRONMENT environment..."

# Create SSL directory
mkdir -p /etc/nginx/ssl

# Generate certificates for all domains
echo "📜 Generating Let's Encrypt certificates..."
certbot certonly \
    --nginx \
    --non-interactive \
    --agree-tos \
    --email $EMAIL \
    --expand \
    $STAGING_FLAG \
    -d $(IFS=,; echo "${DOMAINS[*]}")

# Create symlinks for easier management
echo "🔗 Creating certificate symlinks..."
for domain in "${DOMAINS[@]}"; do
    if [ -f "/etc/letsencrypt/live/$domain/fullchain.pem" ]; then
        ln -sf "/etc/letsencrypt/live/$domain/fullchain.pem" "/etc/nginx/ssl/$domain.crt"
        ln -sf "/etc/letsencrypt/live/$domain/privkey.pem" "/etc/nginx/ssl/$domain.key"
        echo "✅ Certificate symlinks created for $domain"
    fi
done

# Setup automatic renewal
echo "🔄 Setting up automatic certificate renewal..."
if ! crontab -l | grep -q "certbot renew"; then
    (crontab -l 2>/dev/null; echo "0 12 * * * /usr/bin/certbot renew --quiet --nginx") | crontab -
    echo "✅ Automatic renewal configured"
else
    echo "✅ Automatic renewal already configured"
fi

# Test renewal
echo "🧪 Testing certificate renewal..."
certbot renew --dry-run

echo "🎉 SSL setup completed successfully!"
echo ""
echo "📋 SSL Summary:"
for domain in "${DOMAINS[@]}"; do
    if [ -f "/etc/nginx/ssl/$domain.crt" ]; then
        echo "  ✅ $domain: $(openssl x509 -in /etc/nginx/ssl/$domain.crt -noout -dates | grep notAfter | cut -d= -f2)"
    fi
done
```

**SSL Renewal Script:**
```bash
#!/bin/bash
# scripts/ssl-renew.sh

set -e

echo "🔄 Renewing SSL certificates..."

# Renew certificates
certbot renew --nginx --quiet

# Reload nginx to use new certificates
systemctl reload nginx

echo "✅ SSL certificates renewed successfully!"
```

### Nginx Configuration

**Staging Nginx Config (Multi-Domain):**
```nginx
# nginx/nginx.staging.conf
# Main site - qa.conciliaextrato.com
server {
    listen 80;
    server_name qa.conciliaextrato.com;
    
    location / {
        proxy_pass http://site:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location /health {
        proxy_pass http://site:3000/health;
        access_log off;
    }
}

# SaaS app - app.qa.conciliaextrato.com
server {
    listen 80;
    server_name app.qa.conciliaextrato.com;
    
    location / {
        proxy_pass http://saas:3001;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location /health {
        proxy_pass http://saas:3001/health;
        access_log off;
    }
}

# API - api.qa.conciliaextrato.com
server {
    listen 80;
    server_name api.qa.conciliaextrato.com;
    
    location / {
        proxy_pass http://api:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location /health {
        proxy_pass http://api:8080/health;
        access_log off;
    }
}
```

**Production Nginx Config (Multi-Domain):**
```nginx
# nginx/nginx.prod.conf
# Main site - conciliaextrato.com
server {
    listen 80;
    server_name conciliaextrato.com www.conciliaextrato.com;
    
    # Redirect HTTP to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name conciliaextrato.com www.conciliaextrato.com;
    
    # SSL configuration (Let's Encrypt certificates)
    ssl_certificate /etc/nginx/ssl/conciliaextrato.com.crt;
    ssl_certificate_key /etc/nginx/ssl/conciliaextrato.com.key;
    
    # SSL settings
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-CHACHA20-POLY1305;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    
    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    
    location / {
        proxy_pass http://site:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts
        proxy_connect_timeout 30s;
        proxy_send_timeout 30s;
        proxy_read_timeout 30s;
    }
    
    location /health {
        proxy_pass http://site:3000/health;
        access_log off;
    }
}

# SaaS app - app.conciliaextrato.com
server {
    listen 80;
    server_name app.conciliaextrato.com;
    
    # Redirect HTTP to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name app.conciliaextrato.com;
    
    # SSL configuration (Let's Encrypt certificates)
    ssl_certificate /etc/nginx/ssl/app.conciliaextrato.com.crt;
    ssl_certificate_key /etc/nginx/ssl/app.conciliaextrato.com.key;
    
    # SSL settings
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-CHACHA20-POLY1305;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    
    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    
    location / {
        proxy_pass http://saas:3001;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts
        proxy_connect_timeout 30s;
        proxy_send_timeout 30s;
        proxy_read_timeout 30s;
    }
    
    location /health {
        proxy_pass http://saas:3001/health;
        access_log off;
    }
}

# API - api.conciliaextrato.com
server {
    listen 80;
    server_name api.conciliaextrato.com;
    
    # Redirect HTTP to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name api.conciliaextrato.com;
    
    # SSL configuration (Let's Encrypt certificates)
    ssl_certificate /etc/nginx/ssl/api.conciliaextrato.com.crt;
    ssl_certificate_key /etc/nginx/ssl/api.conciliaextrato.com.key;
    
    # SSL settings
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-CHACHA20-POLY1305;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    
    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    
    location / {
        proxy_pass http://api:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts
        proxy_connect_timeout 30s;
        proxy_send_timeout 30s;
        proxy_read_timeout 30s;
    }
    
    location /health {
        proxy_pass http://api:8080/health;
        access_log off;
    }
}
```

## User Experience

### Developer Workflow

**Staging Deployment:**
1. Developer creates feature branch
2. Developer merges to `staging` branch
3. GitHub Actions automatically runs tests
4. If tests pass, automatic deployment to qa.conciliaextrato.com
5. Developer can test features in staging environment
6. If staging tests pass, merge to `main` branch for production

**Production Deployment:**
1. Developer merges tested code to `main` branch
2. GitHub Actions automatically runs comprehensive tests
3. If tests pass, automatic deployment to conciliaextrato.com
4. Health checks verify successful deployment
5. Monitoring alerts team of deployment status

### Emergency Deployment

**Emergency Scenario Response:**
1. Team identifies critical issue requiring immediate deployment
2. Developer creates hotfix branch from `main`
3. Hotfix is tested and merged to `main`
4. Automatic deployment to production occurs
5. If deployment fails, automatic rollback to previous version
6. Team is notified of deployment status via monitoring alerts

## Implementation Plan

### Phase 1: Infrastructure Setup (Week 1)
1. **Digital Ocean Droplet Setup**
   - Create production droplet with Docker support
   - Run server setup script for initial configuration
   - Configure SSH access and security groups
   - Setup domain DNS configuration

2. **GitHub Actions Pipeline**
   - Create basic CI/CD pipeline with testing
   - Setup environment variables and secrets
   - Configure staging and production deployment jobs
   - Test pipeline with sample deployments

### Phase 2: Environment Configuration (Week 2)
1. **Staging Environment**
   - Configure qa.conciliaextrato.com subdomain
   - Setup staging-specific Docker Compose configuration
   - Configure staging environment variables
   - Test staging deployment workflow

2. **Production Environment**
   - Configure conciliaextrato.com domain
   - Setup production-specific Docker Compose configuration
   - Configure production environment variables
   - Setup SSL certificates via Cloudflare

### Phase 3: Monitoring and Automation (Week 3)
1. **Health Checks and Monitoring**
   - Implement application health check endpoints
   - Setup basic monitoring and alerting
   - Configure log rotation and management
   - Test monitoring and alerting systems

2. **Emergency Procedures**
   - Create emergency deployment scripts
   - Setup rollback procedures
   - Document emergency response procedures
   - Test emergency deployment scenarios

### Phase 4: Testing and Validation (Week 4)
1. **End-to-End Testing**
   - Test complete deployment pipeline
   - Validate staging and production environments
   - Test emergency deployment procedures
   - Performance testing and optimization

2. **Documentation and Training**
   - Document deployment procedures
   - Create troubleshooting guides
   - Train team on new deployment processes
   - Create maintenance and monitoring procedures

## Dependencies

### External Dependencies
- **Digital Ocean Account**: For droplet hosting
- **GitHub Repository**: For CI/CD pipeline
- **Cloudflare Account**: For DNS and SSL management
- **Domain Registration**: conciliaextrato.com domain

### Internal Dependencies
- **Docker Configuration**: Existing Docker Compose setup
- **Application Health Checks**: Health check endpoints in application
- **Environment Variables**: Proper environment variable configuration
- **Database Migrations**: Automated migration system

### Security Dependencies
- **SSH Key Management**: Secure SSH key generation and management
- **Environment Secrets**: Secure storage of sensitive configuration
- **SSL Certificates**: Proper SSL certificate management
- **Access Controls**: Proper access controls for deployment

## Risks and Mitigation

### Technical Risks
- **Deployment Failures**: Implement comprehensive health checks and automatic rollback
- **SSL Certificate Issues**: Use Cloudflare-managed certificates with automatic renewal
- **Database Migration Failures**: Implement migration rollback procedures
- **Docker Image Issues**: Use multi-stage builds and image scanning

### Operational Risks
- **Server Downtime**: Implement zero-downtime deployment with health checks
- **Data Loss**: Implement automated backups and disaster recovery
- **Security Vulnerabilities**: Implement security scanning in CI/CD pipeline
- **Performance Issues**: Implement performance monitoring and alerting

### Business Risks
- **Customer Impact**: Implement staging environment for testing
- **Compliance Issues**: Maintain LGPD compliance in deployment process
- **Cost Overruns**: Monitor resource usage and implement cost controls
- **Team Training**: Provide comprehensive training on new deployment processes
