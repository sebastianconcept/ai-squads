---
description: BankFlow Tech Stack - Technical architecture and technology choices
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# BankFlow Tech Stack

> Last Updated: December 2024
> Version: 1.0

## Architecture Overview

### High-Level Architecture
```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Frontend      │    │   API Gateway    │    │   Processing    │
│   (HTMX/Alpine) │◄──►│   (Rust/Axum)    │◄──►│   Engine        │
└─────────────────┘    └──────────────────┘    │   (Rust/Tokio)  │
                                                └─────────────────┘
                              │                          │
                              ▼                          ▼
                    ┌──────────────────┐    ┌─────────────────┐
                    │   PostgreSQL     │    │   Local File    │
                    │   (Primary DB)   │    │   Storage       │
                    └──────────────────┘    └─────────────────┘
                              │
                              ▼
                    ┌──────────────────┐
                    │   Redis          │
                    │   (Cache/Queue)  │
                    └──────────────────┘
```

## Frontend Technologies

### Core Frontend Stack
- **HTMX**: Server-driven UI with minimal JavaScript
- **Alpine.js**: Lightweight reactive framework for interactions
- **Tailwind CSS**: Utility-first CSS framework
- **Tera Templates**: Server-side templating engine

### Rationale
- **Simplicity**: Minimal JavaScript, server-driven approach
- **Performance**: Fast page loads, minimal bundle size
- **SEO-friendly**: Server-side rendering
- **Maintenance**: Less complexity than SPA frameworks

### Frontend Architecture
```rust
// Template structure
templates/
├── base.html              # Base template with layout
├── dashboard.html         # Main dashboard
├── upload.html           # File upload interface
├── results.html          # Processing results
└── components/
    ├── file_upload.html  # Upload component
    ├── progress.html    # Progress indicator
    └── job_list.html    # Job listing component
```

## Backend Technologies

### Core Backend Stack
- **Rust**: High-performance systems programming language
- **Axum**: Modern web framework for Rust
- **Tokio**: Async runtime for Rust
- **SQLx**: Type-safe SQL toolkit

### Rationale
- **Performance**: High-throughput file processing
- **Memory safety**: No crashes from memory issues  
- **Concurrency**: Excellent async/await support
- **Ecosystem**: Rich CSV/Excel parsing libraries

### Backend Architecture
```rust
src/
├── main.rs               # Application entry point
├── config.rs            # Configuration management
├── models/               # Data models
│   ├── user.rs          # User model
│   ├── job.rs           # Processing job model
│   └── transaction.rs    # Transaction model
├── services/             # Business logic
│   ├── auth.rs          # Authentication service
│   ├── upload.rs        # File upload service
│   ├── processing.rs    # Processing service
│   └── notification.rs  # Notification service
├── parsers/              # Bank parsers
│   ├── itau.rs          # Itaú parser
│   ├── bradesco.rs      # Bradesco parser
│   └── traits.rs         # Parser traits
├── api/                  # API handlers
│   ├── auth.rs          # Auth endpoints
│   ├── files.rs         # File endpoints
│   └── jobs.rs          # Job endpoints
└── utils/                # Utilities
    ├── crypto.rs        # Encryption utilities
    └── validation.rs    # Validation utilities
```

## Database Technologies

### Primary Database
- **PostgreSQL**: Primary relational database
- **Redis**: Caching and job queue

### Rationale
- **ACID compliance**: Financial data requires consistency
- **JSON support**: Flexible schema for different bank formats
- **Full-text search**: Search through processed extracts
- **Mature ecosystem**: Robust tooling and monitoring

### Database Schema
```sql
-- Core tables
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    company VARCHAR(255),
    subscription_tier subscription_tier DEFAULT 'free',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    last_login TIMESTAMP WITH TIME ZONE,
    is_active BOOLEAN DEFAULT true
);

CREATE TABLE processing_jobs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id),
    original_filename VARCHAR(255) NOT NULL,
    file_size BIGINT NOT NULL,
    file_hash VARCHAR(64) NOT NULL,
    detected_bank VARCHAR(100),
    status processing_status DEFAULT 'queued',
    started_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    completed_at TIMESTAMP WITH TIME ZONE,
    error_message TEXT,
    input_file_path VARCHAR(500) NOT NULL,
    output_file_path VARCHAR(500),
    transaction_count INTEGER
);

-- Enums
CREATE TYPE subscription_tier AS ENUM ('free', 'professional', 'business', 'enterprise');
CREATE TYPE processing_status AS ENUM ('queued', 'processing', 'completed', 'failed', 'cancelled');
```

## Infrastructure Technologies

### Cloud Platform
- **Digital Ocean Droplet**: Single server deployment with Docker Compose
- **Local File Storage**: Temporary file storage with automatic cleanup
- **Cloudflare**: CDN and DDoS protection (optional)

### Rationale
- **Cost-effective**: Single server deployment for MVP phase
- **Simple Setup**: Docker Compose handles all services
- **Local Storage**: Works well with temporary file storage approach
- **Easy Management**: Single server to monitor and maintain

### Infrastructure Configuration

#### Local Development
```yaml
# docker-compose.yml
version: '3.8'
services:
  app:
    build: .
    ports: ["8080:8080"]
    environment:
      - RUST_LOG=debug
      - DATABASE_URL=postgresql://bankflow:bankflow123@db:5432/bankflow_dev
      - REDIS_URL=redis://redis:6379
      - UPLOAD_DIR=/app/uploads
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [db, redis]
    restart: unless-stopped

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_DB=bankflow_dev
      - POSTGRES_USER=bankflow
      - POSTGRES_PASSWORD=bankflow123
    ports: ["5432:5432"]
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    ports: ["6379:6379"]
    volumes:
      - redis_data:/data
    restart: unless-stopped

volumes:
  postgres_data:
  redis_data:
```

#### Staging Environment
```yaml
# docker-compose.staging.yml
version: '3.8'
services:
  app:
    build: .
    ports: ["8080:8080"]
    environment:
      - RUST_LOG=info
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - UPLOAD_DIR=/app/uploads
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=staging
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [db, redis]
    restart: unless-stopped
    deploy:
      resources:
        limits: { memory: 1G, cpus: '1.0' }
        reservations: { memory: 512M, cpus: '0.5' }

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_DB=${POSTGRES_DB}
      - POSTGRES_USER=${POSTGRES_USER}
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    volumes: [redis_data:/data]
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports: ["80:80", "443:443"]
    volumes:
      - ./nginx/nginx.staging.conf:/etc/nginx/nginx.conf
      - ./nginx/ssl:/etc/nginx/ssl
    depends_on: [app]
    restart: unless-stopped

volumes:
  postgres_data:
  redis_data:
```

#### Production Environment
```yaml
# docker-compose.prod.yml
version: '3.8'
services:
  app:
    build: .
    ports: ["8080:8080"]
    environment:
      - RUST_LOG=warn
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
      - UPLOAD_DIR=/app/uploads
      - JWT_SECRET=${JWT_SECRET}
      - ENVIRONMENT=production
    volumes:
      - ./uploads:/app/uploads
      - ./logs:/app/logs
    depends_on: [db, redis]
    restart: always
    deploy:
      resources:
        limits: { memory: 2G, cpus: '2.0' }
        reservations: { memory: 1G, cpus: '1.0' }
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_DB=${POSTGRES_DB}
      - POSTGRES_USER=${POSTGRES_USER}
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./scripts/backup.sh:/usr/local/bin/backup.sh
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${POSTGRES_USER} -d ${POSTGRES_DB}"]
      interval: 30s
      timeout: 10s
      retries: 3

  redis:
    image: redis:7-alpine
    volumes: [redis_data:/data]
    restart: always
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 30s
      timeout: 10s
      retries: 3

  nginx:
    image: nginx:alpine
    ports: ["80:80", "443:443"]
    volumes:
      - ./nginx/nginx.prod.conf:/etc/nginx/nginx.conf
      - ./nginx/ssl:/etc/nginx/ssl
      - ./logs/nginx:/var/log/nginx
    depends_on: [app]
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

### Server Requirements
- **Minimum**: 2GB RAM, 1 CPU, 25GB SSD ($12/month)
- **Recommended**: 4GB RAM, 2 CPU, 80GB SSD ($24/month)
- **Services**: PostgreSQL, Redis, BankFlow app, Nginx

## File Storage Technologies

### Local File Storage
- **Temporary Storage**: Files stored locally with automatic cleanup
- **UUID-based Naming**: Unique file identifiers for security
- **Automatic Cleanup**: Files removed after processing (30-minute retention)
- **Volume Mounting**: Docker volumes for persistent storage

### Rationale
- **Simplicity**: No external storage dependencies for MVP
- **Cost-effective**: No storage costs during low-load phase
- **Performance**: Local file access is faster than network storage
- **Development Speed**: Faster implementation without external services

### Implementation
```rust
use std::path::PathBuf;
use tokio::fs;
use uuid::Uuid;

pub struct LocalFileStorage {
    upload_dir: PathBuf,
}

impl LocalFileStorage {
    pub fn new(upload_dir: PathBuf) -> Self {
        Self { upload_dir }
    }
    
    pub async fn store_file(&self, content: &[u8]) -> Result<String, StorageError> {
        let file_id = Uuid::new_v4().to_string();
        let file_path = self.upload_dir.join(&file_id);
        
        fs::write(&file_path, content).await?;
        
        // Schedule cleanup after processing
        self.schedule_cleanup(file_path, Duration::from_secs(1800)); // 30 min
        
        Ok(file_id)
    }
    
    pub async fn read_file(&self, file_id: &str) -> Result<Vec<u8>, StorageError> {
        let file_path = self.upload_dir.join(file_id);
        fs::read(file_path).await.map_err(|_| StorageError::FileNotFound)
    }
    
    pub async fn delete_file(&self, file_id: &str) -> Result<(), StorageError> {
        let file_path = self.upload_dir.join(file_id);
        fs::remove_file(file_path).await.map_err(|_| StorageError::FileNotFound)
    }
    
    fn schedule_cleanup(&self, file_path: PathBuf, delay: Duration) {
        tokio::spawn(async move {
            tokio::time::sleep(delay).await;
            let _ = fs::remove_file(file_path).await;
        });
    }
}
```

## Security Technologies

### Authentication & Authorization
- **JWT**: Stateless authentication tokens
- **Argon2**: Password hashing
- **AES-256-GCM**: Sensitive data encryption

### Data Protection
- **Encryption**: AES-256-GCM for sensitive data
- **Transport**: TLS 1.3 for all communications
- **Compliance**: LGPD compliance for Brazilian market

### Security Features
- **Audit Logging**: Comprehensive activity tracking
- **Access Control**: Role-based permissions
- **Data Retention**: Automated file cleanup policies
- **Backup Strategy**: Automated PostgreSQL backups

### Security Implementation
```rust
use argon2::{Argon2, PasswordHash, PasswordHasher};
use aes_gcm::{Aes256Gcm, Key, Nonce};

pub struct SecurityService {
    encryption_key: Key<Aes256Gcm>,
    argon2: Argon2<'static>,
}

impl SecurityService {
    pub fn hash_password(&self, password: &str) -> Result<String, SecurityError> {
        let salt = SaltString::generate(&mut OsRng);
        let password_hash = self.argon2.hash_password(password.as_bytes(), &salt)?;
        Ok(password_hash.to_string())
    }
    
    pub fn encrypt_file_data(&self, data: &[u8]) -> Result<Vec<u8>, SecurityError> {
        let cipher = Aes256Gcm::new(&self.encryption_key);
        let nonce = Nonce::from_slice(b"unique nonce");
        cipher.encrypt(nonce, data).map_err(|_| SecurityError::EncryptionFailed)
    }
}
```

## Monitoring & Observability

### Application Monitoring
- **Prometheus**: Metrics collection
- **Grafana**: Metrics visualization
- **Basic Logging**: Structured logging with JSON format

### Logging
- **Structured Logging**: JSON-formatted logs
- **File-based Logging**: Local log files with rotation
- **Basic Alerting**: Simple health checks and monitoring

### Monitoring Implementation
```rust
use prometheus::{Counter, Histogram, register_counter, register_histogram};

lazy_static! {
    static ref UPLOAD_COUNTER: Counter = register_counter!(
        "bankflow_uploads_total", 
        "Total number of file uploads"
    ).unwrap();
    
    static ref PROCESSING_DURATION: Histogram = register_histogram!(
        "bankflow_processing_duration_seconds",
        "Time taken to process files"
    ).unwrap();
}
```

## Development Tools

### Development Environment
- **Docker**: Containerized development
- **Cargo**: Rust package manager
- **SQLx CLI**: Database migration tools

### Testing
- **Cargo Test**: Unit and integration testing
- **Testcontainers**: Integration test containers
- **Mockito**: API mocking for tests

### CI/CD
- **GitHub Actions**: Continuous integration
- **Docker**: Containerized deployment
- **Automated Testing**: Comprehensive test suite

## Performance Considerations

### Optimization Strategies
- **Connection Pooling**: Database connection optimization
- **Caching**: Redis-based caching for frequent queries
- **Streaming**: Large file processing without memory issues
- **Parallel Processing**: Concurrent file processing

### Performance Targets
- **Processing Time**: <30 seconds per file
- **Response Time**: <200ms for API calls
- **Concurrent Users**: 50-100 users (single server)
- **Uptime**: 99%+ availability

## Scalability Architecture

### Horizontal Scaling
- **Server Upgrade**: Upgrade to larger Digital Ocean droplet
- **Load Balancing**: Multiple droplets with load balancer (future)
- **CDN**: Cloudflare for static content delivery

### Vertical Scaling
- **Resource Optimization**: Efficient memory and CPU usage
- **Caching Strategies**: Redis caching for frequent queries
- **Database Optimization**: Query optimization and indexing

## Technology Decision Matrix

| Technology | Rationale | Alternatives Considered | Trade-offs |
|------------|-----------|-------------------------|------------|
| Rust | Performance, memory safety | Go, Node.js, Python | Learning curve vs performance |
| HTMX | Simplicity, SEO | React, Vue, Svelte | Less interactivity vs simplicity |
| PostgreSQL | ACID compliance | MongoDB, MySQL | Flexibility vs consistency |
| Redis | Speed, caching | Memcached, In-memory DB | Persistence vs speed |
| Digital Ocean | Cost-effective | AWS, GCP, Azure | Features vs simplicity |
| Local Storage | Simplicity | S3, MinIO | Scalability vs simplicity |

## Future Technology Considerations

### Phase 2+ Technologies
- **Shared Storage**: S3/MinIO for distributed file storage
- **Analytics**: ClickHouse for advanced analytics and metrics
- **Machine Learning**: TensorFlow/PyTorch for categorization
- **Message Queues**: Apache Kafka for event streaming
- **Search**: Elasticsearch for advanced search capabilities

### Technology Evolution
- **Multi-server**: Multiple droplets with load balancing
- **WebAssembly**: Client-side processing capabilities
- **Edge Computing**: Processing closer to users
- **AI/ML**: Advanced financial pattern recognition
