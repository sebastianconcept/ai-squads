---
description: BankFlow Decisions - Decision log and architectural decisions
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# BankFlow Decision Log

> Last Updated: December 2024
> Version: 1.1
> Phase 1 Demo Flow Architectural Decisions Added

## Decision Log Format

Each decision follows this format:
- **Date**: When the decision was made
- **Status**: Proposed, Accepted, Deprecated, Superseded
- **Context**: What led to this decision
- **Decision**: What was decided
- **Consequences**: What this means for the project

---

## ADR-001: Technology Stack Selection

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to select core technology stack for BankFlow microSaaS

**Decision**: Use Rust + Axum + HTMX + Alpine.js + PostgreSQL + Redis

**Rationale**:
- **Rust**: High performance for file processing, memory safety, excellent async support
- **Axum**: Modern async web framework, excellent performance, great HTMX integration
- **HTMX**: Server-driven UI reduces complexity, better SEO, faster development
- **Alpine.js**: Minimal JavaScript for interactivity, lightweight
- **PostgreSQL**: ACID compliance for financial data, mature ecosystem
- **Redis**: Fast caching and job queues

**Consequences**:
- ✅ High performance and reliability
- ✅ Faster development with server-driven approach
- ✅ Strong type safety and memory safety
- ✅ Modern async architecture for file processing
- ⚠️ Team needs to learn Rust (mitigated by comprehensive documentation)
- ⚠️ Less interactive UI compared to SPAs (acceptable for target users)

**Alternatives Considered**:
- **Node.js**: Faster development but lower performance for CPU-intensive tasks
- **Rocket**: Older Rust framework, less active development, synchronous model
- **React/Vue**: More interactive but higher complexity and bundle size
- **MongoDB**: More flexible but less suitable for financial data consistency

---

## ADR-002: File Processing Architecture

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to handle file processing efficiently with stateless instances

**Decision**: Implement stateless job processing with Redis queue and local file storage

**Rationale**:
- **Stateless Design**: Multiple instances can process jobs independently
- **Local Processing**: Files processed locally on the instance that picks up the job
- **Simple Architecture**: No shared file storage needed for MVP
- **User Experience**: Real-time status updates (uploaded > enqueued > processing > processed)
- **Resource Management**: Each instance manages its own file lifecycle

**Consequences**:
- ✅ Simple architecture for MVP phase
- ✅ Stateless instances work independently
- ✅ Real-time status updates for users
- ✅ No shared storage complexity
- ⚠️ Files tied to specific instance (acceptable for MVP)
- ⚠️ Will need shared storage for production scale

**Implementation**:
```rust
pub struct ProcessingService {
    db: PgPool,
    redis: redis::aio::MultiplexedConnection,
    parser_engine: ParserEngine,
    file_storage: LocalFileStorage,
}

impl ProcessingService {
    pub async fn start_worker(&self) {
        loop {
            // Pick up next job from Redis queue
            match self.get_next_job().await {
                Ok(Some(job)) => {
                    let service = self.clone();
                    task::spawn(async move {
                        service.process_job(job).await;
                    });
                }
                Ok(None) => {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
                Err(e) => {
                    eprintln!("Error fetching jobs: {}", e);
                    tokio::time::sleep(Duration::from_secs(5)).await;
                }
            }
        }
    }
    
    async fn process_job(&self, job: ProcessingJob) -> Result<(), ProcessingError> {
        // Update status to processing
        self.update_job_status(job.id, ProcessingStatus::Processing).await?;
        
        // Read file from local storage
        let file_content = self.file_storage.read_file(&job.file_id).await?;
        
        // Process file locally
        let parser = self.parser_engine.detect_bank(&file_content).await?;
        let transactions = parser.parse(&file_content).await?;
        
        // Generate output
        let output_data = self.generate_output(&transactions, OutputFormat::Csv).await?;
        
        // Store processed result temporarily
        let output_file_id = self.file_storage.store_file(&output_data).await?;
        
        // Update job as completed
        self.complete_job(job.id, output_file_id, transactions.len()).await?;
        
        // Clean up original file
        self.file_storage.delete_file(&job.file_id).await?;
        
        Ok(())
    }
}
```

**Status Flow**:
1. **Uploaded**: File uploaded and stored locally
2. **Enqueued**: Job added to Redis queue
3. **Processing**: Instance picks up job and processes file
4. **Processed**: Job completed, result available for download

---

## ADR-003: Bank Parser Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to parse various Brazilian bank statement formats

**Decision**: Implement trait-based parser system with automatic bank detection

**Rationale**:
- **Extensibility**: Easy to add new banks
- **Maintainability**: Each bank parser is isolated
- **Accuracy**: Bank-specific parsing logic
- **Confidence Scoring**: Automatic detection with confidence levels

**Consequences**:
- ✅ Easy to add new banks and formats
- ✅ High accuracy with bank-specific logic
- ✅ Automatic bank detection improves UX
- ⚠️ More initial development effort
- ⚠️ Need to maintain multiple parsers

**Implementation**:
```rust
#[async_trait]
pub trait BankParser: Send + Sync {
    async fn can_parse(&self, file_content: &[u8]) -> Result<f32, ParseError>;
    async fn parse(&self, file_content: &[u8]) -> Result<Vec<NormalizedTransaction>, ParseError>;
    fn bank_info(&self) -> BankInfo;
}

pub struct ParserEngine {
    parsers: Vec<Box<dyn BankParser>>,
}

impl ParserEngine {
    pub async fn detect_bank(&self, file_content: &[u8]) -> Result<Box<dyn BankParser>, ParseError> {
        let mut best_parser = None;
        let mut best_confidence = 0.0;
        
        for parser in &self.parsers {
            match parser.can_parse(file_content).await {
                Ok(confidence) if confidence > best_confidence => {
                    best_confidence = confidence;
                    best_parser = Some(parser);
                }
                _ => continue,
            }
        }
        
        match best_parser {
            Some(parser) if best_confidence > 0.7 => Ok(parser),
            _ => Err(ParseError::BankNotDetected),
        }
    }
}
```

---

## ADR-004: Data Storage Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to store and manage uploaded files and processed results for MVP phase

**Decision**: Use local temporary file storage with automatic cleanup for MVP phase

**Rationale**:
- **Simplicity**: No external dependencies for MVP
- **Cost-effective**: No storage costs during low-load phase
- **Stateless Design**: Works well with multiple Docker instances
- **Automatic Cleanup**: Files removed after processing to prevent disk bloat
- **Development Speed**: Faster implementation without external services

**Consequences**:
- ✅ Simple implementation and deployment
- ✅ No external service dependencies
- ✅ Cost-effective for low-load MVP
- ✅ Works well with stateless Docker instances
- ⚠️ Limited scalability (will need S3 for production scale)
- ⚠️ No redundancy (acceptable for MVP phase)

**Implementation**:
```rust
use std::path::PathBuf;
use tokio::fs;
use uuid::Uuid;

pub struct LocalFileStorage {
    upload_dir: PathBuf,
    cleanup_interval: Duration,
}

impl LocalFileStorage {
    pub fn new(upload_dir: PathBuf) -> Self {
        Self {
            upload_dir,
            cleanup_interval: Duration::from_secs(3600), // 1 hour cleanup
        }
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

**Migration Path**: This decision will be superseded by ADR-004b (S3 Storage Strategy) when scaling beyond MVP phase.

---

## ADR-005: Authentication Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need secure user authentication for the SaaS platform

**Decision**: Implement JWT-based authentication with Argon2 password hashing

**Rationale**:
- **Stateless**: No server-side session storage needed
- **Scalable**: Works well with horizontal scaling
- **Secure**: Argon2 is industry standard for password hashing
- **Simple**: Easy to implement and maintain

**Consequences**:
- ✅ Stateless authentication works well with scaling
- ✅ Industry-standard security practices
- ✅ Simple implementation and maintenance
- ⚠️ JWT tokens cannot be revoked easily (mitigated by short expiration)
- ⚠️ Need to handle token refresh

**Implementation**:
```rust
use argon2::{Argon2, PasswordHash, PasswordHasher, PasswordVerifier};
use jsonwebtoken::{encode, decode, Header, Algorithm, Validation, EncodingKey, DecodingKey};

pub struct AuthService {
    jwt_secret: String,
    argon2: Argon2<'static>,
}

impl AuthService {
    pub fn hash_password(&self, password: &str) -> Result<String, AuthError> {
        let salt = SaltString::generate(&mut OsRng);
        let password_hash = self.argon2.hash_password(password.as_bytes(), &salt)?;
        Ok(password_hash.to_string())
    }
    
    pub fn verify_password(&self, password: &str, hash: &str) -> Result<bool, AuthError> {
        let parsed_hash = PasswordHash::new(hash)?;
        Ok(self.argon2.verify_password(password.as_bytes(), &parsed_hash).is_ok())
    }
    
    pub fn generate_token(&self, user_id: Uuid) -> Result<String, AuthError> {
        let claims = Claims {
            sub: user_id.to_string(),
            exp: (Utc::now() + Duration::hours(24)).timestamp() as usize,
            iat: Utc::now().timestamp() as usize,
        };
        
        encode(&Header::new(Algorithm::HS256), &claims, &EncodingKey::from_secret(self.jwt_secret.as_ref()))
            .map_err(|_| AuthError::TokenGenerationFailed)
    }
}
```

---

## ADR-006: Real-time Updates Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to provide real-time updates for file processing status

**Decision**: Use Server-Sent Events (SSE) for real-time status updates

**Rationale**:
- **Simplicity**: Easier to implement than WebSockets
- **HTTP-based**: Works through firewalls and proxies
- **One-way**: Perfect for status updates
- **Browser Support**: Widely supported in modern browsers
- **HTMX Compatible**: Works well with HTMX

**Consequences**:
- ✅ Simple implementation and maintenance
- ✅ Works well with existing HTTP infrastructure
- ✅ Perfect for one-way status updates
- ⚠️ One-way communication only (sufficient for our use case)
- ⚠️ Connection management complexity

**Implementation**:
```rust
use axum::response::sse::{Sse, Event, KeepAlive};

pub async fn job_status_stream(
    Path(job_id): Path<Uuid>,
    State(state): State<AppState>,
    current_user: CurrentUser,
) -> impl IntoResponse {
    let stream = async_stream::stream! {
        let mut interval = tokio::time::interval(Duration::from_secs(2));
        
        loop {
            interval.tick().await;
            
            match state.job_service.get_job_status(job_id, current_user.id).await {
                Ok(status) => {
                    let event = format!("data: {}\n\n", serde_json::to_string(&status).unwrap());
                    yield Ok::<_, axum::Error>(event);
                    
                    if status.is_terminal() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    };
    
    Sse::new(stream).keep_alive(
        KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive-text"),
    )
}
```

---

## ADR-007: Error Handling Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need comprehensive error handling for file processing and API operations

**Decision**: Implement structured error handling with custom error types and comprehensive logging

**Rationale**:
- **Debugging**: Easy to trace and debug issues
- **User Experience**: Clear error messages for users
- **Monitoring**: Structured errors enable better monitoring
- **Maintenance**: Easier to maintain and extend

**Consequences**:
- ✅ Better debugging and monitoring capabilities
- ✅ Improved user experience with clear error messages
- ✅ Easier maintenance and extension
- ⚠️ More initial development effort
- ⚠️ Need to maintain error documentation

**Implementation**:
```rust
#[derive(Debug, thiserror::Error)]
pub enum BankFlowError {
    #[error("Authentication failed: {0}")]
    AuthenticationFailed(String),
    
    #[error("File processing failed: {0}")]
    ProcessingFailed(String),
    
    #[error("Storage error: {0}")]
    StorageError(String),
    
    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),
    
    #[error("Validation error: {0}")]
    ValidationError(String),
}

impl IntoResponse for BankFlowError {
    fn into_response(self) -> Response {
        let (status, error_message) = match self {
            BankFlowError::AuthenticationFailed(_) => {
                (StatusCode::UNAUTHORIZED, "Authentication failed")
            }
            BankFlowError::ProcessingFailed(_) => {
                (StatusCode::UNPROCESSABLE_ENTITY, "Processing failed")
            }
            BankFlowError::StorageError(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Storage error")
            }
            BankFlowError::DatabaseError(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Database error")
            }
            BankFlowError::ValidationError(_) => {
                (StatusCode::BAD_REQUEST, "Validation error")
            }
        };
        
        let body = Json(json!({
            "error": error_message,
            "message": self.to_string()
        }));
        
        (status, body).into_response()
    }
}
```

---

## ADR-008: Deployment Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need reliable deployment strategy for the SaaS platform MVP

**Decision**: Use single Digital Ocean Droplet with Docker Compose for MVP deployment

**Rationale**:
- **Cost-effective**: Single server deployment for MVP phase
- **Simple Setup**: Docker Compose handles all services
- **Local Storage**: Works well with temporary file storage approach
- **Easy Management**: Single server to monitor and maintain
- **Scalability**: Can migrate to multi-instance later

**Consequences**:
- ✅ Very cost-effective for MVP phase
- ✅ Simple single-server deployment
- ✅ Easy to manage and monitor
- ✅ All services on one machine
- ⚠️ Single point of failure (acceptable for MVP)
- ⚠️ Limited horizontal scaling (can upgrade server size)

**Server Requirements**:
- **Minimum**: 2GB RAM, 1 CPU, 25GB SSD ($12/month)
- **Recommended**: 4GB RAM, 2 CPU, 80GB SSD ($24/month)
- **Services**: PostgreSQL, Redis, BankFlow app, Nginx

**Implementation**:
```dockerfile
# Multi-stage build for Rust backend
FROM rust:1.70 as builder

WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src/ src/

# Build with optimizations
RUN cargo build --release

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/bankflow /usr/local/bin/

# Create upload directory
RUN mkdir -p /app/uploads
ENV UPLOAD_DIR=/app/uploads

EXPOSE 8080
CMD ["bankflow"]
```

**Docker Compose Configuration**:
```yaml
# docker-compose.yml
version: '3.8'
services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL=postgresql://postgres:password@db:5432/bankflow
      - REDIS_URL=redis://redis:6379
      - UPLOAD_DIR=/app/uploads
    volumes:
      - ./uploads:/app/uploads
    depends_on:
      - db
      - redis
    restart: unless-stopped

  db:
    image: postgres:15
    environment:
      POSTGRES_DB: bankflow
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - app
    restart: unless-stopped

volumes:
  postgres_data:
```

**Migration Path**: Can migrate to Digital Ocean App Platform or multiple droplets when scaling beyond single server capacity.

---

## ADR-009: Monitoring Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need comprehensive monitoring for production operations

**Decision**: Implement Prometheus metrics with Grafana dashboards and structured logging

**Rationale**:
- **Observability**: Comprehensive system visibility
- **Alerting**: Proactive issue detection
- **Performance**: Track key performance indicators
- **Debugging**: Detailed logs for troubleshooting
- **Industry Standard**: Widely adopted monitoring stack

**Consequences**:
- ✅ Comprehensive system visibility
- ✅ Proactive issue detection and alerting
- ✅ Performance monitoring and optimization
- ✅ Better debugging capabilities
- ⚠️ Additional infrastructure complexity
- ⚠️ Need for monitoring expertise

**Implementation**:
```rust
use prometheus::{Counter, Histogram, Gauge, register_counter, register_histogram, register_gauge};

lazy_static! {
    static ref UPLOAD_COUNTER: Counter = register_counter!(
        "bankflow_uploads_total", 
        "Total number of file uploads"
    ).unwrap();
    
    static ref PROCESSING_DURATION: Histogram = register_histogram!(
        "bankflow_processing_duration_seconds",
        "Time taken to process files"
    ).unwrap();
    
    static ref ACTIVE_JOBS: Gauge = register_gauge!(
        "bankflow_active_jobs",
        "Number of jobs currently being processed"
    ).unwrap();
}

pub struct MetricsService;

impl MetricsService {
    pub fn record_upload(&self) {
        UPLOAD_COUNTER.inc();
    }
    
    pub fn record_processing_time(&self, duration: Duration) {
        PROCESSING_DURATION.observe(duration.as_secs_f64());
    }
    
    pub fn set_active_jobs(&self, count: i64) {
        ACTIVE_JOBS.set(count as f64);
    }
}
```

---

## ADR-010: Security Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need comprehensive security for financial data processing

**Decision**: Implement multi-layered security with encryption, authentication, and compliance

**Rationale**:
- **Data Protection**: Financial data requires highest security
- **Compliance**: LGPD and PCI DSS requirements
- **Trust**: User trust is critical for SaaS adoption
- **Risk Mitigation**: Comprehensive security reduces business risk

**Consequences**:
- ✅ Strong data protection and compliance
- ✅ User trust and confidence
- ✅ Reduced business risk
- ✅ Competitive advantage
- ⚠️ Additional development and maintenance effort
- ⚠️ Ongoing compliance requirements

**Security Layers**:
1. **Transport Security**: TLS 1.3 encryption
2. **Authentication**: JWT with Argon2 password hashing
3. **Data Encryption**: AES-256-GCM for file storage
4. **Access Control**: Role-based access control
5. **Audit Logging**: Comprehensive activity tracking
6. **Compliance**: LGPD and PCI DSS compliance

---

## ADR-011: Stateless Architecture Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to design architecture for multiple stateless Docker instances

**Decision**: Implement stateless architecture with local file processing and Redis job coordination

**Rationale**:
- **Instance Independence**: Each Docker instance can process jobs independently
- **Simple Deployment**: No shared file storage complexity for MVP
- **Cost-effective**: Lower infrastructure costs during low-load phase
- **Horizontal Scaling**: Easy to add more instances as load increases
- **Development Speed**: Faster implementation without distributed storage

**Consequences**:
- ✅ Simple deployment and scaling
- ✅ Cost-effective for MVP phase
- ✅ Each instance is self-contained
- ✅ Easy horizontal scaling
- ⚠️ Files tied to specific instance (acceptable for MVP)
- ⚠️ Will need shared storage for production scale

**Architecture Flow**:
```
User Upload → Instance A (stores file locally) → Redis Queue
Instance B (picks job) → Processes file locally → Updates DB
User Download → Instance A (serves processed file) → Cleanup
```

**Implementation**:
```rust
pub struct StatelessProcessingService {
    db: PgPool,
    redis: redis::aio::MultiplexedConnection,
    parser_engine: ParserEngine,
    local_storage: LocalFileStorage,
    instance_id: String,
}

impl StatelessProcessingService {
    pub async fn start_worker(&self) {
        loop {
            // Try to claim a job from Redis queue
            match self.claim_job().await {
                Ok(Some(job)) => {
                    // Process job on this instance
                    self.process_job_locally(job).await;
                }
                Ok(None) => {
                    // No jobs available, wait
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
                Err(e) => {
                    eprintln!("Error claiming jobs: {}", e);
                    tokio::time::sleep(Duration::from_secs(5)).await;
                }
            }
        }
    }
    
    async fn claim_job(&self) -> Result<Option<ProcessingJob>, ProcessingError> {
        // Use Redis BLPOP to atomically claim a job
        let result: Option<(String, String)> = self.redis
            .blpop("processing_queue", 1)
            .await?;
            
        match result {
            Some((_, job_data)) => {
                let job: ProcessingJob = serde_json::from_str(&job_data)?;
                Ok(Some(job))
            }
            None => Ok(None),
        }
    }
    
    async fn process_job_locally(&self, job: ProcessingJob) -> Result<(), ProcessingError> {
        // Update status to processing
        self.update_job_status(job.id, ProcessingStatus::Processing).await?;
        
        // Read file from local storage (file must be on this instance)
        let file_content = self.local_storage.read_file(&job.file_id).await?;
        
        // Process file locally
        let parser = self.parser_engine.detect_bank(&file_content).await?;
        let transactions = parser.parse(&file_content).await?;
        
        // Generate output and store locally
        let output_data = self.generate_output(&transactions, OutputFormat::Csv).await?;
        let output_file_id = self.local_storage.store_file(&output_data).await?;
        
        // Update job as completed
        self.complete_job(job.id, output_file_id, transactions.len()).await?;
        
        // Clean up original file
        self.local_storage.delete_file(&job.file_id).await?;
        
        Ok(())
    }
}
```

**Status Updates**:
- **Uploaded**: File stored locally on upload instance
- **Enqueued**: Job added to Redis queue
- **Processing**: Instance claims job and processes locally
- **Processed**: Job completed, result available for download

**Migration Path**: This will be enhanced with shared storage (S3) when scaling beyond MVP phase.

---

## ADR-012: Database Schema Naming Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need proper table naming to prevent future conflicts and maintain clear scope

**Decision**: Use descriptive, scoped table names instead of generic names

**Rationale**:
- **Future-Proofing**: Prevents naming conflicts as system evolves
- **Clear Scope**: Table names clearly indicate their purpose
- **Maintainability**: Easier to understand and maintain
- **System Evolution**: Allows for multiple job types and transaction types

**Consequences**:
- ✅ Clear separation of concerns
- ✅ Future-proof naming prevents conflicts
- ✅ Better code readability and maintainability
- ✅ Easier database administration

**Implementation**:
```sql
-- Before: Generic names
CREATE TABLE jobs (...);
CREATE TABLE transactions (...);

-- After: Scoped names
CREATE TABLE bank_statement_input_job (...);
CREATE TABLE bank_transactions (...);
```

**Table Renaming**:
- `jobs` → `bank_statement_input_job`
- `transactions` → `bank_transactions`
- `category VARCHAR(100)` → `category VARCHAR(255)` (increased length)

---

## ADR-013: Bank Parser Architecture Refactor

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to optimize bank parser performance and eliminate dynamic dispatch overhead

**Decision**: Replace trait-based parser with enum-based parser design

**Rationale**:
- **Performance**: Eliminates `Box<dyn BankParser>` overhead
- **Type Safety**: Compile-time optimization instead of runtime dispatch
- **Memory Efficiency**: No heap allocation for parser instances
- **Maintainability**: Clearer code structure with concrete types

**Consequences**:
- ✅ Better performance (no dynamic dispatch)
- ✅ Type safety at compile time
- ✅ Memory efficiency (no heap allocation)
- ✅ Clearer code structure
- ⚠️ Less flexible than trait-based approach (acceptable for known banks)

**Implementation**:
```rust
// Before: Trait-based with dynamic dispatch
pub trait BankParser {
    fn detect_bank(&self, content: &str) -> f32;
    fn parse_transactions(&self, content: &str) -> Result<Vec<Transaction>, ParseError>;
    fn get_bank_name(&self) -> &str;
}

// After: Enum-based with concrete types
pub enum BankParser {
    Santander(SantanderParser),
    Itau(ItauParser),
    Bradesco(BradescoParser),
    BancoBrasil(BancoBrasilParser),
    Caixa(CaixaParser),
}

impl BankParser {
    pub fn detect_bank(&self, content: &str) -> f32;
    pub fn parse_transactions(&self, content: &str) -> Result<Vec<BankTransaction>, ParseError>;
    pub fn get_bank_name(&self) -> &str;
}
```

---

## ADR-014: API Endpoint Scoping Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need better API organization and security for bank statement operations

**Decision**: Scope API endpoints to bank statement operations with proper security

**Rationale**:
- **Security**: Prevents unauthorized access to other users' files
- **Organization**: Clear API structure and purpose
- **Maintainability**: Easier to understand and extend
- **Future-Proofing**: Allows for other job types without conflicts

**Consequences**:
- ✅ Better security through proper scoping
- ✅ Clearer API organization
- ✅ Easier to maintain and extend
- ✅ Prevents cross-user data access

**Implementation**:
```rust
// Before: Generic endpoints
POST /api/v1/files/upload
GET  /api/v1/files/:id/status
GET  /api/v1/files/:id/download
GET  /api/v1/jobs
GET  /api/v1/jobs/:id

// After: Scoped endpoints
POST /api/v1/files/upload                              # File upload with validation
GET  /api/v1/bank-statements/jobs/:id/download         # Download processed CSV (user-isolated)
GET  /api/v1/bank-statements/jobs                      # List user bank statement jobs
GET  /api/v1/bank-statements/jobs/:id                 # Get specific bank statement job details
GET  /api/v1/bank-statements/jobs/:id/status          # Processing status for bank statement job
```

---

## ADR-015: Anonymous Session Authentication Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need frictionless demo experience without requiring user signup

**Decision**: Implement anonymous session authentication with secure cookies

**Rationale**:
- **User Experience**: No signup friction for demo users
- **Security**: Complete isolation between anonymous sessions
- **Simplicity**: No user management complexity for MVP
- **Conversion**: Higher demo completion rates

**Consequences**:
- ✅ Frictionless demo experience
- ✅ Complete session isolation
- ✅ No user management complexity
- ✅ Higher demo conversion rates
- ⚠️ No persistent user accounts (acceptable for demo)

**Implementation**:
```sql
-- Anonymous sessions table
CREATE TABLE anonymous_sessions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_token VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    expires_at TIMESTAMP DEFAULT (NOW() + INTERVAL '24 hours'),
    last_activity TIMESTAMP DEFAULT NOW()
);

-- Jobs linked to sessions (not users)
CREATE TABLE bank_statement_input_job (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID REFERENCES anonymous_sessions(id) ON DELETE CASCADE,
    -- ... other fields
);
```

**Security Features**:
- **HTTP-Only Cookies**: Secure cookie storage
- **Session Expiry**: 24-hour automatic expiry
- **Complete Isolation**: No cross-session data access
- **Rate Limiting**: 10 uploads per hour per session

---

## ADR-016: File Type Support Simplification

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need to simplify MVP scope to ensure timely delivery

**Decision**: Support CSV files only for MVP, defer PDF and Excel support

**Rationale**:
- **Delivery Timeline**: Ensures 4-week MVP delivery
- **Complexity Reduction**: PDF/Excel parsing is complex and time-consuming
- **Core Value**: CSV processing delivers core value proposition
- **Iterative Development**: Can add other formats in future phases

**Consequences**:
- ✅ Faster MVP delivery
- ✅ Reduced development complexity
- ✅ Focus on core value proposition
- ✅ Higher success probability
- ⚠️ Limited file format support (acceptable for MVP)

**Implementation**:
```rust
// Simplified file validation
pub fn validate_file_type(file: &UploadedFile) -> Result<(), ValidationError> {
    match file.content_type() {
        Some(content_type) if content_type == "text/csv" => Ok(()),
        _ => Err(ValidationError::UnsupportedFileType),
    }
}
```

**Migration Path**: PDF and Excel support will be added in Phase 2 based on user feedback and demand.

---

## ADR-017: CloudFlare SSL Termination Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need simplified SSL management for demo deployment

**Decision**: Use CloudFlare for SSL termination with HTTP backend

**Rationale**:
- **Simplicity**: No SSL certificate management on backend
- **Cost-Effective**: CloudFlare free tier sufficient
- **Security**: Enterprise-grade protection and DDoS mitigation
- **Performance**: Global CDN and caching benefits

**Consequences**:
- ✅ Simplified deployment (no SSL certificates)
- ✅ Enhanced security and performance
- ✅ Cost-effective solution
- ✅ Global CDN benefits
- ✅ Automatic SSL certificate management

**Architecture**:
```
Internet → CloudFlare (HTTPS) → Digital Ocean (HTTP) → BankFlow App
```

**CloudFlare Benefits**:
- **SSL/TLS**: Automatic HTTPS with Let's Encrypt
- **Security**: DDoS protection, WAF, bot protection
- **Performance**: Global CDN, caching, compression
- **Headers**: Security headers automatically applied

---

## ADR-018: GitHub Actions CI/CD Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need automated deployment pipeline for staging and production

**Decision**: Use GitHub Actions for CI/CD with Digital Ocean deployment

**Rationale**:
- **Cost-Effective**: GitHub Actions free tier sufficient
- **Integration**: Seamless GitHub integration
- **Automation**: Automated testing and deployment
- **Simplicity**: Easy setup and maintenance

**Consequences**:
- ✅ Automated testing and deployment
- ✅ Cost-effective CI/CD solution
- ✅ Fast deployment (~5 minutes)
- ✅ Easy rollback capability
- ✅ Secure secrets management

**Implementation**:
```yaml
name: Deploy to Digital Ocean
on:
  push:
    branches: [staging, main]

jobs:
  test-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run Tests
        run: cargo test
      - name: Build Docker Image
        run: docker build -t bankflow:${{ github.sha }} .
      - name: Deploy to Staging/Production
        # Conditional deployment based on branch
```

---

## ADR-019: Staging and Production Environment Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need safe testing environment before production releases

**Decision**: Implement staging and production environments with subdomain routing

**Rationale**:
- **Safe Testing**: Test changes before production
- **Quality Assurance**: Validate features in staging first
- **Risk Mitigation**: Reduce production deployment risks
- **Professional Development**: Industry-standard practices

**Consequences**:
- ✅ Safe testing environment
- ✅ Reduced production risks
- ✅ Professional development practices
- ✅ Better quality assurance
- ✅ Easy rollback capability

**Environment Setup**:
- **Staging**: `staging.bankflow.com` (staging branch)
- **Production**: `bankflow.com` (main branch)
- **Deployment Flow**: staging → main → production

**Development Workflow**:
1. Feature Development → Feature branches
2. Staging Deploy → Merge to staging branch
3. Testing → Validate on staging environment
4. Production Release → Merge staging → main
5. Monitoring → Monitor both environments

---

## ADR-020: Shared Database Architecture Strategy

**Date**: December 2024  
**Status**: Accepted  
**Context**: Need cost-effective database solution for staging and production

**Decision**: Use shared PostgreSQL and Redis instances with isolated databases/namespaces

**Rationale**:
- **Cost Efficiency**: Single infrastructure for both environments
- **Resource Optimization**: Better utilization of server resources
- **Simplified Management**: One set of services to maintain
- **Complete Isolation**: Separate databases and Redis namespaces

**Consequences**:
- ✅ Cost-effective solution
- ✅ Resource optimization
- ✅ Simplified management
- ✅ Complete environment isolation
- ✅ Single backup strategy

**Implementation**:
```sql
-- Shared PostgreSQL with separate databases
CREATE DATABASE bankflow_staging;
CREATE DATABASE bankflow_production;
```

```redis
# Shared Redis with namespace isolation
SET staging:session:abc123 "session_data"
SET production:session:def456 "session_data"
```

**Environment Configuration**:
- **Staging**: `DATABASE_URL="postgresql://user:pass@localhost:5432/bankflow_staging"`
- **Production**: `DATABASE_URL="postgresql://user:pass@localhost:5432/bankflow_production"`
- **Redis**: `REDIS_PREFIX="staging:"` or `REDIS_PREFIX="production:"`

---

## Decision Review Process

### Review Schedule
- **Monthly**: Review all decisions for relevance
- **Quarterly**: Assess decision impact on project goals
- **Annually**: Comprehensive decision audit

### Review Criteria
- **Relevance**: Is the decision still applicable?
- **Impact**: Has the decision achieved expected outcomes?
- **Alternatives**: Are there better alternatives now available?
- **Documentation**: Is the decision properly documented?

### Decision Updates
- **Status Changes**: Update decision status as needed
- **New Decisions**: Add new decisions as they're made
- **Superseded Decisions**: Mark old decisions as superseded
- **Lessons Learned**: Document lessons from decision outcomes
