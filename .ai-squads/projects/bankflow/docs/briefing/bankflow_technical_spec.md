# BankFlow - Technical Architecture & Implementation Guide
*Para: Backend Engineer & UI Implementer*

## 🏗️ **System Architecture Overview**

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
                    │   PostgreSQL     │    │   File Storage  │
                    │   (Primary DB)   │    │   (S3/MinIO)    │
                    └──────────────────┘    └─────────────────┘
                              │
                              ▼
                    ┌──────────────────┐    ┌─────────────────┐
                    │   Redis          │    │   ClickHouse    │
                    │   (Cache/Queue)  │    │   (Analytics)   │
                    └──────────────────┘    └─────────────────┘
```

### Tech Stack Rationale

**Frontend: HTMX + Alpine.js**
- ✅ **Simplicity**: Minimal JavaScript, server-driven
- ✅ **Performance**: Fast page loads, minimal bundle size
- ✅ **SEO-friendly**: Server-side rendering
- ✅ **Maintenance**: Less complexity than SPA frameworks

**Backend: Rust + Axum**
- ✅ **Performance**: High-throughput file processing
- ✅ **Memory safety**: No crashes from memory issues  
- ✅ **Concurrency**: Excellent async/await support
- ✅ **Ecosystem**: Rich CSV/Excel parsing libraries

**Database: PostgreSQL**
- ✅ **ACID compliance**: Financial data requires consistency
- ✅ **JSON support**: Flexible schema for different bank formats
- ✅ **Full-text search**: Search through processed extracts
- ✅ **Mature ecosystem**: Robust tooling and monitoring

---

## 🎯 **Core Domain Models**

### User & Authentication
```rust
#[derive(Serialize, Deserialize, sqlx::FromRow)]
pub struct User {
    pub id: Uuid,
    pub email: String,
    pub password_hash: String,
    pub name: String,
    pub company: Option<String>,
    pub subscription_tier: SubscriptionTier,
    pub created_at: DateTime<Utc>,
    pub last_login: Option<DateTime<Utc>>,
    pub is_active: bool,
}

#[derive(Serialize, Deserialize, sqlx::Type)]
#[sqlx(type_name = "subscription_tier", rename_all = "lowercase")]
pub enum SubscriptionTier {
    Free,
    Professional,
    Business,
    Enterprise,
}
```

### File Processing
```rust
#[derive(Serialize, Deserialize, sqlx::FromRow)]
pub struct ProcessingJob {
    pub id: Uuid,
    pub user_id: Uuid,
    pub original_filename: String,
    pub file_size: i64,
    pub file_hash: String,
    pub detected_bank: Option<String>,
    pub status: ProcessingStatus,
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub error_message: Option<String>,
    pub input_file_path: String,
    pub output_file_path: Option<String>,
    pub transaction_count: Option<i32>,
}

#[derive(Serialize, Deserialize, sqlx::Type)]
#[sqlx(type_name = "processing_status", rename_all = "lowercase")]
pub enum ProcessingStatus {
    Queued,
    Processing,
    Completed,
    Failed,
    Cancelled,
}
```

### Bank Data Models
```rust
#[derive(Serialize, Deserialize)]
pub struct NormalizedTransaction {
    pub date: NaiveDate,
    pub description: String,
    pub amount: Decimal,
    pub balance: Option<Decimal>,
    pub transaction_type: TransactionType,
    pub category: Option<String>,
    pub reference: Option<String>,
}

#[derive(Serialize, Deserialize, sqlx::Type)]
#[sqlx(type_name = "transaction_type", rename_all = "lowercase")]
pub enum TransactionType {
    Credit,
    Debit,
}

#[derive(Serialize, Deserialize)]
pub struct BankInfo {
    pub name: String,
    pub code: String,
    pub supported_formats: Vec<String>,
    pub parser_version: String,
    pub confidence_score: f32,
}
```

---

## 🔧 **Core Services Architecture**

### File Upload Service
```rust
use axum::{extract::Multipart, http::StatusCode, Json};
use tokio_util::io::ReaderStream;

pub struct FileUploadService {
    storage: Arc<dyn FileStorage>,
    validator: FileValidator,
}

impl FileUploadService {
    pub async fn handle_upload(
        &self,
        user_id: Uuid,
        multipart: Multipart,
    ) -> Result<UploadResponse, ApiError> {
        // 1. Validate file (size, type, virus scan)
        let file_info = self.validator.validate_upload(&multipart).await?;
        
        // 2. Generate unique file path
        let file_path = self.generate_file_path(&user_id, &file_info);
        
        // 3. Stream to storage (S3/MinIO)
        let upload_result = self.storage.store_file(file_path, multipart).await?;
        
        // 4. Create processing job
        let job = ProcessingJob::new(user_id, upload_result);
        let job_id = self.queue_processing_job(job).await?;
        
        Ok(UploadResponse {
            job_id,
            estimated_processing_time: file_info.estimated_processing_time(),
        })
    }
}

// File validation rules
pub struct FileValidator;

impl FileValidator {
    pub async fn validate_upload(&self, multipart: &Multipart) -> Result<FileInfo, ValidationError> {
        // Max file size: 50MB
        if file.size > 50 * 1024 * 1024 {
            return Err(ValidationError::FileTooLarge);
        }
        
        // Allowed formats: PDF, CSV, XLS, XLSX
        match file.content_type {
            "application/pdf" | "text/csv" | 
            "application/vnd.ms-excel" | 
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" => {},
            _ => return Err(ValidationError::UnsupportedFormat),
        }
        
        // Basic virus scan (ClamAV integration)
        self.scan_for_viruses(&file).await?;
        
        Ok(FileInfo::from_multipart_file(file))
    }
}
```

### Bank Parser Engine
```rust
use async_trait::async_trait;

#[async_trait]
pub trait BankParser: Send + Sync {
    async fn can_parse(&self, file_content: &[u8]) -> Result<f32, ParseError>; // confidence 0-1
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

// Example: Itaú Parser
pub struct ItauParser;

#[async_trait]
impl BankParser for ItauParser {
    async fn can_parse(&self, content: &[u8]) -> Result<f32, ParseError> {
        let text = String::from_utf8_lossy(content);
        
        let indicators = [
            "BANCO ITAÚ",
            "341", // Banco code
            "Ag:",
            "C/C:",
        ];
        
        let matches = indicators.iter()
            .filter(|&indicator| text.contains(indicator))
            .count();
            
        Ok(matches as f32 / indicators.len() as f32)
    }
    
    async fn parse(&self, content: &[u8]) -> Result<Vec<NormalizedTransaction>, ParseError> {
        // Implement Itaú-specific parsing logic
        // Handle PDF extraction, CSV parsing, date formats, etc.
        todo!()
    }
    
    fn bank_info(&self) -> BankInfo {
        BankInfo {
            name: "Banco Itaú".to_string(),
            code: "341".to_string(),
            supported_formats: vec!["PDF".to_string(), "CSV".to_string()],
            parser_version: "1.0.0".to_string(),
            confidence_score: 0.95,
        }
    }
}
```

### Processing Queue System
```rust
use tokio::task;
use sqlx::PgPool;

pub struct ProcessingService {
    db: PgPool,
    storage: Arc<dyn FileStorage>,
    parser_engine: ParserEngine,
    notification_service: NotificationService,
}

impl ProcessingService {
    pub async fn start_worker(&self) {
        loop {
            match self.get_next_job().await {
                Ok(Some(job)) => {
                    let service = self.clone();
                    task::spawn(async move {
                        service.process_job(job).await;
                    });
                }
                Ok(None) => {
                    // No jobs, sleep briefly
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
        
        // Download file from storage
        let file_content = self.storage.read_file(&job.input_file_path).await?;
        
        // Detect bank and parse
        let parser = self.parser_engine.detect_bank(&file_content).await?;
        let transactions = parser.parse(&file_content).await?;
        
        // Generate output in requested format(s)
        let output_data = self.generate_output(&transactions, OutputFormat::Csv).await?;
        
        // Save to storage
        let output_path = self.storage.store_processed_file(job.id, output_data).await?;
        
        // Update job as completed
        self.complete_job(job.id, output_path, transactions.len()).await?;
        
        // Send notification
        self.notification_service.notify_completion(job.user_id, job.id).await?;
        
        Ok(())
    }
}
```

---

## 🔐 **Security & Data Protection**

### Encryption Strategy
```rust
use aes_gcm::{Aes256Gcm, Key, Nonce};
use argon2::{Argon2, PasswordHash, PasswordHasher, PasswordVerifier};

pub struct SecurityService {
    encryption_key: Key<Aes256Gcm>,
    argon2: Argon2<'static>,
}

impl SecurityService {
    // Encrypt sensitive file data
    pub fn encrypt_file_data(&self, data: &[u8]) -> Result<Vec<u8>, SecurityError> {
        let cipher = Aes256Gcm::new(&self.encryption_key);
        let nonce = Nonce::from_slice(b"unique nonce"); // Generate random nonce
        
        cipher.encrypt(nonce, data)
            .map_err(|_| SecurityError::EncryptionFailed)
    }
    
    // Password hashing for user authentication
    pub fn hash_password(&self, password: &str) -> Result<String, SecurityError> {
        let salt = SaltString::generate(&mut OsRng);
        let password_hash = self.argon2.hash_password(password.as_bytes(), &salt)?;
        Ok(password_hash.to_string())
    }
}
```

### LGPD Compliance
```rust
// Data retention policies
pub struct DataRetentionService {
    db: PgPool,
}

impl DataRetentionService {
    // Delete user data after account deletion
    pub async fn purge_user_data(&self, user_id: Uuid) -> Result<(), DataError> {
        let mut tx = self.db.begin().await?;
        
        // Delete processed files
        sqlx::query!("DELETE FROM processing_jobs WHERE user_id = $1", user_id)
            .execute(&mut *tx).await?;
            
        // Delete file storage
        self.delete_user_files(user_id).await?;
        
        // Anonymize analytics data
        sqlx::query!("UPDATE analytics_events SET user_id = NULL WHERE user_id = $1", user_id)
            .execute(&mut *tx).await?;
            
        tx.commit().await?;
        Ok(())
    }
    
    // Auto-delete files after retention period
    pub async fn cleanup_expired_files(&self) -> Result<(), DataError> {
        let cutoff_date = Utc::now() - Duration::days(90); // 90-day retention
        
        let expired_jobs = sqlx::query_as!(
            ProcessingJob,
            "SELECT * FROM processing_jobs WHERE created_at < $1",
            cutoff_date
        ).fetch_all(&self.db).await?;
        
        for job in expired_jobs {
            self.delete_job_files(job.id).await?;
        }
        
        Ok(())
    }
}
```

---

## 🚀 **Performance Optimization**

### Database Optimization
```sql
-- Indexes for common queries
CREATE INDEX CONCURRENTLY idx_processing_jobs_user_status 
ON processing_jobs(user_id, status);

CREATE INDEX CONCURRENTLY idx_processing_jobs_created_at 
ON processing_jobs(created_at);

-- Partitioning for large tables
CREATE TABLE analytics_events (
    id UUID DEFAULT gen_random_uuid(),
    user_id UUID,
    event_type VARCHAR(50),
    event_data JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
) PARTITION BY RANGE (created_at);

-- Monthly partitions
CREATE TABLE analytics_events_2024_01 PARTITION OF analytics_events
FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');
```

### Caching Strategy
```rust
use redis::AsyncCommands;

pub struct CacheService {
    redis: redis::aio::MultiplexedConnection,
}

impl CacheService {
    // Cache frequently accessed data
    pub async fn cache_user_limits(&self, user_id: Uuid, limits: &UserLimits) -> Result<(), CacheError> {
        let key = format!("user_limits:{}", user_id);
        let value = serde_json::to_string(limits)?;
        
        self.redis.setex(key, 3600, value).await?; // 1 hour TTL
        Ok(())
    }
    
    // Cache parsed bank formats for performance
    pub async fn cache_parser_result(&self, file_hash: &str, result: &ParseResult) -> Result<(), CacheError> {
        let key = format!("parse_result:{}", file_hash);
        let value = serde_json::to_string(result)?;
        
        self.redis.setex(key, 86400, value).await?; // 24 hour TTL
        Ok(())
    }
}
```

### File Processing Optimization
```rust
use tokio::io::{AsyncReadExt, BufReader};
use tokio_stream::StreamExt;

impl ProcessingService {
    // Stream large files instead of loading into memory
    pub async fn process_large_file(&self, file_path: &str) -> Result<Vec<NormalizedTransaction>, ProcessingError> {
        let file = tokio::fs::File::open(file_path).await?;
        let reader = BufReader::new(file);
        let mut lines = reader.lines();
        
        let mut transactions = Vec::new();
        
        while let Some(line) = lines.next().await {
            let line = line?;
            
            // Process line incrementally
            if let Some(transaction) = self.parse_transaction_line(&line).await? {
                transactions.push(transaction);
                
                // Yield control every 1000 transactions to prevent blocking
                if transactions.len() % 1000 == 0 {
                    tokio::task::yield_now().await;
                }
            }
        }
        
        Ok(transactions)
    }
    
    // Parallel processing for multiple files
    pub async fn process_batch(&self, jobs: Vec<ProcessingJob>) -> Result<(), ProcessingError> {
        let futures = jobs.into_iter().map(|job| {
            let service = self.clone();
            tokio::spawn(async move {
                service.process_job(job).await
            })
        });
        
        // Process up to 5 files concurrently
        let results = futures::stream::iter(futures)
            .buffer_unordered(5)
            .collect::<Vec<_>>()
            .await;
            
        // Handle any errors
        for result in results {
            result??; // Unwrap JoinHandle and ProcessingError
        }
        
        Ok(())
    }
}
```

---

## 📊 **Monitoring & Observability**

### Application Metrics
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

### Structured Logging
```rust
use tracing::{info, warn, error, instrument};
use serde_json::json;

#[instrument(skip(self, file_content))]
pub async fn process_file(&self, job_id: Uuid, file_content: &[u8]) -> Result<(), ProcessingError> {
    info!(
        job_id = %job_id,
        file_size = file_content.len(),
        "Starting file processing"
    );
    
    match self.parse_file(file_content).await {
        Ok(transactions) => {
            info!(
                job_id = %job_id,
                transaction_count = transactions.len(),
                "File processed successfully"
            );
            Ok(())
        }
        Err(e) => {
            error!(
                job_id = %job_id,
                error = %e,
                "File processing failed"
            );
            Err(e)
        }
    }
}
```

### Health Checks
```rust
use axum::{http::StatusCode, Json};
use serde_json::json;

pub async fn health_check(
    State(app_state): State<AppState>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    let mut status = "healthy";
    let mut checks = Vec::new();
    
    // Database connectivity
    match sqlx::query("SELECT 1").fetch_one(&app_state.db).await {
        Ok(_) => checks.push(json!({"name": "database", "status": "ok"})),
        Err(_) => {
            checks.push(json!({"name": "database", "status": "error"}));
            status = "unhealthy";
        }
    }
    
    // Redis connectivity
    match app_state.redis.ping().await {
        Ok(_) => checks.push(json!({"name": "redis", "status": "ok"})),
        Err(_) => {
            checks.push(json!({"name": "redis", "status": "error"}));
            status = "unhealthy";
        }
    }
    
    // File storage
    match app_state.storage.health_check().await {
        Ok(_) => checks.push(json!({"name": "storage", "status": "ok"})),
        Err(_) => {
            checks.push(json!({"name": "storage", "status": "error"}));
            status = "unhealthy";
        }
    }
    
    let response = json!({
        "status": status,
        "checks": checks,
        "timestamp": chrono::Utc::now().to_rfc3339()
    });
    
    match status {
        "healthy" => Ok(Json(response)),
        _ => Err(StatusCode::SERVICE_UNAVAILABLE),
    }
}
```

---

## 🌐 **API Design**

### RESTful Endpoints
```rust
use axum::{
    extract::{Path, Query, State, Multipart},
    http::StatusCode,
    Json, Router,
    routing::{get, post, delete},
};

pub fn create_router(state: AppState) -> Router {
    Router::new()
        .route("/api/v1/auth/login", post(auth::login))
        .route("/api/v1/auth/logout", post(auth::logout))
        .route("/api/v1/users/me", get(users::get_profile))
        .route("/api/v1/files/upload", post(files::upload))
        .route("/api/v1/files/:id/status", get(files::get_status))
        .route("/api/v1/files/:id/download", get(files::download))
        .route("/api/v1/jobs", get(jobs::list_jobs))
        .route("/api/v1/jobs/:id", get(jobs::get_job))
        .route("/api/v1/jobs/:id", delete(jobs::cancel_job))
        .route("/api/v1/health", get(health_check))
        .with_state(state)
}

// File upload endpoint
pub async fn upload(
    State(state): State<AppState>,
    current_user: CurrentUser,
    multipart: Multipart,
) -> Result<Json<UploadResponse>, ApiError> {
    // Rate limiting check
    state.rate_limiter.check_upload_limit(current_user.id).await?;
    
    // Upload and queue processing
    let response = state.file_service.handle_upload(current_user.id, multipart).await?;
    
    Ok(Json(response))
}

// Job status endpoint with Server-Sent Events
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
        axum::response::sse::KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive-text"),
    )
}
```

### Rate Limiting
```rust
use governor::{Quota, RateLimiter, state::{InMemoryState, NotKeyed}};
use std::num::NonZeroU32;

pub struct RateLimitService {
    upload_limiter: RateLimiter<NotKeyed, InMemoryState, QuantaClock>,
}

impl RateLimitService {
    pub fn new() -> Self {
        // 10 uploads per minute per user
        let quota = Quota::per_minute(NonZeroU32::new(10).unwrap());
        let upload_limiter = RateLimiter::direct(quota);
        
        Self { upload_limiter }
    }
    
    pub async fn check_upload_limit(&self, user_id: Uuid) -> Result<(), RateLimitError> {
        match self.upload_limiter.check() {
            Ok(_) => Ok(()),
            Err(_) => Err(RateLimitError::TooManyRequests),
        }
    }
}
```

---

## 🎨 **Frontend Implementation (HTMX + Alpine.js)**

### File Upload Component
```html
<!-- File Upload with Progress -->
<div 
    x-data="fileUpload()" 
    class="upload-container"
>
    <div 
        x-show="!uploading"
        @drop.prevent="handleDrop($event)"
        @dragover.prevent
        @dragenter.prevent
        class="dropzone"
        :class="{ 'dragover': isDragOver }"
    >
        <input 
            type="file" 
            multiple 
            accept=".pdf,.csv,.xls,.xlsx"
            @change="handleFileSelect($event)"
            class="file-input"
            id="fileInput"
        >
        <label for="fileInput" class="upload-label">
            <svg class="upload-icon"><!-- Upload icon --></svg>
            <span>Arraste arquivos aqui ou clique para selecionar</span>
            <small>PDF, CSV, XLS, XLSX até 50MB</small>
        </label>
    </div>
    
    <!-- Upload Progress -->
    <div x-show="uploading" class="progress-container">
        <div class="progress-bar">
            <div 
                class="progress-fill" 
                :style="`width: ${uploadProgress}%`"
            ></div>
        </div>
        <p x-text="`Enviando... ${uploadProgress}%`"></p>
    </div>
    
    <!-- File List -->
    <div x-show="files.length > 0" class="file-list">
        <template x-for="file in files" :key="file.id">
            <div class="file-item" :class="file.status">
                <span x-text="file.name"></span>
                <span class="file-status" x-text="file.statusText"></span>
                <button 
                    x-show="file.status === 'completed'"
                    @click="downloadFile(file.id)"
                    class="download-btn"
                >
                    Download
                </button>
            </div>
        </template>
    </div>
</div>

<script>
function fileUpload() {
    return {
        files: [],
        uploading: false,
        uploadProgress: 0,
        isDragOver: false,
        
        async handleFileSelect(event) {
            const selectedFiles = Array.from(event.target.files);
            await this.uploadFiles(selectedFiles);
        },
        
        async handleDrop(event) {
            this.isDragOver = false;
            const droppedFiles = Array.from(event.dataTransfer.files);
            await this.uploadFiles(droppedFiles);
        },
        
        async uploadFiles(fileList) {
            this.uploading = true;
            this.uploadProgress = 0;
            
            for (const file of fileList) {
                const fileObj = {
                    id: Date.now() + Math.random(),
                    name: file.name,
                    status: 'uploading',
                    statusText: 'Enviando...'
                };
                
                this.files.push(fileObj);
                
                try {
                    const jobId = await this.uploadSingleFile(file);
                    fileObj.jobId = jobId;
                    fileObj.status = 'processing';
                    fileObj.statusText = 'Processando...';
                    
                    // Start polling for status
                    this.pollJobStatus(jobId, fileObj);
                    
                } catch (error) {
                    fileObj.status = 'error';
                    fileObj.statusText = 'Erro no upload';
                }
            }
            
            this.uploading = false;
        },
        
        async uploadSingleFile(file) {
            const formData = new FormData();
            formData.append('file', file);
            
            const response = await fetch('/api/v1/files/upload', {
                method: 'POST',
                body: formData,
                headers: {
                    'X-Requested-With': 'XMLHttpRequest'
                }
            });
            
            if (!response.ok) {
                throw new Error('Upload failed');
            }
            
            const result = await response.json();
            return result.job_id;
        },
        
        async pollJobStatus(jobId, fileObj) {
            const eventSource = new EventSource(`/api/v1/jobs/${jobId}/status`);
            
            eventSource.onmessage = (event) => {
                const status = JSON.parse(event.data);
                
                if (status.status === 'completed') {
                    fileObj.status = 'completed';
                    fileObj.statusText = `✓ ${status.transaction_count} transações`;
                    eventSource.close();
                } else if (status.status === 'failed') {
                    fileObj.status = 'error';
                    fileObj.statusText = 'Erro no processamento';
                    eventSource.close();
                } else {
                    fileObj.statusText = 'Processando...';
                }
            };
            
            eventSource.onerror = () => {
                eventSource.close();
            };
        },
        
        async downloadFile(jobId) {
            window.open(`/api/v1/files/${jobId}/download`, '_blank');
        }
    }
}
</script>
```

### Dashboard with HTMX
```html
<!-- Main Dashboard -->
<div 
    hx-get="/dashboard/stats" 
    hx-trigger="load, every 30s"
    hx-target="#stats-container"
>
    <div id="stats-container">
        <!-- Will be populated by HTMX -->
    </div>
</div>

<!-- Recent Jobs Table -->
<div class="jobs-table">
    <div 
        hx-get="/dashboard/jobs"
        hx-trigger="load, every 10s"
        hx-target="#jobs-table-body"
    >
        <table>
            <thead>
                <tr>
                    <th>Arquivo</th>
                    <th>Banco</th>
                    <th>Status</th>
                    <th>Transações</th>
                    <th>Data</th>
                    <th>Ações</th>
                </tr>
            </thead>
            <tbody id="jobs-table-body">
                <!-- Populated by HTMX -->
            </tbody>
        </table>
    </div>
</div>

<!-- Pagination -->
<div 
    class="pagination"
    hx-target="#jobs-table-body"
    hx-swap="innerHTML"
>
    <button 
        hx-get="/dashboard/jobs?page=1"
        class="btn btn-outline"
    >
        Primeira
    </button>
    <button 
        hx-get="/dashboard/jobs?page={{ current_page - 1 }}"
        class="btn btn-outline"
        {{ if current_page <= 1 }}disabled{{ end }}
    >
        Anterior
    </button>
    <span>Página {{ current_page }} de {{ total_pages }}</span>
    <button 
        hx-get="/dashboard/jobs?page={{ current_page + 1 }}"
        class="btn btn-outline"
        {{ if current_page >= total_pages }}disabled{{ end }}
    >
        Próxima
    </button>
</div>
```

---

## 🚀 **Deployment & Infrastructure**

### Docker Configuration
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

EXPOSE 8080
CMD ["bankflow"]
```

### Docker Compose (Development)
```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL=postgresql://postgres:password@db:5432/bankflow
      - REDIS_URL=redis://redis:6379
      - S3_ENDPOINT=http://minio:9000
    depends_on:
      - db
      - redis
      - minio
    volumes:
      - ./uploads:/app/uploads

  db:
    image: postgres:15
    environment:
      POSTGRES_DB: bankflow
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: password
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./migrations:/docker-entrypoint-initdb.d
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

  minio:
    image: minio/minio
    ports:
      - "9000:9000"
      - "9001:9001"
    environment:
      MINIO_ROOT_USER: minioadmin
      MINIO_ROOT_PASSWORD: minioadmin
    command: server /data --console-address ":9001"
    volumes:
      - minio_data:/data

volumes:
  postgres_data:
  minio_data:
```

### Production Configuration (Railway/Fly.io)
```toml
# fly.toml
app = "bankflow"
primary_region = "gru" # São Paulo

[build]
  dockerfile = "Dockerfile"

[env]
  PORT = "8080"

[[services]]
  http_checks = []
  internal_port = 8080
  processes = ["app"]
  protocol = "tcp"
  script_checks = []

  [services.concurrency]
    hard_limit = 25
    soft_limit = 20
    type = "connections"

  [[services.ports]]
    force_https = true
    handlers = ["http"]
    port = 80

  [[services.ports]]
    handlers = ["tls", "http"]
    port = 443

  [[services.tcp_checks]]
    grace_period = "1s"
    interval = "15s"
    restart_limit = 0
    timeout = "2s"

[mounts]
  source = "uploads"
  destination = "/app/uploads"
```

---

## 📋 **Development Workflow**

### Project Structure
```
bankflow/
├── Cargo.toml
├── .env.example
├── migrations/
│   └── 001_initial_schema.sql
├── src/
│   ├── main.rs
│   ├── config.rs
│   ├── models/
│   │   ├── mod.rs
│   │   ├── user.rs
│   │   └── job.rs
│   ├── services/
│   │   ├── mod.rs
│   │   ├── auth.rs
│   │   ├── upload.rs
│   │   └── processing.rs
│   ├── parsers/
│   │   ├── mod.rs
│   │   ├── itau.rs
│   │   ├── bradesco.rs
│   │   └── traits.rs
│   ├── api/
│   │   ├── mod.rs
│   │   ├── auth.rs
│   │   ├── files.rs
│   │   └── jobs.rs
│   └── utils/
│       ├── mod.rs
│       ├── crypto.rs
│       └── validation.rs
├── templates/
│   ├── base.html
│   ├── dashboard.html
│   └── components/
├── static/
│   ├── css/
│   ├── js/
│   └── images/
├── tests/
│   ├── integration/
│   └── unit/
└── docs/
    ├── api.md
    └── deployment.md
```

### Testing Strategy
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use sqlx::PgPool;
    use tempfile::TempDir;

    // Integration test
    #[sqlx::test]
    async fn test_file_upload_and_processing(pool: PgPool) {
        let app_state = AppState::new_test(pool).await;
        
        // Create test file
        let test_file = create_test_csv();
        
        // Upload file
        let upload_response = app_state.file_service
            .handle_upload(test_user_id(), test_file)
            .await
            .unwrap();
            
        // Wait for processing
        let job = wait_for_job_completion(upload_response.job_id).await;
        
        assert_eq!(job.status, ProcessingStatus::Completed);
        assert!(job.transaction_count.unwrap() > 0);
    }
    
    // Unit test for parser
    #[test]
    fn test_itau_parser_detection() {
        let parser = ItauParser::new();
        let test_content = include_bytes!("../test_data/itau_sample.pdf");
        
        let confidence = parser.can_parse(test_content).unwrap();
        assert!(confidence > 0.8);
    }
}
```

### CI/CD Pipeline (.github/workflows/ci.yml)
```yaml
name: CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: postgres
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
        
    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: |
          ~/.cargo/registry
          ~/.cargo/git
          target
        key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
        
    - name: Run tests
      run: cargo test
      env:
        DATABASE_URL: postgresql://postgres:postgres@localhost:5432/test
        
    - name: Run clippy
      run: cargo clippy -- -D warnings
      
    - name: Check formatting
      run: cargo fmt --check

  deploy:
    if: github.ref == 'refs/heads/main'
    needs: test
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Deploy to production
      run: |
        # Deploy to Railway/Fly.io
        echo "Deploying to production..."
```

---

## 🎯 **MVP Implementation Timeline**

### Week 1: Core Infrastructure
- [ ] Project setup and configuration
- [ ] Database schema and migrations
- [ ] Basic file upload endpoint
- [ ] User authentication system
- [ ] Docker development environment

### Week 2: Processing Engine
- [ ] File storage service (local/S3)
- [ ] Basic CSV parser for 3 banks
- [ ] Job queue system with Redis
- [ ] Processing status tracking
- [ ] Error handling and logging

### Week 3: Frontend & UX
- [ ] HTMX-based dashboard
- [ ] File upload with progress
- [ ] Job status real-time updates
- [ ] Download processed files
- [ ] Responsive design

### Week 4: Polish & Deploy
- [ ] Integration testing
- [ ] Performance optimization
- [ ] Security review
- [ ] Production deployment
- [ ] Monitoring setup

---

**Remember**: Start simple, measure everything, optimize based on real usage patterns. The goal is to have working software in users' hands as quickly as possible, then iterate based on feedback.**