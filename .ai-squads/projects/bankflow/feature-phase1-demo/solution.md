---
description: Feature Solution - Phase 1 Demo Flow
type: feature-solution
status: planned
priority: high
---

# Solution: Phase 1 Demo Flow Implementation

## Solution Overview

**How will we solve it?**
We will build a complete demo flow that validates our core value proposition through a web-based application that automatically processes Brazilian bank statements. The solution includes a professional landing page, intuitive file upload interface, real-time processing with status updates, and secure download of processed CSV files.

## Solution Design

### High-Level Architecture
The solution follows a microservices architecture with four main components:
- **Site Crate**: Landing page and demo interface (HTMX + Alpine.js)
- **API Crate**: Core processing logic and endpoints (Rust + Axum)
- **Service Crate**: Background processing and job queue (Redis-based)
- **Infrastructure**: PostgreSQL database and Redis cache

### Core Components
- **Landing Page**: Professional marketing page with clear value proposition
- **File Upload System**: Drag-and-drop interface with validation and progress tracking
- **Bank Parser Engine**: Automated detection and parsing of Brazilian bank formats
- **Processing Queue**: Background job processing with Redis
- **Real-time Updates**: Server-sent events for processing status
- **Download System**: Secure CSV generation and file delivery

### Data Flow
1. **User visits landing page** and clicks "Try Demo"
2. **Anonymous session created** automatically with secure cookie (no signup required)
3. **User uploads bank statement** file via drag-and-drop interface
4. **File validated and stored** securely with unique identifier linked to session
5. **Processing job queued** in Redis with job ID and session ID
6. **Background worker** detects bank format and parses transactions
7. **Real-time updates** sent to user via Server-Sent Events
8. **Processed CSV generated** and made available for download (session-isolated)
9. **Files cleaned up** after download or session expiry (24 hours)

## Technical Approach

### Technology Stack
- **Backend**: Rust with Axum web framework
- **Frontend**: HTMX + Alpine.js for dynamic interactions
- **Database**: PostgreSQL for data persistence
- **Cache/Queue**: Redis for job processing and caching
- **Templates**: Askama for server-side rendering
- **Deployment**: Docker containers on Digital Ocean

### Architecture Patterns
- **Microservices**: Separate crates for different concerns
- **Event-Driven**: Redis-based job queue for processing
- **Real-time**: Server-Sent Events for status updates
- **Stateless**: API services are stateless and scalable
- **Secure**: File handling with proper validation and cleanup

## Implementation Strategy

### Phase 1A: Core Infrastructure (Week 1)
**Focus**: Establish technical foundation
- Database schema design and migrations
- Anonymous session system with secure cookies (no signup required)
- File upload service with validation
- Quality gates and testing framework

### Phase 1B: Processing Engine (Week 2)
**Focus**: Build core value-delivering engine
- Bank detection algorithm with confidence scoring
- Parser implementations for 5 major Brazilian banks
- Redis-based processing queue with background workers
- Error handling and retry logic

### Phase 1C: User Experience (Week 3)
**Focus**: Deliver seamless user experience
- Professional landing page with value proposition
- Intuitive drag-and-drop upload interface
- Real-time status updates via Server-Sent Events
- Mobile-responsive design and accessibility

### Phase 1D: Production Deployment (Week 4)
**Focus**: Production-ready MVP
- Security audit and performance optimization
- GitHub Actions CI/CD pipeline setup
- Digital Ocean deployment automation
- Monitoring and alerting setup
- Final testing and quality assurance

## Detailed Technical Specifications

### Database Schema
```sql
-- Anonymous sessions table (for demo users)
CREATE TABLE anonymous_sessions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_token VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    expires_at TIMESTAMP DEFAULT (NOW() + INTERVAL '24 hours'),
    last_activity TIMESTAMP DEFAULT NOW()
);

-- Bank statement input jobs table (processing status)
CREATE TABLE bank_statement_input_job (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID REFERENCES anonymous_sessions(id) ON DELETE CASCADE,
    status VARCHAR(50) NOT NULL DEFAULT 'pending',
    file_path VARCHAR(500) NOT NULL,
    bank_detected VARCHAR(100),
    confidence_score DECIMAL(5,2),
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Bank transactions table (parsed data)
CREATE TABLE bank_transactions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    job_id UUID REFERENCES bank_statement_input_job(id) ON DELETE CASCADE,
    date DATE NOT NULL,
    description TEXT NOT NULL,
    amount DECIMAL(15,2) NOT NULL,
    balance DECIMAL(15,2),
    category VARCHAR(255),
    created_at TIMESTAMP DEFAULT NOW()
);

-- File metadata table
CREATE TABLE file_metadata (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    job_id UUID REFERENCES bank_statement_input_job(id) ON DELETE CASCADE,
    original_filename VARCHAR(255) NOT NULL,
    file_size BIGINT NOT NULL,
    mime_type VARCHAR(100) NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);
```

### API Endpoints
```
POST /api/v1/files/upload                              # File upload with validation
GET  /api/v1/bank-statements/jobs/:id/download         # Download processed CSV (user-isolated via job ownership)
GET  /api/v1/bank-statements/jobs                      # List user bank statement jobs
GET  /api/v1/bank-statements/jobs/:id                 # Get specific bank statement job details
GET  /api/v1/bank-statements/jobs/:id/status          # Processing status for bank statement job
```

### Bank Parser Architecture
```rust
// Enum-based parser design for better performance and type safety
pub enum BankParser {
    Santander(SantanderParser),
    Itau(ItauParser),
    Bradesco(BradescoParser),
    BancoBrasil(BancoBrasilParser),
    Caixa(CaixaParser),
}

impl BankParser {
    pub fn detect_bank(&self, content: &str) -> f32; // Confidence score 0-1
    pub fn parse_transactions(&self, content: &str) -> Result<Vec<BankTransaction>, ParseError>;
    pub fn get_bank_name(&self) -> &str;
}

// Concrete parser implementations
pub struct SantanderParser;
pub struct ItauParser;
pub struct BradescoParser;
pub struct BancoBrasilParser;
pub struct CaixaParser;

impl SantanderParser {
    // Santander-specific parsing logic
}

impl ItauParser {
    // Itaú-specific parsing logic
}

// ... other implementations
```

### Processing Queue
```rust
// Job structure
pub struct ProcessingJob {
    pub id: Uuid,
    pub file_path: PathBuf,
    pub user_id: Uuid,
    pub created_at: DateTime<Utc>,
}

// Queue operations
pub struct JobQueue {
    redis: RedisPool,
}

impl JobQueue {
    pub async fn enqueue(&self, job: ProcessingJob) -> Result<(), QueueError>;
    pub async fn dequeue(&self) -> Result<Option<ProcessingJob>, QueueError>;
    pub async fn update_status(&self, job_id: Uuid, status: JobStatus) -> Result<(), QueueError>;
}
```

## Security Considerations

### File Upload Security
- File type validation (CSV only for MVP)
- File size limits (10MB maximum)
- Secure file naming with UUIDs
- Virus scanning integration (future)

### Data Protection
- **Anonymous Sessions**: Cookie-based sessions with secure tokens (no signup required)
- **Session Isolation**: Each anonymous session is completely isolated from others
- **File Cleanup**: Automatic cleanup after processing/download or session expiry
- **No Persistent Storage**: Sensitive data only stored temporarily during processing
- **Rate Limiting**: 10 uploads per hour per anonymous session
- **Session Security**: Secure HTTP-only cookies with CSRF protection
- **Access Control**: Users can only access their own bank statement jobs and downloads
- **Session Validation**: All API endpoints validate session ownership before data access
- **Download Security**: Downloads are scoped to session ownership - users can only download files from their own sessions
- **Authorization Middleware**: All endpoints validate session token and ownership before processing

### Infrastructure Security
- **HTTPS via CloudFlare**: SSL termination at CloudFlare edge (HTTPS → HTTP to backend)
- **HTTP Backend**: Digital Ocean backend runs HTTP (simplified deployment)
- **Security Headers**: CloudFlare provides security headers and DDoS protection
- **Input Validation**: Server-side validation and sanitization
- **SQL Injection Prevention**: Parameterized queries and input sanitization

### Anonymous Session Implementation
```rust
// Anonymous session management
pub struct AnonymousSession {
    pub id: Uuid,
    pub token: String,
    pub expires_at: DateTime<Utc>,
}

impl AnonymousSession {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            token: generate_secure_token(),
            expires_at: Utc::now() + Duration::hours(24),
        }
    }
    
    pub fn is_valid(&self) -> bool {
        Utc::now() < self.expires_at
    }
}

// Middleware for session ownership validation
pub async fn validate_job_ownership(
    job_id: Uuid,
    session_id: Uuid,
    db: &Database
) -> Result<bool, AuthError> {
    let job = db.get_bank_statement_job(job_id).await?;
    Ok(job.session_id == session_id)
}

// Download endpoint with session ownership validation
pub async fn download_processed_file(
    job_id: Uuid,
    session_id: Uuid,
    db: &Database
) -> Result<FileResponse, DownloadError> {
    // Validate session owns this job
    if !validate_job_ownership(job_id, session_id, db).await? {
        return Err(DownloadError::Unauthorized);
    }
    
    // Proceed with secure download
    let file_path = db.get_job_file_path(job_id).await?;
    Ok(FileResponse::from_path(file_path))
}

// Cookie-based session middleware
pub async fn extract_session_from_cookie(
    cookies: &CookieJar
) -> Result<Uuid, AuthError> {
    let session_cookie = cookies.get("bankflow_session")
        .ok_or(AuthError::MissingSession)?;
    
    let session_id = Uuid::parse_str(&session_cookie.value())
        .map_err(|_| AuthError::InvalidSession)?;
    
    // Validate session exists and is not expired
    let session = db.get_anonymous_session(session_id).await?;
    if !session.is_valid() {
        return Err(AuthError::ExpiredSession);
    }
    
    Ok(session_id)
}
```

## Performance Requirements

### Processing Performance
- **File Processing**: <30 seconds per file
- **Concurrent Processing**: 10+ files simultaneously
- **Memory Usage**: <512MB per processing job
- **Database Queries**: <100ms for status checks

### User Experience Performance
- **Page Load**: <2 seconds for all pages
- **File Upload**: Progress indication for large files
- **Real-time Updates**: <1 second latency for status updates
- **Download**: <5 seconds for CSV generation

## Scalability Considerations

### Horizontal Scaling
- Stateless API services
- Redis-based job queue
- Database connection pooling
- Load balancer ready

### Vertical Scaling
- Optimized Rust performance
- Efficient memory usage
- Database query optimization
- Caching strategies

## Deployment Strategy

### Local Development
- Docker Compose with PostgreSQL and Redis
- Hot reloading for development
- Comprehensive testing suite
- Quality gates enforcement

### Production Deployment Architecture

#### CloudFlare + Digital Ocean Setup
```
Internet → CloudFlare (HTTPS) → Digital Ocean (HTTP) → BankFlow App
```

**CloudFlare Configuration**:
- **SSL/TLS**: Automatic HTTPS with Let's Encrypt certificates
- **Proxy Mode**: Orange cloud enabled (traffic routed through CloudFlare)
- **Security**: DDoS protection, WAF, bot protection
- **Performance**: Global CDN, caching, compression
- **Headers**: Security headers automatically applied

**Subdomain Setup**:
- **Production**: `bankflow.com` → Digital Ocean droplet
- **Staging**: `staging.bankflow.com` → Same Digital Ocean droplet
- **Routing**: CloudFlare routes both domains to same IP
- **Environment Detection**: Application detects environment via Host header
- **Database Isolation**: Shared PostgreSQL with separate databases (`bankflow_staging`, `bankflow_production`)
- **Cache Isolation**: Shared Redis with separate namespaces (`staging:`, `production:`)

**Digital Ocean Backend**:
- **Droplet**: 2GB RAM, 1 CPU minimum
- **Docker**: Containerized application deployment
- **HTTP Only**: No SSL certificates needed (CloudFlare handles HTTPS)
- **Shared Services**: Single PostgreSQL and Redis instances
- **Firewall**: Only CloudFlare IPs allowed (no direct internet access)

### Shared Database Architecture

**PostgreSQL Setup**:
```sql
-- Shared PostgreSQL instance with separate databases
CREATE DATABASE bankflow_staging;
CREATE DATABASE bankflow_production;

-- Each environment connects to its own database
-- Staging: postgresql://user:pass@localhost:5432/bankflow_staging
-- Production: postgresql://user:pass@localhost:5432/bankflow_production
```

**Redis Setup**:
```redis
# Shared Redis instance with namespace isolation
# Staging keys: staging:session:*, staging:job:*, staging:file:*
# Production keys: production:session:*, production:job:*, production:file:*

# Environment-specific key prefixes
SET staging:session:abc123 "session_data"
SET production:session:def456 "session_data"
```

**Benefits of Shared Infrastructure**:
- **Cost Effective**: Single PostgreSQL and Redis instances
- **Resource Efficient**: Better resource utilization
- **Simplified Management**: One set of services to maintain
- **Complete Isolation**: Separate databases and Redis namespaces
- **Easy Backup**: Single backup strategy for both environments

#### Deployment Benefits
- **Simplified SSL**: No certificate management on backend
- **Enhanced Security**: CloudFlare provides enterprise-grade protection
- **Better Performance**: Global CDN and caching
- **Cost Effective**: CloudFlare free tier sufficient for demo
- **Easy Scaling**: CloudFlare handles traffic spikes automatically

### CI/CD Pipeline Architecture

#### GitHub Actions + Digital Ocean Workflow
```
Staging: GitHub Push (staging) → GitHub Actions → Deploy to staging.bankflow.com
Production: GitHub Push (main) → GitHub Actions → Deploy to bankflow.com
```

**Environment Strategy**:
- **Staging Environment**: `staging.bankflow.com` (staging branch)
- **Production Environment**: `bankflow.com` (main branch)
- **Deployment Flow**: All changes go to staging first, then promoted to production

**Pipeline Stages**:
1. **Staging Push**: Developer pushes to staging branch
2. **Automated Testing**: GitHub Actions runs test suite
3. **Staging Deploy**: Deploy to staging.bankflow.com
4. **Staging Validation**: Manual testing and validation
5. **Production Merge**: Merge staging → main for production release
6. **Production Deploy**: Deploy to bankflow.com
7. **Health Check**: Verify both environments
8. **Notification**: Success/failure notifications

**GitHub Actions Configuration**:
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
      - name: Push to Registry
        run: docker push ghcr.io/username/bankflow:${{ github.sha }}
      
      # Staging Deployment
      - name: Deploy to Staging
        if: github.ref == 'refs/heads/staging'
        uses: appleboy/ssh-action@v0.1.5
        with:
          host: ${{ secrets.DO_HOST }}
          username: ${{ secrets.DO_USERNAME }}
          key: ${{ secrets.DO_SSH_KEY }}
          script: |
            docker pull ghcr.io/username/bankflow:${{ github.sha }}
            cd /opt/bankflow-staging
            # Set environment variables for staging
            export DATABASE_URL="postgresql://user:pass@localhost:5432/bankflow_staging"
            export REDIS_URL="redis://localhost:6379"
            export REDIS_PREFIX="staging:"
            export ENVIRONMENT="staging"
            docker-compose down
            docker-compose up -d
            
      # Production Deployment
      - name: Deploy to Production
        if: github.ref == 'refs/heads/main'
        uses: appleboy/ssh-action@v0.1.5
        with:
          host: ${{ secrets.DO_HOST }}
          username: ${{ secrets.DO_USERNAME }}
          key: ${{ secrets.DO_SSH_KEY }}
          script: |
            docker pull ghcr.io/username/bankflow:${{ github.sha }}
            cd /opt/bankflow-production
            # Set environment variables for production
            export DATABASE_URL="postgresql://user:pass@localhost:5432/bankflow_production"
            export REDIS_URL="redis://localhost:6379"
            export REDIS_PREFIX="production:"
            export ENVIRONMENT="production"
            docker-compose down
            docker-compose up -d
```

**Digital Ocean Integration**:
- **SSH Key Authentication**: Secure deployment without passwords
- **Docker Compose**: Simple container orchestration
- **Zero-Downtime Deployment**: Rolling updates with health checks
- **Environment Variables**: Secure configuration management
- **Backup Strategy**: Automated database backups before deployment

**Environment Setup**:
- **Staging Directory**: `/opt/bankflow-staging/` (staging.bankflow.com)
- **Production Directory**: `/opt/bankflow-production/` (bankflow.com)
- **Shared Infrastructure**: Single Redis and PostgreSQL instances
- **Database Isolation**: Separate databases (`bankflow_staging`, `bankflow_production`)
- **Environment Configs**: Different configs for staging vs production
- **Domain Routing**: CloudFlare routes subdomains to same droplet

**Development Workflow**:
1. **Feature Development**: Work on feature branches
2. **Staging Deploy**: Merge to staging branch → auto-deploy to staging.bankflow.com
3. **Testing**: Validate features on staging environment
4. **Production Release**: Merge staging → main → auto-deploy to bankflow.com
5. **Monitoring**: Monitor both environments for issues

**Benefits**:
- **Safe Testing**: Test changes on staging before production
- **Automated Testing**: Every push runs full test suite
- **Fast Deployment**: ~5 minutes from push to staging, ~5 minutes to production
- **Rollback Capability**: Quick rollback to previous version
- **Environment Isolation**: Staging and production completely separate
- **Security**: Secrets managed in GitHub Actions
- **Cost Effective**: GitHub Actions free tier sufficient for demo

## Quality Assurance

### Testing Strategy
- Unit tests for all core functions
- Integration tests for API endpoints
- End-to-end tests for complete demo flow
- Performance tests for processing speed

### Quality Gates
- Code coverage >80%
- All tests passing
- Security scan clean
- Performance targets met
- Accessibility compliance (WCAG 2.1 AA)

## Risk Mitigation

### Technical Risks
- **Bank Format Changes**: Flexible parser architecture with versioning
- **Performance Issues**: File size limits and optimized algorithms
- **Security Vulnerabilities**: Comprehensive security audit and testing
- **Scalability Problems**: Stateless architecture and horizontal scaling

### Business Risks
- **Market Validation**: Focus on core value proposition
- **Competition**: Rapid development and market entry
- **User Adoption**: Professional UI and seamless experience
- **Technical Complexity**: Phased development approach

## Success Metrics

### Technical Metrics
- Processing time <30 seconds per file
- Parsing accuracy >99%
- System uptime >99.5%
- Error rate <1%

### Business Metrics
- Demo completion rate >80%
- User satisfaction NPS >50
- Processing success rate >95%
- User engagement time >2 minutes

## Future Enhancements

### Phase 2 Features
- User authentication and accounts
- Payment processing and billing
- Advanced parsing options
- API access for integrations

### Long-term Vision
- Machine learning for improved accuracy
- Support for additional file formats
- Integration with accounting software
- Multi-language support

## Notes

This solution prioritizes the core value proposition validation over feature completeness. The demo must be compelling enough to prove market demand before investing in full product development. Success metrics focus on user experience and technical performance rather than business features like authentication or billing.
