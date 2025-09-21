---
description: UX-Optimized Implementation Guide - ConciliaExtrato Pre-launch Page
type: technical-implementation
status: planned
---

# UX-Optimized Implementation Guide: ConciliaExtrato Pre-launch Page

## Overview
This guide provides a comprehensive, UX-optimized implementation for the ConciliaExtrato pre-launch page that maximizes conversion while maintaining excellent user experience. The approach combines HTMX for seamless server-side rendering with Alpine.js for smooth client-side interactions, all optimized for Lighthouse 100/100 scores.

**📋 Design System**: See `design-system-integration.md` for the complete ConciliaExtrato design system including all components, tokens, and styling specifications.

**📊 Analytics & Monitoring**: See `analytics-monitoring-spec.md` for comprehensive analytics integration, Core Web Vitals tracking, Prometheus metrics, and monitoring specifications.

## Tech Stack Integration

### Core Technologies
- **Backend**: Rust + Axum + Askama templates
- **Frontend**: HTMX + Alpine.js (minimal JS)
- **Database**: PostgreSQL with SQLx
- **CSS**: Custom CSS inline + async loading
- **Fonts**: Inter with preload + font-display swap
- **Images**: WebP + lazy loading + responsive
- **Hosting**: Digital Ocean + Docker + Nginx
- **Performance**: Lighthouse 100/100 optimization

## UX Design Principles

### 1. Progressive Disclosure
- **Initial State**: Clean, focused form with single email field
- **Loading State**: Clear feedback with elegant spinner
- **Success State**: Celebratory confirmation with next steps
- **Error State**: Helpful error messages with retry options

### 2. Contextual Feedback
- **Real-time validation**: Immediate email format feedback
- **Loading indicators**: Clear progress indication during submission
- **Success celebration**: Positive reinforcement for completion
- **Error recovery**: Easy retry mechanisms

### 3. Accessibility First
- **Keyboard navigation**: Full keyboard accessibility
- **Screen reader support**: Proper ARIA labels and announcements
- **Focus management**: Clear focus indicators and management
- **Error announcements**: Screen reader accessible error messages

## Implementation Phases

### Phase 1: Foundation Setup (Week 1)
**Focus**: Route restructuring and UX-optimized template creation

#### 1.1 Route Restructuring
```rust
// main.rs - Updated routing
async fn create_app() -> Router {
    Router::new()
        .route("/", get(handlers::prelaunch_page))           // ConciliaExtrato pre-launch
        .route("/product", get(handlers::product_page))      // Current landing content
        .route("/product/demo", get(handlers::product_demo_page)) // Current demo
        .route("/prelaunch/register", post(handlers::register_email)) // HTMX lead capture
        .route("/health", get(handlers::health_check))       // Performance monitoring
        .route("/demo", get(handlers::product_demo_page))    // Backward compatibility
        .nest_service("/static", ServeDir::new("static"))
}
```

#### 1.2 Template System Updates (UX-Enhanced)
```rust
// templates.rs - Enhanced template structs with UX focus
#[derive(Template)]
#[template(path = "prelaunch.html")]
pub struct PrelaunchTemplate {
    pub title: String,
    pub description: String,
    pub canonical_url: String,
    pub og_image: String,
    pub keywords: Vec<String>,
}

impl PrelaunchTemplate {
    pub fn new() -> Self {
        Self {
            title: "ConciliaExtrato - Conciliação Automática de Extratos Bancários".to_string(),
            description: "Transforme extratos de qualquer banco brasileiro em dados organizados. Itaú, Bradesco, Santander, BB, Caixa. Feito especialmente para contadores brasileiros.".to_string(),
            canonical_url: "https://conciliaextrato.com/".to_string(),
            og_image: "https://conciliaextrato.com/og-image.webp".to_string(),
            keywords: vec![
                "conciliação bancária".to_string(),
                "extratos bancários".to_string(),
                "contabilidade".to_string(),
                "automação contábil".to_string(),
                "normalizar extratos".to_string(),
                "organizar dados bancários".to_string(),
                "conciliar conta".to_string(),
                "software contador".to_string()
            ],
        }
    }
}
```

### Phase 2: UX-Optimized Lead Capture System (Week 1-2)
**Focus**: HTMX-powered lead capture with enhanced user experience

#### 2.1 Database Schema (Performance Optimized)
```sql
-- migrations/20250101000000_create_prelaunch_leads.sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE prelaunch_leads (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    email VARCHAR(255) UNIQUE NOT NULL,
    ip_address INET,
    user_agent TEXT,
    source VARCHAR(100) NOT NULL DEFAULT 'prelaunch-page',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    
    CONSTRAINT valid_email CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

CREATE INDEX idx_prelaunch_leads_email ON prelaunch_leads(email);
CREATE INDEX idx_prelaunch_leads_created_at ON prelaunch_leads(created_at);
CREATE INDEX idx_prelaunch_leads_source ON prelaunch_leads(source);
```

#### 2.2 HTMX Lead Capture Handler (UX-Enhanced)
```rust
// handlers.rs - HTMX-powered lead capture with UX optimization
pub async fn register_email(Form(form): Form<EmailForm>) -> impl IntoResponse {
    // Validate email
    if !is_valid_email(&form.email) {
        return Html(r#"
            <div class="form-message error" 
                 x-data="{ show: false }" 
                 x-init="setTimeout(() => show = true, 100)"
                 x-show="show"
                 x-transition:enter="transition ease-out duration-300"
                 x-transition:enter-start="opacity-0 transform scale-95"
                 x-transition:enter-end="opacity-100 transform scale-100">
                ❌ Por favor, insira um email válido
            </div>
        "#).into_response();
    }
    
    // Check if already registered
    if is_email_registered(&form.email).await {
        return Html(r#"
            <div class="form-message warning" 
                 x-data="{ show: false }" 
                 x-init="setTimeout(() => show = true, 100)"
                 x-show="show"
                 x-transition:enter="transition ease-out duration-300"
                 x-transition:enter-start="opacity-0 transform scale-95"
                 x-transition:enter-end="opacity-100 transform scale-100">
                ⚠️ Este email já está cadastrado! Você já receberá nossas novidades.
            </div>
        "#).into_response();
    }
    
    // Save email to database
    match save_prelaunch_email(&form.email).await {
        Ok(_) => {
            Html(r#"
                <div class="form-message success" 
                     x-data="{ show: false }" 
                     x-init="setTimeout(() => show = true, 100)"
                     x-show="show"
                     x-transition:enter="transition ease-out duration-300"
                     x-transition:enter-start="opacity-0 transform scale-95"
                     x-transition:enter-end="opacity-100 transform scale-100">
                    ✅ Perfeito! Você está na lista
                    <br><small>Você será o primeiro a saber quando estiver disponível</small>
                </div>
            "#).into_response()
        }
        Err(_) => {
            (StatusCode::INTERNAL_SERVER_ERROR, Html(r#"
                <div class="form-message error" 
                     x-data="{ show: false }" 
                     x-init="setTimeout(() => show = true, 100)"
                     x-show="show"
                     x-transition:enter="transition ease-out duration-300"
                     x-transition:enter-start="opacity-0 transform scale-95"
                     x-transition:enter-end="opacity-100 transform scale-100">
                    ❌ Erro interno. Tente novamente em alguns minutos.
                </div>
            "#)).into_response()
        }
    }
}
```

### Phase 3: UX-Optimized Design Implementation (Week 2)
**Focus**: ConciliaExtrato branding with enhanced user experience

#### 3.1 Critical CSS (Design System Integrated)
```css
/* Inline in prelaunch.html template - See design-system-integration.md for complete system */
:root {
    /* Primary Colors */
    --blue-primary: #3B82F6;
    --blue-hover: #2563EB;
    
    /* Background Colors */
    --dark: #0F172A;
    --dark-secondary: #1E293B;
    
    /* Text Colors */
    --white: #FFFFFF;
    --gray-light: #94A3B8;
    
    /* Status Colors */
    --success: #22C55E;
    --error: #EF4444;
    --warning: #F59E0B;
    
    /* Typography */
    --font-primary: 'Inter', sans-serif;
    --font-secondary: 'Inter', sans-serif;
    
    /* Glass Effects */
    --glass-bg: rgba(255, 255, 255, 0.05);
    --glass-border: rgba(255, 255, 255, 0.1);
    --glass-blur: blur(20px);
    
    /* Transitions */
    --transition-fast: 0.15s ease;
    --transition-normal: 0.3s ease;
    --transition-slow: 0.5s ease;
}

* { margin: 0; padding: 0; box-sizing: border-box; }

body {
    font-family: var(--font-primary);
    background: var(--dark);
    color: var(--white);
    line-height: 1.6;
    overflow-x: hidden;
}

.hero-title {
    font-family: var(--font-primary);
    font-size: clamp(2rem, 5vw, 3.5rem);
    font-weight: 700;
    line-height: 1.1;
    background: linear-gradient(135deg, var(--white) 0%, var(--blue-primary) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
}

/* Lead Capture Section */
.lead-capture {
    padding: 80px 20px;
    text-align: center;
    position: relative;
    overflow: hidden;
}

/* Form Container */
.lead-form-container {
    max-width: 500px;
    margin: 0 auto;
}

.form-group {
    display: flex;
    gap: 1rem;
    margin-bottom: 1rem;
}

.input-container {
    flex: 1;
    position: relative;
}

.email-input {
    width: 100%;
    padding: 1rem 1.25rem;
    border: 2px solid rgba(255, 255, 255, 0.2);
    border-radius: 0.75rem;
    background: rgba(255, 255, 255, 0.05);
    color: var(--white);
    font-size: 1rem;
    backdrop-filter: blur(10px);
    transition: all 0.3s ease;
}

.email-input:focus {
    outline: none;
    border-color: var(--blue-primary);
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.2);
    background: rgba(255, 255, 255, 0.1);
}

.email-input.error {
    border-color: #ef4444;
    box-shadow: 0 0 0 3px rgba(239, 68, 68, 0.2);
}

.email-input.valid {
    border-color: #10b981;
    box-shadow: 0 0 0 3px rgba(16, 185, 129, 0.2);
}

/* Validation Feedback */
.validation-feedback {
    position: absolute;
    right: 1rem;
    top: 50%;
    transform: translateY(-50%);
    font-size: 1.2rem;
}

.valid-icon {
    color: #10b981;
}

.error-icon {
    color: #ef4444;
}

/* CTA Button */
.cta-button {
    padding: 1rem 2rem;
    background: linear-gradient(135deg, var(--blue-primary) 0%, var(--blue-hover) 100%);
    color: var(--white);
    border: none;
    border-radius: 0.75rem;
    font-weight: 600;
    font-size: 1rem;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
    box-shadow: 0 4px 14px rgba(59, 130, 246, 0.3);
    transition: all 0.3s ease;
    white-space: nowrap;
    min-width: 200px;
}

.cta-button:hover:not(:disabled) {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(59, 130, 246, 0.4);
}

.cta-button:disabled {
    background: #6b7280;
    cursor: not-allowed;
    transform: none;
    box-shadow: none;
}

/* Loading States */
.loading-content {
    display: flex;
    align-items: center;
    gap: 0.5rem;
}

.spinner {
    width: 20px;
    height: 20px;
}

/* HTMX Loading Indicator */
.htmx-indicator {
    opacity: 0;
    transition: opacity 200ms ease-in;
}

.htmx-request .htmx-indicator {
    opacity: 1;
}

.loading-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.5);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    border-radius: 0.75rem;
    backdrop-filter: blur(5px);
}

/* Success State */
.success-state {
    max-width: 500px;
    margin: 0 auto;
    text-align: center;
}

.success-icon {
    font-size: 4rem;
    margin-bottom: 1.5rem;
    animation: celebrate 0.6s ease-out;
}

@keyframes celebrate {
    0% { transform: scale(0); opacity: 0; }
    50% { transform: scale(1.2); opacity: 1; }
    100% { transform: scale(1); opacity: 1; }
}

.success-title {
    font-size: 1.5rem;
    font-weight: 700;
    margin-bottom: 1rem;
    color: var(--white);
}

.success-message {
    font-size: 1.1rem;
    margin-bottom: 2rem;
    opacity: 0.9;
    line-height: 1.6;
}

.success-actions {
    display: flex;
    gap: 1rem;
    justify-content: center;
    flex-wrap: wrap;
}

.secondary-button {
    padding: 0.75rem 1.5rem;
    background: transparent;
    color: var(--white);
    border: 2px solid rgba(255, 255, 255, 0.3);
    border-radius: 0.5rem;
    cursor: pointer;
    transition: all 0.3s ease;
}

.primary-button {
    padding: 0.75rem 1.5rem;
    background: var(--blue-primary);
    color: var(--white);
    border: none;
    border-radius: 0.5rem;
    text-decoration: none;
    font-weight: 600;
    transition: all 0.3s ease;
}

/* Responsive Design */
@media (max-width: 768px) {
    .form-group {
        flex-direction: column;
        gap: 1rem;
    }
    
    .cta-button {
        min-width: auto;
        width: 100%;
    }
    
    .success-actions {
        flex-direction: column;
    }
    
    .secondary-button,
    .primary-button {
        width: 100%;
    }
}

/* Reduced Motion Support */
@media (prefers-reduced-motion: reduce) {
    * {
        animation-duration: 0.01ms !important;
        animation-iteration-count: 1 !important;
        transition-duration: 0.01ms !important;
    }
}
```

#### 3.2 UX-Optimized HTML Template
```html
<!-- Lead Capture Section -->
<section class="lead-capture" id="email-form-section" 
         x-data="leadCaptureApp()" 
         x-intersect="animateForm()">
    
    <!-- Hero Message -->
    <div class="hero-message" 
         x-show="!submitted" 
         x-transition:enter="transition ease-out duration-500"
         x-transition:enter-start="opacity-0 transform translate-y-8"
         x-transition:enter-end="opacity-100 transform translate-y-0">
        <h2 class="hero-title">
            <span x-text="currentText" 
                  x-transition:enter="transition ease-out duration-300"
                  x-transition:enter-start="opacity-0 transform translate-y-4"
                  x-transition:enter-end="opacity-100 transform translate-y-0"
                  x-transition:leave="transition ease-in duration-300"
                  x-transition:leave-start="opacity-100 transform translate-y-0"
                  x-transition:leave-end="opacity-0 transform -translate-y-4">
            </span>
        </h2>
        <p class="hero-subtitle">
            Toda a informação dos seus extratos bancários 
            <span class="highlight">consolidada em um só lugar</span>
        </p>
        <p class="mystery-text">
            Estamos trabalhando para tornar isso realidade para todos os bancos
        </p>
    </div>

    <!-- Lead Capture Form -->
    <div class="lead-form-container" 
         x-show="!submitted" 
         x-transition:enter="transition ease-out duration-500 delay-200"
         x-transition:enter-start="opacity-0 transform translate-y-8"
         x-transition:enter-end="opacity-100 transform translate-y-0">
        
        <form hx-post="/prelaunch/register" 
              hx-target="#form-response"
              hx-swap="innerHTML"
              hx-indicator="#loading-indicator"
              @submit="handleFormSubmit()"
              class="email-form"
              novalidate>
            
            <div class="form-group">
                <div class="input-container">
                    <input type="email" 
                           name="email"
                           placeholder="seu.email@exemplo.com" 
                           required
                           x-model="email"
                           @input="validateEmail()"
                           @blur="validateEmail()"
                           :class="{ 
                               'error': emailError, 
                               'valid': isEmailValid && !emailError 
                           }"
                           class="email-input"
                           aria-label="Endereço de email"
                           aria-describedby="email-help email-error">
                    
                    <!-- Real-time validation feedback -->
                    <div class="validation-feedback" 
                         x-show="email.length > 0"
                         x-transition:enter="transition ease-out duration-200"
                         x-transition:enter-start="opacity-0 transform scale-95"
                         x-transition:enter-end="opacity-100 transform scale-100">
                        <span x-show="isEmailValid" class="valid-icon">✓</span>
                        <span x-show="emailError" class="error-icon">✗</span>
                    </div>
                </div>
                
                <button type="submit" 
                        :disabled="!isEmailValid || loading"
                        :class="{ 
                            'loading': loading,
                            'disabled': !isEmailValid 
                        }"
                        class="cta-button"
                        aria-label="Cadastrar email para receber novidades">
                    <span x-show="!loading" class="btn-text">
                        Ser o primeiro a saber
                    </span>
                    <span x-show="loading" class="loading-content">
                        <svg class="spinner" viewBox="0 0 24 24" aria-hidden="true">
                            <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4" fill="none" stroke-dasharray="32" stroke-dashoffset="32">
                                <animate attributeName="stroke-dasharray" dur="1s" values="0 32;16 16;0 32;0 32" repeatCount="indefinite"/>
                                <animate attributeName="stroke-dashoffset" dur="1s" values="0;-16;-32;-32" repeatCount="indefinite"/>
                            </circle>
                        </svg>
                        <span class="loading-text">Cadastrando...</span>
                    </span>
                </button>
            </div>
            
            <!-- Help text -->
            <p id="email-help" class="form-help">
                📧 Você será o primeiro a saber quando o ConciliaExtrato estiver pronto
            </p>
            
            <!-- Error message -->
            <p id="email-error" 
               x-show="emailError" 
               class="error-message"
               x-transition:enter="transition ease-out duration-200"
               x-transition:enter-start="opacity-0 transform translate-y-2"
               x-transition:enter-end="opacity-100 transform translate-y-0">
                Por favor, insira um email válido
            </p>
            
            <!-- HTMX Response Container -->
            <div id="form-response" class="form-response"></div>
            
            <!-- Loading Indicator -->
            <div id="loading-indicator" class="htmx-indicator">
                <div class="loading-overlay">
                    <div class="loading-spinner"></div>
                    <p>Processando...</p>
                </div>
            </div>
        </form>
    </div>

    <!-- Success State -->
    <div class="success-state" 
         x-show="submitted" 
         x-transition:enter="transition ease-out duration-500"
         x-transition:enter-start="opacity-0 transform scale-95"
         x-transition:enter-end="opacity-100 transform scale-100">
        
        <div class="success-icon" 
             x-transition:enter="transition ease-out duration-600 delay-200"
             x-transition:enter-start="opacity-0 transform scale-0"
             x-transition:enter-end="opacity-100 transform scale-100">
            ✅
        </div>
        
        <h3 class="success-title">
            Perfeito! Você está na lista
        </h3>
        
        <p class="success-message">
            Você será o primeiro a saber quando o ConciliaExtrato estiver disponível.
            <br><small>Verifique sua caixa de entrada (e spam)</small>
        </p>
        
        <div class="success-actions">
            <button @click="resetForm()" class="secondary-button">
                Cadastrar outro email
            </button>
            <a href="/product/demo" class="primary-button">
                Ver demonstração
            </a>
        </div>
    </div>
</section>
```

#### 3.3 Alpine.js App Logic (UX-Enhanced)
```javascript
function leadCaptureApp() {
    return {
        // Text rotation for engagement
        texts: [
            'Papelada descomplicada',
            'Da bagunça à clareza em segundos', 
            'Muitos lançamentos, um só lugar',
            'Extratos organizados, mente tranquila'
        ],
        currentText: 'Papelada descomplicada',
        textIndex: 0,
        
        // Form state management
        email: '',
        emailError: false,
        loading: false,
        isEmailValid: false,
        submitted: false,
        
        // Animation state
        formAnimated: false,
        
        init() {
            this.startTextRotation();
            this.setupHTMXListeners();
            this.preloadImages();
        },
        
        startTextRotation() {
            setInterval(() => {
                this.textIndex = (this.textIndex + 1) % this.texts.length;
                this.currentText = this.texts[this.textIndex];
            }, 3000);
        },
        
        validateEmail() {
            const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
            this.isEmailValid = emailRegex.test(this.email);
            this.emailError = this.email.length > 0 && !this.isEmailValid;
            
            // Clear error when email becomes valid
            if (this.isEmailValid) {
                this.emailError = false;
            }
        },
        
        handleFormSubmit() {
            if (!this.isEmailValid) {
                this.emailError = true;
                return false;
            }
            
            this.loading = true;
            this.emailError = false;
            
            // HTMX will handle the actual submission
            // This is just for UI state management
        },
        
        setupHTMXListeners() {
            // Listen for HTMX events
            document.body.addEventListener('htmx:afterRequest', (event) => {
                this.loading = false;
                
                if (event.detail.xhr.status === 200) {
                    // Success - HTMX will update the form-response div
                    setTimeout(() => {
                        this.submitted = true;
                        this.trackSuccess();
                    }, 100);
                } else {
                    // Error - HTMX will show error message
                    this.trackError();
                }
            });
            
            document.body.addEventListener('htmx:beforeRequest', () => {
                this.loading = true;
            });
        },
        
        animateForm() {
            if (!this.formAnimated) {
                this.formAnimated = true;
                // Trigger any entrance animations
            }
        },
        
        resetForm() {
            this.submitted = false;
            this.email = '';
            this.emailError = false;
            this.isEmailValid = false;
            this.loading = false;
            
            // Clear HTMX response
            document.getElementById('form-response').innerHTML = '';
        },
        
        trackSuccess() {
            // Google Analytics tracking
            if (typeof gtag !== 'undefined') {
                gtag('event', 'lead_capture_success', {
                    'event_category': 'conversion',
                    'event_label': 'prelaunch_signup',
                    'value': 1
                });
            }
        },
        
        trackError() {
            // Google Analytics tracking
            if (typeof gtag !== 'undefined') {
                gtag('event', 'lead_capture_error', {
                    'event_category': 'error',
                    'event_label': 'prelaunch_signup_error'
                });
            }
        },
        
        preloadImages() {
            // Preload critical images
            const imageUrls = [
                '/static/og-image.webp',
                '/static/favicon-32x32.png'
            ];
            
            imageUrls.forEach(url => {
                const img = new Image();
                img.src = url;
            });
        }
    }
}
```

### Phase 4: Performance Optimization & Analytics (Week 2-3)
**Focus**: Lighthouse 100/100 scores, Core Web Vitals, and comprehensive monitoring

#### 4.1 Performance Optimizations
- **Critical CSS**: Inline for above-the-fold content
- **Non-critical CSS**: Async loading with preload
- **Fonts**: Preload with font-display: swap
- **Images**: WebP format with lazy loading
- **JavaScript**: Deferred loading for HTMX and Alpine.js

#### 4.2 Analytics & Monitoring Integration
```html
// Track email submissions via HTMX
    document.body.addEventListener('htmx:afterSettle', (event) => {
        if (event.detail.target.id === 'form-response') {
            const isSuccess = event.detail.target.querySelector('.form-message.success');
            if (isSuccess) {
                gtag('event', 'email_signup', {
                    event_category: 'engagement',
                    event_label: 'prelaunch_email',
                    value: 1
                });
            }
        }
    });
</script>

<!-- Core Web Vitals tracking -->
<script>
    // Core Web Vitals tracking
    import('https://unpkg.com/web-vitals@3/dist/web-vitals.js').then(({ getCLS, getFID, getFCP, getLCP, getTTFB }) => {
        getCLS(console.log);
        getFID(console.log);
        getFCP(console.log);
        getLCP(console.log);
        getTTFB(console.log);
    });
</script>
```

#### 4.3 Health Check Endpoint
```rust
// crates/site/src/handlers.rs
pub async fn health_check() -> impl IntoResponse {
    Json(serde_json::json!({
        "status": "healthy",
        "timestamp": chrono::Utc::now(),
        "version": env!("CARGO_PKG_VERSION")
    }))
}
```

#### 4.4 Monitoring Implementation
```rust
use prometheus::{Counter, Histogram, register_counter, register_histogram};

lazy_static! {
    static ref LEAD_CAPTURE_COUNTER: Counter = register_counter!(
        "conciliaextrato_leads_total", 
        "Total number of lead captures"
    ).unwrap();
    
    static ref LEAD_CAPTURE_DURATION: Histogram = register_histogram!(
        "conciliaextrato_lead_capture_duration_seconds",
        "Time taken to process lead capture"
    ).unwrap();
    
    static ref PAGE_VIEWS_COUNTER: Counter = register_counter!(
        "conciliaextrato_page_views_total",
        "Total number of page views"
    ).unwrap();
}
```

#### 4.5 SEO Optimization
```html
<!-- Structured Data -->
<script type="application/ld+json">
{
    "@context": "https://schema.org",
    "@type": "SoftwareApplication",
    "name": "ConciliaExtrato",
    "url": "https://conciliaextrato.com/",
    "description": "Software de conciliação automática de extratos bancários para contadores e administrativos financeiros",
    "applicationCategory": "BusinessApplication",
    "operatingSystem": "Web Browser",
    "offers": {
        "@type": "Offer",
        "price": "0",
        "priceCurrency": "BRL",
        "availability": "ComingSoon"
    },
    "provider": {
        "@type": "Organization",
        "name": "ConciliaExtrato",
        "url": "https://conciliaextrato.com/"
    },
    "audience": {
        "@type": "Audience",
        "audienceType": "Contadores e Profissionais de Finanças"
    }
}
</script>
```

### Phase 5: Deployment (Week 3)
**Focus**: Docker containerization and production deployment

#### 5.1 Docker Configuration
```dockerfile
# Multi-stage build para otimizar tamanho
FROM rust:1.75-slim as builder

WORKDIR /app
COPY . .

# Build release otimizado
RUN cargo build --release --workspace

FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy compiled binaries
COPY --from=builder /app/target/release/site /app/site
COPY --from=builder /app/target/release/api /app/api

# Copy static files
COPY static/ /app/static/

# Create upload directory
RUN mkdir -p /app/uploads /app/logs

EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

CMD ["./site"]
```

#### 5.2 Nginx Configuration
```nginx
# nginx/nginx.conf
events {
    worker_connections 1024;
}

http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    # Gzip compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_types text/plain text/css text/xml text/javascript 
               application/javascript application/json
               application/xml+rss application/atom+xml
               image/svg+xml;

    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header Referrer-Policy strict-origin-when-cross-origin;
    add_header X-XSS-Protection "1; mode=block";

    upstream app {
        server app:8080;
    }

    # Rate limiting
    limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
    limit_req_zone $binary_remote_addr zone=prelaunch:10m rate=1r/s;

    server {
        listen 80;
        server_name conciliaextrato.com www.conciliaextrato.com;

        # Static files caching
        location /static/ {
            alias /app/static/;
            expires 1y;
            add_header Cache-Control "public, immutable";
            add_header Vary Accept-Encoding;
        }

        # Prelaunch form with rate limiting
        location /prelaunch/register {
            limit_req zone=prelaunch burst=5 nodelay;
            proxy_pass http://app;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }

        # All other requests
        location / {
            proxy_pass http://app;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
    }
}
```

## UX Benefits of This Approach

### 1. **Seamless User Experience**
- No page navigation required
- Immediate feedback and validation
- Smooth transitions between states
- Contextual help and error messages

### 2. **High Conversion Optimization**
- Single email field reduces friction
- Real-time validation prevents errors
- Success celebration reinforces positive action
- Clear next steps after signup

### 3. **Accessibility Excellence**
- Full keyboard navigation support
- Screen reader accessible
- Clear focus indicators
- Proper ARIA labels and descriptions

### 4. **Performance Optimized**
- HTMX provides server-side rendering benefits
- Alpine.js adds minimal JavaScript overhead
- Smooth animations with CSS transitions
- Progressive enhancement approach

### 5. **Mobile-First Design**
- Responsive form layout
- Touch-friendly button sizes
- Optimized for small screens
- Reduced motion support

## Success Metrics

### Technical Targets
- **Lighthouse Performance**: 100/100
- **Lighthouse Accessibility**: 100/100
- **Lighthouse Best Practices**: 100/100
- **Lighthouse SEO**: 100/100
- **Core Web Vitals**: All green (LCP < 2.5s, FID < 100ms, CLS < 0.1)

### Business Targets
- **Lead Capture Rate**: > 5% of visitors
- **Lead Volume**: 100 leads/month in first quarter
- **Conversion Rate**: > 20% of leads become customers
- **SEO Performance**: Top 20 for "conciliação bancária" in 6 months

## Implementation Timeline

### Week 1: Foundation
- Route restructuring and template creation
- HTMX lead capture system implementation
- PostgreSQL database setup

### Week 2: Design and Performance
- ConciliaExtrato branding implementation
- Glass morphism effects and animations
- Performance optimization and SEO

### Week 3: Deployment and Testing
- Docker containerization
- Production deployment
- Comprehensive testing and validation

## Quality Assurance

### Pre-Deployment Checklist
- [ ] All routes tested and working
- [ ] HTMX lead capture form tested
- [ ] Lighthouse scores 100/100 across all categories
- [ ] Core Web Vitals all green
- [ ] Responsive design tested on all devices
- [ ] SEO optimization verified
- [ ] Performance benchmarks met

### Post-Deployment Monitoring
- [ ] Monitor Core Web Vitals (LCP, FID, CLS, FCP, TTFB)
- [ ] Track lead capture rates and conversion funnels
- [ ] Monitor error rates and performance metrics
- [ ] Analyze user behavior and conversion patterns
- [ ] Monitor Prometheus metrics for system health
- [ ] Track Google Analytics events and goals
- [ ] Monitor Brazilian market engagement metrics
- [ ] Optimize based on real user data and performance insights

This UX-optimized approach ensures maximum conversion while maintaining excellent user experience, accessibility standards, and performance targets.