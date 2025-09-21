---
description: Analytics & Monitoring Specification - ConciliaExtrato Pre-launch Page
type: analytics-monitoring-specification
status: planned
---

# Analytics & Monitoring Specification: ConciliaExtrato Pre-launch Page

## Overview
This document provides comprehensive analytics and monitoring specifications for the ConciliaExtrato pre-launch page, including Google Analytics 4 integration, Core Web Vitals tracking, Prometheus metrics, and health monitoring.

## Analytics Integration

### 1. Google Analytics 4 Setup
```html
<!-- Google Analytics 4 - Async loading -->
<script async src="https://www.googletagmanager.com/gtag/js?id=G-XXXXXXXXXX"></script>
<script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    gtag('config', 'G-XXXXXXXXXX', {
        page_title: 'ConciliaExtrato - Pré-lançamento',
        page_location: 'https://conciliaextrato.com',
        send_page_view: true,
        // Brazilian market focus
        custom_map: {
            'custom_parameter_1': 'brazilian_market',
            'custom_parameter_2': 'accounting_professionals'
        }
    });
</script>
```

### 2. HTMX Event Tracking
```javascript
// Track email submissions via HTMX
document.body.addEventListener('htmx:afterSettle', (event) => {
    if (event.detail.target.id === 'form-response') {
        const isSuccess = event.detail.target.querySelector('.form-message.success');
        const isError = event.detail.target.querySelector('.form-message.error');
        const isWarning = event.detail.target.querySelector('.form-message.warning');
        
        if (isSuccess) {
            gtag('event', 'email_signup', {
                event_category: 'engagement',
                event_label: 'prelaunch_email',
                value: 1,
                custom_parameter_1: 'brazilian_market'
            });
        } else if (isError) {
            gtag('event', 'email_signup_error', {
                event_category: 'error',
                event_label: 'prelaunch_email_error',
                value: 0
            });
        } else if (isWarning) {
            gtag('event', 'email_duplicate', {
                event_category: 'engagement',
                event_label: 'prelaunch_email_duplicate',
                value: 1
            });
        }
    }
});

// Track form interactions
document.body.addEventListener('htmx:beforeRequest', (event) => {
    if (event.detail.target.id === 'form-response') {
        gtag('event', 'form_submission_start', {
            event_category: 'engagement',
            event_label: 'prelaunch_form_start'
        });
    }
});

// Track demo page visits
document.addEventListener('DOMContentLoaded', function() {
    const demoLinks = document.querySelectorAll('a[href*="/product/demo"]');
    demoLinks.forEach(link => {
        link.addEventListener('click', function() {
            gtag('event', 'demo_page_visit', {
                event_category: 'navigation',
                event_label: 'prelaunch_to_demo',
                value: 1
            });
        });
    });
});
```

### 3. Core Web Vitals Tracking
```javascript
// Core Web Vitals tracking
import('https://unpkg.com/web-vitals@3/dist/web-vitals.js').then(({ getCLS, getFID, getFCP, getLCP, getTTFB }) => {
    // Largest Contentful Paint
    getLCP((metric) => {
        gtag('event', 'web_vitals', {
            event_category: 'performance',
            event_label: 'LCP',
            value: Math.round(metric.value),
            custom_parameter_1: 'brazilian_market'
        });
    });
    
    // First Input Delay
    getFID((metric) => {
        gtag('event', 'web_vitals', {
            event_category: 'performance',
            event_label: 'FID',
            value: Math.round(metric.value),
            custom_parameter_1: 'brazilian_market'
        });
    });
    
    // Cumulative Layout Shift
    getCLS((metric) => {
        gtag('event', 'web_vitals', {
            event_category: 'performance',
            event_label: 'CLS',
            value: Math.round(metric.value * 1000), // Convert to integer
            custom_parameter_1: 'brazilian_market'
        });
    });
    
    // First Contentful Paint
    getFCP((metric) => {
        gtag('event', 'web_vitals', {
            event_category: 'performance',
            event_label: 'FCP',
            value: Math.round(metric.value),
            custom_parameter_1: 'brazilian_market'
        });
    });
    
    // Time to First Byte
    getTTFB((metric) => {
        gtag('event', 'web_vitals', {
            event_category: 'performance',
            event_label: 'TTFB',
            value: Math.round(metric.value),
            custom_parameter_1: 'brazilian_market'
        });
    });
});
```

## Monitoring Implementation

### 1. Health Check Endpoint
```rust
// crates/site/src/handlers.rs
use axum::{response::Json, http::StatusCode};
use serde_json::json;
use chrono::Utc;

pub async fn health_check() -> impl IntoResponse {
    Json(json!({
        "status": "healthy",
        "timestamp": Utc::now().to_rfc3339(),
        "version": env!("CARGO_PKG_VERSION"),
        "service": "conciliaextrato-prelaunch",
        "uptime": get_uptime(),
        "database": check_database_health().await,
        "memory_usage": get_memory_usage()
    }))
}

async fn check_database_health() -> String {
    // Check PostgreSQL connection
    match sqlx::PgPool::connect(&std::env::var("DATABASE_URL").unwrap()).await {
        Ok(_) => "healthy".to_string(),
        Err(_) => "unhealthy".to_string(),
    }
}

fn get_uptime() -> u64 {
    // Get system uptime
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

fn get_memory_usage() -> u64 {
    // Get memory usage in MB
    // Implementation depends on system monitoring library
    0
}
```

### 2. Prometheus Metrics
```rust
// crates/site/src/metrics.rs
use prometheus::{Counter, Histogram, register_counter, register_histogram, Registry};
use lazy_static::lazy_static;

lazy_static! {
    static ref REGISTRY: Registry = Registry::new();
    
    // Lead capture metrics
    static ref LEAD_CAPTURE_COUNTER: Counter = register_counter!(
        "conciliaextrato_leads_total", 
        "Total number of lead captures"
    ).unwrap();
    
    static ref LEAD_CAPTURE_DURATION: Histogram = register_histogram!(
        "conciliaextrato_lead_capture_duration_seconds",
        "Time taken to process lead capture"
    ).unwrap();
    
    // Page view metrics
    static ref PAGE_VIEWS_COUNTER: Counter = register_counter!(
        "conciliaextrato_page_views_total",
        "Total number of page views"
    ).unwrap();
    
    // Error metrics
    static ref ERROR_COUNTER: Counter = register_counter!(
        "conciliaextrato_errors_total",
        "Total number of errors"
    ).unwrap();
    
    // Performance metrics
    static ref RESPONSE_TIME: Histogram = register_histogram!(
        "conciliaextrato_response_time_seconds",
        "Response time for HTTP requests"
    ).unwrap();
}

pub fn register_metrics() {
    REGISTRY.register(Box::new(LEAD_CAPTURE_COUNTER.clone())).unwrap();
    REGISTRY.register(Box::new(LEAD_CAPTURE_DURATION.clone())).unwrap();
    REGISTRY.register(Box::new(PAGE_VIEWS_COUNTER.clone())).unwrap();
    REGISTRY.register(Box::new(ERROR_COUNTER.clone())).unwrap();
    REGISTRY.register(Box::new(RESPONSE_TIME.clone())).unwrap();
}

pub fn increment_lead_capture() {
    LEAD_CAPTURE_COUNTER.inc();
}

pub fn record_lead_capture_duration(duration: f64) {
    LEAD_CAPTURE_DURATION.observe(duration);
}

pub fn increment_page_views() {
    PAGE_VIEWS_COUNTER.inc();
}

pub fn increment_errors() {
    ERROR_COUNTER.inc();
}

pub fn record_response_time(duration: f64) {
    RESPONSE_TIME.observe(duration);
}
```

### 3. Metrics Endpoint
```rust
// crates/site/src/handlers.rs
use prometheus::Encoder;

pub async fn metrics() -> impl IntoResponse {
    let encoder = prometheus::TextEncoder::new();
    let metric_families = REGISTRY.gather();
    let mut buffer = Vec::new();
    encoder.encode(&metric_families, &mut buffer).unwrap();
    
    (
        StatusCode::OK,
        [("content-type", "text/plain; version=0.0.4; charset=utf-8")],
        buffer,
    )
}
```

### 4. Middleware for Metrics
```rust
// crates/site/src/middleware.rs
use axum::{
    extract::Request,
    middleware::Next,
    response::Response,
};
use std::time::Instant;

pub async fn metrics_middleware(request: Request, next: Next) -> Response {
    let start = Instant::now();
    
    // Increment page views
    metrics::increment_page_views();
    
    let response = next.run(request).await;
    
    // Record response time
    let duration = start.elapsed().as_secs_f64();
    metrics::record_response_time(duration);
    
    response
}
```

## Key Performance Indicators (KPIs)

### 1. Technical KPIs
- **Lighthouse Performance**: 100/100
- **Lighthouse Accessibility**: 100/100
- **Lighthouse Best Practices**: 100/100
- **Lighthouse SEO**: 100/100
- **Core Web Vitals**: All green (LCP < 2.5s, FID < 100ms, CLS < 0.1)

### 2. Business KPIs
- **Lead Capture Rate**: > 5% of visitors
- **Lead Volume**: 100 leads/month in first quarter
- **Conversion Rate**: > 20% of leads become customers
- **Demo Access Rate**: > 20% of leads access demo
- **Brazilian Market Focus**: > 80% of leads from Brazil

### 3. Analytics KPIs
- **Page Views**: Track daily/weekly/monthly trends
- **Bounce Rate**: < 40% for pre-launch page
- **Time on Page**: > 2 minutes average
- **Form Completion Rate**: Track form abandonment
- **Error Rate**: < 1% of form submissions

## Monitoring Dashboard

### 1. Real-time Metrics
- Lead capture rate (per hour/day)
- Page views and unique visitors
- Core Web Vitals scores
- Error rates and response times
- Database health status

### 2. Brazilian Market Analytics
- Geographic distribution of leads
- Portuguese language engagement
- Accounting professional demographics
- Mobile vs desktop usage patterns
- Peak usage times (Brazilian timezone)

### 3. Conversion Funnel Analysis
```
Page View → Form Start → Form Complete → Demo Visit → Customer
    100%        80%          5%           20%         20%
```

## Alerting Configuration

### 1. Critical Alerts
- **System Down**: Health check endpoint returns error
- **High Error Rate**: > 5% error rate for 5 minutes
- **Performance Degradation**: Core Web Vitals below thresholds
- **Database Issues**: Connection failures or slow queries

### 2. Business Alerts
- **Low Lead Volume**: < 10 leads per day
- **High Bounce Rate**: > 60% bounce rate
- **Form Issues**: > 10% form submission failures
- **Demo Access Drop**: < 15% demo access rate

## Implementation Checklist

### Analytics Setup
- [ ] Configure Google Analytics 4 property
- [ ] Set up Brazilian market custom parameters
- [ ] Implement HTMX event tracking
- [ ] Add Core Web Vitals tracking
- [ ] Test all analytics events

### Monitoring Setup
- [ ] Implement health check endpoint
- [ ] Set up Prometheus metrics
- [ ] Configure metrics endpoint
- [ ] Add middleware for automatic tracking
- [ ] Set up monitoring dashboard

### Alerting Setup
- [ ] Configure critical system alerts
- [ ] Set up business metric alerts
- [ ] Test alert notifications
- [ ] Configure escalation procedures
- [ ] Document alert response procedures

### Testing & Validation
- [ ] Test all analytics events in staging
- [ ] Validate Core Web Vitals tracking
- [ ] Test health check endpoint
- [ ] Verify Prometheus metrics collection
- [ ] Test alerting system

This comprehensive analytics and monitoring setup ensures complete visibility into the ConciliaExtrato pre-launch page performance, user behavior, and business metrics, with special focus on the Brazilian market.
