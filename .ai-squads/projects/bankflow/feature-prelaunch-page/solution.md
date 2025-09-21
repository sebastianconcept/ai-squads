---
description: Feature Solution - Pre-launch Page
type: feature-solution
status: planned
---

# Solution: Pre-launch Page

## Solution Overview
Create a high-performance pre-launch page for ConciliaExtrato.com that serves as the main landing page (`/`) with advanced lead capture functionality, while reorganizing existing content to `/product` and `/product/demo` routes. The solution uses Rust + HTMX + Alpine.js stack optimized for Lighthouse 100/100 scores with comprehensive SEO and Core Web Vitals optimization.

## Technical Approach

### 1. Route Restructuring
- **New main route (`/`)**: ConciliaExtrato pre-launch page with HTMX lead capture
- **Product route (`/product`)**: Move current landing page content
- **Demo route (`/product/demo`)**: Move current demo page
- **Lead capture endpoint (`/prelaunch/register`)**: HTMX-powered email collection
- **Health check (`/health`)**: Performance monitoring endpoint

### 2. Template System Updates (Askama + Rust)
- **New template**: `prelaunch.html` with ConciliaExtrato branding and glass morphism
- **Updated template**: `product.html` (renamed from `landing.html`)
- **Updated template**: `product-demo.html` (renamed from `demo.html`)
- **Template routing**: Update handlers to serve correct templates with SEO optimization
- **Structured data**: JSON-LD schema for software application

### 3. Lead Capture Implementation (HTMX + Alpine.js)
- **Frontend**: HTMX-powered email capture with Alpine.js animations
- **Backend**: Rust Axum endpoint with PostgreSQL storage
- **Database**: PostgreSQL with proper indexing and constraints
- **Validation**: Client-side (Alpine.js) and server-side (Rust) validation
- **Loading States**: Elegant spinner with Alpine.js transitions
- **HTMX Integration**: Seamless form submission with server-side rendering
- **Animations**: Glass morphism effects and smooth transitions

### 4. Design Considerations (ConciliaExtrato Branding)
- **Pre-launch messaging**: "ConciliaExtrato - Conciliação Automática de Extratos Bancários"
- **Lead capture CTA**: "Seja o primeiro a saber quando estiver disponível"
- **Geographic focus**: Brazilian market with Portuguese language
- **Professional positioning**: Targets contadores brasileiros (Brazilian accountants)
- **Design system**: Dark theme with blue primary (#3B82F6) and glass morphism effects
- **Typography**: Inter font family with gradient text effects
- **Loading states**: Alpine.js powered spinner with smooth transitions
- **Success animations**: Celebratory feedback with Alpine.js transitions
- **Responsive design**: Mobile-first with clamp() for fluid typography
- **Performance**: Critical CSS inline, non-critical CSS async loaded

## User Experience

### Pre-launch Page Flow (ConciliaExtrato)
1. **Landing**: User sees ConciliaExtrato pre-launch page with glass morphism design
2. **Interest**: User reads about automatic bank statement reconciliation
3. **Capture**: User enters email with HTMX-powered form
4. **Confirmation**: User receives confirmation with Alpine.js animations
5. **Navigation**: User can access product demo via `/product/demo`

### Lead Capture Experience (HTMX + Alpine.js)
- **Simple form**: Email address with Alpine.js validation
- **Compelling copy**: "Toda a informação dos seus extratos bancários consolidada em um só lugar"
- **Mystery element**: "Estamos trabalhando para tornar isso realidade para todos os bancos"
- **Clear value**: "Seja o primeiro a saber quando estiver disponível"
- **HTMX submission**: Server-side rendering with seamless form submission
- **Loading feedback**: Alpine.js spinner with smooth transitions
- **Success animation**: Celebratory feedback with Alpine.js transitions
- **Privacy assurance**: LGPD compliance and clear data usage
- **Immediate feedback**: HTMX-powered success/error messages
- **Text rotation**: Dynamic headline rotation with Alpine.js

## Implementation Plan

### Phase 1: Route Restructuring
1. Create new pre-launch template
2. Update routing in `main.rs`
3. Move existing templates to new routes
4. Update navigation links

### Phase 2: Lead Capture System
1. Create lead capture API endpoint
2. Implement email validation
3. Add lead storage mechanism
4. Create frontend form with validation

### Phase 3: Design and Content
1. Design pre-launch page layout
2. Create compelling pre-launch copy
3. Add lead capture form styling
4. Implement responsive design

### Phase 4: Testing and Validation
1. Test all route changes
2. Validate lead capture functionality
3. Test email validation
4. Verify existing demo functionality

## Dependencies (Enhanced Stack)
- **Askama templates**: For HTML templating with SEO optimization
- **Axum routing**: For route management with middleware
- **Serde**: For JSON serialization and form handling
- **SQLx**: For PostgreSQL database operations
- **HTMX**: For seamless form submission and server-side rendering
- **Alpine.js**: For client-side interactivity and animations
- **Email validation**: For lead capture validation (client + server)
- **PostgreSQL**: For lead storage with proper indexing
- **Nginx**: For static file serving and performance optimization
- **Docker**: For containerized deployment

## Risks and Mitigation

### Risk: Breaking existing functionality
- **Mitigation**: Thorough testing of all route changes and demo functionality

### Risk: Poor lead capture conversion
- **Mitigation**: A/B testing of different form designs and copy

### Risk: Email validation issues
- **Mitigation**: Robust client and server-side validation

### Risk: Performance impact
- **Mitigation**: Minimal additional dependencies and efficient implementation

### Risk: SEO impact from route changes
- **Mitigation**: Proper redirects and meta tags for new routes
