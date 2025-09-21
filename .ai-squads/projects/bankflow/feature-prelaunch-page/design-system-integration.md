---
description: ConciliaExtrato Design System Integration
type: design-system-specification
status: planned
---

# ConciliaExtrato Design System Integration

## Overview
This document provides the complete design system integration for the ConciliaExtrato pre-launch page, including all design tokens, components, and styling specifications from the comprehensive ConciliaExtrato guide.

## Design Tokens

### Color Palette
```css
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
    
    /* Spacing */
    --spacing-xs: 0.25rem;
    --spacing-sm: 0.5rem;
    --spacing-md: 1rem;
    --spacing-lg: 1.5rem;
    --spacing-xl: 2rem;
    --spacing-2xl: 3rem;
    
    /* Border Radius */
    --radius-sm: 0.375rem;
    --radius-md: 0.5rem;
    --radius-lg: 0.75rem;
    --radius-xl: 1rem;
    
    /* Shadows */
    --shadow-sm: 0 1px 2px rgba(0, 0, 0, 0.05);
    --shadow-md: 0 4px 6px rgba(0, 0, 0, 0.1);
    --shadow-lg: 0 10px 15px rgba(0, 0, 0, 0.1);
    --shadow-xl: 0 20px 25px rgba(0, 0, 0, 0.1);
    
    /* Glass Effects */
    --glass-bg: rgba(255, 255, 255, 0.05);
    --glass-border: rgba(255, 255, 255, 0.1);
    --glass-blur: blur(20px);
    
    /* Transitions */
    --transition-fast: 0.15s ease;
    --transition-normal: 0.3s ease;
    --transition-slow: 0.5s ease;
}
```

## Typography System

### Font Loading (Performance Optimized)
```html
<!-- Fonts (optimized) -->
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="preload" href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" as="style" onload="this.onload=null;this.rel='stylesheet'">
<noscript><link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"></noscript>
```

### Typography Scale
```css
/* Typography Scale */
.text-xs { font-size: 0.75rem; line-height: 1rem; }
.text-sm { font-size: 0.875rem; line-height: 1.25rem; }
.text-base { font-size: 1rem; line-height: 1.5rem; }
.text-lg { font-size: 1.125rem; line-height: 1.75rem; }
.text-xl { font-size: 1.25rem; line-height: 1.75rem; }
.text-2xl { font-size: 1.5rem; line-height: 2rem; }
.text-3xl { font-size: 1.875rem; line-height: 2.25rem; }
.text-4xl { font-size: 2.25rem; line-height: 2.5rem; }
.text-5xl { font-size: 3rem; line-height: 1; }

/* Hero Title with Gradient */
.hero-title {
    font-family: var(--font-primary);
    font-size: clamp(2rem, 5vw, 3.5rem);
    font-weight: 700;
    line-height: 1.1;
    background: linear-gradient(135deg, var(--white) 0%, var(--blue-primary) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    will-change: contents;
}

/* Responsive Typography */
@media (max-width: 768px) {
    .hero-title {
        font-size: 2.5rem;
        line-height: 1.2;
    }
}
```

## Component Library

### 1. Glass Card Component
```css
.glass-card {
    background: var(--glass-bg);
    backdrop-filter: var(--glass-blur);
    border: 1px solid var(--glass-border);
    border-radius: var(--radius-xl);
    will-change: transform;
    transition: all var(--transition-normal);
}

.glass-card:hover {
    background: rgba(255, 255, 255, 0.08);
    border-color: rgba(255, 255, 255, 0.15);
    transform: translateY(-2px);
}
```

### 2. Navbar Component
```css
.navbar-glass {
    position: fixed;
    top: 0;
    width: 100%;
    background: rgba(15, 23, 42, 0.8);
    backdrop-filter: var(--glass-blur);
    border-bottom: 1px solid var(--glass-border);
    padding: var(--spacing-md) 0;
    z-index: 1000;
    transition: all var(--transition-normal);
}

.navbar-glass.scrolled {
    background: rgba(15, 23, 42, 0.95);
    padding: var(--spacing-sm) 0;
}

.nav-content {
    display: flex;
    justify-content: space-between;
    align-items: center;
}

.logo-text {
    color: var(--white);
    font-weight: 700;
}

.logo-accent {
    color: var(--blue-primary);
    font-weight: 700;
}
```

### 3. Form Components

#### Email Input
```css
.email-input {
    background: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(255, 255, 255, 0.2);
    border-radius: var(--radius-lg);
    padding: 0.875rem 1rem;
    color: var(--white);
    font-size: 1rem;
    backdrop-filter: blur(10px);
    transition: all var(--transition-fast);
    flex: 1;
}

.email-input:focus {
    outline: none;
    border-color: var(--blue-primary);
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.2);
    background: rgba(255, 255, 255, 0.1);
}

.email-input.error {
    border-color: var(--error);
    box-shadow: 0 0 0 3px rgba(239, 68, 68, 0.2);
}

.email-input.valid {
    border-color: var(--success);
    box-shadow: 0 0 0 3px rgba(34, 197, 94, 0.2);
}
```

#### CTA Button
```css
.cta-button {
    background: linear-gradient(135deg, var(--blue-primary) 0%, var(--blue-hover) 100%);
    color: var(--white);
    border: none;
    border-radius: var(--radius-lg);
    padding: 0.875rem 1.5rem;
    font-weight: 600;
    font-size: 1rem;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: var(--spacing-sm);
    box-shadow: 0 4px 14px rgba(59, 130, 246, 0.3), inset 0 1px 0 rgba(255, 255, 255, 0.2);
    transition: all var(--transition-fast);
    white-space: nowrap;
}

.cta-button:hover:not(:disabled) {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(59, 130, 246, 0.4), inset 0 1px 0 rgba(255, 255, 255, 0.2);
}

.cta-button:disabled {
    background: #6b7280;
    cursor: not-allowed;
    transform: none;
    box-shadow: none;
}

.cta-button.loading {
    background: #6b7280;
}
```

### 4. Message Components
```css
.form-message {
    padding: var(--spacing-md);
    border-radius: var(--radius-md);
    margin-top: var(--spacing-md);
    font-weight: 500;
    transition: all var(--transition-normal);
}

.form-message.success {
    background: rgba(34, 197, 94, 0.1);
    color: var(--success);
    border: 1px solid rgba(34, 197, 94, 0.2);
}

.form-message.error {
    background: rgba(239, 68, 68, 0.1);
    color: var(--error);
    border: 1px solid rgba(239, 68, 68, 0.2);
}

.form-message.warning {
    background: rgba(245, 158, 11, 0.1);
    color: var(--warning);
    border: 1px solid rgba(245, 158, 11, 0.2);
}
```

### 5. Loading Components
```css
.spinner {
    width: 16px;
    height: 16px;
    animation: spin 1s linear infinite;
}

@keyframes spin {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
}

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
    border-radius: var(--radius-lg);
    backdrop-filter: blur(5px);
}
```

## Animation System

### 1. Gradient Orbs (Background Effects)
```css
.gradient-orb {
    position: absolute;
    border-radius: 50%;
    filter: blur(60px);
    opacity: 0.3;
    will-change: transform;
    contain: layout style paint;
}

.orb-1 {
    width: 300px;
    height: 300px;
    background: radial-gradient(circle, var(--blue-primary) 0%, transparent 70%);
    top: 20%;
    left: 10%;
    animation: float 6s ease-in-out infinite;
}

.orb-2 {
    width: 200px;
    height: 200px;
    background: radial-gradient(circle, var(--blue-hover) 0%, transparent 70%);
    top: 60%;
    right: 15%;
    animation: float 8s ease-in-out infinite reverse;
}

.orb-3 {
    width: 150px;
    height: 150px;
    background: radial-gradient(circle, var(--white) 0%, transparent 70%);
    bottom: 20%;
    left: 20%;
    animation: float 10s ease-in-out infinite;
}

@keyframes float {
    0%, 100% { transform: translate3d(0, 0, 0); }
    33% { transform: translate3d(10px, -20px, 0); }
    66% { transform: translate3d(-10px, 10px, 0); }
}
```

### 2. Success Animation
```css
.success-icon {
    font-size: 4rem;
    margin-bottom: var(--spacing-lg);
    animation: celebrate 0.6s ease-out;
}

@keyframes celebrate {
    0% { transform: scale(0); opacity: 0; }
    50% { transform: scale(1.2); opacity: 1; }
    100% { transform: scale(1); opacity: 1; }
}
```

### 3. Slide In Animation
```css
@keyframes slideIn {
    from { opacity: 0; transform: translateY(20px); }
    to { opacity: 1; transform: translateY(0); }
}

.slide-in {
    animation: slideIn 0.5s ease-out;
}
```

## Layout System

### 1. Container
```css
.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 0 var(--spacing-lg);
}

@media (max-width: 768px) {
    .container {
        padding: 0 var(--spacing-md);
    }
}
```

### 2. Grid System
```css
.features-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: var(--spacing-xl);
    margin-top: var(--spacing-2xl);
}

@media (max-width: 768px) {
    .features-grid {
        grid-template-columns: 1fr;
        gap: var(--spacing-lg);
    }
}
```

### 3. Flexbox Utilities
```css
.flex { display: flex; }
.flex-col { flex-direction: column; }
.items-center { align-items: center; }
.justify-center { justify-content: center; }
.justify-between { justify-content: space-between; }
.gap-sm { gap: var(--spacing-sm); }
.gap-md { gap: var(--spacing-md); }
.gap-lg { gap: var(--spacing-lg); }
```

## Responsive Design

### Breakpoints
```css
/* Mobile First Approach */
@media (min-width: 640px) { /* sm */ }
@media (min-width: 768px) { /* md */ }
@media (min-width: 1024px) { /* lg */ }
@media (min-width: 1280px) { /* xl */ }
@media (min-width: 1536px) { /* 2xl */ }
```

### Responsive Utilities
```css
@media (max-width: 768px) {
    .form-group {
        flex-direction: column;
        gap: var(--spacing-md);
    }
    
    .cta-button {
        justify-content: center;
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
```

## Accessibility Features

### 1. Focus Management
```css
.focus-visible {
    outline: 2px solid var(--blue-primary);
    outline-offset: 2px;
}

.skip-link {
    position: absolute;
    top: -40px;
    left: 6px;
    background: var(--blue-primary);
    color: var(--white);
    padding: 8px;
    text-decoration: none;
    border-radius: var(--radius-sm);
}

.skip-link:focus {
    top: 6px;
}
```

### 2. Reduced Motion Support
```css
@media (prefers-reduced-motion: reduce) {
    * {
        animation-duration: 0.01ms !important;
        animation-iteration-count: 1 !important;
        transition-duration: 0.01ms !important;
    }
}
```

### 3. High Contrast Support
```css
@media (prefers-contrast: high) {
    :root {
        --glass-bg: rgba(255, 255, 255, 0.1);
        --glass-border: rgba(255, 255, 255, 0.3);
    }
}
```

## Performance Optimizations

### 1. Critical CSS (Inline)
```css
/* Critical above-the-fold styles */
:root { /* Design tokens */ }
body { /* Base styles */ }
.hero-title { /* Hero typography */ }
.container { /* Layout */ }
```

### 2. Non-Critical CSS (Async Load)
```html
<link rel="preload" href="/static/css/styles.css" as="style" onload="this.onload=null;this.rel='stylesheet'">
<noscript><link rel="stylesheet" href="/static/css/styles.css"></noscript>
```

### 3. Will-Change Optimization
```css
.glass-card {
    will-change: transform;
}

.gradient-orb {
    will-change: transform;
    contain: layout style paint;
}

.hero-title {
    will-change: contents;
}
```

## Implementation Checklist

### Design System Integration
- [ ] Add all design tokens to CSS variables
- [ ] Implement glass morphism effects
- [ ] Add gradient orb animations
- [ ] Create component library
- [ ] Implement responsive design
- [ ] Add accessibility features
- [ ] Optimize for performance
- [ ] Test across devices and browsers

### Component Implementation
- [ ] Glass card component
- [ ] Navbar with glass effect
- [ ] Form components with validation
- [ ] Button components with states
- [ ] Message components
- [ ] Loading components
- [ ] Animation system
- [ ] Layout utilities

This comprehensive design system provides all the visual and interaction specifications needed to implement the ConciliaExtrato pre-launch page with consistent, accessible, and performant design.
