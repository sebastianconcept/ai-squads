# BankFlow Design System Implementation Guide

> **Version**: 1.0.0  
> **Last Updated**: December 2024  
> **Tech Stack**: Rust + HTMX + Alpine.js + Askama Templates + Custom CSS

## Overview

This guide provides comprehensive instructions for implementing the BankFlow design system across all pages and SaaS components. The design system is built around a modern dark theme with vibrant green accents, emphasizing glassmorphism effects and smooth interactions.

## Quick Start

### 1. Include Design System CSS

Add the design system CSS variables and base styles to your main CSS file:

```css
/* Design System CSS Variables */
:root {
  /* Colors */
  --color-bg-primary: #000000;
  --color-bg-surface: #1A1A1A;
  --color-bg-interactive: rgba(255, 255, 255, 0.1);
  --color-text-primary: #FFFFFF;
  --color-text-secondary: #CCCCCC;
  --color-text-muted: #999999;
  --color-accent-green: #66FF66;
  --color-accent-green-hover: #00E600;
  --color-accent-green-active: #00CC00;

  /* Spacing */
  --spacing-xs: 4px;
  --spacing-sm: 8px;
  --spacing-md: 16px;
  --spacing-lg: 24px;
  --spacing-xl: 32px;
  --spacing-2xl: 48px;
  --spacing-3xl: 64px;
  --spacing-4xl: 96px;

  /* Typography */
  --font-family-primary: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  --text-heading1: 64px;
  --text-heading2: 48px;
  --text-heading3: 32px;
  --text-heading4: 24px;
 --text-body-large: 18px;
  --text-body: 16px;
  --text-body-small: 14px;
  --text-caption: 12px;

  /* Radius */
  --radius-sm: 4px;
  --radius-md: 8px;
  --radius-lg: 12px;
  --radius-xl: 16px;
  --radius-2xl: 20px;
  --radius-pill: 9999px;

  /* Transitions */
  --transition-fast: 0.15s ease-in-out;
  --transition-normal: 0.2s ease-in-out;
  --transition-slow: 0.3s ease-in-out;
}

/* Base Styles */
* {
  box-sizing: border-box;
}

body {
  font-family: var(--font-family-primary);
  background-color: var(--color-bg-primary);
  color: var(--color-text-primary);
  margin: 0;
  padding: 0;
  line-height: 1.6;
}

/* Focus Styles */
*:focus-visible {
  outline: 2px solid var(--color-accent-green);
  outline-offset: 2px;
}

/* Reduced Motion */
@media (prefers-reduced-motion: reduce) {
  * {
    animation-duration: 0.01ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.01ms !important;
  }
}
```

### 2. Include Required Libraries

Add HTMX and Alpine.js to your Askama templates:

```html
<!-- In your base template -->
<script src="https://unpkg.com/htmx.org@1.9.10"></script>
<script src="https://unpkg.com/alpinejs@3.14.1/dist/cdn.min.js" defer></script>
```

## Component Implementation

### Buttons

#### Primary Button
```html
<button class="btn btn-primary">
  Contact Us
</button>
```

```css
.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  border: none;
  cursor: pointer;
  font-family: var(--font-family-primary);
  font-weight: 500;
  transition: var(--transition-normal);
  text-decoration: none;
  border-radius: var(--radius-pill);
}

.btn-primary {
  background-color: var(--color-accent-green);
  color: var(--color-bg-primary);
  padding: var(--spacing-sm) var(--spacing-md);
  font-size: var(--text-body);
}

.btn-primary:hover {
  background-color: var(--color-accent-green-hover);
  transform: translateY(-1px);
}

.btn-primary:active {
  background-color: var(--color-accent-green-active);
  transform: translateY(0px);
}

.btn-primary:disabled {
  background-color: var(--color-text-muted);
  cursor: not-allowed;
  transform: none;
}
```

#### Secondary Button (Glassmorphism)
```html
<button class="btn btn-secondary">
  <svg class="btn-icon" width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
    <!-- Paper plane icon -->
  </svg>
  Send Message
</button>
```

```css
.btn-secondary {
  background-color: var(--color-bg-interactive);
  color: var(--color-text-primary);
  padding: var(--spacing-sm) var(--spacing-md);
  font-size: var(--text-body);
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.btn-secondary:hover {
  background-color: rgba(255, 255, 255, 0.15);
}

.btn-secondary:active {
  transform: scale(0.98);
}

.btn-icon {
  margin-right: var(--spacing-xs);
}
```

### Input Fields

#### Email Input (Pill Style)
```html
<input type="email" 
       class="input input-pill" 
       placeholder="agency.midu@gmail.com"
       hx-post="/api/contact"
       hx-target="#contact-response"
       hx-indicator="#loading">
```

```css
.input {
  font-family: var(--font-family-primary);
  font-size: var(--text-body);
  color: var(--color-text-primary);
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: var(--transition-normal);
}

.input-pill {
  background-color: var(--color-bg-interactive);
  border-radius: var(--radius-pill);
  padding: var(--spacing-md) var(--spacing-lg);
  backdrop-filter: blur(10px);
}

.input-pill::placeholder {
  color: rgba(255, 255, 255, 0.6);
}

.input-pill:focus {
  outline: none;
  border-color: var(--color-accent-green);
  box-shadow: 0 0 0 2px rgba(102, 255, 102, 0.2);
}

.input-pill:hover {
  background-color: rgba(255, 255, 255, 0.15);
}
```

### Cards

#### Default Card
```html
<div class="card">
  <h3 class="card-title">Plan Title</h3>
  <p class="card-description">Plan description goes here</p>
  <ul class="card-features">
    <li class="feature-item">
      <svg class="feature-icon" width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
        <!-- Checkmark icon -->
      </svg>
      Feature description
    </li>
  </ul>
</div>
```

```css
.card {
  background-color: var(--color-bg-surface);
  border-radius: var(--radius-lg);
  padding: var(--spacing-lg);
  border: 1px solid rgba(255, 255, 255, 0.1);
  transition: var(--transition-normal);
}

.card:hover {
  transform: translateY(-2px);
  box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
}

.card-title {
  font-size: var(--text-heading4);
  font-weight: 600;
  color: var(--color-accent-green);
  margin: 0 0 var(--spacing-md) 0;
}

.card-description {
  font-size: var(--text-body);
  color: var(--color-text-primary);
  margin: 0 0 var(--spacing-lg) 0;
}

.card-features {
  list-style: none;
  padding: 0;
  margin: 0;
}

.feature-item {
  display: flex;
  align-items: center;
  gap: var(--spacing-sm);
  font-size: var(--text-body);
  color: var(--color-text-primary);
  margin-bottom: var(--spacing-sm);
}

.feature-icon {
  color: var(--color-accent-green);
  flex-shrink: 0;
}
```

#### Glassmorphism Card
```html
<div class="card card-glass">
  <div class="card-content">
    <!-- Card content -->
  </div>
</div>
```

```css
.card-glass {
  background-color: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(15px);
  border: 1px solid rgba(255, 255, 255, 0.2);
}
```

### Layered Stack Component

#### Green Gradient Stack
```html
<div class="layered-stack" x-data="{ hover: false }" @mouseenter="hover = true" @mouseleave="hover = false">
  <div class="stack-layer stack-base"></div>
  <div class="stack-layer stack-mid"></div>
  <div class="stack-layer stack-top"></div>
</div>
```

```css
.layered-stack {
  position: relative;
  width: 200px;
  height: 200px;
  perspective: 1000px;
  transition: var(--transition-normal);
}

.layered-stack:hover {
  transform: rotateY(5deg) rotateX(5deg);
}

.stack-layer {
  position: absolute;
  border-radius: var(--radius-2xl);
}

.stack-base {
  width: 100%;
  height: 100%;
  background-color: #009900;
  transform: translateZ(0px) rotate(0deg);
}

.stack-mid {
  width: 90%;
  height: 90%;
  background-color: #33CC33;
  transform: translateZ(10px) rotate(5deg);
  top: 5%;
  left: 5%;
}

.stack-top {
  width: 80%;
  height: 80%;
  background-color: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(15px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  transform: translateZ(20px) rotate(10deg);
  top: 10%;
  left: 10%;
}
```

### Navigation

#### Header Navigation
```html
<header class="nav-header">
  <div class="nav-container">
    <div class="nav-logo">
      <svg class="logo-icon" width="32" height="32" viewBox="0 0 24 24" fill="currentColor">
        <!-- Logo SVG -->
      </svg>
      <span class="logo-text">BankFlow</span>
    </div>
    
    <nav class="nav-menu">
      <a href="/product" class="nav-item">Product</a>
      <a href="/pricing" class="nav-item nav-item-active">Pricing</a>
      <a href="/about" class="nav-item">About</a>
    </nav>
    
    <button class="btn btn-primary">Contact Us</button>
  </div>
</header>
```

```css
.nav-header {
  height: 80px;
  background-color: transparent;
  position: sticky;
  top: 0;
  z-index: 1000;
  backdrop-filter: blur(10px);
}

.nav-container {
  max-width: 1280px;
  margin: 0 auto;
  padding: 0 var(--spacing-lg);
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.nav-logo {
  display: flex;
  align-items: center;
  gap: var(--spacing-sm);
}

.logo-icon {
  color: var(--color-accent-green);
}

.logo-text {
  font-size: var(--text-body-large);
  font-weight: 600;
  color: var(--color-text-primary);
}

.nav-menu {
  display: flex;
  align-items: center;
  gap: var(--spacing-lg);
}

.nav-item {
  color: var(--color-text-primary);
  text-decoration: none;
  font-size: var(--text-body);
  font-weight: 500;
  padding: var(--spacing-sm) var(--spacing-md);
  border-radius: var(--radius-md);
  transition: var(--transition-normal);
}

.nav-item:hover {
  background-color: var(--color-bg-interactive);
}

.nav-item-active {
  background-color: rgba(255, 255, 255, 0.2);
}
```

### Social Buttons

#### Social Media Links
```html
<div class="social-links">
  <a href="https://behance.net/midustudio" class="social-btn">
    <svg class="social-icon" width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
      <!-- Behance icon -->
    </svg>
    @midustudio
  </a>
  
  <a href="https://instagram.com/olesiadudorkina" class="social-btn">
    <svg class="social-icon" width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
      <!-- Instagram icon -->
    </svg>
    @olesiadudorkina
  </a>
</div>
```

```css
.social-links {
  display: flex;
  gap: var(--spacing-md);
  flex-wrap: wrap;
}

.social-btn {
  display: flex;
  align-items: center;
  gap: var(--spacing-xs);
  background-color: var(--color-bg-interactive);
  color: var(--color-text-primary);
  border-radius: var(--radius-pill);
  padding: var(--spacing-sm) var(--spacing-md);
  font-size: var(--text-body-small);
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  transition: var(--transition-normal);
  text-decoration: none;
}

.social-btn:hover {
  background-color: rgba(255, 255, 255, 0.15);
  transform: translateY(-1px);
}

.social-btn:active {
  transform: scale(0.98);
}

.social-icon {
  flex-shrink: 0;
}
```

### Credit Card Display

#### Credit Card Component
```html
<div class="credit-card-display">
  <div class="card-stack">
    <div class="credit-card card-1">
      <div class="card-header">
        <span class="card-brand">PimBank</span>
        <svg class="card-icon" width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
          <!-- WiFi/NFC icon -->
        </svg>
      </div>
      <div class="card-number">722 **** 9133</div>
    </div>
    
    <div class="credit-card card-2">
      <div class="card-header">
        <span class="card-brand">PimBank</span>
        <svg class="card-icon" width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
          <!-- WiFi/NFC icon -->
        </svg>
      </div>
      <div class="card-number">722 **** 9133</div>
    </div>
  </div>
</div>
```

```css
.credit-card-display {
  width: 100px;
  height: 100px;
  border-radius: var(--radius-lg);
  background-color: var(--color-bg-surface);
  overflow: hidden;
  position: relative;
  border: 1px solid rgba(255, 255, 255, 0.1);
}

.card-stack {
  position: relative;
  width: 100%;
  height: 100%;
}

.credit-card {
  position: absolute;
  width: 80px;
  height: 50px;
  border-radius: var(--radius-md);
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  padding: var(--spacing-xs);
  font-size: 8px;
  color: var(--color-text-primary);
}

.card-1 {
  background: linear-gradient(135deg, #3B82F6 0%, #10B981 100%);
  transform: rotate(-5deg) translate(5px, 10px);
  z-index: 2;
}

.card-2 {
  background: linear-gradient(135deg, #8B5CF6 0%, #3B82F6 100%);
  transform: rotate(5deg) translate(10px, 5px);
  z-index: 1;
}

.card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.card-brand {
  font-weight: 600;
  font-size: 7px;
}

.card-icon {
  opacity: 0.8;
}

.card-number {
  font-family: var(--font-family-mono);
  font-size: 7px;
  letter-spacing: 0.5px;
}
```

## Layout Patterns

### Centered Section Layout
```html
<section class="section-centered">
  <div class="container">
    <h1 class="heading-primary">Get in Touch</h1>
    <div class="content-center">
      <!-- Main content -->
    </div>
  </div>
</section>
```

```css
.section-centered {
  padding: var(--spacing-4xl) 0;
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
}

.container {
  max-width: 1280px;
  margin: 0 auto;
  padding: 0 var(--spacing-lg);
}

.heading-primary {
  font-size: var(--text-heading1);
  font-weight: 700;
  color: var(--color-text-primary);
  text-align: center;
  margin: 0 0 var(--spacing-2xl) 0;
  letter-spacing: -0.02em;
}

.content-center {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: var(--spacing-xl);
}
```

### Grid Layouts
```html
<!-- Three Column Grid -->
<div class="grid grid-three">
  <div class="grid-item">Item 1</div>
  <div class="grid-item">Item 2</div>
  <div class="grid-item">Item 3</div>
</div>
```

```css
.grid {
  display: grid;
  gap: var(--spacing-xl);
}

.grid-three {
  grid-template-columns: repeat(3, 1fr);
}

.grid-item {
  /* Grid item styles */
}

/* Responsive Grid */
@media (max-width: 768px) {
  .grid-three {
    grid-template-columns: 1fr;
  }
}

@media (min-width: 769px) and (max-width: 1023px) {
  .grid-three {
    grid-template-columns: repeat(2, 1fr);
  }
}
```

## HTMX Integration

### Form Submission with HTMX
```html
<form hx-post="/api/contact" 
      hx-target="#contact-response" 
      hx-indicator="#loading-spinner"
      class="contact-form">
  <input type="email" 
         name="email" 
         class="input input-pill" 
         placeholder="Enter your email"
         required>
  <button type="submit" class="btn btn-primary">
    Subscribe
  </button>
</form>

<div id="contact-response" class="response-container">
  <!-- Response will be inserted here -->
</div>

<div id="loading-spinner" class="htmx-indicator">
  <div class="spinner"></div>
</div>
```

```css
.contact-form {
  display: flex;
  gap: var(--spacing-md);
  align-items: center;
}

.response-container {
  margin-top: var(--spacing-lg);
}

.htmx-indicator {
  display: none;
}

.htmx-request .htmx-indicator {
  display: block;
}

.spinner {
  width: 20px;
  height: 20px;
  border: 2px solid rgba(255, 255, 255, 0.3);
  border-top: 2px solid var(--color-accent-green);
  border-radius: 50%;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
```

### Dynamic Content Updates
```html
<div hx-get="/api/status" 
     hx-trigger="every 5s"
     hx-target="#status-display"
     class="status-container">
  <div id="status-display">
    <!-- Status will be updated here -->
  </div>
</div>
```

## Alpine.js Integration

### Reactive Form State
```html
<div x-data="{ 
  email: '', 
  isValid: false,
  checkEmail() { 
    this.isValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(this.email) 
  }
}">
  <input type="email" 
         x-model="email" 
         @input="checkEmail()"
         :class="{ 'input-valid': isValid, 'input-invalid': !isValid }"
         class="input input-pill"
         placeholder="Enter your email">
  
  <button :disabled="!isValid" 
          class="btn btn-primary">
    Subscribe
  </button>
</div>
```

```css
.input-valid {
  border-color: var(--color-accent-green);
}

.input-invalid {
  border-color: #ff6b6b;
}
```

### Loading States
```html
<div x-data="{ loading: false }" 
     @htmx:before-request="loading = true"
     @htmx:after-request="loading = false">
  
  <button hx-post="/api/action" 
          class="btn btn-primary"
          :disabled="loading">
    <span x-show="!loading">Submit</span>
    <span x-show="loading">Loading...</span>
  </button>
  
  <div x-show="loading" class="loading-overlay">
    <div class="spinner"></div>
  </div>
</div>
```

### Interactive Components
```html
<div x-data="{ 
  isOpen: false,
  toggle() { this.isOpen = !this.isOpen }
}">
  <button @click="toggle()" class="btn btn-secondary">
    Toggle Menu
  </button>
  
  <div x-show="isOpen" 
       x-transition:enter="transition ease-out duration-200"
       x-transition:enter-start="opacity-0 transform scale-95"
       x-transition:enter-end="opacity-100 transform scale-100"
       x-transition:leave="transition ease-in duration-150"
       x-transition:leave-start="opacity-100 transform scale-100"
       x-transition:leave-end="opacity-0 transform scale-95"
       class="dropdown-menu">
    <!-- Menu content -->
  </div>
</div>
```

## Askama Template Integration

### Base Template Structure
```rust
// In your Askama template struct
#[derive(Template)]
#[template(path = "base.html")]
pub struct BaseTemplate {
    pub title: String,
    pub content: String,
}
```

```html
<!-- base.html -->
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ title }}</title>
    <link rel="stylesheet" href="/static/css/design-system.css">
    <script src="https://unpkg.com/htmx.org@1.9.10"></script>
    <script src="https://unpkg.com/alpinejs@3.14.1/dist/cdn.min.js" defer></script>
</head>
<body>
    <header class="nav-header">
        <!-- Navigation content -->
    </header>
    
    <main>
        {{ content|safe }}
    </main>
    
    <footer>
        <!-- Footer content -->
    </footer>
</body>
</html>
```

### Component Templates
```rust
// Component template structs
#[derive(Template)]
#[template(path = "components/button.html")]
pub struct ButtonTemplate {
    pub variant: String,
    pub text: String,
    pub href: Option<String>,
}

#[derive(Template)]
#[template(path = "components/card.html")]
pub struct CardTemplate {
    pub title: String,
    pub description: String,
    pub features: Vec<String>,
}
```

```html
<!-- components/button.html -->
{% if href %}
<a href="{{ href }}" class="btn btn-{{ variant }}">
    {{ text }}
</a>
{% else %}
<button class="btn btn-{{ variant }}">
    {{ text }}
</button>
{% endif %}
```

## Responsive Design

### Mobile-First Approach
```css
/* Mobile styles (default) */
.hero-section {
  padding: var(--spacing-xl) 0;
  text-align: center;
}

.hero-title {
  font-size: var(--text-heading2);
  margin-bottom: var(--spacing-lg);
}

/* Tablet styles */
@media (min-width: 768px) {
  .hero-section {
    padding: var(--spacing-2xl) 0;
  }
  
  .hero-title {
    font-size: var(--text-heading1);
  }
}

/* Desktop styles */
@media (min-width: 1024px) {
  .hero-section {
    padding: var(--spacing-4xl) 0;
  }
  
  .grid-three {
    grid-template-columns: repeat(3, 1fr);
  }
}
```

### Touch-Friendly Design
```css
/* Ensure minimum touch target size */
.btn, .nav-item, .social-btn {
  min-height: 44px;
  min-width: 44px;
}

/* Improve touch interactions */
@media (hover: none) and (pointer: coarse) {
  .btn:hover {
    transform: none;
  }
  
  .card:hover {
    transform: none;
  }
}
```

## Accessibility Guidelines

### Color Contrast
- Ensure minimum 4.5:1 contrast ratio for normal text
- Use 7:1 contrast ratio for enhanced accessibility
- Test with color contrast analyzers

### Keyboard Navigation
```css
/* Focus styles */
.btn:focus-visible,
.input:focus-visible,
.nav-item:focus-visible {
  outline: 2px solid var(--color-accent-green);
  outline-offset: 2px;
}

/* Skip links */
.skip-link {
  position: absolute;
  top: -40px;
  left: 6px;
  background: var(--color-accent-green);
  color: var(--color-bg-primary);
  padding: 8px;
  text-decoration: none;
  z-index: 1000;
}

.skip-link:focus {
  top: 6px;
}
```

### Screen Reader Support
```html
<!-- Semantic HTML -->
<main>
  <section aria-labelledby="hero-title">
    <h1 id="hero-title">Get in Touch</h1>
    <p>Contact us for more information</p>
  </section>
</main>

<!-- ARIA labels -->
<button aria-label="Send message" class="btn btn-primary">
  <svg aria-hidden="true" class="btn-icon">
    <!-- Icon -->
  </svg>
</button>

<!-- Form labels -->
<label for="email-input">Email Address</label>
<input id="email-input" type="email" class="input input-pill">
```

## Performance Optimization

### CSS Optimization
```css
/* Use CSS custom properties for theming */
:root {
  --theme-primary: #66FF66;
  --theme-bg: #000000;
}

/* Optimize animations */
.btn {
  will-change: transform;
}

.btn:hover {
  transform: translateY(-1px);
}

/* Remove will-change after animation */
.btn:not(:hover) {
  will-change: auto;
}
```

### Image Optimization
```html
<!-- Responsive images -->
<img src="/static/images/hero-mobile.jpg" 
     srcset="/static/images/hero-mobile.jpg 768w,
             /static/images/hero-desktop.jpg 1024w"
     sizes="(max-width: 768px) 100vw, 50vw"
     alt="Hero image"
     loading="lazy">
```

## Testing Guidelines

### Visual Testing
- Test on multiple devices and browsers
- Verify color contrast ratios
- Check responsive breakpoints
- Validate accessibility with screen readers

### Component Testing
```rust
// Rust integration tests
#[tokio::test]
async fn test_button_rendering() {
    let button = ButtonTemplate {
        variant: "primary".to_string(),
        text: "Click me".to_string(),
        href: None,
    };
    
    let html = button.render().unwrap();
    assert!(html.contains("btn btn-primary"));
    assert!(html.contains("Click me"));
}
```

## Common Patterns

### Loading States
```html
<div hx-post="/api/upload" 
     hx-indicator="#upload-progress"
     class="upload-form">
  <input type="file" name="file" class="input">
  <button type="submit" class="btn btn-primary">
    Upload File
  </button>
</div>

<div id="upload-progress" class="htmx-indicator">
  <div class="progress-bar">
    <div class="progress-fill"></div>
  </div>
</div>
```

### Error Handling
```html
<div hx-post="/api/submit" 
     hx-target="#form-response"
     class="form-container">
  <form>
    <input type="email" name="email" class="input input-pill" required>
    <button type="submit" class="btn btn-primary">Submit</button>
  </form>
  
  <div id="form-response" class="response-container">
    <!-- Success/error messages appear here -->
  </div>
</div>
```

### Success States
```html
<div class="success-message">
  <svg class="success-icon" width="24" height="24" viewBox="0 0 24 24" fill="currentColor">
    <!-- Checkmark icon -->
  </svg>
  <span>Success! Your message has been sent.</span>
</div>
```

```css
.success-message {
  display: flex;
  align-items: center;
  gap: var(--spacing-sm);
  color: var(--color-accent-green);
  font-size: var(--text-body);
  padding: var(--spacing-md);
  background-color: rgba(102, 255, 102, 0.1);
  border-radius: var(--radius-md);
  border: 1px solid rgba(102, 255, 102, 0.3);
}

.success-icon {
  flex-shrink: 0;
}
```

## Troubleshooting

### Common Issues

1. **HTMX not working**: Ensure HTMX is loaded before Alpine.js
2. **Alpine.js not reactive**: Check for JavaScript errors in console
3. **CSS variables not working**: Verify CSS is loaded and variables are defined
4. **Glassmorphism not showing**: Check browser support for `backdrop-filter`

### Debug Tools
```html
<!-- HTMX debugging -->
<script>
  document.body.addEventListener('htmx:beforeRequest', function(evt) {
    console.log('HTMX Request:', evt.detail);
  });
</script>

<!-- Alpine.js debugging -->
<script>
  document.addEventListener('alpine:init', () => {
    Alpine.store('debug', {
      log: (message) => console.log('Alpine Debug:', message)
    });
  });
</script>
```

## Best Practices

1. **Consistency**: Always use design system tokens
2. **Accessibility**: Test with keyboard navigation and screen readers
3. **Performance**: Optimize images and minimize CSS/JS
4. **Progressive Enhancement**: Ensure functionality without JavaScript
5. **Mobile-First**: Design for mobile, enhance for desktop
6. **Testing**: Test across devices and browsers regularly

This design system provides a solid foundation for building consistent, accessible, and performant interfaces for the BankFlow platform. Follow these guidelines to maintain design consistency across all pages and components.

