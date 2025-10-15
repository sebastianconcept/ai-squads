# UIDev Conventions Guide - Motta Design System Integration

## Overview

This guide defines the JavaScript organization conventions and Motta Design System integration patterns for BankFlow's site and saas crates. Follow these conventions to ensure maintainable, debuggable development and optimized production builds.

## JavaScript Organization Strategy

### **File Structure**

```
product/bankflow/crates/site/
├── assets/
│   ├── css/
│   │   └── main.css                    # Tailwind imports + Motta components
│   ├── js/
│   │   ├── templates/                  # Template-specific JavaScript
│   │   │   ├── prelaunch.js           # Prelaunch page functionality
│   │   │   ├── product.js             # Product page functionality
│   │   │   └── product-demo.js        # Demo page functionality
│   │   ├── shared/                     # Shared utilities and components
│   │   │   ├── utils.js               # Common utilities
│   │   │   ├── api.js                 # API communication
│   │   │   ├── components.js          # Reusable components
│   │   │   ├── validation.js          # Form validation
│   │   │   └── analytics.js           # Analytics tracking
│   │   └── app.js                      # Main application entry point
│   └── images/                         # Static images
├── dist/                               # Generated assets (gitignored)
│   ├── css/
│   │   └── main-[hash].css            # Compiled CSS with hash
│   ├── js/
│   │   ├── prelaunch-[hash].js        # Template-specific bundles
│   │   ├── product-[hash].js
│   │   ├── product-demo-[hash].js
│   │   ├── dashboard-[hash].js        # SaaS templates
│   │   ├── settings-[hash].js
│   │   ├── billing-[hash].js
│   │   └── vendor-[hash].js           # Vendor libraries
│   └── manifest.json                   # Asset manifest for Rust
├── templates/                          # Askama HTML templates
│   ├── prelaunch.html
│   ├── product.html
│   └── product-demo.html
├── package.json                        # Node.js dependencies
├── tailwind.config.js                  # Tailwind + Motta configuration
├── vite.config.js                      # Vite bundling configuration
└── postcss.config.js                   # PostCSS configuration
```

### **SaaS Crate Structure**

```
product/bankflow/crates/saas/
├── assets/
│   ├── css/
│   │   └── main.css                    # SaaS-specific Motta components
│   ├── js/
│   │   ├── templates/                  # SaaS template JavaScript
│   │   │   ├── dashboard.js           # Dashboard functionality
│   │   │   ├── settings.js            # Settings page functionality
│   │   │   └── billing.js             # Billing page functionality
│   │   └── shared/                     # SaaS-specific shared utilities
│   │       ├── charts.js              # Chart.js integration
│   │       ├── forms.js               # Form handling
│   │       └── auth.js                # Authentication utilities
│   └── images/
├── dist/                               # Generated assets
├── templates/                          # SaaS HTML templates
│   ├── dashboard.html
│   ├── settings.html
│   └── billing.html
└── [same config files as site crate]
```

## JavaScript Module Conventions

### **Template-Specific Modules**

Each template gets its own JavaScript module following this pattern:

```javascript
// assets/js/templates/prelaunch.js
import Alpine from 'alpinejs';
import { apiClient } from '../shared/api.js';
import { validateEmail } from '../shared/validation.js';
import { trackEvent } from '../shared/analytics.js';

// Alpine.js component for prelaunch page
Alpine.data('prelaunchApp', () => ({
  // State
  currentMode: 'lead',
  email: '',
  isLoading: false,
  error: null,
  
  // Computed properties
  get isEmailValid() {
    return validateEmail(this.email);
  },
  
  // Methods
  async submitEmail() {
    if (!this.isEmailValid) {
      this.error = 'Invalid email format';
      return;
    }
    
    this.isLoading = true;
    this.error = null;
    
    try {
      await apiClient.submitLead(this.email);
      trackEvent('email_submitted', { source: 'prelaunch' });
      this.email = '';
      // Show success message
    } catch (error) {
      this.error = 'Failed to submit email. Please try again.';
      console.error('Email submission failed:', error);
    } finally {
      this.isLoading = false;
    }
  },
  
  // Lifecycle
  init() {
    trackEvent('page_viewed', { page: 'prelaunch' });
  }
}));

// Export for testing
export { prelaunchApp };
```

### **Shared Module Conventions**

#### **API Client Module**
```javascript
// assets/js/shared/api.js
class ApiClient {
  constructor(baseUrl = '/api/v1') {
    this.baseUrl = baseUrl;
  }
  
  async request(endpoint, options = {}) {
    const url = `${this.baseUrl}${endpoint}`;
    const config = {
      headers: {
        'Content-Type': 'application/json',
        ...options.headers
      },
      ...options
    };
    
    try {
      const response = await fetch(url, config);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('API request failed:', error);
      throw error;
    }
  }
  
  async submitLead(email) {
    return this.request('/leads', {
      method: 'POST',
      body: JSON.stringify({ email })
    });
  }
  
  async uploadFile(file) {
    const formData = new FormData();
    formData.append('file', file);
    
    return this.request('/files/upload', {
      method: 'POST',
      body: formData,
      headers: {} // Let browser set Content-Type for FormData
    });
  }
}

export const apiClient = new ApiClient();
```

#### **Validation Module**
```javascript
// assets/js/shared/validation.js
export function validateEmail(email) {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
}

export function validateFile(file, options = {}) {
  const { maxSize = 10 * 1024 * 1024, allowedTypes = [] } = options;
  
  if (file.size > maxSize) {
    return { valid: false, error: 'File too large' };
  }
  
  if (allowedTypes.length > 0 && !allowedTypes.includes(file.type)) {
    return { valid: false, error: 'Invalid file type' };
  }
  
  return { valid: true };
}

export function validateRequired(value) {
  return value && value.trim().length > 0;
}
```

#### **Analytics Module**
```javascript
// assets/js/shared/analytics.js
export function trackEvent(eventName, properties = {}) {
  // Only track in production
  if (process.env.NODE_ENV === 'production') {
    // Google Analytics 4
    if (typeof gtag !== 'undefined') {
      gtag('event', eventName, properties);
    }
    
    // Hotjar
    if (typeof hj !== 'undefined') {
      hj('event', eventName);
    }
  }
  
  // Development logging
  if (process.env.NODE_ENV === 'development') {
    console.log('Analytics Event:', eventName, properties);
  }
}

export function trackPageView(pageName) {
  trackEvent('page_view', { page_name: pageName });
}
```

## Motta Design System Integration

### **CSS Organization**

#### **Main CSS File**
```css
/* assets/css/main.css */
@import 'tailwindcss/base';
@import 'tailwindcss/components';
@import 'tailwindcss/utilities';

/* Motta Design System Components */
@layer components {
  /* Glassmorphism Cards */
  .glass-card {
    @apply backdrop-blur-glass bg-white/10 border border-white/20 rounded-2xl;
  }
  
  .glass-card-dark {
    @apply backdrop-blur-glass bg-black/20 border border-white/10 rounded-2xl;
  }
  
  /* Motta Buttons */
  .btn-primary {
    @apply px-6 py-3 bg-gradient-to-r from-accent-lime to-primary-500 text-white rounded-xl font-semibold hover:scale-105 transition-transform;
  }
  
  .btn-secondary {
    @apply px-6 py-3 bg-transparent border border-white/20 text-white hover:bg-white/5 rounded-xl font-semibold transition-colors;
  }
  
  /* BankFlow Specific Components */
  .file-upload-zone {
    @apply glass-card p-8 border-2 border-dashed border-primary-500/30 hover:border-primary-500/50 transition-colors;
  }
  
  .processing-indicator {
    @apply flex items-center space-x-3 text-primary-400;
  }
  
  .bank-detection-card {
    @apply glass-card p-6 hover:shadow-glow transition-shadow;
  }
}

/* Custom Utilities */
@layer utilities {
  .text-glow {
    text-shadow: 0 0 20px rgba(34, 197, 94, 0.5);
  }
  
  .bg-mesh {
    background-image: 
      radial-gradient(circle at 25% 25%, rgba(34, 197, 94, 0.1) 0%, transparent 50%),
      radial-gradient(circle at 75% 75%, rgba(132, 204, 22, 0.1) 0%, transparent 50%);
  }
}
```

### **Template Integration**

#### **Askama Template Pattern**
```html
<!-- templates/prelaunch.html -->
<!DOCTYPE html>
<html lang="pt-BR" class="dark">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ title }}</title>
    
    <!-- Preload critical assets -->
    <link rel="preload" href="{{ "main.css"|asset_url }}" as="style">
    <link rel="preload" href="{{ "prelaunch.js"|asset_url }}" as="script">
    
    <!-- Stylesheets -->
    <link href="{{ "main.css"|asset_url }}" rel="stylesheet">
    
    <!-- External dependencies -->
    <script src="https://unpkg.com/htmx.org@1.9.10"></script>
    <script src="https://unpkg.com/alpinejs@3.13.3/dist/cdn.min.js" defer></script>
</head>
<body class="bg-neutral-950 text-white min-h-screen">
    <!-- Navigation -->
    <nav class="navbar-glass fixed top-0 w-full z-50">
        <div class="max-w-7xl mx-auto px-6 py-4">
            <div class="flex items-center justify-between">
                <div class="flex items-center space-x-2">
                    <div class="w-8 h-8 bg-gradient-to-r from-accent-lime to-primary-500 rounded-lg"></div>
                    <span class="text-xl font-bold">BankFlow</span>
                </div>
                <!-- Navigation items -->
            </div>
        </div>
    </nav>

    <!-- Main Content -->
    <main class="pt-20">
        <div x-data="prelaunchApp()" class="container mx-auto px-6 py-12">
            <!-- Template content using Motta components -->
            <div class="glass-card p-8">
                <h1 class="text-4xl font-bold text-glow mb-6">{{ title }}</h1>
                <!-- Content -->
            </div>
        </div>
    </main>
    
    <!-- Template-specific JavaScript -->
    <script src="{{ "prelaunch.js"|asset_url }}" defer></script>
</body>
</html>
```

## Development Workflow

### **Development Commands**

```bash
# Start development with hot reloading
npm run dev

# Build for production
npm run build:production

# Watch CSS changes only
npm run css:dev

# Watch JS changes only
npm run js:dev
```

### **Development vs Production**

#### **Development Mode**
- **CSS**: Unminified with source maps
- **JavaScript**: Unminified with source maps, console.log preserved
- **Assets**: No hashing, served from `/assets/`
- **Cache**: `no-cache` headers
- **Hot Reloading**: Enabled for all asset types

#### **Production Mode**
- **CSS**: Minified and compressed
- **JavaScript**: Minified, obfuscated, console.log removed
- **Assets**: Content-based hashing, served from `/assets/`
- **Cache**: Long-term caching with `immutable` flag
- **Optimization**: Tree shaking, dead code elimination

### **Asset Pipeline**

#### **Vite Configuration**
```javascript
// vite.config.js
import { defineConfig } from 'vite';
import { resolve } from 'path';

export default defineConfig(({ mode }) => ({
  root: './assets',
  
  build: {
    outDir: '../dist',
    emptyOutDir: true,
    
    rollupOptions: {
      input: {
        // Site crate templates
        'prelaunch': resolve(__dirname, 'assets/js/templates/prelaunch.js'),
        'product': resolve(__dirname, 'assets/js/templates/product.js'),
        'product-demo': resolve(__dirname, 'assets/js/templates/product-demo.js'),
        
        // SaaS crate templates
        'dashboard': resolve(__dirname, 'assets/js/templates/dashboard.js'),
        'settings': resolve(__dirname, 'assets/js/templates/settings.js'),
        'billing': resolve(__dirname, 'assets/js/templates/billing.js')
      },
      output: {
        entryFileNames: mode === 'production' 
          ? 'js/[name]-[hash].js' 
          : 'js/[name].js',
        chunkFileNames: mode === 'production' 
          ? 'js/[name]-[hash].js' 
          : 'js/[name].js',
        manualChunks: {
          'vendor-alpine': ['alpinejs'],
          'vendor-htmx': ['htmx.org'],
          'vendor-charts': ['chart.js']
        }
      }
    },
    
    minify: mode === 'production' ? 'terser' : false,
    terserOptions: mode === 'production' ? {
      compress: {
        drop_console: true,
        drop_debugger: true
      }
    } : {},
    
    sourcemap: mode === 'development'
  }
}));
```

## Quality Standards

### **Code Quality**

#### **JavaScript Standards**
- Use ES6+ features (import/export, arrow functions, destructuring)
- Follow Alpine.js conventions for component structure
- Implement proper error handling with try/catch
- Use async/await for asynchronous operations
- Add JSDoc comments for complex functions

#### **CSS Standards**
- Use Tailwind utility classes as primary styling method
- Create custom components in `@layer components`
- Use Motta design system colors and spacing
- Implement responsive design with mobile-first approach
- Follow glassmorphism patterns for modern UI

### **Performance Standards**

#### **Bundle Size Targets**
- **CSS**: < 50KB gzipped
- **JavaScript per template**: < 20KB gzipped
- **Vendor libraries**: < 30KB gzipped
- **Total page load**: < 100KB gzipped

#### **Performance Metrics**
- **First Contentful Paint**: < 1.5s
- **Largest Contentful Paint**: < 2.5s
- **Cumulative Layout Shift**: < 0.1
- **First Input Delay**: < 100ms

### **Accessibility Standards**

#### **WCAG 2.1 AA Compliance**
- Proper color contrast ratios (4.5:1 for normal text)
- Keyboard navigation support
- Screen reader compatibility
- Focus management and indicators
- ARIA labels and semantic HTML

#### **Implementation**
```html
<!-- Accessible button example -->
<button 
  class="btn-primary"
  aria-label="Submit email for notifications"
  :disabled="isLoading"
  @click="submitEmail"
>
  <span x-show="!isLoading">Submit</span>
  <span x-show="isLoading" aria-hidden="true">Loading...</span>
</button>
```

## Testing Conventions

### **JavaScript Testing**
```javascript
// assets/js/templates/__tests__/prelaunch.test.js
import { prelaunchApp } from '../prelaunch.js';

describe('PrelaunchApp', () => {
  test('validates email correctly', () => {
    const app = prelaunchApp();
    expect(app.isEmailValid).toBe(false);
    
    app.email = 'test@example.com';
    expect(app.isEmailValid).toBe(true);
  });
});
```

### **CSS Testing**
- Use browser dev tools for responsive testing
- Test with screen readers
- Validate color contrast ratios
- Test keyboard navigation

## Deployment Checklist

### **Pre-Deployment**
- [ ] All JavaScript modules properly imported/exported
- [ ] CSS follows Motta design system conventions
- [ ] Templates use asset URL filters
- [ ] Performance metrics meet targets
- [ ] Accessibility compliance verified
- [ ] Cross-browser compatibility tested

### **Post-Deployment**
- [ ] Asset hashing working correctly
- [ ] Cache headers properly set
- [ ] CDN integration functional
- [ ] Performance monitoring active
- [ ] Error tracking configured

## Troubleshooting

### **Common Issues**

#### **Asset Not Loading**
- Check asset manifest generation
- Verify asset URL filter implementation
- Ensure proper file paths in Vite config

#### **Hot Reloading Not Working**
- Check Vite dev server configuration
- Verify file watching setup
- Ensure proper port configuration

#### **Production Build Issues**
- Check for unused imports (tree shaking)
- Verify minification settings
- Ensure proper source map configuration

### **Debug Tools**
- Browser dev tools for CSS/JS debugging
- Vite dev server for build issues
- Lighthouse for performance analysis
- axe-core for accessibility testing

---

**This guide should be followed by all @uidev agents working on BankFlow's frontend implementation to ensure consistency and quality across all templates and components.**
