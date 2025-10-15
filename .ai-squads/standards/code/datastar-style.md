# Datastar Code Style Guide

## Context

Datastar-specific code style rules for SquadsAI projects using Datastar for reactive, hypermedia-driven web applications. This guide complements our comprehensive Datastar tech stack standards documented in `../tech-stacks/datastar-frontend.md` and integrates with our existing HTML, CSS, and JavaScript style guides.

## Datastar-Specific Standards

### Data Attribute Formatting
- Use **kebab-case** for all `data-*` attributes (e.g., `data-bind-user-name`, `data-if-is-logged-in`)
- Place each `data-*` attribute on its own line for readability
- Group related data attributes together
- Use descriptive names that clearly indicate purpose
- Order data attributes logically: bindings, conditionals, loops, events, SSE

### Signal Naming Conventions
- Use **camelCase** for signal names in JavaScript (e.g., `userName` from `data-signals-user-name`)
- Use **descriptive names** that express intent (e.g., `isUserLoggedIn`, `currentUserProfile`)
- Use **boolean prefixes** for boolean signals (e.g., `is`, `has`, `can`, `should`)
- Use **verb prefixes** for action signals (e.g., `handle`, `process`, `validate`)

### Component Structure Standards
- Use **semantic HTML** as the foundation
- Add **Datastar attributes** for reactivity
- Keep **HTML structure** clean and readable
- Separate **presentation** from **behavior**

## HTML Structure with Datastar

### Attribute Ordering
```html
<!-- Preferred order: id, class, data-bind, data-if/for, data-sse, aria-*, event handlers -->
<div id="user-profile"
     class="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-md
            hover:shadow-lg transition-shadow duration-200"
     data-bind-user-profile
     data-if-is-user-loaded
     data-sse="/api/user-updates"
     aria-live="polite"
     role="region">
  <h2 data-text="$userProfile.name"
      class="text-xl font-semibold text-gray-900 dark:text-white">
    User Profile
  </h2>
</div>
```

### Multi-line Datastar Attributes
```html
<!-- Bindings on separate lines for complex objects -->
<form data-submit="/api/users"
      data-method="POST"
      data-bind-form-data
      data-bind-validation-errors
      class="space-y-4">
  
  <input type="text"
         data-bind-name
         data-bind-email
         class="w-full px-3 py-2 border border-gray-300 rounded-md
                focus:outline-none focus:ring-2 focus:ring-blue-500
                dark:bg-gray-700 dark:border-gray-600 dark:text-white" />
  
  <div data-if-has-validation-errors
       class="text-red-600 dark:text-red-400">
    <ul data-for-validation-errors>
      <li data-text="$item.message"></li>
    </ul>
  </div>
</form>
```

### Conditional Rendering Patterns
```html
<!-- Use semantic structure with Datastar conditionals -->
<main class="container mx-auto px-4 py-8">
  <section data-if-is-loading
           class="flex justify-center items-center min-h-64">
    <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
  </section>
  
  <section data-if-has-error
           class="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-md p-4">
    <h3 class="text-red-800 dark:text-red-200 font-medium">
      Error
    </h3>
    <p data-text="$errorMessage"
       class="text-red-700 dark:text-red-300 mt-2"></p>
  </section>
  
  <section data-if-is-data-loaded
           class="space-y-6">
    <h1 data-text="$pageTitle"
        class="text-3xl font-bold text-gray-900 dark:text-white"></h1>
    
    <div data-for-items
         class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      <article class="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6">
        <h2 data-text="$item.title"
            class="text-lg font-semibold text-gray-900 dark:text-white mb-2"></h2>
        <p data-text="$item.description"
           class="text-gray-600 dark:text-gray-300"></p>
      </article>
    </div>
  </section>
</main>
```

## JavaScript Integration Patterns

### Minimal JavaScript Approach
```javascript
// Keep JavaScript minimal - let Datastar handle reactivity
// Only add JavaScript for complex interactions not covered by Datastar

// Custom event handlers for complex interactions
document.addEventListener('DOMContentLoaded', function() {
  // Initialize any custom functionality
  initializeCustomComponents();
});

// Custom component initialization
function initializeCustomComponents() {
  // Handle complex interactions that Datastar doesn't cover
  setupCustomValidation();
  initializeThirdPartyLibraries();
}

// SSE connection management (if needed beyond Datastar's built-in)
function setupCustomSSE() {
  const eventSource = new EventSource('/api/custom-stream');
  
  eventSource.onmessage = function(event) {
    const data = JSON.parse(event.data);
    // Handle custom SSE data
    handleCustomSSEData(data);
  };
  
  eventSource.onerror = function(event) {
    console.error('SSE connection error:', event);
    // Implement reconnection logic
    setTimeout(setupCustomSSE, 5000);
  };
}
```

### Signal Management
```javascript
// Access Datastar signals when needed
function updateUserProfile(userData) {
  // Update signals that Datastar will react to
  window.datastar.setSignal('userProfile', userData);
  window.datastar.setSignal('isUserLoaded', true);
}

// Handle complex form validation
function validateForm(formData) {
  const errors = {};
  
  if (!formData.email) {
    errors.email = 'Email is required';
  }
  
  if (!formData.name) {
    errors.name = 'Name is required';
  }
  
  // Update validation errors signal
  window.datastar.setSignal('validationErrors', errors);
  window.datastar.setSignal('hasValidationErrors', Object.keys(errors).length > 0);
  
  return Object.keys(errors).length === 0;
}
```

### Component Lifecycle Management
```javascript
// Handle component initialization and cleanup
class DatastarComponent {
  constructor(element) {
    this.element = element;
    this.initialize();
  }
  
  initialize() {
    this.setupEventListeners();
    this.loadInitialData();
  }
  
  setupEventListeners() {
    // Add custom event listeners
    this.element.addEventListener('custom-event', this.handleCustomEvent.bind(this));
  }
  
  loadInitialData() {
    // Load initial data and update signals
    fetch('/api/initial-data')
      .then(response => response.json())
      .then(data => {
        window.datastar.setSignal('initialData', data);
      });
  }
  
  destroy() {
    // Cleanup when component is removed
    this.element.removeEventListener('custom-event', this.handleCustomEvent);
  }
}
```

## CSS Integration with Datastar

### Datastar-Specific CSS Classes
```css
/* State-based styling with Datastar attributes */
[data-loading="true"] {
  opacity: 0.6;
  pointer-events: none;
}

[data-error="true"] {
  border-color: theme('colors.red.500');
  background-color: theme('colors.red.50');
}

[data-success="true"] {
  border-color: theme('colors.green.500');
  background-color: theme('colors.green.50');
}

/* Responsive Datastar components */
.datastar-card {
  @apply bg-white dark:bg-gray-800 rounded-lg shadow-md p-6;
  @apply transition-all duration-200;
}

.datastar-card:hover {
  @apply shadow-lg transform scale-105;
}

/* SSE connection status indicators */
[data-sse-connected="true"] .connection-indicator {
  @apply bg-green-500;
}

[data-sse-connected="false"] .connection-indicator {
  @apply bg-red-500;
}

[data-sse-connecting="true"] .connection-indicator {
  @apply bg-yellow-500 animate-pulse;
}
```

### Tailwind CSS with Datastar
```html
<!-- Use Tailwind classes with Datastar attributes -->
<div class="datastar-card
            bg-white dark:bg-gray-800
            p-4 rounded-lg shadow-md
            hover:shadow-lg transition-shadow duration-200
            data-loading:opacity-50 data-loading:pointer-events-none
            data-error:border-red-500 data-error:bg-red-50
            data-success:border-green-500 data-success:bg-green-50"
     data-bind-card-data
     data-if-is-card-visible>
  
  <h3 class="text-lg font-semibold
              text-gray-900 dark:text-white
              mb-2"
      data-text="$cardData.title">
    Card Title
  </h3>
  
  <p class="text-gray-600 dark:text-gray-300
             data-loading:animate-pulse"
     data-text="$cardData.description">
    Card description
  </p>
</div>
```

## Server-Sent Events Patterns

### SSE Integration Standards
```html
<!-- SSE connection with proper error handling -->
<div id="live-updates"
     data-sse="/api/live-updates"
     data-sse-retry="5000"
     data-sse-timeout="30000"
     class="space-y-4">
  
  <!-- Connection status indicator -->
  <div class="flex items-center space-x-2">
    <div class="w-2 h-2 rounded-full
                data-sse-connected:bg-green-500
                data-sse-disconnected:bg-red-500
                data-sse-connecting:bg-yellow-500
                data-sse-connecting:animate-pulse"></div>
    <span class="text-sm text-gray-600 dark:text-gray-400">
      <span data-text="$sseStatus">Connecting...</span>
    </span>
  </div>
  
  <!-- Live data display -->
  <div data-for-live-items
       class="bg-white dark:bg-gray-800 rounded-lg p-4 shadow-sm">
    <h4 data-text="$item.title"
        class="font-medium text-gray-900 dark:text-white"></h4>
    <p data-text="$item.content"
       class="text-gray-600 dark:text-gray-300 mt-1"></p>
    <time data-text="$item.timestamp"
          class="text-xs text-gray-500 dark:text-gray-400 mt-2 block"></time>
  </div>
</div>
```

### Backend SSE Response Format
```javascript
// Backend SSE response format for Datastar
function sendSSEUpdate(res, targetId, data, action = 'update') {
  const sseData = {
    target: `#${targetId}`,
    action: action, // 'update', 'replace', 'append', 'prepend'
    data: data
  };
  
  res.write(`data: ${JSON.stringify(sseData)}\n\n`);
}

// Example usage in backend
app.get('/api/live-updates', (req, res) => {
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
    'Access-Control-Allow-Origin': '*'
  });
  
  // Send initial data
  sendSSEUpdate(res, 'live-updates', {
    liveItems: initialItems,
    sseStatus: 'connected'
  });
  
  // Send updates periodically
  setInterval(() => {
    sendSSEUpdate(res, 'live-updates', {
      liveItems: getUpdatedItems()
    });
  }, 5000);
});
```

## Form Handling Patterns

### Datastar Form Standards
```html
<!-- Comprehensive form with Datastar bindings -->
<form data-submit="/api/users"
      data-method="POST"
      data-bind-form-data
      data-bind-validation-errors
      data-bind-submission-state
      class="space-y-6">
  
  <!-- Form title -->
  <h2 class="text-2xl font-bold text-gray-900 dark:text-white">
    Create User
  </h2>
  
  <!-- Name field -->
  <div class="space-y-2">
    <label for="name"
           class="block text-sm font-medium text-gray-700 dark:text-gray-300">
      Full Name
    </label>
    <input type="text"
           id="name"
           name="name"
           data-bind-name
           data-bind-field-error="name"
           class="w-full px-3 py-2 border border-gray-300 rounded-md
                  focus:outline-none focus:ring-2 focus:ring-blue-500
                  dark:bg-gray-700 dark:border-gray-600 dark:text-white
                  data-field-error:border-red-500 data-field-error:bg-red-50"
           placeholder="Enter full name"
           required />
    
    <!-- Field-specific error -->
    <div data-if-field-error="name"
         class="text-red-600 dark:text-red-400 text-sm">
      <span data-text="$fieldError.name"></span>
    </div>
  </div>
  
  <!-- Email field -->
  <div class="space-y-2">
    <label for="email"
           class="block text-sm font-medium text-gray-700 dark:text-gray-300">
      Email Address
    </label>
    <input type="email"
           id="email"
           name="email"
           data-bind-email
           data-bind-field-error="email"
           class="w-full px-3 py-2 border border-gray-300 rounded-md
                  focus:outline-none focus:ring-2 focus:ring-blue-500
                  dark:bg-gray-700 dark:border-gray-600 dark:text-white
                  data-field-error:border-red-500 data-field-error:bg-red-50"
           placeholder="Enter email address"
           required />
    
    <div data-if-field-error="email"
         class="text-red-600 dark:text-red-400 text-sm">
      <span data-text="$fieldError.email"></span>
    </div>
  </div>
  
  <!-- Submit button with loading state -->
  <button type="submit"
          data-bind-submitting
          class="w-full bg-blue-600 hover:bg-blue-700
                 disabled:bg-gray-400 disabled:cursor-not-allowed
                 text-white font-medium py-2 px-4 rounded-md
                 transition-colors duration-200
                 data-submitting:animate-pulse">
    <span data-if-not-submitting>Create User</span>
    <span data-if-submitting>Creating...</span>
  </button>
  
  <!-- Form-level errors -->
  <div data-if-has-form-errors
       class="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-md p-4">
    <h3 class="text-red-800 dark:text-red-200 font-medium">
      Please correct the following errors:
    </h3>
    <ul data-for-form-errors
        class="text-red-700 dark:text-red-300 mt-2 list-disc list-inside">
      <li data-text="$formError"></li>
    </ul>
  </div>
  
  <!-- Success message -->
  <div data-if-form-success
       class="bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-md p-4">
    <p class="text-green-800 dark:text-green-200">
      User created successfully!
    </p>
  </div>
</form>
```

## Component Composition Patterns

### Reusable Datastar Components
```html
<!-- Reusable card component with Datastar -->
<template id="datastar-card">
  <div class="datastar-card
              bg-white dark:bg-gray-800
              rounded-lg shadow-md p-6
              hover:shadow-lg transition-shadow duration-200
              data-loading:opacity-50 data-loading:pointer-events-none">
    
    <!-- Card header -->
    <div class="flex items-center justify-between mb-4">
      <h3 class="text-lg font-semibold text-gray-900 dark:text-white"
          data-text="$cardTitle">
        Card Title
      </h3>
      
      <!-- Loading indicator -->
      <div data-if-is-loading
           class="animate-spin rounded-full h-4 w-4 border-b-2 border-blue-600"></div>
    </div>
    
    <!-- Card content -->
    <div class="space-y-2">
      <p class="text-gray-600 dark:text-gray-300"
         data-text="$cardContent">
        Card content
      </p>
      
      <!-- Dynamic content slot -->
      <div data-slot="content"
           class="mt-4">
        <!-- Slot content goes here -->
      </div>
    </div>
    
    <!-- Card actions -->
    <div data-slot="actions"
         class="mt-4 flex space-x-2">
      <!-- Action buttons go here -->
    </div>
  </div>
</template>

<!-- Usage of the card component -->
<div data-component="datastar-card"
     data-props='{"cardTitle": "$user.name", "cardContent": "$user.bio", "isLoading": "$isLoading"}'>
  
  <div data-slot="content">
    <p class="text-sm text-gray-500 dark:text-gray-400">
      Member since: <span data-text="$user.joinDate"></span>
    </p>
  </div>
  
  <div data-slot="actions">
    <button class="btn-primary"
            data-click="editUser">
      Edit Profile
    </button>
    <button class="btn-secondary"
            data-click="viewProfile">
      View Profile
    </button>
  </div>
</div>
```

## Error Handling Patterns

### Comprehensive Error Handling
```html
<!-- Global error boundary -->
<div id="error-boundary"
     data-if-has-global-error
     class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
  
  <div class="bg-white dark:bg-gray-800 rounded-lg shadow-xl p-6 max-w-md w-full mx-4">
    <h2 class="text-xl font-semibold text-gray-900 dark:text-white mb-4">
      Something went wrong
    </h2>
    
    <p class="text-gray-600 dark:text-gray-300 mb-4"
       data-text="$globalError.message">
      An unexpected error occurred.
    </p>
    
    <div class="flex space-x-3">
      <button class="btn-primary"
              data-click="retryOperation">
        Try Again
      </button>
      <button class="btn-secondary"
              data-click="reloadPage">
        Reload Page
      </button>
    </div>
  </div>
</div>

<!-- Component-level error handling -->
<div class="component-container">
  <div data-if-has-component-error
       class="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-md p-4 mb-4">
    <h3 class="text-red-800 dark:text-red-200 font-medium">
      Component Error
    </h3>
    <p class="text-red-700 dark:text-red-300 mt-2"
       data-text="$componentError.message">
      Error message
    </p>
    <button class="mt-2 text-red-600 dark:text-red-400 text-sm underline"
            data-click="retryComponent">
      Retry
    </button>
  </div>
  
  <!-- Normal component content -->
  <div data-if-not-component-error>
    <!-- Component content -->
  </div>
</div>
```

## Performance Optimization Patterns

### Lazy Loading and Code Splitting
```html
<!-- Lazy-loaded components -->
<div data-component="lazy-user-list"
     data-props='{"loadOnVisible": true}'
     class="min-h-64">
  
  <!-- Loading placeholder -->
  <div data-if-is-loading
       class="flex items-center justify-center h-64">
    <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
  </div>
  
  <!-- Actual content -->
  <div data-if-is-loaded
       class="space-y-4">
    <div data-for-users
         class="bg-white dark:bg-gray-800 rounded-lg p-4 shadow-sm">
      <h3 data-text="$user.name"
          class="font-medium text-gray-900 dark:text-white"></h3>
      <p data-text="$user.email"
         class="text-gray-600 dark:text-gray-300 text-sm"></p>
    </div>
  </div>
</div>
```

### Efficient SSE Management
```html
<!-- Optimized SSE with connection pooling -->
<div id="optimized-sse"
     data-sse="/api/optimized-stream"
     data-sse-batch="true"
     data-sse-throttle="100"
     class="space-y-2">
  
  <!-- Batch updates indicator -->
  <div data-if-is-batching
       class="text-xs text-gray-500 dark:text-gray-400">
    Processing updates...
  </div>
  
  <!-- Efficient list rendering -->
  <div data-for-batched-items
       data-key="id"
       class="transition-all duration-200">
    <div class="bg-white dark:bg-gray-800 rounded p-3 shadow-sm">
      <span data-text="$item.content"></span>
    </div>
  </div>
</div>
```

## Testing Patterns

### Datastar Component Testing
```javascript
// Jest test for Datastar components
describe('Datastar User Form', () => {
  beforeEach(() => {
    document.body.innerHTML = `
      <form data-submit="/api/users"
            data-bind-form-data
            data-bind-validation-errors>
        <input type="text" data-bind-name />
        <input type="email" data-bind-email />
        <button type="submit">Submit</button>
      </form>
    `;
  });
  
  test('should bind form data to signals', () => {
    const nameInput = document.querySelector('[data-bind-name]');
    const emailInput = document.querySelector('[data-bind-email]');
    
    nameInput.value = 'John Doe';
    emailInput.value = 'john@example.com';
    
    // Trigger input events
    nameInput.dispatchEvent(new Event('input'));
    emailInput.dispatchEvent(new Event('input'));
    
    // Verify signals are updated
    expect(window.datastar.getSignal('name')).toBe('John Doe');
    expect(window.datastar.getSignal('email')).toBe('john@example.com');
  });
  
  test('should handle form submission', async () => {
    const form = document.querySelector('form');
    const submitSpy = jest.fn();
    
    // Mock fetch
    global.fetch = jest.fn(() =>
      Promise.resolve({
        json: () => Promise.resolve({ success: true })
      })
    );
    
    // Trigger form submission
    form.dispatchEvent(new Event('submit'));
    
    await new Promise(resolve => setTimeout(resolve, 0));
    
    expect(fetch).toHaveBeenCalledWith('/api/users', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({})
    });
  });
});
```

## Best Practices Summary

### Development Workflow
1. **Start with Semantic HTML**: Build proper HTML structure first
2. **Add Datastar Attributes**: Add reactive bindings systematically
3. **Implement SSE**: Add real-time updates where needed
4. **Style with Tailwind**: Apply consistent styling patterns
5. **Test Thoroughly**: Test bindings, SSE, and user interactions
6. **Optimize Performance**: Monitor bundle size and SSE efficiency

### Code Organization
- **Component-First**: Organize by reusable components
- **Signal Management**: Use descriptive signal names
- **Error Handling**: Implement comprehensive error boundaries
- **Performance**: Optimize SSE connections and rendering
- **Accessibility**: Maintain WCAG compliance
- **Testing**: Test user behavior, not implementation details

### Anti-Patterns to Avoid
- **Over-engineering**: Don't add unnecessary JavaScript complexity
- **Poor SSE Management**: Don't create too many SSE connections
- **Inconsistent Naming**: Don't mix naming conventions
- **Poor Error Handling**: Don't ignore error states
- **Accessibility Issues**: Don't forget keyboard navigation
- **Performance Problems**: Don't ignore bundle size and update frequency

## Reference

For comprehensive Datastar tech stack standards, build tools, and advanced patterns, see `../tech-stacks/datastar-frontend.md`.

For general HTML, CSS, and JavaScript style guidelines, see:
- `html-style.md` - HTML structure and semantic standards
- `css-style.md` - CSS and Tailwind CSS formatting standards  
- `javascript-style.md` - JavaScript patterns and conventions
