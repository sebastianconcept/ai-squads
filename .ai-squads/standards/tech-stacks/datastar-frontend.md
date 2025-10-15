# Datastar Frontend Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using Datastar for building reactive, hypermedia-driven web applications with minimal JavaScript and backend-controlled frontend updates.

## Technology Stack Preferences

### For Datastar Frontend

- **Framework**: Datastar for reactive web applications
- **Approach**: Hypermedia-driven development with Server-Sent Events (SSE)
- **Backend Integration**: Datastar SDKs (Go, Python, PHP, Ruby, Rust)
- **Component Libraries**: DatastarUI for server-side rendered components
- **Styling**: CSS, Tailwind CSS, or CSS-in-JS for component styling
- **Build Tools**: Vite, Webpack, or native bundlers for asset optimization
- **Testing**: Jest, Vitest, Playwright for component and E2E testing
- **Deployment**: CDN delivery, static hosting, or integrated with backend

## Code Style and Standards

### Datastar Best Practices
- **Declarative Reactivity**: Use `data-*` attributes for reactive bindings
- **Minimal JavaScript**: Keep JavaScript minimal, leverage backend control
- **Server-Sent Events**: Use SSE for real-time updates from backend
- **Hypermedia Approach**: Let backend drive frontend state and updates
- **Progressive Enhancement**: Build with HTML-first, enhance with Datastar

### HTML Structure Standards
- **Semantic HTML**: Use proper semantic HTML elements
- **Accessibility**: Ensure ARIA attributes and keyboard navigation
- **Data Attributes**: Use descriptive `data-*` attribute names
- **Clean Markup**: Keep HTML clean and readable
- **Component Structure**: Organize components with clear boundaries

### Naming Conventions
- **Data Attributes**: kebab-case with descriptive names (e.g., `data-bind-user-name`)
- **Signals**: camelCase in JavaScript (e.g., `userName` from `data-signals-user-name`)
- **Components**: PascalCase for component names (e.g., `UserProfile`)
- **Files**: kebab-case for file names (e.g., `user-profile.html`)
- **CSS Classes**: kebab-case or BEM methodology (e.g., `user-profile__title`)

## Project Structure Standards

### Directory Layout
```
datastar-app/
├── public/                    # Static assets
│   ├── css/                  # Stylesheets
│   ├── js/                   # Minimal JavaScript
│   └── images/               # Images and icons
├── templates/                 # HTML templates
│   ├── components/           # Reusable components
│   ├── layouts/              # Page layouts
│   ├── partials/             # Template partials
│   └── pages/                # Page templates
├── backend/                   # Backend integration
│   ├── handlers/             # Request handlers
│   ├── models/               # Data models
│   └── sse/                  # Server-Sent Events
├── assets/                    # Build assets
│   ├── scss/                 # SCSS source files
│   └── js/                   # JavaScript source files
├── tests/                     # Test files
│   ├── unit/                 # Unit tests
│   ├── integration/          # Integration tests
│   └── e2e/                  # End-to-end tests
└── config/                    # Configuration files
    ├── datastar.json         # Datastar configuration
    └── build.json            # Build configuration
```

### Template Organization
```
templates/
├── layouts/
│   ├── base.html             # Base layout template
│   ├── dashboard.html        # Dashboard layout
│   └── auth.html             # Authentication layout
├── components/
│   ├── forms/                # Form components
│   │   ├── user-form.html    # User form component
│   │   └── login-form.html   # Login form component
│   ├── navigation/           # Navigation components
│   │   ├── header.html       # Header component
│   │   └── sidebar.html      # Sidebar component
│   └── data/                 # Data display components
│       ├── user-list.html    # User list component
│       └── data-table.html   # Data table component
└── pages/
    ├── dashboard/            # Dashboard pages
    ├── users/                # User management pages
    └── auth/                 # Authentication pages
```

## Datastar Patterns and Standards

### Reactive Bindings
```html
<!-- Input binding with signal -->
<input data-bind-title placeholder="Enter title" />
<div data-text="$title"></div>

<!-- Conditional rendering -->
<div data-if="$isLoggedIn">
  <p>Welcome, <span data-text="$userName"></span>!</p>
</div>

<!-- List rendering -->
<ul data-for="$users">
  <li data-text="$item.name"></li>
</ul>

<!-- Class binding -->
<div data-class="$isActive ? 'active' : 'inactive'">
  Content
</div>
```

### Server-Sent Events Integration
```html
<!-- SSE connection for real-time updates -->
<div id="notifications" data-sse="/api/notifications">
  <div data-for="$notifications">
    <div data-text="$item.message"></div>
  </div>
</div>

<!-- Backend-controlled updates -->
<div id="live-data" data-sse="/api/live-data">
  <span data-text="$currentValue"></span>
</div>
```

### Form Handling
```html
<!-- Form with Datastar binding -->
<form data-submit="/api/users" data-method="POST">
  <input data-bind-name name="name" required />
  <input data-bind-email name="email" type="email" required />
  <button type="submit">Create User</button>
</form>

<!-- Form validation feedback -->
<div data-if="$errors.name">
  <span data-text="$errors.name" class="error"></span>
</div>
```

### Component Composition
```html
<!-- Reusable component with props -->
<div data-component="user-card" data-props='{"user": "$user"}'>
  <h3 data-text="$props.user.name"></h3>
  <p data-text="$props.user.email"></p>
</div>

<!-- Component with slots -->
<div data-component="modal" data-props='{"title": "User Details"}'>
  <div data-slot="content">
    <p data-text="$user.description"></p>
  </div>
</div>
```

## Backend Integration Standards

### Datastar SDK Usage
- **Go SDK**: Use for Go backend services
- **Python SDK**: Use for Python/FastAPI/Django backends
- **PHP SDK**: Use for PHP/Laravel applications
- **Ruby SDK**: Use for Ruby/Rails applications
- **Rust SDK**: Use for Rust/Axum/Warp backends

### Server-Sent Events Implementation
```go
// Go example
func handleSSE(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "text/event-stream")
    w.Header().Set("Cache-Control", "no-cache")
    w.Header().Set("Connection", "keep-alive")
    
    // Send updates to specific elements
    fmt.Fprintf(w, "data: {\"target\": \"#notifications\", \"data\": {\"notifications\": [{\"message\": \"New notification\"}]}}\n\n")
}
```

### API Response Format
```json
{
  "target": "#element-id",
  "data": {
    "signalName": "value",
    "users": [...],
    "errors": {...}
  },
  "action": "update|replace|append|prepend"
}
```

## Styling Standards

### CSS Integration
- **Scoped Styles**: Use CSS Modules or scoped styling
- **Utility Classes**: Leverage Tailwind CSS for rapid styling
- **Component Styles**: Organize styles by component
- **Responsive Design**: Mobile-first approach with CSS Grid/Flexbox
- **Theme Support**: Use CSS custom properties for theming

### Datastar-Specific Styling
```css
/* Component-specific styles */
.user-card {
  border: 1px solid var(--border-color);
  border-radius: 8px;
  padding: 1rem;
}

.user-card[data-state="active"] {
  background-color: var(--active-bg);
}

/* Reactive styling with data attributes */
[data-loading="true"] {
  opacity: 0.6;
  pointer-events: none;
}

[data-error="true"] {
  border-color: var(--error-color);
}
```

## Performance Standards

### Optimization Strategies
- **Minimal JavaScript**: Keep JavaScript bundle size minimal
- **SSE Efficiency**: Use efficient Server-Sent Events
- **Template Caching**: Cache compiled templates
- **Asset Optimization**: Optimize CSS and images
- **CDN Delivery**: Use CDN for static assets

### Bundle Optimization
- **Tree Shaking**: Remove unused code
- **Code Splitting**: Split code by routes/components
- **Lazy Loading**: Load components on demand
- **Compression**: Use gzip/brotli compression
- **Caching**: Implement proper caching strategies

## Testing Standards

### Testing Strategy
- **Unit Tests**: Test individual components and functions
- **Integration Tests**: Test Datastar bindings and SSE
- **E2E Tests**: Test complete user workflows
- **Visual Tests**: Test UI components and layouts
- **Performance Tests**: Test SSE performance and updates

### Testing Tools
- **Jest/Vitest**: Unit and integration testing
- **Playwright**: End-to-end testing
- **Testing Library**: Component testing utilities
- **MSW**: Mock Server-Sent Events
- **Storybook**: Component development and testing

### Test Examples
```javascript
// Unit test for Datastar binding
describe('User Form', () => {
  test('should bind input to signal', () => {
    const input = document.querySelector('[data-bind-name]');
    const display = document.querySelector('[data-text="$name"]');
    
    input.value = 'John Doe';
    input.dispatchEvent(new Event('input'));
    
    expect(display.textContent).toBe('John Doe');
  });
});

// Integration test for SSE
describe('Real-time Updates', () => {
  test('should update UI on SSE message', async () => {
    const mockSSE = new EventSource('/api/test-sse');
    const element = document.querySelector('#live-data');
    
    // Simulate SSE message
    const event = new MessageEvent('message', {
      data: JSON.stringify({
        target: '#live-data',
        data: { value: 'updated' }
      })
    });
    
    mockSSE.dispatchEvent(event);
    
    expect(element.textContent).toBe('updated');
  });
});
```

## Security Standards

### Frontend Security
- **XSS Prevention**: Sanitize user input and use Content Security Policy
- **CSRF Protection**: Implement CSRF tokens for state-changing operations
- **Input Validation**: Validate all user inputs
- **HTTPS**: Use HTTPS for all production deployments
- **Content Security Policy**: Implement strict CSP headers

### SSE Security
- **Authentication**: Authenticate SSE connections
- **Rate Limiting**: Implement rate limiting for SSE endpoints
- **Data Sanitization**: Sanitize data sent via SSE
- **CORS Policy**: Configure proper CORS policies
- **Access Control**: Implement proper access control for SSE streams

## Deployment Standards

### Environment Configuration
- **Environment Variables**: Use environment variables for configuration
- **Build Configuration**: Separate build configs for different environments
- **Feature Flags**: Implement feature flags for gradual rollouts
- **Monitoring**: Set up monitoring for SSE connections and performance

### Static Deployment
- **CDN**: Use CDN for static asset delivery
- **Caching**: Implement proper caching strategies
- **Compression**: Enable gzip/brotli compression
- **Security Headers**: Implement security headers
- **SSL/TLS**: Use proper SSL/TLS configuration

### Backend Integration
- **Load Balancing**: Implement load balancing for SSE connections
- **Scaling**: Design for horizontal scaling
- **Monitoring**: Monitor SSE connection health
- **Error Handling**: Implement proper error handling and recovery

## Integration Patterns

### With Modern Frameworks
- **React Integration**: Use Datastar for specific reactive components
- **Vue Integration**: Leverage Datastar for real-time features
- **Svelte Integration**: Combine with Svelte for enhanced reactivity
- **Angular Integration**: Use Datastar for specific use cases

### With Backend Services
- **Microservices**: Integrate with microservices architecture
- **API Gateway**: Use API Gateway for SSE routing
- **Message Queues**: Integrate with message queues for real-time updates
- **Database**: Use database triggers for real-time data updates

## Quality Assurance

### Code Quality Tools
- **HTML Validator**: Validate HTML structure and accessibility
- **CSS Linter**: Use stylelint for CSS quality
- **JavaScript Linter**: Use ESLint for minimal JavaScript
- **Template Linter**: Use template-specific linters
- **Accessibility Testing**: Test with screen readers and keyboard navigation

### Performance Monitoring
- **SSE Performance**: Monitor SSE connection performance
- **Update Frequency**: Track update frequency and efficiency
- **Memory Usage**: Monitor memory usage for long-running connections
- **Bundle Size**: Track JavaScript bundle size
- **Load Times**: Monitor page load and update times

### Continuous Integration
- **Automated Testing**: Run tests on every commit
- **Build Validation**: Validate builds and deployments
- **Performance Testing**: Automated performance regression testing
- **Security Scanning**: Regular security vulnerability scanning
- **Accessibility Testing**: Automated accessibility testing

## Best Practices Summary

### Development Workflow
1. **Start with HTML**: Build semantic HTML structure first
2. **Add Datastar Bindings**: Add reactive bindings with `data-*` attributes
3. **Implement SSE**: Add real-time updates via Server-Sent Events
4. **Style Components**: Apply CSS styling and responsive design
5. **Test Thoroughly**: Test components, bindings, and SSE functionality
6. **Optimize Performance**: Optimize bundle size and SSE efficiency

### Common Patterns
- **Form Handling**: Use Datastar form bindings with backend validation
- **Real-time Updates**: Implement SSE for live data updates
- **Component Reuse**: Create reusable components with props and slots
- **Error Handling**: Implement proper error states and user feedback
- **Loading States**: Show loading indicators during async operations
- **Progressive Enhancement**: Ensure functionality without JavaScript

### Anti-Patterns to Avoid
- **Over-engineering**: Don't add unnecessary JavaScript complexity
- **Poor SSE Management**: Don't create too many SSE connections
- **Insecure Data**: Don't send sensitive data via SSE
- **Poor Error Handling**: Don't ignore SSE connection errors
- **Accessibility Issues**: Don't forget keyboard navigation and screen readers
- **Performance Problems**: Don't ignore bundle size and update frequency
