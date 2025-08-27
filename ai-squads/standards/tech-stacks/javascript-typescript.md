# JavaScript/TypeScript Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using JavaScript/TypeScript for modern web development, both frontend and backend applications.

## Technology Stack Preferences

### For JavaScript/TypeScript

- **Runtime**: Node.js 18+ LTS, Deno for modern TypeScript-first development
- **Package Manager**: npm 9+, Yarn 1.22+, pnpm for efficient dependency management
- **Frameworks**: Express, Fastify, NestJS for backend; React, Vue, Svelte for frontend
- **Build Tools**: Vite, Webpack, esbuild for bundling and optimization
- **Testing**: Jest, Vitest, Playwright for unit and E2E testing
- **Linting**: ESLint with TypeScript rules, Prettier for code formatting
- **Type Safety**: TypeScript 5.0+ with strict mode enabled
- **Best Practices**: ES6+ features, async/await, functional programming patterns

## Code Style and Standards

### TypeScript Best Practices
- **Strict Mode**: Enable all strict TypeScript compiler options
- **Type Definitions**: Create comprehensive interfaces and types
- **Generic Types**: Use generics for reusable, type-safe components
- **Union Types**: Leverage union types for flexible APIs
- **Type Guards**: Implement proper type guards for runtime type checking

### JavaScript/TypeScript Style Guidelines
- **ES6+ Features**: Use modern JavaScript features (arrow functions, destructuring, etc.)
- **Async/Await**: Prefer async/await over Promises for better readability
- **Functional Programming**: Use pure functions, avoid side effects
- **Error Handling**: Implement proper error handling with try-catch and error boundaries
- **Immutability**: Use immutable data patterns where possible

### Naming Conventions
- **Variables/Functions**: camelCase (e.g., `getUserData`, `isValid`)
- **Constants**: UPPER_SNAKE_CASE (e.g., `API_BASE_URL`)
- **Classes**: PascalCase (e.g., `UserService`)
- **Interfaces**: PascalCase with descriptive names (e.g., `UserProfile`)
- **Files**: kebab-case (e.g., `user-service.ts`)

## Project Structure Standards

### Directory Layout
```
project/
├── src/                    # Source files
│   ├── components/         # React/Vue components
│   ├── services/           # Business logic services
│   ├── utils/              # Utility functions
│   ├── types/              # TypeScript type definitions
│   └── index.ts            # Main entry point
├── public/                 # Static assets
├── tests/                  # Test files
├── package.json            # Dependencies and scripts
├── tsconfig.json           # TypeScript configuration
├── .eslintrc.js            # ESLint configuration
├── .prettierrc             # Prettier configuration
└── README.md               # Project documentation
```

### Package Configuration
- **Dependencies**: Use exact versions for production dependencies
- **Dev Dependencies**: Include all necessary development tools
- **Scripts**: Define clear npm scripts for common tasks
- **TypeScript**: Configure strict mode and proper module resolution
- **ESLint**: Set up comprehensive linting rules

## Testing Standards

### Testing Strategy
- **Unit Tests**: Test individual functions and components
- **Integration Tests**: Test component interactions and API endpoints
- **E2E Tests**: Test complete user workflows
- **Coverage**: Maintain minimum 80% code coverage
- **Mocking**: Use proper mocking for external dependencies

### Testing Tools
- **Jest**: Primary testing framework for unit and integration tests
- **Vitest**: Fast testing framework for Vite-based projects
- **Playwright**: E2E testing for web applications
- **Testing Library**: Component testing utilities
- **MSW**: Mock Service Worker for API mocking

## Build and Deployment

### Build Configuration
- **Bundling**: Use Vite for fast development and optimized builds
- **Code Splitting**: Implement dynamic imports for better performance
- **Tree Shaking**: Remove unused code in production builds
- **Minification**: Compress and optimize production code
- **Source Maps**: Generate source maps for debugging

### Performance Optimization
- **Lazy Loading**: Implement code splitting and lazy loading
- **Caching**: Use proper caching strategies for static assets
- **Bundle Analysis**: Analyze bundle size and optimize accordingly
- **CDN**: Use CDN for static asset delivery
- **Service Workers**: Implement PWA capabilities where appropriate

## Integration Patterns

### Frontend-Backend Integration
- **REST APIs**: Design RESTful APIs with proper HTTP status codes
- **GraphQL**: Use Apollo Client/Server for flexible data fetching
- **Real-time**: Implement WebSockets, Server-Sent Events, Socket.io
- **State Management**: Use Redux Toolkit, Zustand, React Query
- **Authentication**: Implement JWT tokens, OAuth2 integration

### Microservices Architecture
- **API Gateway**: Use API Gateway pattern for service coordination
- **BFF Pattern**: Implement Backend for Frontend for optimized APIs
- **Service Discovery**: Use service discovery for dynamic service location
- **Load Balancing**: Implement proper load balancing strategies
- **Circuit Breakers**: Use circuit breakers for fault tolerance

## Quality Assurance

### Code Quality Tools
- **ESLint**: Comprehensive JavaScript/TypeScript linting
- **Prettier**: Consistent code formatting
- **Husky**: Git hooks for pre-commit validation
- **Lint-staged**: Run linters only on staged files
- **TypeScript**: Compile-time type checking

### Continuous Integration
- **Build Pipeline**: Automated build and test pipeline
- **Code Coverage**: Track and maintain code coverage metrics
- **Performance Testing**: Automated performance regression testing
- **Security Scanning**: Regular security vulnerability scanning
- **Dependency Updates**: Automated dependency update management

## Security Standards

### Frontend Security
- **XSS Prevention**: Sanitize user input and use Content Security Policy
- **CSRF Protection**: Implement CSRF tokens for state-changing operations
- **Authentication**: Secure JWT storage and token refresh
- **Input Validation**: Validate all user inputs on both client and server
- **HTTPS**: Use HTTPS for all production deployments
- **Content Security Policy**: Implement strict CSP headers
- **Subresource Integrity**: Use SRI for external resources
- **Secure Headers**: Implement security headers (HSTS, X-Frame-Options)

### Backend Security
- **Input Sanitization**: Sanitize all incoming data
- **Rate Limiting**: Implement rate limiting for API endpoints
- **Authentication**: Secure authentication and authorization
- **Data Encryption**: Encrypt sensitive data at rest and in transit
- **Logging**: Implement secure logging without sensitive data exposure
- **CORS Policy**: Configure proper CORS policies
- **API Security**: Use API keys, OAuth2, or JWT tokens
- **Dependency Scanning**: Regular vulnerability scanning with npm audit

## Deployment Standards

### Environment Configuration
- **Environment Variables**: Use environment variables for configuration
- **Secrets Management**: Secure management of API keys and secrets
- **Configuration Files**: Separate configuration for different environments
- **Feature Flags**: Implement feature flags for gradual rollouts

### Containerization
- **Docker**: Use multi-stage Docker builds for minimal images
- **Base Images**: Use official Node.js runtime images
- **Security**: Regular security updates and vulnerability scanning
- **Optimization**: Optimize container size and startup time
