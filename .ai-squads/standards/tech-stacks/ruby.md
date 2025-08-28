# Ruby Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using Ruby and Ruby on Rails for modern web application development, APIs, and backend services.

## Technology Stack Preferences

### For Ruby

- **Version**: Ruby 3.2+ with YJIT for performance
- **Web Framework**: Ruby on Rails 7+ with Hotwire for modern web apps
- **API Framework**: Sinatra, Grape for lightweight APIs
- **Testing**: RSpec, Minitest, FactoryBot for test data
- **Package Management**: Bundler with Gemfile.lock for dependency locking
- **Database**: PostgreSQL (primary), Redis for caching, SQLite for development
- **Background Jobs**: Sidekiq, Delayed Job for async processing
- **Best Practices**: Convention over configuration, DRY principles, metaprogramming

## Code Style and Standards

### Ruby Style Guidelines
- **Ruby Style Guide**: Follow the official Ruby Style Guide
- **RuboCop**: Use RuboCop for automated style checking
- **Method Length**: Keep methods under 10 lines when possible
- **Class Length**: Keep classes focused and under 100 lines
- **Naming**: Use descriptive names that express intent

### Ruby Best Practices
- **DRY Principle**: Don't Repeat Yourself - extract common patterns
- **Single Responsibility**: Each class should have one reason to change
- **Open/Closed**: Open for extension, closed for modification
- **Dependency Injection**: Inject dependencies rather than hard-coding them
- **Metaprogramming**: Use judiciously and document thoroughly

### Naming Conventions
- **Classes**: PascalCase (e.g., `UserService`, `OrderProcessor`)
- **Modules**: PascalCase (e.g., `Authentication`, `DataValidation`)
- **Methods**: snake_case (e.g., `process_order`, `validate_user`)
- **Variables**: snake_case (e.g., `user_count`, `order_total`)
- **Constants**: UPPER_SNAKE_CASE (e.g., `MAX_RETRY_ATTEMPTS`)
- **Files**: snake_case (e.g., `user_service.rb`, `order_processor.rb`)

## Project Structure Standards

### Rails Application Layout
```
app/
├── controllers/             # Controller classes
│   ├── api/                # API controllers
│   └── web/                # Web controllers
├── models/                  # ActiveRecord models
├── views/                   # View templates
│   ├── layouts/            # Application layouts
│   ├── partials/           # Reusable view partials
│   └── components/         # View components
├── helpers/                 # View helper modules
├── mailers/                 # Mailer classes
├── jobs/                    # Background job classes
├── services/                # Service objects
├── decorators/              # Presenter/decorator classes
├── validators/              # Custom validators
└── concerns/                # Shared concerns
```

### Service Layer Organization
```
app/services/
├── authentication/          # Authentication services
├── payment/                 # Payment processing services
├── notification/            # Notification services
├── data_processing/         # Data processing services
└── external_api/            # External API integration services
```

### Test Organization
```
spec/
├── models/                  # Model specs
├── controllers/             # Controller specs
├── services/                # Service specs
├── requests/                # Request specs
├── system/                  # System/feature specs
├── factories/               # Factory definitions
├── support/                 # Test support files
└── fixtures/                # Test fixtures
```

## Rails Development Standards

### Model Best Practices
- **Validations**: Use strong validations with custom error messages
- **Associations**: Define clear and appropriate associations
- **Scopes**: Use scopes for common queries
- **Callbacks**: Use callbacks sparingly and document their purpose
- **Concerns**: Extract shared behavior into concerns

### Controller Best Practices
- **Thin Controllers**: Keep controllers thin, move logic to services
- **Strong Parameters**: Use strong parameters for security
- **Error Handling**: Implement proper error handling and logging
- **Authentication**: Use Devise or similar for authentication
- **Authorization**: Implement proper authorization with Pundit or CanCanCan

### View Best Practices
- **Partials**: Use partials for reusable view components
- **Helpers**: Extract view logic into helper methods
- **Components**: Use ViewComponent for complex view logic
- **Hotwire**: Leverage Hotwire for dynamic interactions
- **Stimulus**: Use Stimulus for JavaScript interactions

## API Development Standards

### RESTful API Design
- **Resource Naming**: Use plural nouns for resources
- **HTTP Methods**: Use appropriate HTTP methods (GET, POST, PUT, DELETE)
- **Status Codes**: Return appropriate HTTP status codes
- **Error Handling**: Provide consistent error response format
- **Versioning**: Implement API versioning strategy

### API Documentation
- **OpenAPI/Swagger**: Use OpenAPI specification for API documentation
- **Examples**: Provide request/response examples
- **Authentication**: Document authentication requirements
- **Rate Limiting**: Document rate limiting policies
- **Error Codes**: Document all possible error codes

### API Testing
- **Request Specs**: Test API endpoints with request specs
- **Integration Tests**: Test complete API workflows
- **Authentication Tests**: Test authentication and authorization
- **Error Handling**: Test error scenarios and edge cases
- **Performance Tests**: Test API response times

## Database Standards

### PostgreSQL Best Practices
- **Indexing**: Create appropriate indexes for query performance
- **Constraints**: Use database constraints for data integrity
- **Migrations**: Write reversible migrations with proper rollback
- **Seeds**: Use seed data for development and testing
- **Backups**: Implement regular database backup strategy

### ActiveRecord Best Practices
- **Query Optimization**: Use includes, joins, and eager loading
- **Batch Operations**: Use find_each for large datasets
- **Transactions**: Use transactions for data consistency
- **Callbacks**: Use callbacks judiciously
- **Scopes**: Use scopes for common query patterns

## Testing Standards

### RSpec Configuration
- **Factory Bot**: Use FactoryBot for test data creation
- **Database Cleaner**: Ensure clean test database state
- **VCR**: Use VCR for external API testing
- **Time Helpers**: Use time helpers for time-dependent tests
- **Coverage**: Maintain high test coverage (90%+)

### Test Organization
- **Unit Tests**: Test individual methods and classes
- **Integration Tests**: Test component interactions
- **System Tests**: Test complete user workflows
- **API Tests**: Test API endpoints and responses
- **Performance Tests**: Test application performance

### Test Data Management
- **Factories**: Use FactoryBot for test data creation
- **Fixtures**: Use fixtures for static test data
- **Seeds**: Use seed data for development environment
- **Database Cleaner**: Ensure clean test state between tests

## Performance Standards

### Application Performance
- **Caching**: Implement appropriate caching strategies
- **Background Jobs**: Use background jobs for long-running tasks
- **Database Optimization**: Optimize database queries and indexes
- **Asset Pipeline**: Optimize asset compilation and delivery
- **Monitoring**: Use application performance monitoring

### Caching Strategies
- **Fragment Caching**: Cache view fragments
- **Russian Doll Caching**: Implement nested caching
- **Redis Caching**: Use Redis for session and data caching
- **CDN**: Use CDN for static asset delivery
- **HTTP Caching**: Implement proper HTTP caching headers

## Security Standards

### Authentication and Authorization
- **Devise**: Use Devise for authentication
- **Pundit/CanCanCan**: Implement proper authorization
- **JWT Tokens**: Use JWT for API authentication
- **OAuth2**: Implement OAuth2 for third-party authentication
- **Two-Factor**: Support two-factor authentication

### Data Security
- **Strong Parameters**: Use strong parameters for security
- **SQL Injection**: Prevent SQL injection with proper query methods
- **XSS Prevention**: Sanitize user input and use content security policy
- **CSRF Protection**: Implement CSRF protection
- **Data Encryption**: Encrypt sensitive data at rest and in transit

## Deployment Standards

### Environment Configuration
- **Environment Variables**: Use environment variables for configuration
- **Secrets Management**: Secure management of API keys and secrets
- **Configuration Files**: Separate configuration for different environments
- **Feature Flags**: Implement feature flags for gradual rollouts

### Containerization
- **Docker**: Use Docker for containerization
- **Multi-Stage Builds**: Use multi-stage builds for minimal images
- **Base Images**: Use official Ruby runtime images
- **Security**: Regular security updates and vulnerability scanning
- **Optimization**: Optimize container size and startup time

### CI/CD Pipeline
- **GitHub Actions**: Use GitHub Actions for CI/CD
- **Automated Testing**: Run full test suite on every commit
- **Code Quality**: Run RuboCop and other quality checks
- **Security Scanning**: Regular security vulnerability scanning
- **Automated Deployment**: Automated deployment to staging/production

## Integration Patterns

### With JavaScript Frontends
- **API Integration**: Provide RESTful APIs for frontend consumption
- **WebSocket Support**: Use ActionCable for real-time features
- **Hotwire**: Use Hotwire for dynamic interactions
- **Stimulus**: Use Stimulus for JavaScript interactions

### With External Services
- **HTTP Clients**: Use HTTParty or Faraday for external API calls
- **Background Jobs**: Use Sidekiq for async processing
- **Message Queues**: Use Redis or RabbitMQ for message queuing
- **Monitoring**: Integrate with New Relic, Datadog, or similar services

## Quality Assurance

### Code Quality Tools
- **RuboCop**: Automated Ruby style checking
- **Brakeman**: Security vulnerability scanning
- **Bundler Audit**: Dependency vulnerability scanning
- **SimpleCov**: Code coverage reporting
- **Reek**: Code smell detection
- **StandardRB**: Alternative Ruby style guide and formatter
- **Fasterer**: Performance optimization suggestions
- **Rails Best Practices**: Rails-specific best practices checking

### Code Review Process
- **Pull Request Reviews**: Require code reviews for all changes
- **Automated Checks**: Run automated quality checks in CI
- **Testing Requirements**: Require passing tests before merge
- **Documentation**: Ensure code is properly documented
- **Performance Review**: Review performance implications of changes
- **Security Review**: Review security implications of changes
- **Accessibility Review**: Ensure accessibility standards are met
- **Internationalization**: Consider i18n requirements
