# Ruby Style Guide

## Context

Ruby-specific code style rules for SquadsAI projects using Ruby on Rails. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/ruby.md`.

## Ruby Naming Conventions

### Methods and Variables
- Use **snake_case** for methods and variables (e.g., `user_profile`, `calculate_total`)
- Use **snake_case** for file names (e.g., `user_controller.rb`, `payment_processor.rb`)
- Use **snake_case** for directory names (e.g., `user_management/`, `api/v1/`)

### Classes and Modules
- Use **PascalCase** for classes and modules (e.g., `UserProfile`, `PaymentProcessor`)
- Use **PascalCase** for constants that represent classes or modules
- Use **PascalCase** for namespaces and module names

### Constants
- Use **UPPER_SNAKE_CASE** for constants (e.g., `MAX_RETRY_COUNT`, `DEFAULT_TIMEOUT`)
- Use **UPPER_SNAKE_CASE** for environment variables and configuration values
- Use **UPPER_SNAKE_CASE** for magic numbers and configuration constants

### Symbols
- Use **snake_case** for symbols (e.g., `:user_profile`, `:payment_status`)
- Use symbols for hash keys when appropriate
- Use symbols for method names in metaprogramming contexts

## Ruby-Specific Formatting

### Indentation
- Use **2 spaces** for indentation (never tabs)
- Maintain consistent indentation throughout files
- Align nested structures for readability

### String Formatting
- Use **single quotes** for strings: `'Hello World'`
- Use **double quotes** only when interpolation is needed: `"Hello #{name}"`
- Use **heredoc** for multi-line strings
- Use **%w()** for arrays of strings when appropriate

### Hash and Array Syntax
- Use **new hash syntax** when possible: `{ key: value }`
- Use **symbol keys** for hash keys when appropriate
- Use **array literals** with proper spacing: `[1, 2, 3]`
- Use **trailing commas** for multi-line hashes and arrays

### Method Definitions
- Use **def** with proper spacing and alignment
- Use **method chaining** when it improves readability
- Use **guard clauses** for early returns
- Use **keyword arguments** for methods with many parameters

## Ruby Best Practices

### Variable Naming
- Use meaningful names that inspire their purpose
- Focus on **what it is first** and **why it exists second**
- Use temporary variables for clarity without abbreviations
- Break complex operations into smaller, readable steps
- Use meaningful variable names without abbreviations

### Method Design
- Keep methods small and focused (single responsibility)
- Use descriptive method names that explain the action
- Use **bang methods** (`!`) sparingly and only when they modify the receiver
- Use **predicate methods** (ending with `?`) for boolean returns
- Use **private methods** to hide implementation details

### Class Design
- Follow **Single Responsibility Principle**
- Use **inheritance** sparingly, prefer **composition**
- Use **modules** for shared behavior and mixins
- Keep classes focused and cohesive
- Use **attr_accessor**, `attr_reader`, `attr_writer` appropriately

### Error Handling
- Use **begin/rescue** blocks for exception handling
- Use **specific exception types** when possible
- Use **ensure** blocks for cleanup code
- Use **raise** with meaningful error messages
- Use **fail** as an alias for raise (both are acceptable)

## Rails-Specific Conventions

### Controller Conventions
- Use **RESTful actions** and routing
- Keep controllers thin, move business logic to models or services
- Use **strong parameters** for mass assignment
- Use **before_action** for common setup and authorization
- Use **respond_to** blocks for different response formats

### Model Conventions
- Use **Active Record** associations appropriately
- Use **validations** for data integrity
- Use **callbacks** sparingly and only when necessary
- Use **scopes** for common queries
- Use **enums** for status fields

### View Conventions
- Use **ERB** templates with proper indentation
- Use **partials** for reusable view components
- Use **helpers** for view logic
- Use **content_for** for layout customization
- Use **local variables** in partials

### Test Conventions
- Use **RSpec** for testing (preferred)
- Use **FactoryBot** for test data
- Use **VCR** for external API testing
- Use **Capybara** for integration testing
- Use **shoulda-matchers** for common test patterns

## Code Organization

### File Structure
- Follow **Rails conventions** for file organization
- Use **namespaces** for organizing related functionality
- Use **concerns** for shared behavior across models
- Use **services** for complex business logic
- Use **policies** for authorization logic

### Documentation
- Use **YARD** or **RDoc** for method documentation
- Document **public APIs** and complex methods
- Use **inline comments** for non-obvious business logic
- Explain the **"why"** behind implementation choices
- Keep comments concise and relevant

## Performance Considerations

### Database
- Use **N+1 query prevention** with includes and joins
- Use **counter caches** for frequently accessed counts
- Use **database indexes** appropriately
- Use **bulk operations** when possible
- Use **background jobs** for long-running tasks

### Memory
- Use **lazy loading** for large datasets
- Use **streaming** for large file operations
- Use **object pooling** when appropriate
- Use **garbage collection** tuning for production

## Security Best Practices

### Input Validation
- Use **strong parameters** for all user input
- Validate and sanitize all external data
- Use **parameter filtering** for sensitive data
- Use **CSRF protection** for all forms
- Use **XSS protection** for user-generated content

### Authentication
- Use **Devise** or similar for authentication
- Use **Pundit** or similar for authorization
- Use **secure session management**
- Use **HTTPS** in production
- Use **secure cookies** with proper flags

## Reference

For comprehensive Ruby tech stack standards, build tools, and advanced patterns, see `../tech-stacks/ruby.md`.
