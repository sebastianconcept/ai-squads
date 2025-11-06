# JavaScript Code Style Standards

## General Principles

- Write clear, readable code
- Prefer modern ES6+ features
- Use const by default, let when needed, avoid var
- Follow functional programming patterns where appropriate

## Code Organization

- Use modules (ES6 imports/exports)
- Keep functions small and focused
- Separate concerns (UI, logic, data)
- Use meaningful file and directory names

## Naming Conventions

- Use camelCase for variables and functions
- Use PascalCase for classes and constructors
- Use UPPER_SNAKE_CASE for constants
- Use descriptive names that convey intent
- Prefix private methods/properties with underscore if needed

## Variables and Constants

- Use const for values that don't change
- Use let for values that need reassignment
- Avoid var entirely
- Declare variables at the top of their scope

## Functions

- Prefer arrow functions for callbacks
- Use function declarations for named functions
- Keep functions pure when possible
- Avoid side effects
- Use default parameters appropriately

## Objects and Arrays

- Use object destructuring for cleaner code
- Use array methods (map, filter, reduce) over loops
- Prefer spread operator for copying
- Use template literals for string interpolation

## Error Handling

- Use try/catch for async operations
- Provide meaningful error messages
- Handle errors at appropriate levels
- Consider using Result-like patterns for complex flows

## Async/Await

- Prefer async/await over promises chains
- Handle errors in async functions
- Use Promise.all for parallel operations
- Understand when to use Promise.race

## Alpine.js Specific

- Keep Alpine directives minimal and readable
- Use x-data for component state
- Prefer x-bind over x-show when appropriate
- Keep Alpine logic in data() functions
- Separate Alpine components logically

## Testing

- Write tests for critical logic
- Use modern testing frameworks
- Test user interactions, not implementation
- Keep tests maintainable

## Documentation

- Use JSDoc for function documentation
- Include parameter and return types
- Document complex logic
- Keep comments up to date
