# JavaScript Style Guide

## Context

JavaScript-specific code style rules for SquadsAI projects using modern JavaScript and frontend frameworks. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/javascript-typescript.md`.

## Framework Support

### Primary Framework: React
- Use functional components with hooks (preferred)
- Follow React best practices and conventions
- Use modern React patterns (React 18+ features)

### Alternative Frameworks
- **Vue 3**: Use Composition API for complex components
- **Svelte**: For lightweight, performant applications
- **Angular**: For enterprise applications requiring strong typing
- **Backend**: Express, Fastify, NestJS based on project requirements

## JavaScript Naming Conventions

### Variables and Functions
- Use **camelCase** for variables and functions (e.g., `userProfile`, `calculateTotal`)
- Use **camelCase** for file names (e.g., `userController.js`, `paymentProcessor.js`)
- Use **camelCase** for directory names (e.g., `userManagement/`, `api/v1/`)

### Classes and Components
- Use **PascalCase** for classes and React components (e.g., `UserProfile`, `PaymentProcessor`)
- Use **PascalCase** for constructor functions
- Use **PascalCase** for React component files

### Constants
- Use **UPPER_SNAKE_CASE** for constants (e.g., `MAX_RETRY_COUNT`, `DEFAULT_TIMEOUT`)
- Use **UPPER_SNAKE_CASE** for environment variables and configuration values
- Use **UPPER_SNAKE_CASE** for magic numbers and configuration constants

### Private Members
- Use **underscore prefix** for private methods and properties (e.g., `_privateMethod`, `_privateProperty`)
- Use **#** for true private class fields when supported (ES2022+)

## JavaScript-Specific Formatting

### Indentation
- Use **2 spaces** for indentation (never tabs)
- Maintain consistent indentation throughout files
- Align nested structures for readability

### String Formatting
- Use **single quotes** for strings: `'Hello World'`
- Use **double quotes** only when interpolation is needed
- Use **template literals** for multi-line strings or complex interpolation: `` `Hello ${name}` ``
- Use **template literals** for string concatenation

### Object and Array Syntax
- Use **object shorthand** when possible: `{ name, age }` instead of `{ name: name, age: age }`
- Use **destructuring** for cleaner code: `const { name, age } = user`
- Use **spread operator** for object/array operations: `{ ...obj1, ...obj2 }`
- Use **trailing commas** for multi-line objects and arrays

### Function Definitions
- Use **arrow functions** for simple functions: `const add = (a, b) => a + b`
- Use **function declarations** for hoisting needs
- Use **const** for function expressions: `const processData = function() {}`
- Use **default parameters** when appropriate: `function greet(name = 'World') {}`

## Modern JavaScript Features

### ES6+ Features
- Use **const** and **let** instead of **var**
- Use **arrow functions** for callbacks and simple functions
- Use **template literals** for string interpolation
- Use **destructuring** for cleaner variable assignment
- Use **spread/rest operators** for flexible parameter handling

### Async/Await
- Use **async/await** instead of **Promises** when possible
- Use **try/catch** for error handling in async functions
- Use **Promise.all()** for parallel operations
- Use **Promise.race()** for timeout scenarios

## Framework-Specific Conventions

### React Conventions
- Use **functional components** with hooks (preferred)
- Use **PascalCase** for component names
- Use **camelCase** for props and event handlers
- Use **useState** and **useEffect** hooks appropriately
- Use **React.memo()** for expensive components
- Use **useCallback** and **useMemo** for performance optimization

### Vue 3 Conventions
- Use **Composition API** for complex components
- Use **PascalCase** for component names
- Use **kebab-case** for props and events
- Use **ref()** and **reactive()** for state management
- Use **computed()** for derived state
- Use **watch()** and **watchEffect()** for side effects

### Svelte Conventions
- Use **PascalCase** for component names
- Use **kebab-case** for props and events
- Use **$:** for reactive statements
- Use **{#if}**, **{#each}** for control flow
- Use **bind:** for two-way binding
- Use **on:** for event handling

### Express/Node.js Conventions
- Use **camelCase** for route handlers and middleware
- Use **async/await** for database operations
- Use **try/catch** for error handling
- Use **middleware** for cross-cutting concerns
- Use **environment variables** for configuration

## Props and State Management

### React State Management
- Use **destructuring** for props: `const { name, age } = props`
- Use **default props** or **default parameters** for optional props
- Use **prop types** or **TypeScript** for type safety
- Use **local state** for component-specific data
- Use **Context API** for shared state
- Use **Redux Toolkit** or **Zustand** for complex state

### Vue State Management
- Use **props** for parent-to-child communication
- Use **emits** for child-to-parent communication
- Use **provide/inject** for deep component communication
- Use **Pinia** for global state management
- Use **composables** for reusable state logic

### Svelte State Management
- Use **props** for component parameters
- Use **stores** for shared state
- Use **context** for component communication
- Use **derived stores** for computed values

## Event Handling

### React Event Handling
- Use **camelCase** for event handlers: `onClick`, `onSubmit`
- Use **arrow functions** for event handlers to avoid binding issues
- Use **event.preventDefault()`** when needed
- Use **controlled components** for form inputs

### Vue Event Handling
- Use **kebab-case** for event names: `@click`, `@submit`
- Use **v-on** or **@** for event binding
- Use **event modifiers** for common patterns: `@click.prevent`
- Use **custom events** for component communication

### Svelte Event Handling
- Use **on:** for event binding: `on:click`, `on:submit`
- Use **event modifiers** for common patterns: `on:click|preventDefault`
- Use **createEventDispatcher** for custom events

## Code Organization

### Import/Export
- Use **ES6 modules** with import/export
- Use **named exports** for multiple exports
- Use **default exports** for main component/function
- Use **barrel exports** for clean imports: `export * from './components'`

### File Structure
- Use **index.js** files for barrel exports
- Group related components in directories
- Use **consistent naming** for component files
- Separate **business logic** from **UI components**

### Project Structure
```
src/
├── components/          # Reusable UI components
├── pages/              # Page-level components
├── hooks/              # Custom React hooks
├── services/           # API and business logic
├── utils/              # Utility functions
├── types/              # TypeScript type definitions
├── styles/             # CSS and styling
└── tests/              # Test files
```

## Best Practices

### Performance
- Use **React.memo()** for expensive components
- Use **useCallback** and **useMemo** for expensive calculations
- Use **lazy loading** for route-based code splitting
- Use **virtual scrolling** for large lists
- Use **code splitting** with dynamic imports

### Error Handling
- Use **try/catch** blocks for synchronous code
- Use **error boundaries** for React components
- Use **proper error logging** and user feedback
- Handle **async errors** with try/catch in async functions
- Implement **fallback UI** for error states

### Testing
- Use **Jest** for unit testing
- Use **React Testing Library** for component testing
- Use **Vitest** for fast testing in Vite projects
- Use **Playwright** for E2E testing
- Use **mocking** for external dependencies
- Test **user behavior** rather than implementation details

## Accessibility

### Semantic HTML
- Use **semantic HTML elements** in JSX
- Use **ARIA attributes** when needed
- Use **proper heading hierarchy**
- Use **alt text** for images

### Keyboard Navigation
- Ensure **keyboard accessibility** for all interactive elements
- Use **tabIndex** appropriately
- Handle **keyboard events** for custom components
- Test **keyboard-only navigation**

### Screen Reader Support
- Use **proper ARIA labels** and descriptions
- Implement **skip links** for navigation
- Ensure **logical tab order**
- Test with **screen reader software**

## Build and Development Tools

### Package Management
- Use **npm 9+** or **pnpm** for dependency management
- Use **exact versions** for production dependencies
- Use **dev dependencies** for development tools
- Implement **automated dependency updates**

### Build Tools
- Use **Vite** for fast development and optimized builds
- Use **Webpack** for complex bundling requirements
- Use **esbuild** for fast builds
- Implement **code splitting** and **tree shaking**

### Code Quality Tools
- Use **ESLint** with TypeScript rules
- Use **Prettier** for consistent formatting
- Use **Husky** for git hooks
- Use **lint-staged** for pre-commit validation

## Reference

For comprehensive JavaScript/TypeScript tech stack standards, build tools, and advanced patterns, see `../tech-stacks/javascript-typescript.md`.
