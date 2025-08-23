# Development Best Practices

## Context

Global guidelines for SquadsAI projects. Language-specific best practices are handled by dedicated style guides in the `code-style/` directory.

## Available Style Guides

- **Ruby**: `code-style/ruby-style.md` - Ruby and Rails best practices
- **JavaScript**: `code-style/javascript-style.md` - JavaScript and React best practices  
- **CSS**: `code-style/css-style.md` - CSS and TailwindCSS best practices
- **HTML**: `code-style/html-style.md` - HTML structure and accessibility
- **Rust**: `code-style/rust-style.md` - Rust language best practices
- **Smalltalk**: `code-style/smalltalk-style.md` - Smalltalk language best practices

<conditional-block context-check="core-principles">
IF this Core Principles section already read in current context:
  SKIP: Re-reading this section
  NOTE: "Using Core Principles already in context"
ELSE:
  READ: The following principles

## Core Principles

### Keep It Simple
- Prefer maitanting good Separations of Concerns and flexibility to extend
- Implement code in the fewest lines possible
- Avoid over-engineering solutions
- Choose straightforward approaches over clever ones

### Optimize for Readability
- Prioritize code clarity over micro-optimizations
- Write self-documenting code with clear variable names
- Add comments for "what it is" and "why does it exist"
- When is of significant complexity comment "applicability" and provide an example or two


### Separations of Concerns
- Extract repeated business logic to methods in the responsible class or struct
- Prefer to create a new struct or class when you need to extend a method but there is none with that concern for it yet
- Prefer pure functional methods whenever possible to favour testability
- Extract repeated UI markup to reusable components
- Prefer functions doing 1 thing over many
- Prefer functions doing up to 3 things of the same concern over mixed concerns
- When a function is doing more than 3 or 4 things, consider factoring something to meaningful sub methods to maintain complexity at bay

### File Structure
- Keep files or directory names focused on a single concern
- Group related functionality together
- For structures with many properties, prefer 1 file or directory
- Use consistent naming conventions across the product
</conditional-block>

<conditional-block context-check="dependencies" task-condition="choosing-external-library">
IF current task involves choosing an external library:
  IF Dependencies section already read in current context:
    SKIP: Re-reading this section
    NOTE: "Using Dependencies guidelines already in context"
  ELSE:
    READ: The following guidelines
ELSE:
  SKIP: Dependencies section not relevant to current task

## Dependencies

### Choose Libraries Wisely
When adding third-party dependencies:
- Select the most popular and actively maintained option
- Check the library's GitHub repository for:
  - Recent commits (within last 6 months)
  - Active issue resolution
  - Number of stars/downloads
  - Clear documentation
</conditional-block>

<conditional-block context-check="language-specific" task-condition="language-development">
IF current task involves development in a specific language:
  IF corresponding language style guide already in context:
    SKIP: Re-reading language guide
    NOTE: "Using [LANGUAGE] best practices already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get [LANGUAGE] best practices from code-style/[LANGUAGE]-style.md"
        PROCESS: Returned language-specific best practices
      ELSE:
        READ: @~/.agent-os/standards/code-style/[LANGUAGE]-style.md
    </context_fetcher_strategy>
ELSE:
  SKIP: Language-specific best practices not relevant to current task
</conditional-block>

## Development Areas

### Code Quality
- Follow **language-specific style guides** for naming conventions and formatting
- Use **automated linting** and formatting tools
- Implement **code reviews** for all changes
- Maintain **consistent code style** across the project

### Testing Strategy
- Write **tests first** (TDD approach) when possible
- Aim for **high test coverage** (80%+ for critical paths)
- Use **language-appropriate testing frameworks**
- Test **user behavior** rather than implementation details
- Implement **integration tests** for critical workflows

### Security Practices
- Validate **all user inputs** and external data
- Use **parameterized queries** to prevent injection attacks
- Implement **proper authentication** and authorization
- Follow **OWASP guidelines** for web applications
- Regular **security audits** and dependency updates

### Performance Optimization
- Profile **critical code paths** before optimization
- Use **appropriate data structures** for the task
- Implement **caching strategies** where beneficial
- Optimize **database queries** and reduce N+1 problems
- Use **background jobs** for long-running tasks

### Deployment and DevOps
- Use **version control** for all code changes
- Implement **automated testing** in CI/CD pipelines
- Use **environment-specific configurations**
- Monitor **application performance** and errors
- Implement **rollback strategies** for deployments

### Project-Specific Best Practices
- Follow **project-specific conventions** defined in `.agent-os/product/` directory
- Use **consistent patterns** established by the development team
- Maintain **backward compatibility** when making changes
- Document **project-specific decisions** and rationale
- Follow **established workflows** for development and deployment

## Integration with Style Guides

### When to Use Universal vs. Language-Specific
- **Universal Best Practices**: Apply to all projects and languages
- **Language-Specific**: Use when working with specific language code
- **Project-Specific**: Follow established patterns in the current project

### Smart Loading Strategy
- **Core Principles**: Always loaded for fundamental development practices
- **Language-Specific**: Loaded when working with specific language code
- **Development Areas**: Loaded based on current development focus
- **Dependencies**: Loaded when choosing external libraries

## Summary

This best practices guide provides universal development principles while intelligently integrating with language-specific style guides. For comprehensive development guidance, always reference the appropriate language style guide in the `code-style/` directory.
