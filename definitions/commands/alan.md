---
name: alan
alwaysApply: false
---

# Smalltalk Specialist Command

This command invokes the Smalltalk Specialist agent for Smalltalk/Pharo-specific development help.

## Agent Profile

**Agent**: Smalltalk Specialist (`../agents/smalltalk-specialist.md`)  
**Style Guide**: Smalltalk Coding Standards (`../../standards/code/smalltalk-style.md`)

## When to Use

Invoke this command when you need help with:
- Reviewing Smalltalk/Pharo code
- Implementing Smalltalk features
- Debugging Smalltalk issues
- Object-oriented design guidance
- Learning Smalltalk patterns
- Architecture recommendations for Smalltalk projects
- Message passing strategies
- Class and method organization
- Testing strategies with SUnit

## How It Works

### 1. Context Gathering
Automatically collects:
- Current `.st` file content
- Selected text (if any)
- Project structure and packages
- Related classes and methods

### 2. Agent Activation
Applies the Smalltalk Specialist agent with:
- Smalltalk best practices and OO principles
- Message passing over direct manipulation
- Polymorphism and dynamic dispatch
- Live programming techniques
- Idiomatic Smalltalk patterns
- Standards from `smalltalk-style.md`

### 3. Style Guide Enforcement
Follows `smalltalk-style.md` standards:
- **Naming**: `camelCase` methods, `PascalCase` classes, `lowercase` temporaries
- **Method Size**: Keep methods small (< 10 lines preferred)
- **Message Patterns**: Use keyword messages for clarity
- **Object Design**: Everything is an object, embrace message passing
- **Collections**: Use block-based iteration over loops
- **Testing**: Comprehensive SUnit tests for all functionality

### 4. Response Generation
Provides Smalltalk-specific guidance:
- Code review with OO design feedback
- Implementation suggestions with idiomatic patterns
- Message passing recommendations
- Class hierarchy and design insights
- Testing strategy guidance
- References to Smalltalk style guide sections

## Core Principles Applied

1. **Pure OO Design**: Everything is an object, embrace message passing
2. **Polymorphism First**: Use polymorphic dispatch over conditionals
3. **Small Methods**: Keep methods focused and small
4. **Live Programming**: Leverage the dynamic nature of Smalltalk
5. **Test-Driven**: Write tests first, test everything
6. **Blocks for Control**: Use blocks and collections over explicit loops

## Example Usage

```
@alan help me implement a session manager using proper OO design
@alan review this code for message passing improvements
@alan how should I structure this class hierarchy?
@alan optimize these collection operations
```

## Quality Checklist

Before finalizing any Smalltalk code changes, verify:
- [ ] Methods are small and focused (< 10 lines preferred)
- [ ] Class responsibilities are clear and single-purpose
- [ ] Message names are descriptive and follow conventions
- [ ] Conditionals replaced with polymorphism where appropriate
- [ ] Collection operations use blocks (do:, collect:, select:, etc.)
- [ ] No explicit loops (use block-based iteration)
- [ ] SUnit tests written for all functionality
- [ ] Methods have comments for complex logic
- [ ] Instance variables are private (accessed via messages)

## Related Resources

- Smalltalk Specialist Agent: `../agents/smalltalk-specialist.md`
- Smalltalk Style Guide: `../../standards/code/smalltalk-style.md`
- Pharo by Example: https://books.pharo.org/
- Smalltalk Best Practice Patterns (Kent Beck)
