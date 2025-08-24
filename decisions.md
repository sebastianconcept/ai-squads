# Architecture Decisions

## Overview

This document records significant architectural decisions made during the development of SquadsAI projects. Each decision includes the context, considered alternatives, consequences, and rationale.

## AD-001: Rust Module Organization for Scalable Complexity

**Date**: 2025-08-23  
**Status**: Accepted  
**Type**: Architecture Standard  

### Context

As Rust projects grow in complexity, maintaining clear organization and fast understandability becomes challenging. Traditional approaches of keeping all related structs in single files lead to:
- Unwieldy files exceeding 300+ lines
- Difficulty locating specific functionality
- Challenges for multiple developers working simultaneously
- Reduced code review effectiveness
- Increased cognitive load when navigating large modules

### Decision

We have decided to implement **directory-based module organization** as the default approach for Rust projects when modules contain multiple related structs or complex functionality.

**Key Principles**:
1. **One file per struct/enum** for better separation of concerns
2. **Directory structure** when modules contain 3+ related data structures
3. **Clear thresholds**: Use directory structure when single file exceeds 200-300 lines
4. **mod.rs organization** with proper re-exports for clean public APIs
5. **Type aliases** for complex callback types to improve readability

### Considered Alternatives

1. **Single File Approach**: Keep all related structs in one file
   - ❌ Becomes unwieldy beyond 200-300 lines
   - ❌ Difficult for multiple developers to work simultaneously
   - ❌ Harder to locate specific functionality

2. **Mixed Approach**: Some modules as files, others as directories
   - ❌ Inconsistent patterns across the codebase
   - ❌ Developers need to learn multiple organization styles
   - ❌ Decision fatigue for each new module

3. **Package-Level Organization**: Organize at the crate level only
   - ❌ Too coarse-grained for complex modules
   - ❌ Doesn't address internal module complexity
   - ❌ Maintains large, hard-to-navigate files

### Consequences

#### Positive Consequences

1. **Maintainability**: Each file has a single responsibility and is easier to modify
2. **Readability**: Developers can quickly locate specific functionality
3. **Testability**: Individual components can be tested in isolation
4. **Scalability**: Easy to add new structs without cluttering existing files
5. **Code Review**: Smaller, focused files are easier to review
6. **Parallel Development**: Multiple developers can work on different structs simultaneously
7. **Fast Understandability**: New team members can quickly grasp module structure
8. **Clean APIs**: Proper re-exports maintain clean public interfaces
9. **Proper Coupling**: Clear dependencies between related components
10. **Error Handling**: Better organization supports clean error propagation patterns

#### Negative Consequences

1. **File Count**: More files to navigate (mitigated by clear naming and structure)
2. **Initial Setup**: Slightly more complex module initialization (mitigated by templates and examples)
3. **Learning Curve**: New developers need to understand the pattern (mitigated by comprehensive documentation)

### Implementation Guidelines

#### When to Use Directory Structure

- **Multiple Related Structs**: When a module contains 3+ related data structures
- **Complex Functionality**: When individual structs have substantial implementation
- **Growing Modules**: When a single file exceeds 200-300 lines
- **Team Development**: When multiple developers work on the same module
- **Clear Separation**: When structs represent distinct concepts that can be understood independently

#### When to Keep Single File

- **Simple Modules**: When a module has only 1-2 simple structs
- **Tight Coupling**: When structs are so tightly coupled they can't be separated
- **Small Implementation**: When total module size is under 150 lines
- **Prototype/Experimental**: During early development when structure is still evolving

#### Structure Pattern

```
src/session_manager/
├── mod.rs                    # Module organization and re-exports
├── auto_save_config.rs       # AutoSaveConfig struct and implementation
├── active_session.rs         # ActiveSession struct and NetworkStatus enum
├── session_event_callbacks.rs # SessionEventCallbacks with type aliases
├── session_health_status.rs  # SessionHealthStatus enum
├── restoration_state.rs      # RestorationState enum
└── session_manager.rs        # Main SessionManager struct
```

### Rationale

This decision prioritizes **long-term maintainability** and **team productivity** over short-term simplicity. While it requires more initial setup, it provides:

1. **Scalable Architecture**: Supports projects as they grow from small prototypes to production systems
2. **Team Efficiency**: Enables parallel development without merge conflicts
3. **Code Quality**: Smaller files are easier to understand, test, and maintain
4. **Consistency**: Single pattern across all complex modules reduces cognitive load
5. **Future-Proofing**: Structure supports growth without major refactoring

The pattern aligns with Rust's philosophy of **zero-cost abstractions** - we get better organization without runtime performance costs, while maintaining compile-time guarantees and clean APIs.

### References

- **Rust Style Guide**: `.squads-ai/standards/code/rust-style.md`
- **Implementation Examples**: Session management module patterns
- **Quality Gates**: Local development setup and CI integration
- **Tech Stack Standards**: `../tech-stacks/rust-and-smalltalk.md`
