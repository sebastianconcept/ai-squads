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

---

## AD-002: Rust Error Handling Standards for Production Systems

**Date**: 2025-08-23  
**Status**: Accepted  
**Type**: Code Quality Standard  

### Context

As our Rust projects move from prototypes to production systems, we need robust error handling that provides meaningful context to API consumers while maintaining clean, maintainable code. Traditional approaches of using string-based errors or generic error wrappers lead to:
- Poor error context for debugging and user experience
- Inconsistent error handling patterns across modules
- Difficulty in error propagation and chaining
- Lack of structured error information for monitoring and logging
- Poor integration with error handling libraries and tools
- Unclear documentation of error handling patterns and best practices
- Lack of real-world examples showing pattern evolution and refinement

### Decision

We have decided to implement **comprehensive error handling standards** that prioritize structured error types, contextual error information, and consistent patterns across all Rust modules.

**Key Principles**:
1. **Structured Error Types**: Use `#[from]` and `#[source]` attributes for error chaining
2. **Error Contextualization**: Wrap lower-level errors in domain-specific contexts with specific operation names
3. **Helper Method Patterns**: Standardize common error handling operations
4. **Result-Based APIs**: Prefer `Result<T, E>` over `Option<T>` for fallible operations
5. **No String-Based Errors**: Avoid generic string error variants in favor of structured types
6. **Specific Error Variants**: Use operation-specific error names rather than generic categories

### Considered Alternatives

1. **String-Based Error Variants**: Use `AppErrorVariant(String)` for simple errors
   - ❌ Loses error context and structure
   - ❌ Difficult to handle programmatically
   - ❌ Poor integration with error handling libraries

2. **Generic Error Wrappers**: Use `AppErrorVariant(#[from] GenericError)` for all errors
   - ❌ Too generic, loses domain-specific context
   - ❌ Difficult to provide meaningful error messages
   - ❌ Poor API design for consumers

3. **Mixed Approach**: Allow both structured and string-based errors
   - ❌ Inconsistent patterns across the codebase
   - ❌ Developers need to choose between multiple approaches
   - ❌ Code review complexity increases

### Consequences

#### Positive Consequences

1. **Better Error Context**: API consumers receive meaningful error information
2. **Consistent Patterns**: Standardized error handling across all modules
3. **Error Chaining**: Proper error propagation with `#[from]` and `#[source]`
4. **Debugging Support**: Structured errors provide better debugging information
5. **Monitoring Integration**: Errors can be properly categorized and monitored
6. **Library Integration**: Better integration with error handling libraries
7. **Code Quality**: Cleaner, more maintainable error handling code
8. **Team Productivity**: Consistent patterns reduce learning curve and review time

#### Negative Consequences

1. **Initial Complexity**: More complex error type definitions (mitigated by templates and examples)
2. **Learning Curve**: Developers need to understand error handling patterns (mitigated by comprehensive documentation)
3. **Code Verbosity**: Slightly more verbose error type definitions (mitigated by helper methods)

### Implementation Guidelines

#### Documentation and Organization Standards

**1. Comprehensive Pattern Documentation**
- **Numbered Sections**: All error handling patterns are organized in numbered sections for easy reference
- **Before/After Examples**: Clear documentation of pattern evolution from generic to specific approaches
- **Real-World Code**: Practical examples using actual SessionManager error patterns
- **Pattern Benefits**: Explicit documentation of why each pattern is preferred

**2. Error Type Design Patterns

**1. Domain-Specific Error Contexts with Operation Names**
```rust
#[derive(Error, Debug)]
pub enum SessionManagerError {
    /// Session storage failed: {0}
    StoreSession(#[source] SessionStorageError),
    /// Session storage unavailable for updating: {0}
    UpdateSessionAccess(#[source] SessionStorageError),
    /// Session storage unavailable for deleting sessions: {0}
    DeleteSessionAccess(#[source] SessionStorageError),
    /// Session delete failed: {0}
    DeleteSession(#[source] SessionStorageError),
    /// Session update failed: {0}
    UpdateSession(#[source] SessionStorageError),
    /// Metadata access error: {0}
    MetadataLoadingAccess(#[source] SessionStorageError),
    /// Session loading access error: {0}
    LoadSessionAccess(#[source] SessionStorageError),
    /// Session not found: {0}
    LoadSession(#[source] SessionStorageError),
    /// Session listing access error: {0}
    ListSessionsAccess(#[source] SessionStorageError),
    /// No active session to update
    NoActiveSessionToUpdate,
    /// No active session found
    NoActiveSessionFound,
}
```

**Pattern Evolution Benefits**:
- **Before**: `SessionStorageMutating` was too generic and didn't provide context about what operation failed
- **After**: Specific variants like `StoreSession`, `LoadSessionAccess`, `MetadataLoadingAccess` give clear context about what failed
- **Result**: Better debugging experience, more maintainable code, and clearer API contracts

**2. Helper Methods for Error Conversion**
```rust
impl SessionManager {
    fn get_storage(&self) -> Result<MutexGuard<SessionStorage>, SessionStorageError> {
        self.storage
            .lock()
            .map_err(|_| SessionStorageError::StorageLockPoisoned)
    }
}
```

**3. Contextual Error Mapping**
```rust
// Before: Generic error handling
storage.lock().map_err(|_| SessionStorageError::StorageLockPoisoned(...))

// After: Specific context with helper methods
storage.lock()
    .map_err(|_| SessionStorageError::StorageLockPoisoned)
    .map_err(SessionManagerError::LoadSessionAccess)
```

#### When to Use Each Pattern

- **Use `#[from]`**: When you want automatic error conversion in calling code
- **Use `#[source]`**: When you want to preserve original error for debugging
- **Use Helper Methods**: For common error handling patterns across your module
- **Use Contextual Errors**: When you need to provide domain-specific error information

#### Refined Error Naming Strategy

**Operation-Specific Names Over Generic Categories**:
- **✅ Good**: `StoreSession`, `LoadSessionAccess`, `MetadataLoadingAccess`
- **❌ Avoid**: `SessionStorageMutating`, `StorageError`, `GenericError`

**Benefits of Operation-Specific Names**:
1. **Clear Context**: Developers immediately know what operation failed
2. **Better Debugging**: Error messages provide actionable information
3. **Maintainable Code**: Easier to locate and fix specific error scenarios
4. **API Clarity**: Consumers understand exactly what went wrong
5. **Consistent Patterns**: Standardized approach across all error types

### Rationale

This decision prioritizes **production readiness** and **developer experience** over short-term simplicity. While it requires more initial setup, it provides:

1. **Production Quality**: Structured errors support proper monitoring, logging, and debugging
2. **Developer Experience**: Consistent patterns make code easier to understand and maintain
3. **API Quality**: Better error information for API consumers
4. **Maintainability**: Standardized patterns reduce cognitive load and review complexity
5. **Future-Proofing**: Structure supports growth and integration with production tools
6. **Documentation Quality**: Comprehensive, organized documentation with real-world examples
7. **Pattern Evolution**: Clear documentation of how patterns have improved over time

The pattern aligns with Rust's philosophy of **zero-cost abstractions** - we get better error handling without runtime performance costs, while maintaining compile-time guarantees and clean APIs.

### Documentation Improvements

The comprehensive restructuring of the error handling documentation provides:

1. **Clear Organization**: Numbered sections make patterns easy to find and reference
2. **Pattern Evolution**: Before/after examples show the refinement process
3. **Real-World Examples**: SessionManager patterns demonstrate practical application
4. **Comprehensive Coverage**: All aspects of error handling are documented with examples
5. **Developer Guidance**: Clear patterns for implementing robust error handling

### References

- **Rust Style Guide**: `.squads-ai/standards/code/rust-style.md` - Error Handling section
- **Implementation Examples**: Session management module error patterns
- **Quality Gates**: Error handling standards integrated into local development setup
- **Tech Stack Standards**: `../tech-stacks/rust-and-smalltalk.md`
