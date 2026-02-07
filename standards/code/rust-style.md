---
description: Rust Coding Standards - Automatic application for all Rust files
globs:
  - "**/*.rs"
  - "**/Cargo.toml"
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Rust Coding Standards

> **Automatic Application**: This rule automatically applies when editing `.rs` files or `Cargo.toml`.

## Overview

This document defines comprehensive Rust coding standards for AI Squads projects. These standards ensure consistent, high-quality Rust code across all projects.

## Quick Reference

### Code Formatting
- Use `cargo fmt` for automatic formatting
- 4 spaces for indentation (not tabs)
- Line length: 100 characters maximum
- Trailing commas in multi-line structs, enums, and function calls

### Naming Conventions
- **Functions and variables**: `snake_case`
- **Types and traits**: `PascalCase`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Lifetimes**: Single lowercase letter (e.g., `'a`, `'b`)
- **Modules**: `snake_case`

### Error Handling
- Use `displaydoc` + `thiserror` for all error types: `displaydoc::Display` for the `Display` impl via doc comments, `thiserror::Error` for the `Error` trait
- NOT `Box<dyn std::error::Error>` — always typed errors
- Use `Result<T, E>` for fallible operations
- Prefer `?` operator over `unwrap()` or `expect()`
- Prefer `expect` over `unwrap()` in tests
- `unwrap` calls are only acceptable in tests
- Operational code cannot have panics, create a variant when needed
- Create custom error types for modules and libraries
- Prefer `#[from]` for automatic error conversion
- Use `#[source]` for error chaining
- Doc comments on variants serve double duty: rustdoc documentation AND `Display` format string

```rust
use displaydoc::Display;
use thiserror::Error;

// ✅ CORRECT: displaydoc + thiserror — doc comments ARE the Display format strings
#[derive(Display, Debug, Error)]
pub enum MyError {
    /// Operation failed: {0}
    OperationFailed(#[source] InnerError),
}

// ✅ CORRECT: Structs use the doc comment above the derive
/// Configuration error: {0}
#[derive(Display, Debug, Error)]
pub struct ConfigError(pub String);

// ❌ REJECTED: Generic trait objects
pub fn func() -> Result<(), Box<dyn std::error::Error>> { }

// ❌ REJECTED: thiserror #[error("...")] attributes (use doc comments instead)
#[derive(Debug, thiserror::Error)]
pub enum MyError {
    #[error("Operation failed: {0}")]
    OperationFailed(#[source] InnerError),
}
```

### Protocol Design (CRITICAL)
- **PREFER**: Use enum-based design over dynamic dispatch
- **RESIST** use `Box<dyn Trait>` for protocol messages
- Use enums for all protocol types, message types, and status types

```rust
// ✅ CORRECT: Enum-based protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Request {
    Ping(PingRequest),
    Evaluate(EvaluateRequest),
}

// ❌ REJECTED: Trait objects
pub trait Request { }
pub struct Message {
    pub request: Box<dyn Request>, // ❌ NO!
}
```

### Module Organization
- One file per module
- Use directory structure when module has 3+ related structs
- One file per struct/enum for better maintainability
- Group imports: std, external crates, internal modules

### Pre-Commit Quality Gates

**MANDATORY**: Before any commit, run:

```bash
# 1. Format check
cargo fmt --all -- --check

# 2. Format code
cargo fmt

# 3. Check compilation
cargo check

# 4. Run Clippy with warnings as errors
cargo clippy --all-targets --all-features -- -D warnings

# 5. Run all tests
cargo test
```

### Testing Standards
- Aim for **90%+ code coverage**
- Place tests in `tests.rs` files of the mod to be tested
- Use descriptive test names
- Use `proptest` for property-based testing
- Use `criterion` for performance benchmarking
- Use `mockall` for dependency mocking

### Documentation
- Use `///` for public API documentation
- Include examples in doc comments
- Document all public functions, types, and modules
- Focus on "what" and "why" before "how"

## Important Patterns

### Unified Error Hierarchy (Services)

Services should follow a three-tier error hierarchy where `ServiceError` wraps `StartupError`, and `main()` returns a single unified error type:

```rust
use displaydoc::Display;
use thiserror::Error;

/// Configuration error: {0}
#[derive(Display, Debug, Error)]
pub struct ConfigError(pub String);

/// Startup-only errors. Wrapped by [ServiceError::Startup] for unified main() return.
#[derive(Display, Debug, Error)]
pub enum StartupError {
    /// Configuration error: {0}
    Config(#[from] ConfigError),
    /// Database error: {0}
    Database(#[from] sqlx::Error),
    /// Migration error: {0}
    Migrate(#[from] sqlx::migrate::MigrateError),
    /// I/O error: {0}
    Io(#[from] std::io::Error),
}

/// Operational error. Wraps [StartupError] so main() returns one type.
#[derive(Display, Debug, Error)]
pub enum ServiceError {
    /// I/O error: {0}
    Io(#[from] std::io::Error),
    /// Database error: {0}
    Database(#[from] sqlx::Error),
    /// Startup error: {0}
    Startup(#[from] StartupError),
}
```

In `main()`, startup-only error types use `.map_err(StartupError::Variant)?`, while types shared with `ServiceError` use `?` directly:

```rust
#[tokio::main]
async fn main() -> Result<(), ServiceError> {
    let config = ServiceConfig::from_env().map_err(StartupError::Config)?; // startup-only
    config.validate().map_err(StartupError::Config)?;                      // startup-only
    let pool = sqlx::PgPool::connect(&config.database_url).await?;        // shared (Database)
    sqlx::migrate!().run(&pool).await.map_err(StartupError::Migrate)?;    // startup-only
    let listener = tokio::net::TcpListener::bind(addr).await?;            // shared (Io)
    // ...
}
```

### Error Contextualization
Wrap lower-level errors in domain-specific contexts:

```rust
#[derive(Display, Debug, Error)]
pub enum SessionManagerError {
    /// Session storage failed: {0}
    StoreSession(#[source] SessionStorageError),
    /// Session loading access error: {0}
    LoadSessionAccess(#[source] SessionStorageError),
}
```

### Helper Methods for Error Conversion
```rust
impl SessionManager {
    fn get_storage(&self) -> Result<MutexGuard<SessionStorage>, SessionStorageError> {
        self.storage
            .lock()
            .map_err(|_| SessionStorageError::StorageLockPoisoned)
    }
}
```

### Directory-Based Module Structure
When a module contains multiple related structs (3+), use directory structure:

```
src/session_manager/
├── mod.rs                    # Module organization and re-exports
├── auto_save_config.rs       # AutoSaveConfig struct
├── active_session.rs         # ActiveSession struct
└── session_manager.rs        # Main SessionManager struct
```

## Agent Integration

When working with Rust code:
- **@rusty** handles Rust-specific implementation
- This style guide is automatically applied to `.rs` files via the globs configuration