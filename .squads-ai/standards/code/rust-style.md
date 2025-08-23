# Rust Style Guide

## Context

Rust-specific code style rules for SquadsAI projects using Rust for high-performance systems development. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/rust-and-smalltalk.md`.

## Code Formatting
- Use `rustfmt` for automatic code formatting
- 4 spaces for indentation (not tabs)
- Line length: 100 characters maximum
- Use trailing commas in multi-line structs, enums, and function calls
- **CI Requirement**: Run `cargo fmt --all -- --check` to ensure formatting compliance before committing
- **Local Development**: Set up local CI checks using the [Local CI Setup Guide](../local-ci-setup.md) to mirror CI pipeline locally

## Naming Conventions
- **Functions and variables**: `snake_case`
- **Types and traits**: `PascalCase`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Lifetimes**: Single lowercase letter (e.g., `'a`, `'b`)
- **Modules**: `snake_case`

## Function Definitions
- Place opening brace on the same line as function signature
- Use explicit return type annotations for public functions
- Group related functions together

```rust
pub fn calculate_distance(
    point1: Point,
    point2: Point,
) -> f64 {
    let dx = point2.x - point1.x;
    let dy = point2.y - point1.y;
    (dx * dx + dy * dy).sqrt()
}
```

## Struct and Enum Definitions
- One field per line for readability
- Use trailing commas consistently
- Group related fields together
- Use DisplayDoc for enum comments whenever possible

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct User {
    pub id: u64,
    pub username: String,
    pub email: String,
    pub is_active: bool,
}

#[derive(Debug, Clone)]
pub enum Status {
    Active,
    Inactive,
    Suspended,
    Deleted,
}
```

## Error Handling
- Use `Result<T, E>` for fallible operations
- Prefer `?` operator over `unwrap()` or `expect()`
- Prefer `expect` operator over `unwrap()` on tests
- Create custom error types for modules and libraries
- Use `anyhow` for application-level error handling
- Use a StartupError to capture all the errors that can happen during startup (as opposed to nominal runtime operations having errors)

```rust
pub fn read_config(path: &str) -> Result<Config, ConfigError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| ConfigError::IoError(e))?;
    
    toml::from_str(&content)
        .map_err(|e| ConfigError::ParseError(e))
}
```

## Documentation
- Focus on saying what first and why second leaving the how to the code itself
- Always try to be clear and readable exposing intent
- Use `///` for public API documentation
- Include examples in doc comments
- Document all public functions, types, and modules
- Use `//!` for module-level documentation

```rust
/// Calculates the Euclidean distance between two points.
///
/// # Examples
///
/// ```
/// use geometry::Point;
/// let p1 = Point { x: 0.0, y: 0.0 };
/// let p2 = Point { x: 3.0, y: 4.0 };
/// assert_eq!(calculate_distance(p1, p2), 5.0);
/// ```
pub fn calculate_distance(point1: Point, point2: Point) -> f64 {
    // ... implementation
}
```

## Testing Standards

### Unit Testing
- Place tests in `mod tests` blocks
- Use descriptive test names
- Group related tests together
- Use `#[cfg(test)]` for test-only code
- Aim for **90%+ code coverage**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_distance() {
        let p1 = Point { x: 0.0, y: 0.0 };
        let p2 = Point { x: 3.0, y: 4.0 };
        assert_eq!(calculate_distance(p1, p2), 5.0);
    }

    #[test]
    fn test_calculate_distance_zero() {
        let p1 = Point { x: 1.0, y: 1.0 };
        let p2 = Point { x: 1.0, y: 1.0 };
        assert_eq!(calculate_distance(p1, p2), 0.0);
    }
}
```

### Integration Testing
- Use `tests/` directory for integration tests
- Test component interactions and API endpoints
- Test complete workflows and data flows
- Use shared test utilities and fixtures

### Property Testing
- Use **proptest** for property-based testing
- Test invariants and mathematical properties
- Generate random test data for edge cases
- Ensure properties hold across different inputs

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_distance_symmetry(a: f64, b: f64, c: f64, d: f64) {
            let p1 = Point { x: a, y: b };
            let p2 = Point { x: c, y: d };
            let dist1 = calculate_distance(p1, p2);
            let dist2 = calculate_distance(p2, p1);
            assert!((dist1 - dist2).abs() < f64::EPSILON);
        }
    }
}
```

### Benchmarking
- Use **criterion** for performance benchmarking
- Benchmark critical paths and hot code
- Track performance regressions in CI
- Use realistic test data for benchmarks

```rust
#[cfg(test)]
mod benches {
    use super::*;
    use criterion::{black_box, criterion_group, criterion_main, Criterion};

    fn bench_calculate_distance(c: &mut Criterion) {
        let p1 = Point { x: 0.0, y: 0.0 };
        let p2 = Point { x: 3.0, y: 4.0 };
        
        c.bench_function("calculate_distance", |b| {
            b.iter(|| calculate_distance(black_box(p1), black_box(p2)))
        });
    }

    criterion_group!(benches, bench_calculate_distance);
    criterion_main!(benches);
}
```

### Mocking and Test Doubles
- Use **mockall** for dependency mocking
- Create test doubles for external services
- Mock network calls and file system operations
- Use trait objects for testable abstractions

```rust
use mockall::predicate::*;
use mockall::*;

#[automock]
trait Database {
    fn get_user(&self, id: u64) -> Result<User, Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_service_with_mock_db() {
        let mut mock_db = MockDatabase::new();
        mock_db
            .expect_get_user()
            .with(eq(1))
            .times(1)
            .returning(|_| Ok(User { id: 1, name: "Test".to_string() }));

        let service = UserService::new(Box::new(mock_db));
        let user = service.get_user(1).unwrap();
        assert_eq!(user.name, "Test");
    }
}
```

### Test Organization
- Group tests by functionality and module
- Use descriptive test names that explain the scenario
- Create test utilities and helper functions
- Use test fixtures for common setup

```rust
#[cfg(test)]
mod tests {
    use super::*;

    // Test utilities
    fn create_test_user(id: u64, name: &str) -> User {
        User {
            id,
            username: name.to_string(),
            email: format!("{}@example.com", name),
            is_active: true,
        }
    }

    // Test fixtures
    struct TestFixture {
        user: User,
        service: UserService,
    }

    impl TestFixture {
        fn new() -> Self {
            let user = create_test_user(1, "testuser");
            let service = UserService::new();
            Self { user, service }
        }
    }

    #[test]
    fn test_user_creation() {
        let fixture = TestFixture::new();
        assert_eq!(fixture.user.username, "testuser");
    }
}
```

## Performance and Safety
- Prefer `&str` over `String` for function parameters when possible
- Use `Vec<T>` for dynamic collections
- Prefer using `&[<T>]` over `Vec<T>` when possible
- Implement `Clone` and `Copy` traits when appropriate
- Prefer `#[derive(Clone)]` maybe with `derive_more` over implementing our own Derive when possible
- Use `Cow<'a, T>` for avoiding unnecessary allocations
- Leverage the borrow checker - avoid unnecessary cloning

## Module Organization
- One file per module
- If the module needs to define many structs or will scale use a directory instead of a file and prefer one file per struct
- Group imports: std, external crates, internal modules
- Re-export commonly used items

```rust
// std imports
use std::collections::HashMap;
use std::path::Path;

// external crate imports
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;

// internal module imports
use crate::{config::Config, error::AppError};

// re-exports
pub use self::{session::Session, user::User};
```

## Clippy and Linting
- Enable all Clippy lints in `Cargo.toml`
- Enable `#![deny(missing_docs)]` globally
- Fix all warnings before committing
- Use `#![deny(warnings)]` in library crates
- Customize lints when necessary with `#[allow(clippy::lint_name)]`

## Local Development Setup

To ensure code quality and prevent CI failures, set up local development tools that mirror your CI pipeline:

### Required Local Checks
- **Formatting**: `cargo fmt --all -- --check`
- **Linting**: `cargo clippy --all-targets --all-features -- -D warnings`
- **Tests**: `cargo test`
- **Build**: `cargo build --all-features`

### Setup Instructions
1. Follow the [Local CI Setup Guide](../local-ci-setup.md) for general setup
2. Create local commands (Makefile, shell aliases, etc.) that run the above checks
3. Set up pre-commit hooks to automatically run these checks
4. Ensure local checks match exactly what CI runs

### Example Makefile Commands
```makefile
check: fmt clippy test
	@echo "All CI checks passed locally!"

fmt:
	cargo fmt --all -- --check

clippy:
	cargo clippy --all-targets --all-features -- -D warnings

test:
	cargo test
```

**Remember**: Always run local CI checks before committing to ensure your code meets CI standards!

## Reference

For comprehensive Rust tech stack standards, build tools, and advanced patterns, see `../tech-stacks/rust-and-smalltalk.md`.
