# Rust and Smalltalk Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using Rust combined with Smalltalk/Pharo for high-performance systems development.

## Technology Stack Preferences

### For Smalltalk/Pharo

- **Primary Platform**: Pharo (latest stable version)
- **Web Frameworks**: Seaside, Teapot for REST APIs
- **Database**: GemStone/S, PostgreSQL, SQLite
- **Testing**: SUnit, TestRunner
- **Development Tools**: Pharo IDE, Iceberg (Git), Calypso, Playground
- **Deployment**: Pharo Launcher, Docker containers

### For Rust

- **Web Frameworks**: Rocket for backend RESTful APIs, Axum for modern async APIs
- **Messaging**: ZeroMQ for order guaranteed remote messaging
- **ORMs**: Diesel, SeaORM for database operations
- **Runtime**: Tokio for async runtime
- **Testing**: Built-in test framework with criterion for benchmarks
- **Best Practices**: Memory safety, async/await, error handling with Result<T, E>

## Integration Patterns

### Rust with Smalltalk/Pharo
- **FFI Integration**: Use Rust FFI for high-performance extensions
- **Shared Libraries**: Build Rust as shared libraries for Smalltalk consumption
- **Serialization**: Use Protocol Buffers, Cap'n Proto for data exchange
- **Error Handling**: Map Rust Results to Smalltalk exceptions
- **Performance**: Rust for compute-intensive operations, Smalltalk for business logic

## Project Structure Standards

### Rust Project Layout
```
rust-component/
├── src/                    # Source files
├── Cargo.toml             # Dependencies and build configuration
├── Cargo.lock             # Locked dependency versions
├── tests/                 # Integration tests
├── benches/               # Performance benchmarks
├── examples/              # Usage examples
└── README.md              # Project documentation
```

### Smalltalk/Pharo Project Layout
```
smalltalk-component/
├── packages/              # Package definitions
├── classes/               # Class definitions
├── methods/               # Method implementations
├── tests/                 # SUnit test classes
├── .smalltalk.ston        # CI/CD configuration
└── README.md              # Project documentation
```

## Quality Standards

### Rust Development
- **Memory Safety**: Leverage Rust's ownership system
- **Error Handling**: Use Result<T, E> and Option<T> appropriately
- **Testing**: Unit tests for all public functions
- **Documentation**: Comprehensive doc comments for public APIs
- **Performance**: Profile and benchmark critical paths
- **Code Style**: Follow rustfmt formatting standards
- **Linting**: Use clippy for additional checks
- **Dependencies**: Minimize dependencies, use trusted crates

### Smalltalk/Pharo Development
- **Object-Oriented Design**: Follow SOLID principles
- **Message Passing**: Use message-based communication
- **Live Programming**: Leverage Pharo's live development capabilities
- **Testing**: SUnit tests for all classes and methods
- **Refactoring**: Use Pharo's refactoring tools
- **Code Organization**: Use packages and protocols effectively
- **Documentation**: Use class comments and method documentation
- **Debugging**: Use Pharo's debugging and inspection tools

## Testing Standards

### Rust Testing
- **Unit Tests**: Test all public functions with `#[cfg(test)]`
- **Integration Tests**: Use `tests/` directory for integration tests
- **Property Testing**: Use proptest for property-based testing
- **Benchmarks**: Use criterion for performance benchmarking
- **Coverage**: Aim for 90%+ code coverage
- **Mocking**: Use mockall for dependency mocking

### Smalltalk/Pharo Testing
- **SUnit Tests**: Write tests for all classes and methods
- **Test Organization**: Group tests by functionality
- **Test Data**: Use factories for test data creation
- **Test Coverage**: Ensure all code paths are tested
- **Live Testing**: Use Pharo's live testing capabilities
- **Test Runner**: Use TestRunner for test execution

## Deployment Standards

### Rust Components
- **Release Builds**: Use `cargo build --release`
- **Docker**: Multi-stage builds for minimal images
- **Cross-Compilation**: Target multiple platforms when needed
- **Performance**: Profile and optimize release builds
- **CI/CD**: Use GitHub Actions with Rust toolchain
- **Security**: Use cargo-audit for vulnerability scanning

### Smalltalk/Pharo Components
- **Pharo Launcher**: Use for development and testing
- **Docker**: Containerize for production deployment
- **CI/CD**: Use `.smalltalk.ston` for automated testing
- **Monitoring**: Integrate with external monitoring tools
- **Performance**: Profile with Pharo's built-in tools
- **Deployment**: Use Pharo Launcher or Docker containers

