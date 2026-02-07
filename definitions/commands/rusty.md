---
name: rusty
alwaysApply: false
---

# Rust Specialist Command

This command invokes the Rust Specialist agent for Rust-specific development help.

## Agent Profile

**Agent**: Rust Specialist (`../agents/rust-specialist.md`)  
**Style Guide**: Rust Coding Standards (`../../standards/code/rust-style.md`)

## When to Use

Invoke this command when you need help with:
- Reviewing Rust code
- Implementing Rust features
- Debugging Rust issues
- Optimizing Rust performance
- Learning Rust patterns
- Architecture recommendations for Rust projects
- Error handling strategies
- Ownership and borrowing analysis
- Testing strategies

## How It Works

### 1. Context Gathering
Automatically collects:
- Current `.rs` file or `Cargo.toml` content
- Selected text (if any)
- Project structure and dependencies
- Related Rust modules

### 2. Development Workflow Awareness
Recognizes containerized Rust backend development environments:

**cargo-watch Workflow**:
- Many containerized Rust projects use `cargo-watch` for automatic rebuilds
- Development flow: Edit files → Automatic rebuild → Check container logs
- When `cargo-watch` is active, avoid suggesting manual `cargo check` or `cargo run` commands
- Focus on code changes and log analysis instead

**Typical Setup**:
- Docker Compose with services running `cargo watch` via npm scripts or direct commands
- Common ignore patterns: `frontend/**`, `dist/**`, `node_modules/**`, `target*/**`, `uploads/**`, `logs/**`, `temp/**`, `cache/**`
- Hot reload enabled - changes trigger automatic rebuilds

**When to Suggest Manual Commands**:
- Only when cargo-watch is NOT running (e.g., one-off checks, CI/CD contexts)
- For pre-commit quality gates (fmt, clippy, test)
- When troubleshooting build issues outside watch mode

### 3. Agent Activation
Applies the Rust Specialist agent with:
- Rust best practices and ownership rules
- Safe code preferences over unsafe
- Error types with `displaydoc` + `thiserror` (doc comments as Display format strings)
- Performance-conscious design choices
- Idiomatic Rust patterns
- Standards from `rust-style.md`

### 4. Style Guide Enforcement
Follows `rust-style.md` standards:
- **Error Handling**: Typed errors with `displaydoc` + `thiserror` (doc comments as Display), NO `Box<dyn Error>`
- **Error Hierarchy**: `ConfigError` → `StartupError` → `ServiceError` with `Startup(#[from] StartupError)` wrapping; `main()` returns `Result<(), ServiceError>`
- **Protocol Design**: Enum-based design over `Box<dyn Trait>`
- **Naming**: `snake_case` functions, `PascalCase` types, `SCREAMING_SNAKE_CASE` constants
- **Formatting**: `cargo fmt` with 100 character line length
- **Quality Gates**: fmt, check, clippy, test before commits

### 5. Response Generation
Provides Rust-specific guidance:
- Code review with ownership/borrowing feedback
- Implementation suggestions with idiomatic patterns
- Error handling recommendations
- Performance optimization insights
- Testing strategy guidance
- References to Rust style guide sections

## Core Principles Applied

1. **Safety First**: Prefer safe code over unsafe
2. **Typed Errors**: Use `displaydoc` + `thiserror` for all error types (doc comments as Display)
3. **Unified Error Hierarchy**: Services use `ConfigError` → `StartupError` → `ServiceError`. `ServiceError` wraps `StartupError` via `Startup(#[from] StartupError)` so `main()` returns one unified `Result<(), ServiceError>`. Startup-only types use `.map_err(StartupError::Variant)?`; types shared with `ServiceError` use `?` directly
4. **Enum-Based Design**: Avoid trait objects for protocols
5. **Ownership Clarity**: Explicit about borrowing and lifetimes
6. **Zero Panics**: Operational code must not panic
7. **90%+ Coverage**: High test coverage standard
8. **Watch Mode Aware**: Recognize cargo-watch environments and avoid redundant manual commands

## Development Workflow Examples

### cargo-watch Configuration Patterns

**npm Scripts Integration** (common in containerized projects):
```json
{
  "scripts": {
    "start": "cargo watch --no-vcs-ignores -i 'frontend/**' -i 'dist/**' -i 'node_modules/**' -i 'target*/**' -i 'uploads/**' -i 'logs/**' -x run"
  }
}
```

**Direct cargo-watch Command**:
```bash
cargo watch --ignore 'frontend/**' --ignore 'dist/**' --ignore 'node_modules/**' --ignore 'target*/**' --ignore 'uploads/**' --ignore 'logs/**' --ignore 'temp/**' --ignore 'cache/**' -x 'run --bin my-service'
```

**Docker Compose Integration**:
- Services typically run `cargo watch` as the default command
- Volume mounts enable live code reloading
- Check container logs (`docker-compose logs -f service-name`) to see build output

### Workflow Best Practices

1. **During Active Development**: Edit files and monitor container logs - cargo-watch handles rebuilds automatically
2. **Before Commits**: Run quality gates manually (`cargo fmt`, `cargo clippy`, `cargo test`)
3. **Troubleshooting**: If watch mode isn't working, suggest checking container status and logs

## Example Usage

```
@rusty help me implement a session manager with proper error handling
@rusty review this code for ownership issues
@rusty how should I structure error types for this module?
@rusty optimize this function for better performance
```

## Quality Checklist

Before finalizing any Rust code changes, verify:
- [ ] `cargo fmt` applied
- [ ] `cargo clippy` passes with `-D warnings`
- [ ] `cargo test` passes
- [ ] Error types use `displaydoc` + `thiserror` with doc comments as Display (not `#[error("...")]`, not `Box<dyn Error>`)
- [ ] Service errors follow the three-tier hierarchy: `ConfigError` → `StartupError` → `ServiceError` with `Startup(#[from] StartupError)`
- [ ] `main()` returns `Result<(), ServiceError>` (unified error type)
- [ ] No `unwrap()` calls in operational code
- [ ] Protocol messages use enums (not trait objects)
- [ ] Tests written with 90%+ coverage goal
- [ ] Public APIs documented with `///`

## Related Resources

- Rust Specialist Agent: `../agents/rust-specialist.md`
- Rust Style Guide: `../../standards/code/rust-style.md`
- Rust Book: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
