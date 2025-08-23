# C/C++ Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using C/C++ for systems programming, performance-critical applications, and low-level software development.

## Technology Stack Preferences

### For C/C++

- **Standards**: C++17 minimum, C++20/23 preferred for new projects
- **Compilers**: GCC 9+, Clang 10+, MSVC 2019+ (Windows)
- **Build Systems**: CMake 3.16+ (primary), Make for simple projects
- **Package Management**: Conan, vcpkg for dependencies
- **Testing**: Google Test, Catch2 for unit testing
- **Static Analysis**: clang-tidy, cppcheck, PVS-Studio
- **Profiling**: Valgrind, gprof, perf, Intel VTune

## C/C++ Style and Standards

### Code Style Guidelines

#### Modern C++ Practices
- **Smart Pointers**: Prefer `std::unique_ptr`, `std::shared_ptr` over raw pointers
- **RAII**: Use Resource Acquisition Is Initialization for all resource management
- **Exception Safety**: Implement strong exception safety guarantees
- **Range-based Loops**: Use `for (const auto& item : container)` syntax
- **Auto**: Use `auto` for complex types, explicit types for simple ones
- **Const Correctness**: Mark everything const that can be const

#### Naming Conventions
- **Classes**: PascalCase (e.g., `NetworkManager`)
- **Functions**: camelCase (e.g., `processData()`)
- **Variables**: camelCase (e.g., `userCount`)
- **Constants**: UPPER_SNAKE_CASE (e.g., `MAX_CONNECTIONS`)
- **Private Members**: trailing underscore (e.g., `data_`)

#### Memory Management
- **Ownership**: Clear ownership semantics with smart pointers
- **No Raw Delete**: Never use `delete` directly
- **Custom Allocators**: Implement for performance-critical paths
- **Memory Pools**: Use for frequent small allocations
- **Leak Detection**: Enable in debug builds with Valgrind/AddressSanitizer

### Performance Standards

#### Optimization Guidelines
- **Profile First**: Always profile before optimizing
- **Cache-Friendly**: Design data structures for cache locality
- **SIMD**: Use vectorization where applicable (SSE, AVX, NEON)
- **Compiler Flags**: `-O2` for release, `-O0` for debug, `-O3` for performance-critical
- **Benchmarking**: Use Google Benchmark or custom benchmarks

#### Memory Layout
- **Structure Packing**: Minimize padding with `#pragma pack` when needed
- **Cache Lines**: Align to 64-byte cache lines for performance
- **Vectorization**: Use `std::vector` with proper capacity management
- **Small Object Optimization**: Leverage std::string, std::vector optimizations
- **Memory Pools**: Use custom allocators for performance-critical code
- **Data Locality**: Design data structures for cache-friendly access patterns
- **Alignment**: Use appropriate alignment for SIMD operations
- **Memory Access Patterns**: Optimize for sequential memory access

### Security Standards

#### Memory Safety
- **Bounds Checking**: Use `std::array`, `std::vector` with bounds checking
- **Buffer Overflow**: Never use C-style arrays for dynamic data
- **Integer Overflow**: Use `std::numeric_limits` and safe arithmetic
- **Format Strings**: Use `std::format` (C++20) or safe alternatives

#### Input Validation
- **Sanitization**: Validate all external inputs
- **Type Safety**: Use strong types and enums
- **Error Handling**: Use exceptions or `std::expected` (C++23)
- **Buffer Overflow**: Prevent buffer overflows with bounds checking
- **Integer Overflow**: Use safe arithmetic operations
- **Format Strings**: Use safe string formatting functions
- **SQL Injection**: Use parameterized queries when applicable

## Project Structure Standards

### Directory Layout
```
project/
├── src/                    # Source files
├── include/               # Header files
├── tests/                 # Test files
├── docs/                  # Documentation
├── scripts/               # Build and utility scripts
├── third_party/           # External dependencies
├── CMakeLists.txt         # Main build configuration
├── README.md              # Project documentation
└── .clang-format          # Code formatting rules
```

### Build Configuration
- **CMake**: Use modern CMake practices (target-based)
- **Compiler Flags**: Enable all warnings, treat warnings as errors
- **Sanitizers**: Enable AddressSanitizer, UndefinedBehaviorSanitizer in debug
- **Coverage**: Enable code coverage in CI builds
- **Static Analysis**: Run clang-tidy in CI pipeline

### Testing Standards
- **Unit Tests**: Minimum 90% code coverage
- **Integration Tests**: Test component interactions
- **Performance Tests**: Benchmark critical paths
- **Memory Tests**: Use Valgrind for leak detection
- **Cross-Platform**: Test on Linux, Windows, macOS
- **Test Frameworks**: Google Test, Catch2, Boost.Test
- **Mocking**: Use GMock or similar for dependency mocking
- **Property Testing**: Use property-based testing where applicable
- **Fuzzing**: Implement fuzz testing for input validation
- **Static Analysis**: Run static analysis tools in CI

## Integration Patterns

### C/C++ with Rust
- **FFI**: Use C-compatible interfaces for Rust integration
- **Shared Libraries**: Build C++ as shared libraries for Rust consumption
- **Serialization**: Use Protocol Buffers, Cap'n Proto for data exchange
- **Error Handling**: Map C++ exceptions to Rust Results

### C/C++ with Smalltalk
- **Native Extensions**: Use FFI for Smalltalk-C++ communication
- **Shared Memory**: Use shared memory for high-performance data exchange
- **Message Passing**: Implement message queues for async communication

## Quality Assurance

### Code Review Checklist
- [ ] Modern C++ features used appropriately
- [ ] RAII principles followed
- [ ] Exception safety guaranteed
- [ ] Memory management is clear and safe
- [ ] Performance implications considered
- [ ] Cross-platform compatibility verified
- [ ] Tests cover edge cases
- [ ] Documentation is complete and accurate
- [ ] Security considerations addressed
- [ ] Error handling is comprehensive
- [ ] Code follows style guide
- [ ] Performance benchmarks included

### Continuous Integration
- **Build Matrix**: Linux (GCC/Clang), Windows (MSVC), macOS (Clang)
- **Static Analysis**: clang-tidy, cppcheck on every commit
- **Testing**: Run full test suite on all platforms
- **Performance**: Benchmark critical paths in CI
- **Coverage**: Generate and track code coverage reports
- **Security Scanning**: Run security vulnerability scans
- **Dependency Updates**: Automated dependency update management
- **Code Quality**: Automated style and quality checks

## Deployment Standards

### Release Builds
- **Optimization**: Use `-O2` or `-O3` with profile-guided optimization
- **Stripping**: Remove debug symbols for production
- **Dependencies**: Bundle or document all runtime dependencies
- **Signing**: Code sign executables on supported platforms

### Containerization
- **Multi-Stage**: Use multi-stage Docker builds for minimal images
- **Base Images**: Use official language runtime images
- **Security**: Scan for vulnerabilities in base images
- **Size**: Optimize for minimal container size
