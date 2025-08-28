# C++ Style Guide and Coding Standards

## Context

C++-specific code style rules for SquadsAI projects using modern C++ for systems programming and high-performance applications. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/cpp-standards.md`.

## General Principles

### Code Quality Standards
- **Readability**: Code should be self-documenting and easy to understand
- **Maintainability**: Write code that's easy to modify and extend
- **Performance**: Optimize for performance without sacrificing readability
- **Safety**: Prioritize memory safety and exception safety
- **Consistency**: Follow consistent patterns throughout the codebase

### Modern C++ Philosophy
- **Prefer C++ over C**: Use C++ features when available
- **RAII First**: Always use Resource Acquisition Is Initialization
- **Smart Pointers**: Prefer smart pointers over raw pointers
- **Exception Safety**: Guarantee exception safety in all functions
- **Type Safety**: Leverage the type system for safety

## Naming Conventions

### Classes and Structs
```cpp
// Use PascalCase for class names
class NetworkManager;
class UserAuthenticationService;
struct Point2D;

// Template classes
template<typename T>
class Vector;

template<typename Key, typename Value>
class HashMap;
```

### Functions and Methods
```cpp
// Use camelCase for function names
void processUserData();
bool isValidInput(const std::string& input);
std::vector<int> calculatePrimeFactors(int number);

// Member functions
class UserService {
public:
    void authenticateUser(const std::string& username);
    bool isUserLoggedIn() const;
    std::string getUserProfile() const;
private:
    void validateCredentials(const std::string& password);
};
```

### Variables and Constants
```cpp
// Use camelCase for variables
int userCount = 0;
std::string userName;
std::vector<std::string> userList;

// Constants use UPPER_SNAKE_CASE
const int MAX_CONNECTIONS = 100;
const std::string DEFAULT_HOST = "localhost";
constexpr double PI = 3.14159265359;

// Member variables use trailing underscore
class NetworkConnection {
private:
    std::string host_;
    int port_;
    bool isConnected_;
};
```

### Namespaces
```cpp
// Use lowercase with underscores for namespaces
namespace network_utils {
    class ConnectionManager;
}

namespace data_processing {
    namespace algorithms {
        template<typename T>
        void sort(std::vector<T>& data);
    }
}
```

## Code Formatting

### Indentation and Spacing
```cpp
// Use 4 spaces for indentation (no tabs)
class Example {
public:
    void method() {
        if (condition) {
            doSomething();
        } else {
            doSomethingElse();
        }
    }
};

// Spaces around operators
int result = a + b * c;
bool isValid = (x > 0) && (y < 100);

// Spaces after commas
std::vector<int> numbers = {1, 2, 3, 4, 5};
functionCall(param1, param2, param3);
```

### Line Length and Breaking
```cpp
// Keep lines under 120 characters
// Break long lines at logical points
std::vector<std::string> longFunctionCall(
    const std::string& parameter1,
    const std::string& parameter2,
    const std::string& parameter3
);

// Template parameters on separate lines for readability
template<
    typename KeyType,
    typename ValueType,
    typename HashType = std::hash<KeyType>
>
class CustomHashMap;
```

### Braces and Control Structures
```cpp
// Opening brace on same line, closing brace on separate line
if (condition) {
    doSomething();
} else {
    doSomethingElse();
}

// Always use braces, even for single statements
if (condition) {
    return true;
}

// Switch statements with braces
switch (value) {
case 1: {
    int localVar = 42;
    process(localVar);
    break;
}
case 2:
    process(value);
    break;
default:
    handleDefault();
    break;
}
```

## Modern C++ Features

### Smart Pointers
```cpp
// Prefer unique_ptr for exclusive ownership
std::unique_ptr<Resource> resource = std::make_unique<Resource>();

// Use shared_ptr for shared ownership
std::shared_ptr<SharedResource> shared = std::make_shared<SharedResource>();

// Use weak_ptr to break circular references
std::weak_ptr<SharedResource> weak = shared;

// Never use raw pointers for ownership
// Bad: Resource* resource = new Resource();
// Good: auto resource = std::make_unique<Resource>();
```

### RAII and Resource Management
```cpp
class ResourceManager {
public:
    ResourceManager() : resource_(std::make_unique<Resource>()) {}
    ~ResourceManager() = default; // Resource automatically cleaned up
    
    // Disable copy, enable move
    ResourceManager(const ResourceManager&) = delete;
    ResourceManager& operator=(const ResourceManager&) = delete;
    ResourceManager(ResourceManager&&) = default;
    ResourceManager& operator=(ResourceManager&&) = default;
    
private:
    std::unique_ptr<Resource> resource_;
};

// Use RAII for all resources
class FileHandler {
public:
    explicit FileHandler(const std::string& filename) 
        : file_(std::fopen(filename.c_str(), "r")) {
        if (!file_) {
            throw std::runtime_error("Failed to open file: " + filename);
        }
    }
    
    ~FileHandler() {
        if (file_) {
            std::fclose(file_);
        }
    }
    
    // Disable copy
    FileHandler(const FileHandler&) = delete;
    FileHandler& operator=(const FileHandler&) = delete;
    
private:
    FILE* file_;
};
```

### Exception Safety
```cpp
// Basic exception safety guarantee
class SafeContainer {
public:
    void addItem(const Item& item) {
        auto newItems = std::make_unique<std::vector<Item>>(*items_);
        newItems->push_back(item);
        items_ = std::move(newItems);
    }
    
private:
    std::unique_ptr<std::vector<Item>> items_;
};

// Strong exception safety guarantee
class StrongContainer {
public:
    void addItem(const Item& item) {
        auto newItems = std::make_unique<std::vector<Item>>(*items_);
        newItems->push_back(item);
        items_ = std::move(newItems);
    }
    
    void removeItem(size_t index) {
        if (index >= items_->size()) {
            throw std::out_of_range("Index out of range");
        }
        auto newItems = std::make_unique<std::vector<Item>>(*items_);
        newItems->erase(newItems->begin() + index);
        items_ = std::move(newItems);
    }
    
private:
    std::unique_ptr<std::vector<Item>> items_;
};
```

### Range-based For Loops
```cpp
// Use range-based for loops when possible
std::vector<int> numbers = {1, 2, 3, 4, 5};

// Good: Range-based for loop
for (const auto& number : numbers) {
    process(number);
}

// Good: With reference when modifying
for (auto& number : numbers) {
    number *= 2;
}

// Good: With index when needed
for (size_t i = 0; i < numbers.size(); ++i) {
    if (i % 2 == 0) {
        numbers[i] *= 2;
    }
}
```

### Auto Keyword
```cpp
// Use auto for complex types
auto iterator = container.begin();
auto result = std::make_unique<ComplexType>();
auto lambda = [](int x) { return x * x; };

// Be explicit for simple types
int count = 0;           // Good
std::string name = "";   // Good
auto count = 0;          // Less clear
auto name = std::string{}; // Less clear

// Use auto for template deduction
template<typename Container>
auto processContainer(const Container& c) -> decltype(c.size()) {
    return c.size();
}
```

## Memory Management

### Custom Allocators
```cpp
template<typename T>
class PoolAllocator {
public:
    using value_type = T;
    
    PoolAllocator() = default;
    template<typename U>
    PoolAllocator(const PoolAllocator<U>&) noexcept {}
    
    T* allocate(std::size_t n) {
        // Implementation for pool allocation
        return static_cast<T*>(::operator new(n * sizeof(T)));
    }
    
    void deallocate(T* p, std::size_t n) noexcept {
        ::operator delete(p, n * sizeof(T));
    }
};

// Use custom allocators for performance-critical code
std::vector<int, PoolAllocator<int>> fastVector;
```

### Memory Pools
```cpp
class MemoryPool {
public:
    template<typename T>
    T* allocate() {
        // Implementation for object allocation
        return static_cast<T*>(pool_.allocate(sizeof(T)));
    }
    
    template<typename T>
    void deallocate(T* ptr) {
        pool_.deallocate(ptr, sizeof(T));
    }
    
private:
    // Pool implementation
    Pool pool_;
};
```

## Performance Optimization

### Const Correctness
```cpp
class OptimizedClass {
public:
    // Const member functions can be called on const objects
    int getValue() const { return value_; }
    
    // Non-const member functions for modification
    void setValue(int value) { value_ = value; }
    
    // Const reference parameters when not modifying
    void processData(const std::vector<int>& data) const;
    
    // Const member variables
    const int MAX_SIZE = 1000;
    
private:
    int value_;
    mutable int cacheHits_; // Can be modified even in const functions
};
```

### Move Semantics
```cpp
class MoveableClass {
public:
    // Move constructor
    MoveableClass(MoveableClass&& other) noexcept
        : data_(std::move(other.data_)) {
        other.data_ = nullptr;
    }
    
    // Move assignment operator
    MoveableClass& operator=(MoveableClass&& other) noexcept {
        if (this != &other) {
            delete[] data_;
            data_ = std::move(other.data_);
            other.data_ = nullptr;
        }
        return *this;
    }
    
    // Use std::move for rvalue references
    void setData(std::vector<int>&& data) {
        data_ = std::move(data);
    }
    
private:
    std::vector<int> data_;
};
```

### Inline Functions
```cpp
// Use inline for small, frequently called functions
inline int add(int a, int b) {
    return a + b;
}

// Member functions defined in class are implicitly inline
class MathUtils {
public:
    inline int multiply(int a, int b) const {
        return a * b;
    }
    
    // This is also inline
    int divide(int a, int b) const {
        return b != 0 ? a / b : 0;
    }
};
```

## Error Handling

### Exception Design
```cpp
// Custom exception classes
class NetworkException : public std::runtime_error {
public:
    explicit NetworkException(const std::string& message)
        : std::runtime_error("Network error: " + message) {}
};

class ValidationException : public std::invalid_argument {
public:
    explicit ValidationException(const std::string& field, const std::string& message)
        : std::invalid_argument("Validation failed for " + field + ": " + message) {}
};

// Use exceptions for exceptional cases
void connectToServer(const std::string& host, int port) {
    if (host.empty()) {
        throw ValidationException("host", "Host cannot be empty");
    }
    
    if (port < 1 || port > 65535) {
        throw ValidationException("port", "Port must be between 1 and 65535");
    }
    
    // Attempt connection
    if (!attemptConnection(host, port)) {
        throw NetworkException("Failed to connect to " + host + ":" + std::to_string(port));
    }
}
```

### Error Codes and Results
```cpp
// Use std::expected (C++23) or similar for expected errors
enum class ErrorCode {
    Success,
    InvalidInput,
    NetworkError,
    Timeout
};

template<typename T>
class Result {
public:
    static Result<T> success(T value) {
        return Result<T>(std::move(value));
    }
    
    static Result<T> failure(ErrorCode error) {
        return Result<T>(error);
    }
    
    bool isSuccess() const { return hasValue_; }
    bool isFailure() const { return !hasValue_; }
    
    T& value() {
        if (!hasValue_) {
            throw std::runtime_error("Accessing value of failed result");
        }
        return value_;
    }
    
    ErrorCode error() const {
        if (hasValue_) {
            throw std::runtime_error("Accessing error of successful result");
        }
        return error_;
    }
    
private:
    Result(T value) : value_(std::move(value)), hasValue_(true) {}
    Result(ErrorCode error) : error_(error), hasValue_(false) {}
    
    T value_;
    ErrorCode error_;
    bool hasValue_;
};
```

## Testing and Debugging

### Unit Testing
```cpp
#include <gtest/gtest.h>

class CalculatorTest : public ::testing::Test {
protected:
    void SetUp() override {
        calculator_ = std::make_unique<Calculator>();
    }
    
    void TearDown() override {
        calculator_.reset();
    }
    
    std::unique_ptr<Calculator> calculator_;
};

TEST_F(CalculatorTest, AdditionWorks) {
    EXPECT_EQ(calculator_->add(2, 3), 5);
    EXPECT_EQ(calculator_->add(-1, 1), 0);
    EXPECT_EQ(calculator_->add(0, 0), 0);
}

TEST_F(CalculatorTest, DivisionByZeroThrows) {
    EXPECT_THROW(calculator_->divide(10, 0), std::invalid_argument);
}
```

### Debugging and Logging
```cpp
#include <iostream>
#include <cassert>

class Logger {
public:
    enum class Level {
        Debug,
        Info,
        Warning,
        Error
    };
    
    template<typename... Args>
    static void log(Level level, Args&&... args) {
        if (level >= currentLevel_) {
            std::cout << "[" << levelToString(level) << "] ";
            (std::cout << ... << std::forward<Args>(args)) << std::endl;
        }
    }
    
    static void setLevel(Level level) { currentLevel_ = level; }
    
private:
    static std::string levelToString(Level level) {
        switch (level) {
            case Level::Debug: return "DEBUG";
            case Level::Info: return "INFO";
            case Level::Warning: return "WARN";
            case Level::Error: return "ERROR";
            default: return "UNKNOWN";
        }
    }
    
    static Level currentLevel_;
};

// Use assertions for invariants
void processData(const std::vector<int>& data) {
    assert(!data.empty() && "Data cannot be empty");
    
    // Process data...
    
    assert(std::is_sorted(data.begin(), data.end()) && "Data must be sorted");
}

// Use logging for debugging
void complexAlgorithm(const std::vector<int>& data) {
    Logger::log(Logger::Level::Debug, "Starting algorithm with ", data.size(), " elements");
    
    // Algorithm implementation...
    
    Logger::log(Logger::Level::Info, "Algorithm completed successfully");
}
```

## Code Organization

### Header Files
```cpp
// example.h
#pragma once  // Use #pragma once instead of include guards

#include <vector>
#include <string>
#include <memory>

// Forward declarations when possible
class DependencyClass;

namespace example {

class ExampleClass {
public:
    explicit ExampleClass(const std::string& name);
    ~ExampleClass() = default;
    
    // Disable copy, enable move
    ExampleClass(const ExampleClass&) = delete;
    ExampleClass& operator=(const ExampleClass&) = delete;
    ExampleClass(ExampleClass&&) = default;
    ExampleClass& operator=(ExampleClass&&) = default;
    
    // Public interface
    void processData(const std::vector<int>& data);
    std::string getName() const;
    
private:
    // Private implementation
    void validateInput(const std::vector<int>& data);
    
    // Member variables
    std::string name_;
    std::unique_ptr<DependencyClass> dependency_;
};

} // namespace example
```

### Implementation Files
```cpp
// example.cpp
#include "example.h"
#include "dependency.h"
#include <algorithm>
#include <stdexcept>

namespace example {

ExampleClass::ExampleClass(const std::string& name)
    : name_(name)
    , dependency_(std::make_unique<DependencyClass>()) {
    if (name.empty()) {
        throw std::invalid_argument("Name cannot be empty");
    }
}

void ExampleClass::processData(const std::vector<int>& data) {
    validateInput(data);
    
    // Process the data...
    for (const auto& value : data) {
        dependency_->process(value);
    }
}

std::string ExampleClass::getName() const {
    return name_;
}

void ExampleClass::validateInput(const std::vector<int>& data) {
    if (data.empty()) {
        throw std::invalid_argument("Data cannot be empty");
    }
    
    if (std::any_of(data.begin(), data.end(), [](int x) { return x < 0; })) {
        throw std::invalid_argument("Data cannot contain negative values");
    }
}

} // namespace example
```

## Best Practices Summary

### Do's
- ✅ Use smart pointers for memory management
- ✅ Implement RAII for all resources
- ✅ Use const correctness extensively
- ✅ Prefer range-based for loops
- ✅ Use auto for complex types
- ✅ Implement exception safety guarantees
- ✅ Use move semantics for performance
- ✅ Write self-documenting code
- ✅ Use meaningful variable names
- ✅ Implement comprehensive testing

### Don'ts
- ❌ Don't use raw pointers for ownership
- ❌ Don't use C-style casts
- ❌ Don't ignore compiler warnings
- ❌ Don't use global variables
- ❌ Don't use goto statements
- ❌ Don't use C-style arrays for dynamic data
- ❌ Don't use void* without good reason
- ❌ Don't use #define for constants
- ❌ Don't use using namespace std;
- ❌ Don't ignore exception safety

### Performance Guidelines
- 🚀 Profile before optimizing
- 🚀 Use const references for large objects
- 🚀 Prefer move over copy
- 🚀 Use appropriate data structures
- 🚀 Minimize virtual function calls
- 🚀 Use inline for small functions
- 🚀 Leverage compiler optimizations
- 🚀 Use SIMD when applicable
- 🚀 Optimize for cache locality
- 🚀 Use appropriate memory allocators

## Reference

For comprehensive C++ tech stack standards, build tools, and advanced patterns, see `../tech-stacks/cpp-standards.md`.
