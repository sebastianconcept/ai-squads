---
description: ZeroMQ FFI Bridge for Pharo - High-performance messaging integration
squad: elite
version: 1.0.0
encoding: UTF-8
---

# ZeroMQ FFI Bridge Project

## Project Overview

**ZeroMQ FFI Bridge** is a minimalistic, extensible Foreign Function Interface (FFI) bridge to the ZeroMQ library for Pharo 13, following proven SQLite3 patterns. It provides reliable, performant messaging capabilities with comprehensive testing and clear extensibility.

## Current Status

### Phase 1: PLANNING ✅
- **Project planning and architecture design** ✅
- **Requirements analysis and scope definition** ✅
- **Technical approach validation** ✅
- **Team coordination and workflow setup** ✅

### Phase 2: FOUNDATION (Week 1) - PLANNED 🔧
- **BaselineOfZeroMQ creation** - Metacello baseline setup
- **ZeroMQConstants implementation** - Constants and enums
- **ZeroMQLibrary FFI interface** - Main FFI interface following SQLite3 patterns
- **External object wrappers** - Safe FFI object management
- **Basic unit tests** - Test framework setup

### Phase 3: CORE IMPLEMENTATION (Week 2) - PLANNED 📋
- **ZeroMQContext implementation** - Thread-safe context management
- **ZeroMQSocket implementation** - High-level socket abstraction
- **Basic operations** - Bind, connect, send, receive functionality
- **Error handling** - Comprehensive error management

### Phase 4: TESTING & QUALITY (Week 3) - PLANNED 📋
- **Comprehensive unit tests** - Full test coverage
- **Mock implementation** - Testing without real ZeroMQ
- **Integration tests** - End-to-end validation
- **Performance testing** - Performance benchmarks

### Phase 5: EXAMPLES & DOCUMENTATION (Week 4) - PLANNED 📋
- **Request-reply example** - Basic messaging pattern
- **Publish-subscribe example** - Event-driven messaging
- **API documentation** - Comprehensive documentation
- **Usage guides** - Best practices and examples

## Technology Stack

### Core Technologies
- **Pharo 13**: Target Smalltalk environment
- **FFI**: Foreign Function Interface for native library integration
- **ZeroMQ**: High-performance messaging library
- **Metacello**: Package management and baseline system

### Architecture
- **SQLite3 Patterns**: Following proven FFI implementation patterns
- **Thread Safety**: Single OS thread, multiple Smalltalk processes
- **Extensible Design**: Easy to add new features incrementally
- **Comprehensive Testing**: Mock implementation and unit tests

## Squad Integration

### Elite Squad Focus
- **Smalltalk/Pharo Development**: Deep understanding of Pharo object system
- **FFI Implementation**: Foreign function interface expertise
- **Systems Programming**: Native library integration
- **Quality Assurance**: Comprehensive testing and documentation

### Available Agents
- **@agent:alan** - Smalltalk/Pharo development, image-centric workflows
- **@agent:rusty** - Software engineering and architecture
- **@agent:scribas** - Version control and workflow management
- **@agent:team** - Team coordination and quality assurance
- **@agent:steve** - Session kickoff and project guidance

## Development Workflow

### Current Branch Strategy
- **main**: Main development branch
- **feature/***: Feature development branches
- **worktrees**: Active development worktrees for parallel development

### Active Worktrees
- `zeromq-ffi-foundation` - Foundation implementation
- `zeromq-core-implementation` - Core functionality
- `zeromq-testing` - Testing framework and validation
- `zeromq-examples` - Examples and documentation

## Next Steps

### Immediate Priorities (Phase 2 - This Week)
1. **BaselineOfZeroMQ** - Create Metacello baseline for package management
2. **ZeroMQConstants** - Implement all necessary ZeroMQ constants and enums
3. **ZeroMQLibrary** - Create main FFI interface following SQLite3 patterns
4. **External Objects** - Set up safe FFI object wrappers
5. **Basic Tests** - Create initial test framework

### Phase 2 Continuation (Weeks 2-4)
1. **Core Implementation** - Context and socket management
2. **Testing Framework** - Comprehensive unit and integration tests
3. **Examples** - Request-reply and pub-sub examples
4. **Documentation** - API documentation and usage guides

### Long-term Vision
- **Advanced Socket Types** - Dealer, Router, Pair sockets
- **Message Patterns** - Multi-part messages, message routing
- **Socket Options** - Advanced configuration options
- **Polling Support** - Event-driven message handling
- **Security** - Authentication and encryption support

## Integration Notes

### SquadsAI Adoption
- **Adopted Date**: 2025-01-27
- **Squad**: Elite Squad (Smalltalk/Pharo Development)
- **Status**: Planning phase, ready for implementation
- **Workflow**: Elite squad development workflow with Smalltalk/Pharo focus

### Project Standards
- **Code Quality**: High-performance, memory-efficient Pharo code
- **Testing**: Comprehensive unit tests with mock implementation
- **Documentation**: Clear API documentation and usage guides
- **Performance**: Minimal FFI overhead with proper resource management

## Current Issues & Blockers

### Technical Considerations
- **ZMQ Context Startup**: Current implementation has startup issues
- **FFI Patterns**: Need to follow proven SQLite3 patterns
- **Thread Safety**: Single OS thread constraint with multiple Smalltalk processes
- **Memory Management**: Proper cleanup and resource management

### Quality Requirements
- **Test Coverage**: >95% unit test coverage required
- **Performance**: <1ms overhead for basic operations
- **Memory Usage**: No memory leaks in extended testing
- **Error Handling**: 100% error scenarios covered

## Success Metrics

### Technical Metrics
- **Test Coverage**: >95% unit test coverage
- **Performance**: <1ms overhead for basic operations
- **Memory Usage**: No memory leaks in extended testing
- **Error Handling**: 100% error scenarios covered
- **Thread Safety**: Verified under concurrent load

### Developer Experience Metrics
- **API Clarity**: Clear, intuitive API design
- **Documentation Quality**: Comprehensive and clear
- **Example Quality**: Working, well-documented examples
- **Extensibility**: Easy to add new features
- **Integration**: Seamless integration with existing code

## Implementation Guidelines

### Pharo 13 FFI Patterns
Following SQLite3 implementation patterns:
- **FFILibrary subclass**: Main FFI interface
- **External object wrappers**: Safe FFI object management
- **Type mapping**: Proper type conversion
- **Error handling**: Comprehensive error management
- **Resource cleanup**: Proper memory management

### Thread Safety Requirements
- **Single OS thread**: Pharo VM constraint
- **Multiple Smalltalk processes**: Supported
- **Mutex-based synchronization**: For shared resources
- **Context-level safety**: Thread-safe context management

### Performance Considerations
- **Minimal FFI overhead**: Efficient native calls
- **Buffer management**: Optimized message handling
- **Memory efficiency**: Proper resource cleanup
- **Scalability**: Support for high-throughput scenarios

