---
description: Project Decisions Template - Decision log and architectural decisions
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# STUI Development Decisions

This document tracks key architectural and implementation decisions made during STUI development, including rationale, alternatives considered, and consequences.

## Phase 2 Architecture Decisions (2025-08-28)

### Decision: Implement Comprehensive Client-Server Coordination Pattern

**Context**: Phase 2 features require sophisticated coordination between Rust TUI client and Pharo Smalltalk server for professional-grade development tools.

**Decision**: Establish a consistent pattern of client-server coordination where both sides implement complementary functionality:
1. **Client-side**: Rich UI components, user interaction, and local state management
2. **Server-side**: Data processing, business logic, and persistent storage
3. **Protocol**: Standardized message formats for seamless communication

**Rationale**:
- **Professional Quality**: Enables enterprise-grade development tools
- **Scalability**: Server can handle complex operations while client focuses on UX
- **Data Integrity**: Centralized data management with client-side caching
- **User Experience**: Responsive UI with powerful backend capabilities
- **Maintainability**: Clear separation of concerns between client and server

**Alternatives Considered**:
- **Client-only implementation**: Limited functionality, no persistence
- **Server-only approach**: Poor user experience, no offline capability
- **Simple request-response**: Basic functionality but limited coordination

**Consequences**:
- ✅ **Positive**: Professional development experience, robust architecture
- ⚠️ **Neutral**: Increased complexity, requires both client and server development
- ❌ **Negative**: More complex testing and deployment

---

### Decision: Enhance Error Handling with Rich Context and Severity

**Context**: Basic error messages provide insufficient information for debugging and user assistance.

**Decision**: Implement comprehensive error handling with:
1. **Error Severity Levels**: Critical, Error, Warning, Info
2. **Rich Context**: Detailed error information and recovery suggestions
3. **Client-Server Coordination**: Server provides context, client displays appropriately
4. **User-Friendly Formatting**: Clear visual hierarchy and actionable information

**Rationale**:
- **User Experience**: Better error understanding and resolution
- **Debugging**: Comprehensive error information for developers
- **Professional Quality**: Enterprise-grade error reporting
- **Accessibility**: Clear error categorization and guidance

**Alternatives Considered**:
- **Simple error strings**: Basic but insufficient information
- **Error codes only**: Technical but not user-friendly
- **Client-side only**: Limited error context

**Consequences**:
- ✅ **Positive**: Professional error handling, better user experience
- ⚠️ **Neutral**: More complex error structures
- ❌ **Negative**: Increased protocol complexity

---

### Decision: Implement Real Smalltalk Data Integration for Code Completion

**Context**: Generic code completion provides limited value without real Smalltalk environment data.

**Decision**: Build server-side Smalltalk data discovery system that provides:
1. **Class Discovery**: Real Smalltalk class names and hierarchies
2. **Method Suggestions**: Actual method names from the environment
3. **Variable Tracking**: Context-aware variable suggestions
4. **Message Selectors**: Real message selector completion
5. **Package Information**: Smalltalk package organization

**Rationale**:
- **Authentic Experience**: Real Smalltalk development environment
- **User Value**: Meaningful completion suggestions
- **Professional Quality**: IDE-like development experience
- **Context Awareness**: Suggestions based on actual code structure

**Alternatives Considered**:
- **Generic suggestions**: Simple but not useful
- **Static completion**: Limited to predefined lists
- **Client-side only**: No real environment integration

**Consequences**:
- ✅ **Positive**: Authentic Smalltalk development experience
- ⚠️ **Neutral**: Requires Smalltalk environment knowledge
- ❌ **Negative**: Tied to Smalltalk ecosystem

---

### Decision: Extend Session Persistence with Client-Server Coordination

**Context**: Basic session persistence provides limited value without server-side coordination.

**Decision**: Enhance session system with:
1. **Extended Timeout**: Increase from 7 to 30 days for better user experience
2. **Context Restoration**: Server-side context preservation and restoration
3. **State Synchronization**: Real-time state updates between client and server
4. **Metadata Management**: Comprehensive session tracking and analytics

**Rationale**:
- **User Experience**: Longer session retention, better context preservation
- **Professional Quality**: Enterprise-grade session management
- **Data Integrity**: Coordinated state management across client and server
- **Analytics**: Session usage patterns and optimization opportunities

**Alternatives Considered**:
- **Client-only persistence**: Limited coordination, basic functionality
- **Server-only approach**: Poor user experience, no offline capability
- **Short timeouts**: Frequent session loss, poor user experience

**Consequences**:
- ✅ **Positive**: Professional session management, better user experience
- ⚠️ **Neutral**: Increased complexity, longer development time
- ❌ **Negative**: More complex testing and deployment

---

### Decision: Implement Comprehensive Command History with Analytics

**Context**: Basic command history provides limited value without search, filtering, and analytics.

**Decision**: Build enterprise-grade command history system with:
1. **Advanced Search**: Query, category, session, and success-based filtering
2. **Statistics & Analytics**: Usage patterns, success rates, performance metrics
3. **Persistent Storage**: File-based history with export/import capabilities
4. **Smart Suggestions**: Context-aware command suggestions and autocomplete
5. **Client-Server Coordination**: Coordinated history tracking and management

**Rationale**:
- **Professional Workflow**: Enterprise-grade development experience
- **User Productivity**: Better command discovery and reuse
- **Quality Assurance**: Track command success rates and performance
- **Data Management**: Comprehensive history with export/import capabilities

**Alternatives Considered**:
- **Simple history list**: Basic but limited functionality
- **Client-only tracking**: No server-side coordination
- **No persistence**: History lost on restart

**Consequences**:
- ✅ **Positive**: Professional development workflow, comprehensive tracking
- ⚠️ **Neutral**: Increased complexity, more storage requirements
- ❌ **Negative**: More complex testing and deployment

---

### Decision: Use Git Submodules for Smalltalk Implementation Management

**Context**: Need to manage multiple Smalltalk implementations (Pharo, Squeak, etc.) within the STUI project while maintaining clean separation, version control, and team collaboration.

**Decision**: Implement each Smalltalk implementation as a separate git submodule within the STUI project structure:
1. **Pharo Implementation**: `smalltalk/pharo/` as git submodule
2. **Future Implementations**: `smalltalk/squeak/`, `smalltalk/dolphin/` as separate submodules
3. **Independent Versioning**: Each implementation can evolve independently
4. **Clean Integration**: Main STUI repository references specific submodule commits

**Rationale**:
- **Separation of Concerns**: Each Smalltalk implementation is isolated and independently managed
- **Version Control**: Independent commit history and release cycles for each implementation
- **Team Collaboration**: Different teams can work on different Smalltalk implementations
- **Clean Architecture**: Main STUI repository remains focused on core functionality
- **Dependency Management**: Clear dependency boundaries and update control
- **Scalability**: Easy to add new Smalltalk implementations without cluttering main repo

**Alternatives Considered**:
- **Monolithic Repository**: All implementations in one repo - would become unwieldy
- **Separate Repositories**: No integration - harder to coordinate development
- **Copy-Paste Approach**: Manual copying - no version control, hard to maintain
- **Package Management**: External dependency system - adds complexity, less control

**Implementation Details**:
- **Submodule Structure**: `smalltalk/[implementation-name]/` directories
- **Version Pinning**: Main repo pins specific submodule commits for stability
- **Update Process**: Manual submodule updates with testing and validation
- **Documentation**: Clear instructions for submodule management and updates

**Consequences**:
- ✅ **Positive**: Clean separation, independent versioning, team collaboration
- ✅ **Positive**: Scalable architecture for multiple Smalltalk implementations
- ✅ **Positive**: Maintains focus in main STUI repository
- ⚠️ **Neutral**: Requires understanding of git submodule workflow
- ⚠️ **Neutral**: Slightly more complex repository management
- ❌ **Negative**: Submodule updates require coordination and testing

**Status**: Implemented with Pharo submodule, ready for additional implementations

---

## Session Persistence Feature (2025-08-23)

### Decision: Implement Session Persistence with File-Based Storage

**Context**: Need to implement session persistence for STUI to save and restore user sessions across application restarts and network disconnections.

**Decision**: Implement file-based session storage using JSON format with local file system persistence.

**Rationale**:
- **Simplicity**: File-based storage is straightforward to implement and debug
- **Cross-platform**: Works consistently across different operating systems
- **Human-readable**: JSON format allows manual inspection and debugging
- **No external dependencies**: Doesn't require database setup or external services
- **Performance**: Sufficient for typical session data sizes and access patterns

**Alternatives Considered**:
- **Database storage**: Overkill for session data, adds complexity
- **In-memory only**: Sessions lost on application restart
- **Cloud storage**: Requires network, adds latency and complexity
- **Binary format**: Less human-readable, harder to debug

**Consequences**:
- ✅ **Positive**: Simple implementation, easy debugging, cross-platform
- ⚠️ **Neutral**: File I/O operations, manual cleanup needed
- ❌ **Negative**: Not suitable for distributed/multi-user scenarios

---

### Decision: Separate Session Management into Multiple Components

**Context**: Need to organize session persistence functionality in a maintainable way.

**Decision**: Split session functionality into three main components:
1. **SessionPanel**: UI component for session management
2. **SessionStorage**: File I/O and persistence layer
3. **SessionManager**: Business logic and session lifecycle management

**Rationale**:
- **Separation of Concerns**: Each component has a single responsibility
- **Testability**: Components can be tested independently
- **Maintainability**: Changes to one component don't affect others
- **Reusability**: Components can be used in different contexts
- **Clear Interfaces**: Well-defined boundaries between layers

**Alternatives Considered**:
- **Monolithic approach**: All functionality in one struct/class
- **Two-component split**: UI + backend only
- **More granular split**: Additional components for specific functionality

**Consequences**:
- ✅ **Positive**: Clean architecture, easy testing, maintainable code
- ⚠️ **Neutral**: More files to manage, slightly more complex setup
- ❌ **Negative**: None significant

---

### Decision: Use Arc<Mutex<SessionStorage>> for SessionManager

**Context**: SessionManager needs to share SessionStorage with other components while maintaining thread safety.

**Decision**: Use `Arc<Mutex<SessionStorage>>` to share storage between components.

**Rationale**:
- **Thread Safety**: Mutex ensures safe concurrent access
- **Shared Ownership**: Arc allows multiple components to reference storage
- **Rust Idioms**: Follows Rust's ownership and borrowing rules
- **Performance**: Minimal overhead for session operations
- **Simplicity**: Standard Rust pattern for shared mutable state

**Alternatives Considered**:
- **Clone on each access**: Higher memory usage, simpler but less efficient
- **RwLock**: More complex, overkill for session storage
- **Channel-based communication**: More complex, asynchronous operations

**Consequences**:
- ✅ **Positive**: Thread-safe, follows Rust patterns, efficient
- ⚠️ **Neutral**: Slight complexity in error handling (unwrap on lock)
- ❌ **Negative**: Potential for deadlocks if not used carefully

---

### Decision: Implement Session Protocol Extensions in stui-protocol

**Context**: Need to define data structures for session state and metadata.

**Decision**: Add session-related types directly to the existing `stui-protocol` crate.

**Rationale**:
- **Centralized Protocol**: All protocol definitions in one place
- **Consistency**: Follows existing pattern for protocol types
- **Reusability**: Types can be used by both client and server
- **Versioning**: Protocol changes tracked in one location
- **Documentation**: Single source of truth for protocol structure

**Alternatives Considered**:
- **Separate session protocol crate**: Overkill for current needs

**Consequences**:
- ✅ **Positive**: Centralized protocol, consistent with existing structure
- ⚠️ **Neutral**: Protocol crate grows in size
- ❌ **Negative**: None significant

---

## Server-Initiated Messaging Architecture (2025-01-21)

### Decision: Implement Hybrid ZeroMQ Pattern for Real-Time Capabilities

**Context**: Need to transform STUI from a request-response system to a real-time, interactive platform with server-initiated messaging capabilities.

**Decision**: Implement a hybrid ZeroMQ architecture combining REQ-REP (existing) with PUB-SUB (new) patterns.

**Rationale**:
- **Bidirectional Communication**: Maintains existing request-response while adding push notifications
- **Real-Time Updates**: Enables instant server-initiated notifications without client polling
- **Scalability**: PUB-SUB pattern supports unlimited subscribers
- **Backward Compatibility**: Existing clients continue working unchanged
- **ZeroMQ Reliability**: Leverages proven ZeroMQ message delivery guarantees
- **Performance**: No polling required, push-based updates are more efficient

**Architecture Design**:
- **Port 5555**: REQ-REP socket for existing client commands (unchanged)
- **Port 5556**: PUB socket for server-initiated notifications (new)
- **Client Connection**: Clients connect to both ports for full functionality
- **Message Flow**: Commands via REQ-REP, notifications via PUB-SUB

**Alternatives Considered**:
- **WebSocket**: More complex, requires HTTP upgrade, less suitable for terminal applications
- **Server-Sent Events**: Limited to one-way communication, not suitable for bidirectional needs
- **Long Polling**: Inefficient, increases server load, poor user experience
- **Message Queue (Redis/RabbitMQ)**: Overkill, adds external dependencies, increases complexity

**Implementation Phases**:
1. **Phase 1 (Week 1-2)**: Dual socket architecture setup
2. **Phase 2 (Week 2-4)**: Model change observers in Smalltalk
3. **Phase 3 (Week 3-5)**: Notification protocol and message format
4. **Phase 4 (Week 5-8)**: Real-time collaboration foundation

**Consequences**:
- ✅ **Positive**: Real-time capabilities, improved user experience, collaboration support
- ✅ **Positive**: Maintains backward compatibility, incremental rollout possible
- ✅ **Positive**: Scalable architecture, supports unlimited clients
- ⚠️ **Neutral**: Requires client updates for full functionality, dual port management
- ❌ **Negative**: Slightly more complex server setup, additional port management

**Timeline**: Q1 2026 implementation as part of Real-Time Foundation phase.

---

## Code Organization and Quality Standards (2025-08-24)

### Decision: Reorganize Large Monolithic Files into Modular Directory Structure

**Context**: The codebase had large, monolithic files (event_loop.rs, input.rs, server_client.rs) that were difficult to maintain, test, and understand. Following the established SessionManager pattern and updated rust-style.md guidelines.

**Decision**: Break down large files into focused, modular directories with clear separation of concerns:
1. **EventLoop**: Split into `config/`, `events/`, `handler/`, `terminal/`, `event_loop/`, `errors/`
2. **InputHandler**: Split into `key_binding/`, `command/`, `processor/`, `input_handler/`, `errors/`
3. **ServerClient**: Split into `config/`, `connection/`, `server_client/`, `server_result/`, `errors/`
4. **TerminalDetection**: Split into `capabilities/`, `detection/`, `detector/`, `types/`, `errors/`

**Rationale**:
- **Maintainability**: Each module focuses on specific functionality
- **Testability**: Modules can be tested independently
- **Code Organization**: Follows established SessionManager pattern
- **Error Handling**: Centralized domain-specific error types
- **Configuration**: Separated configuration concerns
- **Separation of Concerns**: Clear boundaries between different responsibilities

**Alternatives Considered**:
- **Keep monolithic structure**: Would continue to be difficult to maintain
- **Partial reorganization**: Would not achieve full benefits
- **Different organization pattern**: Could break consistency with existing code

**Consequences**:
- ✅ **Positive**: Better code organization, easier testing, improved maintainability
- ✅ **Positive**: Consistent with established patterns, better error handling
- ✅ **Positive**: Enhanced configurability (e.g., EventLoop performance presets)
- ⚠️ **Neutral**: More files to manage, slightly more complex import structure
- ❌ **Negative**: None significant

---

### Decision: Implement Domain-Specific Error Handling with thiserror

**Context**: Need consistent error handling across all modules following rust-style.md guidelines.

**Decision**: Create dedicated `errors.rs` files for each major module using `thiserror` crate:
- **EventLoopError**: Terminal, event handling, rendering, configuration errors
- **InputError**: Key bindings, commands, handlers, input context errors
- **ServerClientError**: ZeroMQ, connection, serialization, timeout errors
- **TerminalDetectionError**: Terminal type, capabilities, environment, IO errors

**Rationale**:
- **Consistency**: Follows established rust-style.md patterns
- **Error Context**: Provides meaningful error messages with context
- **Type Safety**: Strongly typed error handling
- **Integration**: Seamless integration with anyhow and std::io::Error
- **Documentation**: Self-documenting error types

**Alternatives Considered**:
- **Generic error types**: Less informative, harder to handle specifically
- **String-based errors**: No type safety, harder to match on
- **Custom error traits**: More complex, overkill for current needs

**Consequences**:
- ✅ **Positive**: Consistent error handling, better debugging, type safety
- ✅ **Positive**: Follows Rust best practices, integrates with ecosystem
- ⚠️ **Neutral**: Slightly more code, need to maintain error types
- ❌ **Negative**: None significant

---

### Decision: Implement Domain-Specific Error Handling Pattern

**Context**: Need to provide meaningful error information to API consumers while maintaining clean interfaces and proper error handling throughout the session management system.

**Decision**: Implement a comprehensive error handling pattern that:
1. **Wraps lower-level errors** in domain-specific contexts
2. **Uses helper methods** to centralize error conversion logic
3. **Provides specific error variants** for different operations
4. **Returns domain errors** from public APIs, not implementation details

**Rationale**:
- **Better Debugging**: Developers know exactly which operation failed
- **Cleaner APIs**: Callers get meaningful errors, not raw storage errors
- **Maintainability**: Error handling logic is centralized and consistent
- **Type Safety**: Compiler enforces proper error handling
- **User Experience**: API consumers receive contextual error information
- **Follows Rust Best Practices**: Proper use of `Result<T, E>`, `#[from]`, and `#[source]`

**Implementation Pattern**:
```rust
#[derive(Error, Debug)]
pub enum SessionManagerError {
    /// Session storage failed: {0}
    StoreSession(#[source] SessionStorageError),
    /// Session storage unavailable for updating
    UpdateSessionAccess,
    /// Session delete failed: {0}
    DeleteSession(#[source] SessionStorageError),
    /// No active session to update
    NoActiveSessionToUpdate,
    /// Invalid session data: {0}
    InvalidSessionData(String),
}

impl SessionManager {
    fn get_storage(&self) -> Result<MutexGuard<SessionStorage>, SessionStorageError> {
        self.storage
            .lock()
            .map_err(|_| SessionStorageError::StorageLockPoisoned)
    }
}
```

**Alternatives Considered**:
- **Generic wrapper errors**: Too generic, poor debugging experience
- **Raw error propagation**: Exposes implementation details to API consumers
- **Option-based APIs**: Silent failures, harder to debug
- **Panic on errors**: Unreliable, poor user experience

**Consequences**:
- ✅ **Positive**: Excellent debugging experience, clean APIs, maintainable code
- ✅ **Positive**: Follows Rust error handling best practices
- ✅ **Positive**: Centralized error conversion logic
- ⚠️ **Neutral**: More error variants to maintain
- ❌ **Negative**: None significant

**Impact on Team Standards**:
This pattern has been documented in `.squads-ai/standards/code/rust-style.md` as the standard approach for error handling in Rust projects, establishing a team-wide pattern for:
- Domain-specific error types
- Helper methods for error conversion
- Contextual error mapping
- Result-based API design

---

### Decision: Use JSON for Session Data Serialization

**Context**: Need to serialize session data to files for persistence.

**Decision**: Use JSON format with serde for session data serialization.

**Rationale**:
- **Human-readable**: Easy to inspect and debug session files
- **Widely supported**: Excellent tooling and library support
- **Self-describing**: Structure is clear from the data itself
- **Flexible**: Easy to add/remove fields without breaking compatibility
- **Performance**: Sufficient for session data sizes

**Alternatives Considered**:
- **Binary formats (bincode, serde_cbor)**: Better performance, harder to debug
- **TOML**: More human-readable but less flexible
- **YML**: More human-readable but slower parsing
- **Custom format**: Maximum control but high maintenance cost

**Consequences**:
- ✅ **Positive**: Easy debugging, excellent tooling, flexible
- ⚠️ **Neutral**: Slightly larger file sizes than binary formats
- ❌ **Negative**: None significant

---

### Decision: Implement Context Preservation Methods

**Context**: Need to preserve specific aspects of user work during sessions.

**Decision**: Implement separate methods for preserving workspace context, inspector state, and object registry.

**Rationale**:
- **Granular Control**: Users can choose what to preserve
- **Flexibility**: Different components can preserve different state
- **Performance**: Only preserve what's needed
- **Maintainability**: Clear separation of concerns
- **Extensibility**: Easy to add new preservation methods

**Alternatives Considered**:
- **Single preservation method**: Less flexible, harder to maintain
- **Automatic preservation**: Less control, potential performance issues
- **Configuration-based**: More complex, harder to implement

**Consequences**:
- ✅ **Positive**: Flexible, maintainable, performant
- ⚠️ **Neutral**: More methods to implement and test
- ❌ **Negative**: None significant

---

### Decision: Use Event Callbacks for Session Lifecycle

**Context**: Need to notify other components when session events occur.

**Decision**: Implement callback-based event system for session lifecycle events.

**Rationale**:
- **Loose Coupling**: Components don't need direct references to each other
- **Extensibility**: Easy to add new event handlers
- **Testability**: Callbacks can be mocked for testing
- **Flexibility**: Different components can handle events differently
- **Rust Idioms**: Follows Rust's trait-based design patterns

**Alternatives Considered**:
- **Direct method calls**: Tighter coupling, harder to test
- **Event channels**: More complex, asynchronous operations
- **Observer pattern**: More complex, less flexible

**Consequences**:
- ✅ **Positive**: Loose coupling, extensible, testable
- ⚠️ **Neutral**: Slightly more complex setup
- ❌ **Negative**: None significant

---

### Decision: Implement Auto-save with Configurable Intervals

**Context**: Need to automatically save session state to prevent data loss.

**Decision**: Implement configurable auto-save with default 5-minute intervals.

**Rationale**:
- **Data Safety**: Prevents loss of user work
- **User Control**: Users can configure auto-save behavior
- **Performance**: Configurable intervals balance safety and performance
- **Flexibility**: Different use cases can have different settings
- **Default Safety**: Sensible defaults for most users

**Alternatives Considered**:
- **Fixed intervals**: Less flexible, might not suit all users
- **Event-based saving**: More complex, harder to predict
- **No auto-save**: Simpler but risk of data loss

**Consequences**:
- ✅ **Positive**: Data safety, user control, configurable
- ⚠️ **Neutral**: Slightly more complex implementation
- ❌ **Negative**: None significant

---

### Decision: Defer Multi-Image Architecture to Future Iteration

**Context**: Identified critical gaps in multi-image session management during implementation.

**Decision**: Document multi-image requirements and defer implementation to focus on core functionality first.

**Rationale**:
- **Incremental Development**: Build core features before advanced ones
- **Risk Management**: Avoid over-engineering early in development
- **Demo Focus**: Enable basic session persistence demo first
- **Learning**: Better understanding of requirements through core implementation
- **Iterative Improvement**: Can address multi-image needs in future phases

**Alternatives Considered**:
- **Implement multi-image now**: Higher risk, longer development time
- **Redesign architecture**: Major refactoring, delays core functionality
- **Abandon multi-image**: Limits future scalability

**Consequences**:
- ✅ **Positive**: Faster core feature delivery, lower risk, learn from experience
- ⚠️ **Neutral**: Multi-image support delayed, some technical debt
- ❌ **Negative**: Multi-image scenarios not supported in initial release

---

## Future Decisions to Consider

### Multi-Image Architecture
- **When**: After core session persistence is stable and tested
- **Scope**: Protocol extensions, SessionManager enhancements, UI updates
- **Priority**: High for production readiness

### Session Cleanup Strategies
- **When**: Before production deployment
- **Scope**: Orphaned session detection, automatic cleanup, user controls
- **Priority**: Medium for system health

### Performance Optimization
- **When**: After performance testing reveals bottlenecks
- **Scope**: Session loading/saving, memory usage, startup time
- **Priority**: Low unless performance issues arise

---

## Decision Template

### Decision: [Title]

**Context**: [What is the situation that requires a decision?]

**Decision**: [What was decided?]

**Rationale**: [Why was this decision made?]

**Alternatives Considered**: [What other options were evaluated?]

**Consequences**:
- ✅ **Positive**: [What are the benefits?]
- ⚠️ **Neutral**: [What are the trade-offs?]
- ❌ **Negative**: [What are the drawbacks?]

---

**Last Updated**: 2025-08-31  
**Next Review**: 2025-09-07  
**Status**: Active Development
