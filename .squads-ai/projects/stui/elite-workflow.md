---
description: Elite Squad Development Workflow for STUI Project
squad: elite
version: 1.0.0
encoding: UTF-8
---

# Elite Squad Development Workflow - STUI Project

## Overview

This workflow defines the development process for STUI using the Elite squad's specialized capabilities in Rust and Smalltalk/Pharo development. The workflow emphasizes high-performance, memory-efficient code with comprehensive testing and quality assurance.

## Development Process

### 1. Session Kickoff (Director Agent)

<session_kickoff>
  ACTION: Assess current project state and provide development guidance
  SCOPE: Current Phase 2 progress, immediate priorities, squad agent recommendations
  OUTPUT: Clear development direction and agent activation plan
</session_kickoff>

**Required Steps:**
- Review current Phase 2 status and progress
- Identify immediate development priorities
- Recommend appropriate squad agents for current tasks
- Provide session-specific development guidance

### 2. Feature Development (Software Engineer Agent)

<feature_development>
  ACTION: Implement Rust and Smalltalk/Pharo features according to Phase 2 priorities
  SCOPE: Code completion, error handling, session persistence, theme system
  OUTPUT: Working features with comprehensive testing
</feature_development>

**Development Standards:**
- **Rust Code**: High-performance, memory-efficient, comprehensive error handling
- **Smalltalk Integration**: Clean protocol design, robust error handling
- **Testing**: Maintain 88+ test coverage, add tests for new features
- **Documentation**: Clear API documentation and integration guides

**Current Phase 2 Features:**
1. **Code Completion System**
   - Method and class autocompletion
   - Context-aware suggestions
   - Performance optimization for large codebases

2. **Enhanced Error Display**
   - Clear, actionable error messages
   - Error categorization and suggestions
   - Integration with Smalltalk error handling

3. **Session Persistence**
   - Connection state management
   - Automatic reconnection logic
   - Session configuration persistence

4. **Basic Theme System**
   - Light/dark theme foundation
   - Terminal-compatible color schemes
   - Syntax highlighting preparation

### 3. Quality Assurance (Collaboration Agent)

<quality_assurance>
  ACTION: Ensure code quality, testing coverage, and integration stability
  SCOPE: Code review, testing, integration testing, performance validation
  OUTPUT: High-quality, tested code ready for integration
</quality_assurance>

**Quality Standards:**
- **Code Review**: Comprehensive review of all changes
- **Testing**: Unit tests, integration tests, performance tests
- **Integration**: Seamless Rust ↔ Pharo communication
- **Performance**: ZeroMQ networking performance validation

### 4. Workflow Management (Git Workflow Agent)

<workflow_management>
  ACTION: Manage version control, branches, and development workflow
  SCOPE: Feature branches, worktree management, testing integration
  OUTPUT: Organized development workflow with clear progress tracking
</workflow_management>

**Workflow Standards:**
- **Feature Branches**: Create dedicated branches for Phase 2 features
- **Worktree Management**: Utilize existing worktrees for parallel development
- **Testing Integration**: Ensure all tests pass before merging
- **Documentation**: Update project status and progress tracking

### 5. UX Integration (UX Expert + UI Implementor Agents)

<ux_integration>
  ACTION: Improve user experience and interface implementation
  SCOPE: User interface design, interaction patterns, accessibility
  OUTPUT: Enhanced user experience with professional interface
</ux_integration>

**UX Standards:**
- **Interface Design**: Professional, enterprise-grade aesthetics
- **Interaction Patterns**: Keyboard-driven, terminal-optimized workflows
- **Accessibility**: Screen reader support, keyboard navigation
- **Performance**: Responsive interface with minimal latency

## Technology-Specific Guidelines

### Rust Development

<rust_guidelines>
  **Performance**: Use zero-copy operations, minimize allocations
  **Memory Safety**: Leverage Rust's ownership system, avoid unsafe code
  **Error Handling**: Comprehensive error types with context
  **Testing**: Unit tests, integration tests, performance benchmarks
  **Documentation**: Clear API documentation with examples
</rust_guidelines>

**Key Areas:**
- TUI client performance optimization
- ZeroMQ networking efficiency
- JSON protocol serialization
- Cross-platform compatibility

### Smalltalk/Pharo Integration

<smalltalk_guidelines>
  **Protocol Design**: Clean, extensible JSON protocol
  **Error Handling**: Robust error reporting and recovery
  **Performance**: Efficient object serialization and communication
  **Testing**: Comprehensive protocol testing and validation
  **Documentation**: Clear protocol specification and examples
</smalltalk_guidelines>

**Key Areas:**
- Protocol implementation and testing
- Error handling and reporting
- Session management and persistence
- Multi-dialect architecture preparation

### ZeroMQ Networking

<zeromq_guidelines>
  **Performance**: High-throughput, low-latency communication
  **Reliability**: Robust error handling and reconnection logic
  **Security**: Secure authentication and session management
  **Testing**: Network performance and reliability testing
  **Documentation**: Network architecture and protocol guides
</zeromq_guidelines>

**Key Areas:**
- Network performance optimization
- Connection reliability and recovery
- Security and authentication
- Cross-platform networking

## Development Phases

### Phase 2: Core Development Tools (Current)

**Timeline**: 8-10 weeks (Weeks 1-4: Core Development Excellence)

**Priority Features:**
- [ ] **Code Completion** `M` - Method and class autocompletion  
- [ ] **Enhanced Error Display** `S` - Clear, actionable error messages
- [ ] **Session Persistence** `S` - Remember connection state across restarts
- [ ] **Command History** `S` - Persistent command history
- [ ] **Basic Theme System** `S` - Light/dark terminal themes foundation

**Success Criteria:**
- Code completion system working with method and class suggestions
- Clear, actionable error messages displayed
- Session persistence maintaining connection state
- Basic theme system with light/dark support
- Maintain 88+ test coverage

### Phase 3: Visual Excellence & AI Assistant

**Timeline**: 6-8 weeks

**Key Features:**
- Syntax highlighting engine
- Advanced theme system
- AI assistant integration
- Professional tier launch

### Phase 4: Remote Debugging & Enterprise

**Timeline**: 8-10 weeks

**Key Features:**
- Remote debugging system
- Production monitoring tools
- Enterprise tier features
- Multi-dialect support

## Success Metrics

### Development Metrics
- **Test Coverage**: Maintain 88+ tests passing
- **Performance**: ZeroMQ networking performance targets
- **Quality**: Code review completion and quality standards
- **Progress**: Phase 2 feature completion on schedule

### Integration Metrics
- **Rust ↔ Pharo**: Seamless communication and integration
- **Network Performance**: Reliable ZeroMQ communication
- **User Experience**: Professional interface and workflow
- **Documentation**: Comprehensive guides and examples

## Agent Coordination

### Primary Development Flow
1. **Director Agent**: Session kickoff and project assessment
2. **Software Engineer Agent**: Feature implementation and development
3. **Collaboration Agent**: Quality assurance and testing
4. **Git Workflow Agent**: Version control and workflow management
5. **UX/UI Agents**: User experience and interface improvements

### Agent Activation Triggers
- **Software Engineer**: Rust/Smalltalk development tasks
- **Collaboration**: Code review and quality assurance needs
- **Git Workflow**: Version control and branch management
- **UX Expert**: User experience research and design
- **UI Implementor**: Interface implementation and optimization

## Conclusion

This Elite squad workflow provides a comprehensive development process for STUI, leveraging the squad's specialized capabilities in Rust and Smalltalk/Pharo development. The workflow emphasizes high-quality, performant code with comprehensive testing and professional user experience.
