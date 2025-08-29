---
title: "ZeroMQ Integration Architecture Decision"
type: architectural-decision
date: 2025-08-29
status: approved
stakeholders: ["software-engineer", "git-workflow", "collaboration"]
impact: high
---

# Architectural Decision: ZeroMQ Integration for STUI

## Context

STUI currently operates with a mock TCP demonstration that validates the protocol design but prevents real-world usage. To transform STUI into a production-ready development tool, we need to implement real ZeroMQ communication between the Rust client and Pharo/Smalltalk server.

## Decision

**We will implement a production-ready ZeroMQ integration using a dual-approach strategy:**

1. **Primary Approach**: Use established `github://zeromq/zeromq-pharo` binding
2. **Fallback Strategy**: Custom FFI implementation for production reliability
3. **Architecture**: Maintain REQ/REP pattern with existing STUI JSON protocol

## Rationale

### Why ZeroMQ?
- **Performance**: Superior to TCP for message-oriented communication
- **Reliability**: Built-in patterns for distributed systems
- **Scalability**: Handles multiple concurrent connections efficiently
- **Protocol Preservation**: Works seamlessly with existing STUI JSON protocol

### Why Dual-Approach Strategy?
- **Risk Mitigation**: External bindings may have quality or maintenance issues
- **Production Reliability**: FFI fallback ensures deployment capability
- **Quality Assurance**: Automatic detection and fallback provides robustness

### Why Incremental Implementation?
- **Proven Foundation**: Mock demo validates protocol design
- **Risk Reduction**: Validate each layer before proceeding
- **Quality Gates**: Clear success criteria at each phase

## Implementation Strategy

### Phase 1: ZeroMQ Infrastructure (4-6 hours)
- Evaluate Pharo ZeroMQ binding quality
- Implement FFI fallback if needed
- Create production STUIZMQSocket with auto-detection
- Enhance Rust DemoClient with real ZeroMQ
- End-to-end connectivity validation

### Phase 2: Real Session Management (3-4 hours)
- Complete Pharo session classes with real Workspace integration
- Implement actual context preservation and object inspection
- Full session lifecycle testing with real Smalltalk evaluation

### Phase 3: Production Polish (2-3 hours)
- Production configuration and deployment readiness
- Enhanced error handling and connection monitoring
- Performance optimization and comprehensive documentation

## Technical Requirements

### Performance Targets
- **Throughput**: >100 requests/second sustained
- **Latency**: <100ms p95 response time for simple commands
- **Concurrency**: Support 5+ simultaneous clients
- **Stability**: 8+ hour session duration

### Quality Standards
- Zero crashes in 4+ hour continuous testing
- Memory usage stable and bounded
- Graceful error handling and recovery
- Production deployment documentation

## Risk Mitigation

### Primary Risk: ZeroMQ Binding Quality
- **Assessment Criteria**: Installation success, performance >1000 msg/sec, 24hr stability
- **Mitigation**: Complete FFI fallback implementation ready
- **Strategy**: Automatic detection and fallback

### Technical Risks
- **Integration Complexity**: Incremental development with quality gates
- **Performance Requirements**: Benchmarking and optimization at each phase
- **Session Management**: Real Pharo object complexity managed incrementally

## Consequences

### Positive
- **Production Deployment**: Real-world usage capability
- **Developer Experience**: Actual Smalltalk development workflows
- **Architecture Validation**: Proves STUI protocol design
- **Performance Baseline**: Quantified production characteristics

### Negative
- **Implementation Effort**: 9-13 hours of focused development
- **External Dependencies**: Reliance on ZeroMQ ecosystem
- **Complexity Increase**: More moving parts than mock demo

### Neutral
- **Protocol Preservation**: No breaking changes to existing STUI protocol
- **Demo Compatibility**: Mock demo remains functional for demonstrations

## Alternatives Considered

### Option 1: Enhanced TCP Implementation
- **Pros**: Simpler, no external dependencies
- **Cons**: Performance limitations, manual connection management
- **Verdict**: Rejected - doesn't provide production-grade messaging

### Option 2: WebSocket Integration
- **Pros**: Web-compatible, good tooling
- **Cons**: HTTP overhead, less suited for request-reply patterns
- **Verdict**: Rejected - ZeroMQ better for this use case

### Option 3: Custom Protocol
- **Pros**: Full control, optimized for specific needs
- **Cons**: High implementation effort, reinventing proven solutions
- **Verdict**: Rejected - ZeroMQ provides proven messaging patterns

## Success Metrics

### Functional
- [ ] All 11 STUI protocol commands working via ZeroMQ
- [ ] Real Smalltalk code evaluation functional
- [ ] Session management with actual Pharo objects
- [ ] Context preservation across real sessions

### Performance
- [ ] Sustained >100 requests/second throughput
- [ ] <100ms p95 response time for simple commands
- [ ] Support for 5+ concurrent clients
- [ ] 8+ hour session stability

### Quality
- [ ] Zero crashes in 4-hour continuous testing
- [ ] Memory usage stable and bounded
- [ ] Graceful error handling and recovery
- [ ] Production deployment documentation

## Review and Approval

**Status**: ✅ **APPROVED**

**Stakeholder Sign-off**:
- [x] **@software-engineer**: Implementation approach validated
- [x] **@git-workflow**: Quality gates and testing strategy approved
- [x] **@collaboration**: Workflow and coordination plan confirmed

**Next Actions**:
1. Squad activation for Phase 1 implementation
2. Environment setup verification
3. Implementation initiation with Task 1.1

---

This decision transforms STUI from a demonstration tool into a production-ready development environment, enabling real Smalltalk development workflows accessible from any terminal worldwide.
