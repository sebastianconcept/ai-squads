---
description: Feature Solution - ZeroMQ Integration
type: feature-solution
priority: high
status: designed
---

# Solution: Real ZeroMQ Integration

## Solution Overview

Transform the current mock integration demo into a **production-ready ZeroMQ-based integration** between Rust client and Pharo/Smalltalk server, enabling real Smalltalk development workflows through STUI.

## Architecture Design

### Target Architecture
```
Current:  Rust Client ←→ TCP ←→ Mock Server     ✅ COMPLETE
Target:   Rust Client ←→ ZeroMQ ←→ Pharo Server  🎯 SOLUTION
```

### Communication Pattern
- **Protocol**: ZeroMQ REQ/REP (Request-Reply)
- **Rust Client**: REQ socket connecting to Pharo server
- **Pharo Server**: REP socket serving STUI protocol
- **Message Format**: Existing STUI JSON protocol (no changes)
- **Port Configuration**: 5555 (configurable)

## Solution Components

### 1. Pharo ZeroMQ Integration

#### Primary Approach: External Binding
```smalltalk
"Install established ZeroMQ binding"
Metacello new
    baseline: 'ZeroMQ';
    repository: 'github://zeromq/zeromq-pharo';
    load.

"Create production ZeroMQ socket"
context := ZMQContext new.
socket := context createSocket: ZMQ_REP.
socket bind: 'tcp://*:5555'.
```

#### Fallback Approach: Custom FFI Implementation
```smalltalk
"Direct ZeroMQ library FFI binding for production reliability"
Object subclass: #ZMQLibrary
    instanceVariableNames: ''
    classVariableNames: 'LibraryName'
    category: 'STUI-ZMQ-FFI'.

ZMQContext>>initialize
    contextPointer := self ffiCall: #(void* zmq_ctx_new()) module: ZMQLibrary.

ZMQSocket>>bind: endpoint
    result := self ffiCall: #(int zmq_bind(void* socketPointer, String endpoint))
        module: ZMQLibrary.
```

#### Production STUIZMQSocket
```smalltalk
Object subclass: #STUIZMQSocket
    instanceVariableNames: 'context socket socketType endpoint isConnected implementation'
    classVariableNames: ''
    category: 'STUI-Network'.

STUIZMQSocket>>initialize
    super initialize.
    isConnected := false.
    self detectAndInitializeZMQ.

STUIZMQSocket>>detectAndInitializeZMQ
    "Try primary binding first, fallback to FFI on failure"
    [ 
        context := ZMQContext new.
        socket := context createSocket: ZMQ_REP.
        implementation := #binding.
    ] on: Error do: [ :ex |
        Transcript show: 'ZeroMQ binding failed, using FFI: ', ex messageText; cr.
        implementation := #ffi.
        context := ZMQContext new.  "FFI version"
        socket := context createReplySocket.
    ].
```

### 2. Enhanced STUIServer Integration

#### Real Session Management
```smalltalk
Object subclass: #STUISessionManager
    instanceVariableNames: 'sessions workspace nextSessionId'
    classVariableNames: 'DefaultInstance'
    category: 'STUI-Session'.

STUISessionManager>>createSession
    | sessionId session |
    sessionId := self generateSessionId.
    session := STUISession new
        sessionId: sessionId;
        workspace: Workspace new;
        initialize.
    sessions at: sessionId put: session.
    ^ session.

Object subclass: #STUISession
    instanceVariableNames: 'sessionId workspace evaluationHistory objectRegistry'
    classVariableNames: ''
    category: 'STUI-Session'.

STUISession>>evaluate: codeString
    | result |
    result := workspace evaluate: codeString.
    evaluationHistory add: (Dictionary new
        at: #code put: codeString;
        at: #result put: result;
        at: #timestamp put: DateAndTime now;
        yourself).
    ^ result.
```

#### Enhanced STUIEvaluator with Real Workspace
```smalltalk
STUIEvaluator>>setSession: aSession
    session := aSession.
    workspace := aSession workspace.

STUIEvaluator>>evaluate: codeString
    | result |
    result := workspace evaluate: codeString.
    session recordEvaluation: codeString result: result.
    ^ self formatResult: result.
```

### 3. Rust Client Enhancement

#### Enhanced DemoClient Architecture
```rust
use zmq::{Context as ZmqContext, Socket, REQ};

pub struct DemoClient {
    config: DemoConfig,
    context: ZmqContext,
    socket: Option<Socket>,
    connected: bool,
    current_session_id: Option<String>,
    request_timeout: Duration,
}

impl DemoClient {
    pub fn connect(&mut self) -> Result<()> {
        let socket = self.context.socket(REQ)?;
        
        // Set production socket options
        socket.set_rcvtimeo(self.request_timeout.as_millis() as i32)?;
        socket.set_sndtimeo(self.request_timeout.as_millis() as i32)?;
        socket.set_linger(1000)?;
        
        // Connect to Pharo ZeroMQ server
        let endpoint = format!("tcp://{}", self.config.server.address);
        socket.connect(&endpoint)?;
        
        self.socket = Some(socket);
        self.connected = true;
        Ok(())
    }

    pub fn send_request(&mut self, request: &Request) -> Result<Response> {
        let socket = self.socket.as_ref()
            .ok_or_else(|| anyhow::anyhow!("Not connected to server"))?;

        // Send/receive via ZeroMQ
        let request_json = serde_json::to_string(request)?;
        socket.send(&request_json, 0)?;
        
        let response_bytes = socket.recv_bytes(0)?;
        let response_json = String::from_utf8(response_bytes)?;
        
        let response: Response = serde_json::from_str(&response_json)?;
        Ok(response)
    }
}
```

#### Enhanced Configuration System
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    pub protocol: String,  // "zeromq" | "tcp"
    pub address: String,
    pub zeromq_pattern: String,  // "req-rep"
    pub timeout_ms: u64,
    pub health_checks: bool,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            protocol: "zeromq".to_string(),
            address: "localhost:5555".to_string(),
            zeromq_pattern: "req-rep".to_string(),
            timeout_ms: 5000,
            health_checks: true,
        }
    }
}
```

## Implementation Strategy

### Phase 1: ZeroMQ Infrastructure (4-6 hours)
1. **Evaluate Pharo ZeroMQ binding quality** - Install and test reliability
2. **Implement FFI fallback if needed** - Custom implementation for production reliability  
3. **Create production STUIZMQSocket** - Auto-detecting binding vs FFI
4. **Enhance Rust DemoClient** - Real ZeroMQ integration
5. **Basic integration testing** - End-to-end connectivity validation

### Phase 2: Real Session Management (3-4 hours)
1. **Complete Pharo session classes** - Real Workspace integration
2. **Enhanced context preservation** - Actual Smalltalk object persistence
3. **Real object inspection** - True Pharo object introspection
4. **Integration testing** - Full session lifecycle with real objects

### Phase 3: Production Polish (2-3 hours)
1. **Production configuration** - Environment-specific settings
2. **Error handling & recovery** - Connection monitoring and reconnection
3. **Performance optimization** - Meet production requirements
4. **Deployment readiness** - Documentation and production checklist

## Risk Mitigation Strategy

### Primary Risk: ZeroMQ Binding Quality
**Risk**: External Pharo binding may be insufficient for production use
**Mitigation**: 
- **Quality Assessment Criteria**: Installation success, performance >1000 msg/sec, 24hr stability
- **FFI Fallback Ready**: Complete custom FFI implementation designed
- **Implementation Strategy**: Automatic detection and fallback

### Secondary Risk: Integration Complexity
**Risk**: Complex integration between Rust and Pharo ZeroMQ ecosystems
**Mitigation**:
- **Incremental Approach**: Validate each layer before proceeding
- **Quality Gates**: Defined success criteria at each phase
- **Proven Foundation**: Mock demo validates protocol and architecture

### Performance Risk: Production Requirements
**Risk**: May not meet performance requirements (>20 RPS, <100ms p95)
**Mitigation**:
- **Benchmarking**: Performance testing at each phase
- **Optimization Strategy**: Message processing and connection optimization
- **Monitoring**: Built-in performance metrics and health checks

## Quality Assurance

### Testing Strategy
1. **Unit Testing**: Both Rust and Pharo components
2. **Integration Testing**: End-to-end ZeroMQ communication
3. **Performance Testing**: Load testing with multiple clients
4. **Stability Testing**: Extended session duration validation
5. **Error Recovery Testing**: Network failure scenarios

### Success Criteria
- [ ] All 11 STUI protocol commands working via ZeroMQ
- [ ] Real Smalltalk code evaluation functional
- [ ] Session persistence with actual Pharo objects
- [ ] Performance: >20 RPS sustained, <100ms p95 latency
- [ ] Stability: 8+ hour sessions, 5+ concurrent clients
- [ ] Zero crashes in 4-hour continuous testing

## Deployment Strategy

### Development Environment
- ZeroMQ libraries installed and configured
- Both primary binding and FFI approaches validated
- Comprehensive test suite running

### Production Environment
- Not needed for this demo stage

## Backward Compatibility

### Demo Preservation
- Existing mock demo preserved in `/demo` directory
- Clear differentiation between mock and real integration
- Both modes available for different use cases

### Protocol Compatibility
- No changes to existing STUI JSON protocol but we can introduce changes that are not backgward compatible if we discover something important is needed to add or review
- Seamless upgrade path from mock to real integration
- Configuration-driven protocol selection

This solution provides a comprehensive path from the current working mock demo to a production-ready ZeroMQ integration with robust fallback strategies and quality assurance.
