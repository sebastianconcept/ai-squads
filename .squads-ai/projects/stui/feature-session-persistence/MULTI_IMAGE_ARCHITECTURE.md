---
description: Multi-Image Architecture Implementation Plan
type: technical-plan
status: critical-implementation
---

# Multi-Image Architecture Implementation Plan

## 🚨 **Critical Priority - Implementation Required**

**Status**: 🚨 **CRITICAL - IMPLEMENTATION PAUSED**  
**Priority**: **HIGHEST** - Must be resolved before continuing with other features  
**Timeline**: This Week (Priority 1)  

## 🎯 **Overview**

The current Session Persistence implementation has **critical architectural gaps** that prevent production readiness:

- ❌ **Multi-image architecture missing** - No support for multiple Smalltalk images
- ❌ **Cross-image session management not planned** - Sessions can't handle image switching
- ❌ **Image disconnection cleanup strategy incomplete** - Orphaned sessions will accumulate
- ❌ **Orphaned session handling not addressed** - Memory leaks and data corruption risk

## 🏗️ **Architecture Requirements**

### **Core Requirements**
1. **Image Identification**: Unique and persistent identification of Smalltalk images
2. **Session Isolation**: Sessions must be properly isolated between images
3. **Cross-Image Operations**: Support for managing sessions across multiple images
4. **Disconnection Handling**: Automatic detection and cleanup of disconnected images
5. **Performance Scaling**: Maintain performance with multiple image connections

### **Technical Constraints**
- **Protocol Compatibility**: Must maintain backward compatibility
- **Performance Impact**: Minimal impact on existing functionality
- **Data Integrity**: Ensure session data isolation and integrity
- **User Experience**: Seamless multi-image session management

## 🔧 **Implementation Plan**

### **Phase 1: Protocol Extension (This Week)**

#### **T8.1: Extend Protocol for Image Identification**
**Agent**: @agent:software-engineer  
**Priority**: CRITICAL  
**Timeline**: 2 days  

**Changes Required**:
```rust
// Extend SessionStateData in crates/stui-protocol/src/lib.rs
pub struct SessionStateData {
    // ... existing fields ...
    
    /// Unique image identifier (e.g., "Pharo 11.0", "Squeak 6.0")
    pub image_id: String,
    
    /// Image connection address (e.g., "localhost:5555", "192.168.1.100:5555")
    pub image_address: String,
    
    /// Image metadata (version, dialect, etc.)
    pub image_metadata: HashMap<String, String>,
}

// Add new request types for multi-image management
pub enum Request {
    // ... existing types ...
    
    /// List all sessions for a specific image
    ListImageSessions(ListImageSessionsRequest),
    
    /// Clean up orphaned sessions for an image
    CleanupImageSessions(CleanupImageSessionsRequest),
    
    /// Get session statistics for an image
    GetImageSessionStats(GetImageSessionStatsRequest),
}

// Add corresponding response types
pub enum Response {
    // ... existing types ...
    
    ListImageSessions(ListImageSessionsResponse),
    CleanupImageSessions(CleanupImageSessionsResponse),
    GetImageSessionStats(GetImageSessionStatsResponse),
}
```

**New Request/Response Structures**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListImageSessionsRequest {
    pub image_id: String,
    pub include_orphaned: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListImageSessionsResponse {
    pub image_id: String,
    pub sessions: Vec<SessionInfo>,
    pub total_count: usize,
    pub orphaned_count: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanupImageSessionsRequest {
    pub image_id: String,
    pub cleanup_orphaned: bool,
    pub cleanup_expired: bool,
    pub max_age_hours: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanupImageSessionsResponse {
    pub image_id: String,
    pub sessions_cleaned: usize,
    pub sessions_remaining: usize,
    pub cleanup_details: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetImageSessionStatsRequest {
    pub image_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetImageSessionStatsResponse {
    pub image_id: String,
    pub total_sessions: usize,
    pub active_sessions: usize,
    pub orphaned_sessions: usize,
    pub total_context_size: usize,
    pub last_activity: DateTime<Utc>,
}
```

#### **T8.2: Enhance SessionManager for Multi-Image Support**
**Agent**: @agent:software-engineer  
**Priority**: CRITICAL  
**Timeline**: 3 days  

**Changes Required**:
```rust
// Extend SessionManager in crates/stui-tui/src/session_manager.rs
pub struct SessionManager {
    // ... existing fields ...
    
    /// Active sessions by image ID
    active_sessions: HashMap<String, ActiveSession>,
    
    /// Image connection status and health
    image_connections: HashMap<String, ImageConnectionStatus>,
    
    /// Image-specific session storage
    image_session_storage: HashMap<String, Arc<Mutex<SessionStorage>>>,
}

// New types for multi-image support
#[derive(Debug, Clone)]
pub struct ImageConnectionStatus {
    pub image_id: String,
    pub status: ImageStatus,
    pub last_seen: DateTime<Utc>,
    pub connection_address: String,
    pub metadata: HashMap<String, String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImageStatus {
    Connected,
    Disconnected,
    Orphaned,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct ImageSessionStats {
    pub image_id: String,
    pub total_sessions: usize,
    pub active_sessions: usize,
    pub orphaned_sessions: usize,
    pub total_context_size: usize,
    pub last_activity: DateTime<Utc>,
}
```

**New Methods Required**:
```rust
impl SessionManager {
    // Create session for specific image
    pub fn create_session_for_image(
        &mut self,
        image_id: &str,
        client_id: &str,
        initial_context: Option<SessionStateData>
    ) -> Result<String, SessionError>;
    
    // Restore session for specific image
    pub fn restore_session_for_image(
        &mut self,
        image_id: &str,
        session_id: &str
    ) -> Result<(), SessionError>;
    
    // Get sessions for specific image
    pub fn get_image_sessions(&self, image_id: &str) -> Result<Vec<SessionInfo>, SessionError>;
    
    // Clean up orphaned sessions for image
    pub fn cleanup_orphaned_sessions(&mut self, image_id: &str) -> Result<CleanupResult, SessionError>;
    
    // Get image session statistics
    pub fn get_image_session_stats(&self, image_id: &str) -> Result<ImageSessionStats, SessionError>;
    
    // Update image connection status
    pub fn update_image_connection_status(
        &mut self,
        image_id: &str,
        status: ImageStatus,
        metadata: Option<HashMap<String, String>>
    ) -> Result<(), SessionError>;
    
    // Handle image disconnection
    pub fn handle_image_disconnection(&mut self, image_id: &str) -> Result<(), SessionError>;
    
    // Handle image reconnection
    pub fn handle_image_reconnection(&mut self, image_id: &str) -> Result<(), SessionError>;
}
```

### **Phase 2: Image Disconnection Cleanup (This Week)**

#### **T8.3: Implement Image Disconnection Detection**
**Agent**: @agent:software-engineer  
**Priority**: CRITICAL  
**Timeline**: 2 days  

**Implementation Details**:
- Monitor image connection health through ping responses
- Detect when images become unresponsive
- Mark sessions as orphaned after timeout period
- Provide health status indicators for each image

#### **T8.4: Add Orphaned Session Management**
**Agent**: @agent:software-engineer  
**Priority**: CRITICAL  
**Timeline**: 2 days  

**Implementation Details**:
- Implement orphaned session detection logic
- Add cleanup strategies for orphaned sessions
- Provide manual cleanup commands
- Track cleanup statistics and history

#### **T8.5: Update UI for Multi-Image Display**
**Agent**: @agent:ui-implementor  
**Priority**: CRITICAL  
**Timeline**: 2 days  

**Implementation Details**:
- Extend session panel to show image information
- Group sessions by image
- Display image connection status
- Add image-specific session controls

### **Phase 3: UI Enhancement (Next Week)**

#### **T9.1: Design Multi-Image Session Panel Interface**
**Agent**: @agent:ux-expert  
**Priority**: HIGH  
**Timeline**: 2 days  

**Design Requirements**:
- Clear image identification and status
- Intuitive session grouping by image
- Easy image switching and management
- Clear orphaned session indicators

#### **T9.2: Implement Image Status Indicators**
**Agent**: @agent:ui-implementor  
**Priority**: HIGH  
**Timeline**: 2 days  

**Implementation Details**:
- Connection status indicators for each image
- Health monitoring display
- Connection history and statistics
- Quick connection/disconnection controls

#### **T9.3: Add Image-Specific Session Management Controls**
**Agent**: @agent:ui-implementor  
**Priority**: HIGH  
**Timeline**: 2 days  

**Implementation Details**:
- Image-specific session creation
- Image-specific session restoration
- Image-specific session cleanup
- Cross-image session operations

#### **T9.4: Create Orphaned Session Cleanup Interface**
**Agent**: @agent:ui-implementor  
**Priority**: MEDIUM  
**Timeline**: 1 day  

**Implementation Details**:
- Orphaned session identification
- Cleanup confirmation dialogs
- Cleanup progress indicators
- Cleanup history and statistics

#### **T9.5: Implement Image Switching and Context Preservation**
**Agent**: @agent:ui-implementor  
**Priority**: MEDIUM  
**Timeline**: 2 days  

**Implementation Details**:
- Seamless image switching
- Context preservation across images
- Session state synchronization
- User preference management

## 🧪 **Testing Strategy**

### **Unit Testing**
- **Protocol Extensions**: Test new request/response types
- **SessionManager Extensions**: Test multi-image session operations
- **Image Connection Management**: Test connection status updates
- **Orphaned Session Handling**: Test cleanup and management

### **Integration Testing**
- **Multi-Image Scenarios**: Test with multiple Smalltalk images
- **Image Disconnection**: Test disconnection detection and cleanup
- **Session Isolation**: Verify sessions are properly isolated between images
- **Performance Testing**: Validate performance with multiple connections

### **User Acceptance Testing**
- **Multi-Image Workflows**: Test real-world multi-image usage
- **UI Usability**: Validate multi-image interface design
- **Error Handling**: Test error scenarios and recovery
- **Performance Perception**: Ensure smooth user experience

## 🔒 **Security and Validation**

### **Data Security**
- **Session Isolation**: Ensure sessions cannot access other image data
- **Image Authentication**: Validate image identity and permissions
- **Cleanup Security**: Secure cleanup operations and confirmation
- **Data Integrity**: Validate session data integrity across images

### **Validation and Recovery**
- **Image Validation**: Validate image identity and connection status
- **Session Validation**: Validate session data integrity
- **Cleanup Validation**: Validate cleanup operations and results
- **Recovery Mechanisms**: Robust error handling and recovery

## 📊 **Success Criteria**

### **Functional Requirements**
- ✅ **Image Identification**: Sessions are uniquely tied to specific Smalltalk images
- ✅ **Multi-Image Management**: Multiple image connections can be managed simultaneously
- ✅ **Disconnection Handling**: Image disconnections are detected and handled gracefully
- ✅ **Orphaned Session Cleanup**: Orphaned sessions are identified and can be cleaned up
- ✅ **Session Isolation**: Sessions are properly isolated between images

### **Performance Requirements**
- ✅ **Performance Impact**: Minimal impact on existing functionality
- ✅ **Scalability**: System maintains performance with multiple image connections
- ✅ **Memory Usage**: Efficient memory management for multiple images
- ✅ **Response Time**: Fast response times for multi-image operations

### **User Experience Requirements**
- ✅ **Clear Interface**: Intuitive multi-image session management
- ✅ **Status Visibility**: Clear image connection status and health
- ✅ **Easy Management**: Simple image switching and session management
- ✅ **Error Handling**: Clear error messages and recovery options

## 🎯 **Implementation Timeline**

### **Week 1 (This Week) - CRITICAL**
- **Days 1-2**: Protocol extension (T8.1)
- **Days 3-5**: SessionManager enhancement (T8.2)
- **Days 4-5**: Image disconnection cleanup (T8.3, T8.4)
- **Days 5**: Basic UI updates (T8.5)

### **Week 2 (Next Week) - HIGH**
- **Days 1-2**: Multi-image UI design (T9.1)
- **Days 3-4**: Image status indicators (T9.2)
- **Days 4-5**: Session management controls (T9.3)

### **Week 3 - MEDIUM**
- **Days 1**: Orphaned session cleanup UI (T9.4)
- **Days 2-3**: Image switching and context preservation (T9.5)
- **Days 4-5**: Testing and validation

## 🚨 **Risk Mitigation**

### **Technical Risks**
- **Protocol Complexity**: Incremental implementation with thorough testing
- **Performance Impact**: Continuous performance monitoring and optimization
- **Integration Challenges**: Step-by-step integration with existing components
- **Data Corruption**: Comprehensive validation and integrity checking

### **Timeline Risks**
- **Scope Creep**: Strict focus on multi-image architecture requirements
- **Resource Constraints**: Prioritize critical functionality first
- **Testing Complexity**: Comprehensive testing strategy with early validation
- **Integration Issues**: Continuous integration and testing

## 📋 **Next Steps**

### **Immediate Actions (This Week)**
1. **@agent:software-engineer**: Begin protocol extension (T8.1)
2. **@agent:software-engineer**: Start SessionManager enhancement (T8.2)
3. **@agent:ux-expert**: Begin multi-image UI design (T9.1)
4. **@agent:ui-implementor**: Prepare for UI implementation (T8.5)

### **Success Validation**
- **Protocol Extension**: New request/response types working
- **SessionManager Enhancement**: Multi-image session tracking functional
- **Image Disconnection**: Cleanup strategies implemented and tested
- **UI Updates**: Basic multi-image display working

---

**Last Updated**: 2025-08-23  
**Next Review**: Daily during implementation  
**Status**: Critical Implementation Required
