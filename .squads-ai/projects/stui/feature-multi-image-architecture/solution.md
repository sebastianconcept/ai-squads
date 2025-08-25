---
description: Solution Design - Multi-Image Architecture
type: solution-design
status: active
squad: elite
---

# Solution Design - Multi-Image Architecture

## Architecture Overview

Transform STUI from single-image to multi-image architecture by extending the existing session infrastructure to support multiple Smalltalk images simultaneously. The MVP solution focuses on clean, direct implementation without backward compatibility constraints, enabling faster development and cleaner architecture.

## Technical Implementation

### Rust Development Standards
**CRITICAL**: Use enums over dynamic dispatch for all new types and protocol extensions.

#### **✅ Preferred Approach - Enum-Based Design**
```rust
// GOOD: Use enums for protocol messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Request {
    Ping(PingRequest),
    Evaluate(EvaluateRequest),
    Inspect(InspectRequest),
    // NEW: Multi-image commands
    ListImageSessions(ListImageSessionsRequest),
    SwitchImageContext(SwitchImageContextRequest),
    GetImageConnectionStatus(GetImageConnectionStatusRequest),
}

// GOOD: Use enums for connection states
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConnectionStatus {
    Connected,
    Disconnected,
    Reconnecting,
    Failed,
    Unknown,
}
```

#### **❌ Avoid Dynamic Dispatch**
```rust
// AVOID: Don't use trait objects for protocol messages
pub trait Request {
    fn request_type(&self) -> &str;
    fn serialize(&self) -> Result<Vec<u8>, SerializationError>;
}

// AVOID: Don't use Box<dyn Request> or similar patterns
pub struct Message {
    pub request: Box<dyn Request>, // ❌ Avoid this
}
```

#### **Benefits of Enum-Based Design**
- **Type Safety**: Compile-time guarantees for all message types
- **Performance**: Zero-cost abstractions with static dispatch
- **Consistency**: Follows existing STUI protocol patterns
- **Maintainability**: Clear, explicit type definitions

### Phase 1: Protocol Extension and Data Structures

#### 1.1 Extend Session Data Structures
```rust
// Add to crates/stui-tui/src/session_storage/types.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionStateData {
    // ... existing fields ...
    pub image_id: String,                    // Unique image identifier
    pub image_address: String,               // Connection address (host:port)
    pub image_name: Option<String>,          // Human-readable image name
    pub image_version: Option<String>,       // Smalltalk image version
    pub image_metadata: HashMap<String, String>, // Additional image details
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImageConnectionStatus {
    pub image_id: String,
    pub address: String,
    pub status: ConnectionStatus,
    pub last_seen: DateTime<Utc>,
    pub connection_health: ConnectionHealth,
    pub session_count: usize,
    pub last_activity: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConnectionStatus {
    Connected,
    Disconnected,
    Reconnecting,
    Failed,
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConnectionHealth {
    Healthy,
    Warning,
    Critical,
    Unknown,
}
```

#### 1.2 Extend Protocol Commands
```rust
// Add to crates/stui-protocol/src/lib.rs
pub enum Request {
    // ... existing types ...
    ListImageSessions(ListImageSessionsRequest),
    GetImageSessionStats(GetImageSessionStatsRequest),
    CleanupImageSessions(CleanupImageSessionsRequest),
    SwitchImageContext(SwitchImageContextRequest),
    GetImageConnectionStatus(GetImageConnectionStatusRequest),
}

pub struct ListImageSessionsRequest {
    pub image_id: Option<String>,  // None = all images
    pub include_disconnected: bool,
}

pub struct GetImageSessionStatsRequest {
    pub image_id: String,
}

pub struct CleanupImageSessionsRequest {
    pub image_id: String,
    pub force: bool,
    pub preserve_active: bool,
}

pub struct SwitchImageContextRequest {
    pub from_image_id: String,
    pub to_image_id: String,
    pub preserve_context: bool,
}

pub struct GetImageConnectionStatusRequest {
    pub image_id: String,
}
```

#### 1.3 Update Session Metadata
```rust
// Modify crates/stui-tui/src/session_storage/types.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionMetadata {
    // ... existing fields ...
    pub image_id: String,           // NEW: Image identifier
    pub image_address: String,      // NEW: Connection address
    pub image_name: Option<String>, // NEW: Human-readable name
    pub image_version: Option<String>, // NEW: Image version
    pub connection_status: ConnectionStatus, // NEW: Current connection status
}
```

### Phase 2: Session Manager Enhancement

#### 2.1 Multi-Image Session Tracking
```rust
// Modify crates/stui-tui/src/session_manager/session_manager.rs
pub struct SessionManager {
    // ... existing fields ...
    pub active_sessions: HashMap<String, ActiveSession>, // Sessions by image
    pub image_connections: HashMap<String, ImageConnectionStatus>,
    pub current_image_context: Option<String>, // Currently active image
    pub image_session_map: HashMap<String, Vec<String>>, // Image to session IDs mapping
}

impl SessionManager {
    // NEW: Multi-image session management
    pub fn create_session_for_image(
        &mut self, 
        image_id: &str, 
        image_address: &str,
        session_name: Option<String>,
        description: Option<String>
    ) -> Result<String, SessionManagerError>;
    
    pub fn switch_image_context(
        &mut self, 
        image_id: &str
    ) -> Result<(), SessionManagerError>;
    
    pub fn get_image_session_stats(
        &mut self, 
        image_id: &str
    ) -> Result<ImageSessionStats, SessionManagerError>;
    
    pub fn cleanup_orphaned_sessions(
        &mut self, 
        image_id: &str
    ) -> Result<usize, SessionManagerError>;
    
    pub fn list_image_sessions(
        &mut self, 
        image_id: Option<&str>
    ) -> Result<Vec<SessionMetadata>, SessionManagerError>;
    
    pub fn get_image_connection_status(
        &mut self,
        image_id: &str
    ) -> Result<ImageConnectionStatus, SessionManagerError>;
}
```

#### 2.2 Image Connection Monitoring
```rust
// Add to crates/stui-tui/src/session_manager/session_manager.rs
impl SessionManager {
    pub fn update_image_connection_status(
        &mut self,
        image_id: &str,
        status: ConnectionStatus,
        health: ConnectionHealth,
    ) -> Result<(), SessionManagerError>;
    
    pub fn detect_disconnected_images(&mut self) -> Vec<String>;
    
    pub fn handle_image_disconnection(&mut self, image_id: &str) -> Result<(), SessionManagerError>;
    
    pub fn get_connection_health_summary(&self) -> HashMap<String, ConnectionHealth>;
    
    pub fn start_connection_monitoring(&mut self) -> Result<(), SessionManagerError>;
    
    pub fn handle_network_change(&mut self) -> Result<(), SessionManagerError>;
}
```

### Phase 3: UI Enhancement

#### 3.1 Multi-Image Session Panel
```rust
// Modify crates/stui-tui/src/panel/session_panel.rs
pub struct SessionPanel {
    // ... existing fields ...
    pub image_sessions: HashMap<String, Vec<SessionMetadata>>,
    pub selected_image: Option<String>,
    pub image_connection_status: HashMap<String, ImageConnectionStatus>,
    pub show_image_selector: bool,
    pub image_filter: Option<String>,
}

impl SessionPanel {
    pub fn render_multi_image_view(&self, frame: &mut Frame, area: Rect);
    pub fn render_image_status_indicators(&self, frame: &mut Frame, area: Rect);
    pub fn render_orphaned_session_cleanup(&self, frame: &mut Frame, area: Rect);
    pub fn render_image_selector(&self, frame: &mut Frame, area: Rect);
    pub fn render_image_connection_health(&self, frame: &mut Frame, area: Rect);
}
```

#### 3.2 Image Status Indicators
- **Connection Status**: Visual indicators for each image's connection health
- **Session Count**: Number of active sessions per image
- **Health Monitoring**: Real-time connection health display
- **Quick Actions**: Connect/disconnect, cleanup, switch context

## Integration Points

### Existing System Integration
- **SessionStorage**: Extend to support image-specific session storage
- **SessionManager**: Enhance with multi-image session tracking
- **SessionPanel**: Update UI for multi-image management
- **Protocol Layer**: Extend with multi-image commands
- **Error Handling**: Maintain existing error handling patterns

### MVP Integration Benefits
- **Cleaner Code**: No legacy compatibility layers needed
- **Faster Development**: Direct implementation without migration complexity
- **Better Performance**: Optimized for multi-image from the start
- **Simplified Testing**: No backward compatibility test scenarios

### New Integration Points
- **Image Connection Monitoring**: Real-time connection status tracking
- **Cross-Image Session Management**: Session isolation and context switching
- **Orphaned Session Cleanup**: Automatic cleanup of disconnected image sessions
- **Multi-Image UI**: Professional interface for managing multiple images

## Data Flow

### Session Creation Flow
1. **User Request**: Create session for specific image
2. **Image Validation**: Validate image connection and metadata
3. **Session Creation**: Create session with image identification
4. **Storage**: Store session with image-specific metadata
5. **UI Update**: Update session panel with new session

### Image Context Switching Flow
1. **User Request**: Switch to different image context
2. **Context Preservation**: Preserve current session state
3. **Context Switch**: Switch to target image context
4. **Session Restoration**: Restore sessions for target image
5. **UI Update**: Update interface for new image context

### Connection Monitoring Flow
1. **Background Monitoring**: Continuous connection health monitoring
2. **Status Updates**: Update connection status in real-time
3. **Disconnection Detection**: Detect when images disconnect
4. **Cleanup Trigger**: Trigger orphaned session cleanup
5. **User Notification**: Notify user of connection changes

## Performance Considerations

### Memory Management
- **Efficient Storage**: Use references to avoid data duplication
- **Lazy Loading**: Load image-specific data only when needed
- **Cleanup Strategies**: Regular cleanup of orphaned sessions
- **Connection Pooling**: Efficient management of multiple connections

### Network Optimization
- **Connection Reuse**: Reuse connections when possible
- **Batch Operations**: Batch operations for multiple images
- **Connection Health**: Monitor and optimize connection health
- **Fallback Strategies**: Graceful degradation for failed connections

## Security and Validation

### Data Isolation
- **Session Isolation**: Complete isolation between image sessions
- **Access Control**: Proper access controls for image-specific data
- **Data Validation**: Comprehensive validation of image metadata
- **Secure Communication**: Encrypted communication with all images

### Error Handling
- **Graceful Degradation**: Handle image failures gracefully
- **Data Recovery**: Recover from image disconnection scenarios
- **User Feedback**: Clear error messages and recovery options
- **Rollback Capability**: Ability to rollback to previous stable state

## MVP Implementation Strategy

### Direct Implementation
- **Clean Break**: Fresh start with multi-image architecture
- **No Migration**: Existing sessions can be recreated with new format
- **Direct Enhancement**: Modify existing components for multi-image support
- **Simplified Approach**: No complex migration or fallback strategies

### Implementation Steps
1. **Phase 1**: Extend protocol and data structures for multi-image support
2. **Phase 2**: Enhance SessionManager with multi-image session tracking
3. **Phase 3**: Update UI components for multi-image management
4. **Phase 4**: Integration testing and quality validation

---

**Status**: Solution Design Complete - Ready for Implementation Planning  
**Next Step**: Create goal.md with success criteria and acceptance criteria  
**Implementation Approach**: Incremental development with comprehensive testing
