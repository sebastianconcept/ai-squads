# STUI Session Protocol Specification

## Overview

This document specifies the session management protocol commands that extend the existing STUI message handling system. These commands enable multi-client session management, persistence, and state restoration over the network.

## Protocol Format

All session commands follow the standard STUI message format:

```json
{
  "id": "unique-message-id",
  "command": "command-name",
  "parameters": {
    "param1": "value1",
    "param2": "value2"
  }
}
```

## Response Format

All responses follow the standard STUI response format:

### Success Response
```json
{
  "id": "unique-message-id",
  "status": "success",
  "result": {
    "data": "response data"
  },
  "timestamp": "2025-08-30T10:30:00Z"
}
```

### Error Response
```json
{
  "id": "unique-message-id",
  "status": "error",
  "error": "Error message description",
  "error_type": "ErrorType",
  "timestamp": "2025-08-30T10:30:00Z"
}
```

## Session Management Commands

### 1. create_session

Creates a new session for a specific client.

**Request:**
```json
{
  "id": "create-001",
  "command": "create_session",
  "parameters": {
    "client_id": "client_001"
  }
}
```

**Response:**
```json
{
  "id": "create-001",
  "status": "success",
  "result": {
    "session_id": "session_1_1735567800_1234567890",
    "client_id": "client_001",
    "created_at": "2025-08-30T10:30:00Z",
    "timeout": 3600,
    "is_active": true
  },
  "timestamp": "2025-08-30T10:30:00Z"
}
```

**Error Cases:**
- Missing `client_id` parameter
- Client session limit exceeded
- Client not allowed (blacklisted/whitelist restriction)

### 2. restore_session

Restores an existing session from storage.

**Request:**
```json
{
  "id": "restore-001",
  "command": "restore_session",
  "parameters": {
    "session_id": "session_1_1735567800_1234567890"
  }
}
```

**Response:**
```json
{
  "id": "restore-001",
  "status": "success",
  "result": {
    "session_id": "session_1_1735567800_1234567890",
    "client_id": "client_001",
    "created_at": "2025-08-30T10:30:00Z",
    "last_modified": "2025-08-30T10:35:00Z",
    "timeout": 3600,
    "is_active": true,
    "session_state": {
      "workspace": "test workspace",
      "inspector": "test inspector"
    },
    "workspace_context": {
      "variables": ["var1", "var2", "var3"]
    },
    "inspector_state": {
      "selected_object": "obj_123"
    },
    "object_registry": {
      "obj_123": "Object description"
    }
  },
  "timestamp": "2025-08-30T10:40:00Z"
}
```

**Error Cases:**
- Missing `session_id` parameter
- Session not found
- Session expired or invalid

### 3. update_session_state

Updates the state of an existing session.

**Request:**
```json
{
  "id": "update-001",
  "command": "update_session_state",
  "parameters": {
    "session_id": "session_1_1735567800_1234567890",
    "state_data": {
      "workspace": "updated workspace state",
      "inspector": "updated inspector state",
      "variables": ["var1", "var2", "var3", "var4"]
    }
  }
}
```

**Response:**
```json
{
  "id": "update-001",
  "status": "success",
  "result": {
    "session_id": "session_1_1735567800_1234567890",
    "last_modified": "2025-08-30T10:45:00Z",
    "session_state": {
      "workspace": "updated workspace state",
      "inspector": "updated inspector state",
      "variables": ["var1", "var2", "var3", "var4"]
    },
    "update_successful": true
  },
  "timestamp": "2025-08-30T10:45:00Z"
}
```

**Error Cases:**
- Missing `session_id` parameter
- Missing `state_data` parameter
- Session not found
- Session expired or invalid

### 4. close_session

Closes a specific session.

**Request:**
```json
{
  "id": "close-001",
  "command": "close_session",
  "parameters": {
    "session_id": "session_1_1735567800_1234567890"
  }
}
```

**Response:**
```json
{
  "id": "close-001",
  "status": "success",
  "result": {
    "session_id": "session_1_1735567800_1234567890",
    "closed": true
  },
  "timestamp": "2025-08-30T10:50:00Z"
}
```

**Error Cases:**
- Missing `session_id` parameter
- Session not found

### 5. get_session_info

Gets detailed information about a specific session.

**Request:**
```json
{
  "id": "info-001",
  "command": "get_session_info",
  "parameters": {
    "session_id": "session_1_1735567800_1234567890"
  }
}
```

**Response:**
```json
{
  "id": "info-001",
  "status": "success",
  "result": {
    "session_id": "session_1_1735567800_1234567890",
    "client_id": "client_001",
    "created_at": "2025-08-30T10:30:00Z",
    "last_modified": "2025-08-30T10:45:00Z",
    "timeout": 3600,
    "is_active": true,
    "time_until_expiry": 3510,
    "is_expired": false
  },
  "timestamp": "2025-08-30T10:50:00Z"
}
```

**Error Cases:**
- Missing `session_id` parameter
- Session not found

### 6. list_client_sessions

Lists all sessions for a specific client.

**Request:**
```json
{
  "id": "list-001",
  "command": "list_client_sessions",
  "parameters": {
    "client_id": "client_001"
  }
}
```

**Response:**
```json
{
  "id": "list-001",
  "status": "success",
  "result": {
    "client_id": "client_001",
    "session_count": 2,
    "sessions": [
      {
        "session_id": "session_1_1735567800_1234567890",
        "created_at": "2025-08-30T10:30:00Z",
        "last_modified": "2025-08-30T10:45:00Z",
        "timeout": 3600,
        "is_active": true,
        "time_until_expiry": 3510
      },
      {
        "session_id": "session_2_1735567800_1234567891",
        "created_at": "2025-08-30T10:35:00Z",
        "last_modified": "2025-08-30T10:40:00Z",
        "timeout": 3600,
        "is_active": true,
        "time_until_expiry": 3560
      }
    ]
  },
  "timestamp": "2025-08-30T10:50:00Z"
}
```

**Error Cases:**
- Missing `client_id` parameter

### 7. get_session_statistics

Gets overall session statistics for the system.

**Request:**
```json
{
  "id": "stats-001",
  "command": "get_session_statistics",
  "parameters": {}
}
```

**Response:**
```json
{
  "id": "stats-001",
  "status": "success",
  "result": {
    "total_sessions": 5,
    "active_clients": 2,
    "max_sessions_per_client": 5,
    "default_timeout": 3600,
    "cleanup_interval": 300
  },
  "timestamp": "2025-08-30T10:50:00Z"
}
```

## Error Types

The following error types are used for session management:

- **SessionError**: General session management errors
- **SessionNotFound**: Session not found or cannot be restored
- **RuntimeError**: General runtime errors
- **Error**: Generic errors

## Multi-Client Support

The session protocol supports multiple clients connecting simultaneously:

1. **Client Isolation**: Each client's sessions are completely isolated
2. **Session Limits**: Configurable maximum sessions per client (default: 5)
3. **Resource Management**: Automatic cleanup of expired sessions
4. **State Persistence**: Sessions persist across image restarts

## Security Features

The protocol includes several security features:

1. **Client Validation**: Client ID format validation
2. **Session Validation**: Session integrity and expiry validation
3. **Access Control**: Configurable client whitelist/blacklist
4. **Rate Limiting**: Protection against excessive session creation

## Performance Considerations

1. **Caching**: Session validation results are cached for performance
2. **Background Cleanup**: Expired sessions are cleaned up automatically
3. **Efficient Storage**: Session data is stored efficiently with backup/restore
4. **Scalable Architecture**: Designed to handle multiple concurrent clients

## Integration Points

### With Existing STUI System
- **Message Handler**: Integrates with existing STUIMessageHandler
- **Protocol Compatibility**: Maintains existing STUI protocol structure
- **Error Handling**: Consistent with existing error handling patterns

### With Future Features
- **Authentication**: Framework ready for external authentication
- **Subscription Validation**: Prepared for premium feature gating
- **Remote Debugger**: Architecture supports future remote debugging

## Testing

The protocol includes comprehensive testing:

1. **Unit Tests**: Individual command testing
2. **Integration Tests**: End-to-end session management testing
3. **Error Testing**: Comprehensive error scenario coverage
4. **Performance Testing**: Scalability and performance validation

## Example Usage Scenarios

### Scenario 1: Client Connection and Session Creation
1. Client connects to STUI server
2. Client creates session with `create_session` command
3. Server returns session ID and metadata
4. Client stores session ID for future operations

### Scenario 2: Session State Management
1. Client updates session state with `update_session_state`
2. Server persists state changes
3. Client can restore state with `restore_session`
4. State persists across client reconnections

### Scenario 3: Multi-Client Collaboration
1. Multiple clients connect to same Smalltalk image
2. Each client creates independent sessions
3. Sessions are completely isolated
4. All clients can work simultaneously

## Future Enhancements

1. **Real-time Updates**: WebSocket support for live session updates
2. **Advanced Security**: Encryption and authentication integration
3. **Session Sharing**: Controlled session sharing between clients
4. **Performance Monitoring**: Advanced metrics and monitoring
5. **Plugin System**: Extensible session management capabilities
