# STUI JSON Protocol Specification v1.0.0

This document defines the JSON-based communication protocol between the Rust TUI client and the Pharo Smalltalk server in STUI (Smalltalk User Interface).

## Overview

The protocol uses a request/response pattern over ZeroMQ with JSON serialization. All messages are wrapped in a standardized envelope that includes metadata for routing, versioning, and tracking.

## Message Structure

### Message Envelope

Every message is wrapped in a standardized envelope:

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "version": "1.0.0",
  "timestamp": "2025-01-20T10:30:00Z",
  "payload": {
    "type": "Request|Response",
    "data": { /* actual request/response data */ }
  }
}
```

### Fields

- `id`: UUID v4 for message tracking and correlation
- `version`: Protocol version (currently "1.0.0")
- `timestamp`: ISO 8601 timestamp when message was created
- `payload`: Container for the actual message data
  - `type`: Either "Request" or "Response"
  - `data`: The actual request or response content

## Request Types

### Ping Request

Test server connectivity and get basic status information.

```json
{
  "request_type": "Ping",
  "params": {
    "message": "Hello Server"  // optional
  }
}
```

**Response:**
```json
{
  "response_type": "Ping",
  "data": {
    "message": "Hello Server",  // echoed back
    "status": "ready",
    "uptime": 3600  // seconds
  }
}
```

### Evaluate Request

Execute Smalltalk code and return the result.

```json
{
  "request_type": "Evaluate",
  "params": {
    "code": "1 + 1",
    "context": null,  // optional context
    "return_string": true
  }
}
```

**Response:**
```json
{
  "response_type": "Evaluate",
  "data": {
    "result": "2",
    "result_type": "SmallInteger",
    "execution_time": 5,  // milliseconds
    "has_error": false
  }
}
```

### Inspect Request

Inspect an object and return its structure and variables.

```json
{
  "request_type": "Inspect",
  "params": {
    "target": "Array new: 3",
    "depth": 2,  // optional
    "include_private": false
  }
}
```

**Response:**
```json
{
  "response_type": "Inspect",
  "data": {
    "object_info": {
      "class_name": "Array",
      "string_representation": "an Array(3)",
      "hash": 12345678,
      "size": 24
    },
    "instance_vars": {
      "size": "3",
      "elements": "[nil, nil, nil]"
    },
    "methods": null  // optional
  }
}
```

### BrowseClass Request

Get information about a class, including methods and variables.

```json
{
  "request_type": "BrowseClass",
  "params": {
    "class_name": "Array",
    "include_methods": true,
    "include_instance_vars": true,
    "include_class_vars": false
  }
}
```

**Response:**
```json
{
  "response_type": "BrowseClass",
  "data": {
    "class_info": {
      "name": "Array",
      "superclass": "ArrayedCollection",
      "comment": "I represent a fixed-size indexable collection of objects.",
      "package": "Collections-Sequenceable",
      "is_system": true
    },
    "instance_methods": [
      {
        "name": "at:",
        "source": "at: index\n\t^self basicAt: index",
        "category": "accessing",
        "is_class_method": false,
        "arguments": ["index"],
        "comment": "Answer the element at the given index"
      }
    ],
    "class_methods": [],
    "instance_vars": ["size"],
    "class_vars": null
  }
}
```

### Debug Request

Perform debugging operations.

```json
{
  "request_type": "Debug",
  "params": {
    "operation": {
      "StepInto": null
    },
    "context": "debugger-session-123"
  }
}
```

**Available Debug Operations:**
- `StepInto`: Step into method calls
- `StepOver`: Step over current line
- `StepOut`: Step out of current method
- `Continue`: Continue execution
- `SetBreakpoint`: `{ "line": 42, "file": "MyClass.st" }`
- `RemoveBreakpoint`: `{ "line": 42, "file": "MyClass.st" }`
- `GetStackTrace`: Get current stack trace
- `GetLocalVars`: Get local variables

**Response:**
```json
{
  "response_type": "Debug",
  "data": {
    "result": "stepped into method foo:",
    "debug_info": {
      "current_line": "5",
      "current_method": "foo:",
      "local_vars": "a=1, b=2"
    }
  }
}
```

### SystemInfo Request

Get system information from the Smalltalk environment.

```json
{
  "request_type": "SystemInfo",
  "params": {
    "info_type": "Basic"
  }
}
```

**Info Types:**
- `Basic`: Basic system information
- `Memory`: Memory usage statistics
- `Packages`: Loaded packages
- `Environment`: Environment variables
- `All`: All available information

**Response:**
```json
{
  "response_type": "SystemInfo",
  "data": {
    "info": {
      "pharo_version": "12.0",
      "vm_version": "10.0.8",
      "image_format": "68021",
      "platform": "unix",
      "memory_used": "45MB",
      "memory_free": "123MB"
    }
  }
}
```

### ListClasses Request

Get a list of available classes.

```json
{
  "request_type": "ListClasses",
  "params": {
    "pattern": "Array*",  // optional filter pattern
    "include_system": true,
    "limit": 100  // optional limit
  }
}
```

**Response:**
```json
{
  "response_type": "ListClasses",
  "data": {
    "classes": [
      {
        "name": "Array",
        "superclass": "ArrayedCollection",
        "comment": "I represent a fixed-size indexable collection.",
        "package": "Collections-Sequenceable",
        "is_system": true
      }
    ],
    "total_count": 150
  }
}
```

### GetMethod Request

Get detailed information about a specific method.

```json
{
  "request_type": "GetMethod",
  "params": {
    "class_name": "Array",
    "method_name": "at:",
    "is_class_method": false
  }
}
```

**Response:**
```json
{
  "response_type": "GetMethod",
  "data": {
    "method": {
      "name": "at:",
      "source": "at: index\n\t\"Answer the element at the given index\"\n\t^self basicAt: index",
      "category": "accessing",
      "is_class_method": false,
      "arguments": ["index"],
      "comment": "Answer the element at the given index"
    }
  }
}
```

## Error Handling

All errors are returned using a standardized error response format:

```json
{
  "response_type": "Error",
  "data": {
    "code": "EvaluationError",
    "message": "Syntax error: expected ']'",
    "details": {
      "line": "1",
      "column": "15",
      "suggestion": "Check bracket matching"
    },
    "stack_trace": "MessageNotUnderstood>>signal\n..."  // optional
  }
}
```

### Error Codes

- `InvalidRequest`: Request format is invalid
- `EvaluationError`: Error during code evaluation
- `ObjectNotFound`: Requested object not found
- `ClassNotFound`: Requested class not found
- `MethodNotFound`: Requested method not found
- `PermissionDenied`: Operation not permitted
- `InternalError`: Server internal error
- `Timeout`: Operation timed out
- `UnsupportedOperation`: Operation not supported

## Protocol Versioning

The protocol version is included in every message envelope. Clients and servers should check version compatibility:

- **Major version** changes indicate breaking changes
- **Minor version** changes add new features while maintaining backward compatibility
- **Patch version** changes are bug fixes

Current version: `1.0.0`

## Message Flow Examples

### Simple Evaluation

**Client Request:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "version": "1.0.0",
  "timestamp": "2025-01-20T10:30:00Z",
  "payload": {
    "type": "Request",
    "data": {
      "request_type": "Evaluate",
      "params": {
        "code": "Date today",
        "context": null,
        "return_string": true
      }
    }
  }
}
```

**Server Response:**
```json
{
  "id": "661f9511-f3ac-52e5-b827-557766551111",
  "version": "1.0.0",
  "timestamp": "2025-01-20T10:30:00.123Z",
  "payload": {
    "type": "Response",
    "data": {
      "response_type": "Evaluate",
      "data": {
        "result": "20 January 2025",
        "result_type": "Date",
        "execution_time": 12,
        "has_error": false
      }
    }
  }
}
```

### Error Response

**Server Error Response:**
```json
{
  "id": "772fa622-g4bd-63f6-c938-668877662222",
  "version": "1.0.0",
  "timestamp": "2025-01-20T10:30:01Z",
  "payload": {
    "type": "Response",
    "data": {
      "response_type": "Error",
      "data": {
        "code": "EvaluationError",
        "message": "MessageNotUnderstood: #unknownMethod",
        "details": {
          "receiver": "Object",
          "method": "unknownMethod",
          "suggestion": "Check method name spelling"
        },
        "stack_trace": "Object>>doesNotUnderstand:\nUndefinedObject>>unknownMethod\n..."
      }
    }
  }
}
```

## Implementation Notes

### Serialization

- All timestamps use ISO 8601 format with UTC timezone
- UUIDs are serialized as standard UUID strings
- Binary data should be base64 encoded
- All strings use UTF-8 encoding

### Transport

- Messages are sent over ZeroMQ REQ/REP sockets
- Each request expects exactly one response
- Message size should not exceed 10MB
- Connection timeout: 30 seconds

### Performance

- Keep message payloads reasonably sized
- Use pagination for large result sets
- Consider compression for large messages
- Cache frequently accessed data

### Security

- Validate all incoming messages
- Sanitize code before evaluation
- Limit evaluation context permissions
- Log all security-relevant operations

## Extensions

The protocol is designed to be extensible. New request/response types can be added while maintaining backward compatibility by:

1. Adding new enum variants to `Request` and `Response`
2. Implementing corresponding request/response structs
3. Updating the protocol version (minor version for additions)

## Testing

The protocol implementation includes comprehensive tests for:

- Message serialization/deserialization
- All request/response types
- Error handling
- Version compatibility
- JSON schema validation

Run tests with: `cargo test -p stui-protocol`