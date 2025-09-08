# ZeroMQ FFI Bridge - User Guide

## Overview

The ZeroMQ FFI Bridge provides a complete implementation of ZeroMQ messaging for Pharo, featuring:

- **Low-level FFI interface** for direct ZeroMQ API access
- **High-level abstractions** for easier development
- **Comprehensive error handling** with descriptive messages
- **Advanced features** including message management and polling
- **Extensive testing** with mock implementations

## Installation

```smalltalk
Metacello new
    repository: 'tonel://./src';
    baseline: 'ZeroMQ';
    load.
```

## Quick Start

### Basic Request-Reply Pattern

```smalltalk
"Create context and sockets"
context := ZeroMQContext new.
reqSocket := context createSocket: ZeroMQConstants ZMQ_REQ.
repSocket := context createSocket: ZeroMQConstants ZMQ_REP.

"Bind and connect"
repSocket bind: 'tcp://127.0.0.1:5555'.
reqSocket connect: 'tcp://127.0.0.1:5555'.

"Send request"
data := 'Hello' asByteArray.
reqSocket send: data size: data size flags: 0.

"Receive and reply"
data := ExternalAddress allocate: 100.
repSocket recv: data size: 100 flags: 0.
repSocket send: data size: 5 flags: 0.

"Receive reply"
reqSocket recv: data size: 100 flags: 0.

"Cleanup"
reqSocket close.
repSocket close.
context terminate.
```

### High-Level API

```smalltalk
"Create context"
context := ZeroMQContext new.

"Create socket"
socket := context createSocket: ZeroMQConstants ZMQ_REQ.

"Set options"
socket setOption: ZeroMQConstants ZMQ_LINGER 
    value: (ExternalAddress allocate: 4) 
    size: 4.

"Bind socket"
socket bind: 'tcp://127.0.0.1:5555'.

"Send data"
data := 'Hello World' asByteArray.
socket send: data size: data size flags: 0.

"Receive data"
data := ExternalAddress allocate: 100.
socket recv: data size: 100 flags: 0.

"Cleanup"
socket close.
context terminate.
```

## Core Classes

### ZeroMQContext

High-level context management:

```smalltalk
context := ZeroMQContext new.
context setOption: ZeroMQConstants ZMQ_IO_THREADS value: 4.
socket := context createSocket: ZeroMQConstants ZMQ_REQ.
context terminate.
```

### ZeroMQSocket

High-level socket operations:

```smalltalk
socket := context createSocket: ZeroMQConstants ZMQ_REP.
socket bind: 'tcp://127.0.0.1:5555'.
socket setOption: ZeroMQConstants ZMQ_LINGER value: lingerValue size: 4.
socket send: data size: dataSize flags: 0.
socket close.
```

### ZeroMQMessage

Message management:

```smalltalk
message := ZeroMQMessage new.
message setData: 'Hello World' asByteArray.
data := message data.
size := message size.
message close.
```

### ZeroMQPoller

Multi-socket polling:

```smalltalk
poller := ZeroMQPoller new.
poller addSocket: socket1 events: ZeroMQConstants ZMQ_POLLIN.
poller addSocket: socket2 events: ZeroMQConstants ZMQ_POLLOUT.
events := poller poll: 1000. "1 second timeout"
poller hasEvents ifTrue: [
    "Handle events"
].
```

## Error Handling

### Using ZeroMQError

```smalltalk
"Check results"
result := socket send: data size: dataSize flags: 0.
result < 0 ifTrue: [
    error := ZeroMQError newWithCode: result.
    error isRecoverable ifTrue: [
        "Retry operation"
    ] ifFalse: [
        "Handle fatal error"
    ].
].

"Create custom errors"
error := ZeroMQError newWithMessage: 'Connection failed'.
error := ZeroMQError newWithCode: ZeroMQConstants ZMQ_EAGAIN 
    context: 'send operation'.
```

### Error Classification

```smalltalk
error := ZeroMQError newWithCode: result.

"Check error types"
error isRecoverable.    "EAGAIN, EINTR"
error isFatal.          "All other errors"
error isTimeout.        "ETIMEDOUT"
error isConnectionError. "ECONNREFUSED, ECONNRESET, etc."
error isAddressError.   "EADDRINUSE, EADDRNOTAVAIL"
```

## Socket Types

### Request-Reply (REQ/REP)

```smalltalk
"Request socket"
reqSocket := context createSocket: ZeroMQConstants ZMQ_REQ.
reqSocket connect: 'tcp://127.0.0.1:5555'.
reqSocket send: requestData size: requestSize flags: 0.
reqSocket recv: responseData size: responseSize flags: 0.

"Reply socket"
repSocket := context createSocket: ZeroMQConstants ZMQ_REP.
repSocket bind: 'tcp://127.0.0.1:5555'.
repSocket recv: requestData size: requestSize flags: 0.
repSocket send: responseData size: responseSize flags: 0.
```

### Publish-Subscribe (PUB/SUB)

```smalltalk
"Publisher socket"
pubSocket := context createSocket: ZeroMQConstants ZMQ_PUB.
pubSocket bind: 'tcp://127.0.0.1:5556'.
pubSocket send: messageData size: messageSize flags: 0.

"Subscriber socket"
subSocket := context createSocket: ZeroMQConstants ZMQ_SUB.
subSocket connect: 'tcp://127.0.0.1:5556'.
subSocket setOption: ZeroMQConstants ZMQ_SUBSCRIBE 
    value: (ExternalAddress allocate: 0) size: 0.
subSocket recv: messageData size: messageSize flags: 0.
```

### Push-Pull (PUSH/PULL)

```smalltalk
"Push socket"
pushSocket := context createSocket: ZeroMQConstants ZMQ_PUSH.
pushSocket bind: 'tcp://127.0.0.1:5557'.
pushSocket send: workData size: workSize flags: 0.

"Pull socket"
pullSocket := context createSocket: ZeroMQConstants ZMQ_PULL.
pullSocket connect: 'tcp://127.0.0.1:5557'.
pullSocket recv: workData size: workSize flags: 0.
```

## Socket Options

### Common Options

```smalltalk
"Linger option"
lingerValue := ExternalAddress allocate: 4.
lingerValue int32AtOffset: 0 put: 1000.
socket setOption: ZeroMQConstants ZMQ_LINGER 
    value: lingerValue size: 4.

"Receive timeout"
timeoutValue := ExternalAddress allocate: 4.
timeoutValue int32AtOffset: 0 put: 5000.
socket setOption: ZeroMQConstants ZMQ_RCVTIMEO 
    value: timeoutValue size: 4.

"Send timeout"
socket setOption: ZeroMQConstants ZMQ_SNDTIMEO 
    value: timeoutValue size: 4.

"High water mark"
hwmValue := ExternalAddress allocate: 4.
hwmValue int32AtOffset: 0 put: 1000.
socket setOption: ZeroMQConstants ZMQ_RCVHWM 
    value: hwmValue size: 4.
```

## Message Flags

```smalltalk
"Non-blocking send"
socket send: data size: dataSize flags: ZeroMQConstants ZMQ_DONTWAIT.

"Non-blocking receive"
socket recv: data size: dataSize flags: ZeroMQConstants ZMQ_DONTWAIT.

"Send more (for multi-part messages)"
socket send: part1 size: part1Size flags: ZeroMQConstants ZMQ_SNDMORE.
socket send: part2 size: part2Size flags: 0.
```

## Polling Examples

### Basic Polling

```smalltalk
poller := ZeroMQPoller new.
poller addSocket: socket1 events: ZeroMQConstants ZMQ_POLLIN.
poller addSocket: socket2 events: ZeroMQConstants ZMQ_POLLOUT.

"Poll with timeout"
events := poller poll: 1000.

"Check for events"
poller hasEvents ifTrue: [
    "Handle events"
].
```

### Event-Specific Polling

```smalltalk
"Add sockets with different events"
poller addSocket: inputSocket events: ZeroMQConstants ZMQ_POLLIN.
poller addSocket: outputSocket events: ZeroMQConstants ZMQ_POLLOUT.
poller addSocket: errorSocket events: ZeroMQConstants ZMQ_POLLERR.

"Poll and check specific events"
poller poll: 1000.
poller eventsForSocket: inputSocket > 0 ifTrue: [
    "Handle input event"
].
```

## Error Recovery Strategies

### Recoverable Errors

```smalltalk
[| result |
    result := socket send: data size: dataSize flags: 0.
    result < 0 ifTrue: [
        error := ZeroMQError newWithCode: result.
        error isRecoverable ifTrue: [
            "Retry after delay"
            (Delay forMilliseconds: 100) wait.
            self retrySend: data
        ] ifFalse: [
            error signal
        ]
    ]
] on: ZeroMQError do: [:error |
    "Handle error"
].
```

### Connection Management

```smalltalk
"Handle connection errors"
[| result |
    result := socket connect: address.
    result < 0 ifTrue: [
        error := ZeroMQError newWithCode: result.
        error isConnectionError ifTrue: [
            "Try alternative address"
            socket connect: alternativeAddress
        ] ifFalse: [
            error signal
        ]
    ]
] on: ZeroMQError do: [:error |
    "Handle error"
].
```

## Testing

### Using Mock Implementations

```smalltalk
"Test with mocks"
mockContext := ZeroMQContextMock new.
mockSocket := mockContext createSocket: ZeroMQConstants ZMQ_REQ.
mockSocket bind: 'tcp://127.0.0.1:5555'.
mockSocket close.
mockContext terminate.
```

### Running Tests

```bash
./test ZeroMQ-Tests
```

## Best Practices

### Resource Management

```smalltalk
"Always close sockets and terminate contexts"
context := ZeroMQContext new.
socket := context createSocket: ZeroMQConstants ZMQ_REQ.
[
    "Use socket"
    socket send: data size: dataSize flags: 0.
] ensure: [
    socket close.
    context terminate.
].
```

### Error Handling

```smalltalk
"Always check return values"
result := socket send: data size: dataSize flags: 0.
result < 0 ifTrue: [
    "Handle error appropriately"
    error := ZeroMQError newWithCode: result.
    error signal
].
```

### Performance

```smalltalk
"Use appropriate buffer sizes"
data := ExternalAddress allocate: 1024. "Reasonable buffer size"

"Use non-blocking operations when appropriate"
socket send: data size: dataSize flags: ZeroMQConstants ZMQ_DONTWAIT.

"Set appropriate timeouts"
timeoutValue := ExternalAddress allocate: 4.
timeoutValue int32AtOffset: 0 put: 5000. "5 seconds"
socket setOption: ZeroMQConstants ZMQ_RCVTIMEO 
    value: timeoutValue size: 4.
```

## Troubleshooting

### Common Issues

1. **Address already in use**: Use different port or wait for socket to close
2. **Connection refused**: Check if server is running and address is correct
3. **Resource temporarily unavailable**: Use non-blocking operations or retry
4. **Message too large**: Increase buffer size or split message

### Debugging

```smalltalk
"Enable debug logging"
context setOption: ZeroMQConstants ZMQ_IO_THREADS value: 1.

"Check socket state"
socket getOption: ZeroMQConstants ZMQ_LINGER 
    value: lingerValue size: 4.
```

## API Reference

### ZeroMQContext Methods

- `new` - Create new context
- `createSocket: type` - Create socket with type
- `setOption: option value: value` - Set context option
- `getOption: option` - Get context option
- `shutdown` - Shutdown context
- `terminate` - Terminate context

### ZeroMQSocket Methods

- `bind: address` - Bind socket to address
- `connect: address` - Connect socket to address
- `send: data size: size flags: flags` - Send data
- `recv: data size: size flags: flags` - Receive data
- `setOption: option value: value size: size` - Set socket option
- `getOption: option value: value size: size` - Get socket option
- `close` - Close socket

### ZeroMQMessage Methods

- `new` - Create new message
- `initializeWithSize: size` - Create message with size
- `data` - Get message data
- `setData: byteArray` - Set message data
- `size` - Get message size
- `send: socket flags: flags` - Send message
- `recv: socket flags: flags` - Receive message
- `close` - Close message

### ZeroMQPoller Methods

- `new` - Create new poller
- `addSocket: socket events: events` - Add socket to poller
- `removeSocket: socket` - Remove socket from poller
- `poll: timeout` - Poll for events with timeout
- `poll` - Poll without timeout
- `pollNonBlocking` - Poll without blocking
- `hasEvents` - Check if any socket has events
- `eventsForSocket: socket` - Get events for specific socket
- `clear` - Clear all poll items

## License

This project is licensed under the MIT License.
