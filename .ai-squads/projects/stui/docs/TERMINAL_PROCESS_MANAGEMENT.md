# Terminal Process Management Best Practices

> **Date**: 2025-08-24  
> **Context**: Important reminder from team about maintaining terminal control  
> **Purpose**: Document best practices for managing long-running processes  

## 🎯 **Core Principle**

**Always maintain terminal control and feedback loop** when running programs that may run permanently or for extended periods.

## 🚨 **The Problem**

Long-running processes can:
- Block the terminal indefinitely
- Prevent further commands and feedback
- Make it difficult to stop or manage the process
- Break the development workflow and feedback loop

## ✅ **Best Practices**

### 1. **Use Timeouts for Testing**
```bash
# macOS (using GNU timeout if installed)
gtimeout 30s ./long_running_program

# Linux
timeout 30s ./long_running_program

# Alternative: Use shell job control
./long_running_program &
PID=$!
sleep 30
kill $PID 2>/dev/null || true
```

### 2. **Background Process Management**
```bash
# Start in background with PID tracking
./server_program &
SERVER_PID=$!
echo "Server started (PID: $SERVER_PID)"

# Always cleanup afterward
kill $SERVER_PID 2>/dev/null || true
echo "Server stopped"
```

### 3. **Built-in Process Cleanup**
```bash
#!/bin/bash
# Script with automatic cleanup
set -e

# Start server
./simple_demo_server &
SERVER_PID=$!

# Run client operations
./simple_demo_client

# Always cleanup (even on script failure)
kill $SERVER_PID 2>/dev/null || true
echo "Cleanup complete"
```

### 4. **Interactive Process Control**
```bash
# For development and testing
./program &
echo "Process started. Press any key to continue, Ctrl+C to stop server."
read -n 1
kill %1 2>/dev/null || true
```

### 5. **Signal Handling in Scripts**
```bash
#!/bin/bash
# Proper signal handling
SERVER_PID=""

cleanup() {
    if [[ -n "$SERVER_PID" ]]; then
        kill "$SERVER_PID" 2>/dev/null || true
        echo "Server stopped"
    fi
}

# Setup signal handlers
trap cleanup EXIT INT TERM

# Start server
./server &
SERVER_PID=$!

# Do work...
./client

# Cleanup happens automatically via trap
```

## 🛠️ **Specific Implementation Examples**

### Demo Scripts (Current Implementation ✅)
```bash
# From run_full_integration_demo.sh
./simple_demo_server &
SERVER_PID=$!
echo "📡 Server started (PID: $SERVER_PID)"

# Wait for server to start
sleep 2

# Run client
./simple_demo_client

# Cleanup
kill $SERVER_PID 2>/dev/null || true
echo "✅ Demo server stopped"
```

### Testing Long-Running Services
```bash
# For testing services that should run indefinitely
./service &
SERVICE_PID=$!

# Test for specific duration
sleep 10

# Check if still running
if kill -0 "$SERVICE_PID" 2>/dev/null; then
    echo "✅ Service running successfully"
    kill "$SERVICE_PID"
else
    echo "❌ Service failed"
fi
```

### Development Workflow
```bash
# For development iterations
echo "Starting development server..."
cargo run --bin server &
SERVER_PID=$!

echo "Server PID: $SERVER_PID"
echo "Press Enter to run tests, Ctrl+C to stop server"

read

# Run tests
cargo test

# Stop server
kill $SERVER_PID 2>/dev/null || true
echo "Development session complete"
```

## 🔧 **Tool-Specific Approaches**

### Cargo and Rust
```bash
# For cargo run commands that start servers
cargo run --bin server &
CARGO_PID=$!

# Work with the server
cargo test --test integration_tests

# Cleanup
kill $CARGO_PID 2>/dev/null || true
```

### ZeroMQ Services
```bash
# For ZeroMQ-based services
./zmq_server &
ZMQ_PID=$!

# Allow time for socket binding
sleep 1

# Run ZeroMQ client tests
./zmq_client

# Cleanup
kill $ZMQ_PID 2>/dev/null || true
```

### Pharo/Smalltalk Processes
```bash
# For Pharo headless servers
pharo --headless image.image server.st &
PHARO_PID=$!

# Run integration tests
./run_tests

# Cleanup Pharo process
kill $PHARO_PID 2>/dev/null || true
```

## 📋 **Checklist for Process Management**

### Before Starting Any Long-Running Process:
- [ ] Decide on maximum runtime or timeout
- [ ] Plan cleanup strategy
- [ ] Consider signal handling
- [ ] Test process termination

### In Scripts:
- [ ] Use `set -e` for error handling
- [ ] Capture PID of background processes
- [ ] Implement cleanup functions
- [ ] Use trap handlers for signals
- [ ] Always call cleanup, even on failure

### In Development:
- [ ] Keep processes short-lived when possible
- [ ] Use background jobs with timeouts
- [ ] Monitor process status
- [ ] Document expected runtime

## 🚀 **Integration with STUI Workflow**

### Current Implementation Status ✅
- **Demo Script**: Properly manages server lifecycle
- **Integration Tests**: Run without hanging
- **Process Cleanup**: Automatic via script trap handlers

### For Future Development:
- **Development Scripts**: Include timeout and cleanup
- **CI/CD Pipeline**: Ensure all processes are bounded
- **Production Deployment**: Use proper service management
- **Testing Workflow**: Always include process cleanup

## 💡 **Key Takeaways**

1. **Never run unbounded processes** in development/testing
2. **Always plan for cleanup** before starting processes
3. **Use timeouts** when testing long-running services
4. **Capture PIDs** for proper process management
5. **Implement signal handlers** for graceful shutdown
6. **Test cleanup paths** to ensure they work

---

**Bottom Line**: Maintain control of your terminal and development workflow by properly managing process lifecycles. The current STUI demo implementation demonstrates excellent process management practices.
