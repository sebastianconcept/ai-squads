---
description: STUI - Network-enabled Terminal IDE for Smalltalk Development
squad: elite
version: 1.0.0
encoding: UTF-8
---

# STUI Project

## Project Overview

**STUI** is a network-enabled terminal IDE that revolutionizes Smalltalk development by providing a unified, multi-dialect development environment with AI-powered assistance and remote debugging capabilities.

## Current Status

### Phase 1: COMPLETED ✅
- **Terminal detection and capability querying** ✅
- **Window management system** (panels, splits, overlays) ✅ 
- **Keyboard input handling and command routing** ✅
- **ZeroMQ Pharo integration** ✅ **MAJOR ACHIEVEMENT**
- **JSON Protocol implementation** ✅ **88+ tests passing**
- **Complete Rust TUI client** ✅ **Professional interface**
- **Simple workspace for code evaluation** ✅
- **Basic transcript integration** ✅

### Phase 2: ACTIVE (Weeks 1-4)
- **Code Completion** - Method and class autocompletion  
- **Enhanced Error Display** - Clear, actionable error messages
- **Session Persistence** - Remember connection state across restarts
- **Command History** - Persistent command history
- **Basic Theme System** - Light/dark terminal themes foundation

## Technology Stack

### Core Technologies
- **Rust**: TUI client, networking, protocol implementation
- **Smalltalk/Pharo**: Backend evaluation engine, object system
- **ZeroMQ**: High-performance networking and communication
- **JSON**: Protocol serialization and data exchange

### Architecture
- **Facade Pattern**: Extensible architecture supporting multiple Smalltalk dialects
- **Client-Server**: Rust TUI client ↔ Pharo Smalltalk server
- **Network-First**: Designed for remote development and collaboration

## Squad Integration

### Elite Squad Focus
- **Rust Development**: TUI client, networking protocols, performance optimization
- **Smalltalk/Pharo**: Backend integration, object system design, live programming
- **Systems Programming**: ZeroMQ integration, cross-platform compatibility
- **Full-Stack**: End-to-end development environment

### Available Agents
- **@agent:software-engineer** - Rust/Smalltalk development, architecture
- **@agent:git-workflow** - Version control and workflow management
- **@agent:collaboration** - Team coordination and quality assurance
- **@agent:ux-expert** - User experience research and design
- **@agent:ui-implementor** - Frontend implementation across platforms
- **@agent:product-planner** - Strategic product planning and development

## Development Workflow

### Current Branch Strategy
- **master**: Main development branch
- **feature/***: Feature development branches
- **worktrees**: Active development worktrees for parallel development

### Active Worktrees
- `stui-session-refactor` - Session model refactoring
- `advanced-debugging` - Advanced debugging features
- `json-protocol` - JSON protocol implementation
- `rust-client-integration` - Rust TUI client integration
- `zeromq-integration` - ZeroMQ networking integration

## Next Steps

### Immediate Priorities (Phase 2)
1. **Code Completion System** - Implement method and class autocompletion
2. **Enhanced Error Handling** - Improve error display and debugging
3. **Session Management** - Persistent connections and state management
4. **Theme System** - Basic light/dark theme support

### Long-term Vision
- **AI Assistant Integration** - Claude, OpenAI, Gemini integration
- **Multi-dialect Support** - Pharo, Squeak, VA Smalltalk
- **Remote Debugging** - Production system debugging capabilities
- **Enterprise Features** - Team collaboration and enterprise deployment

## Integration Notes

### SquadsAI Adoption
- **Adopted Date**: 2025-08-23
- **Squad**: Elite Squad (Rust and Smalltalk/Pharo Development)
- **Status**: Active development project
- **Workflow**: Elite squad development workflow with Rust/Smalltalk focus

### Project Standards
- **Code Quality**: High-performance, memory-efficient Rust code
- **Testing**: Comprehensive test coverage (88+ tests currently passing)
- **Documentation**: Clear protocol documentation and integration guides
- **Performance**: ZeroMQ-based high-performance networking
