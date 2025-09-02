---
description: User Experience Design - STUI Welcome Screen & Connection Management
squad: elite
version: 1.0.0
encoding: UTF-8
---

# User Experience Design: STUI Welcome Screen & Connection Management

## Design Philosophy

### Terminal-First Design Principles
- **Keyboard-Driven**: All interactions optimized for keyboard navigation
- **Information Density**: Efficient use of terminal screen space
- **Progressive Disclosure**: Show complexity only when needed
- **Familiar Patterns**: Leverage established terminal UI conventions
- **Error Resilience**: Clear error states with actionable recovery

### Visual Design Language
- **Professional Aesthetic**: Clean, modern terminal interface
- **Clear Hierarchy**: Obvious navigation and status indicators
- **Consistent Spacing**: Standardized layout and component spacing
- **Accessible Colors**: High contrast, colorblind-friendly palette
- **Responsive Layout**: Adapts to different terminal sizes

## Welcome Screen Design

### Screen Layout

```
┌─────────────────────────────────────────────────────────────┐
│                    STUI - Smalltalk TUI IDE                 │
│                                                             │
│  Welcome to STUI - Your Modern Smalltalk Development Hub   │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    Quick Connect                       │ │
│  │                                                         │ │
│  │  [★] Production Server (192.168.1.100)     [Connect]   │ │
│  │  [★] Development VM (localhost:5555)       [Connect]   │ │
│  │  [★] Staging Environment (10.0.0.50)       [Connect]   │ │
│  │                                                         │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                  Recent Connections                    │ │
│  │                                                         │ │
│  │  [●] Test Server (192.168.1.101) - 2 hours ago        │ │
│  │  [●] Local Dev (localhost:5556) - Yesterday           │ │
│  │  [●] Cloud Instance (aws-east) - 3 days ago          │ │
│  │                                                         │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    Actions                             │ │
│  │                                                         │ │
│  │  [N] New Connection    [F] Manage Favorites    [H] Help │ │
│  │                                                         │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                             │
│  Press 'N' for new connection, 'H' for help, or use arrow   │
│  keys to navigate.                                          │
└─────────────────────────────────────────────────────────────┘
```

### Interaction Flow

#### **Primary Navigation**
1. **Arrow Keys**: Navigate between connection options
2. **Enter**: Select highlighted connection or action
3. **Tab**: Move between sections (Quick Connect → Recent → Actions)
4. **Escape**: Return to previous screen or cancel action

#### **Quick Connect Section**
- **Starred Connections**: User-defined favorites with star indicators
- **One-Click Connect**: Direct connection with saved credentials
- **Status Indicators**: Show connection health and last used time
- **Quick Actions**: Connect, Edit, Remove options on hover/selection

#### **Recent Connections Section**
- **Time-Based Ordering**: Most recent connections first
- **Connection Status**: Visual indicators for successful/failed connections
- **Quick Reconnect**: One-click access to recent connections
- **Context Actions**: View details, remove from history

#### **Actions Section**
- **New Connection**: Start connection setup wizard
- **Manage Favorites**: Edit, organize, and configure favorites
- **Help & Documentation**: Access contextual help and guides

## New Connection Flow

### Connection Setup Wizard

#### **Step 1: Connection Type Selection**
```
┌─────────────────────────────────────────────────────────────┐
│                    New Connection Setup                     │
│                                                             │
│  Select connection type:                                    │
│                                                             │
│  [●] Local Development (localhost)                         │
│  [ ] Remote Server (IP/Hostname)                           │
│                                                             │
│  [Continue]  [Cancel]  [Help]                              │
└─────────────────────────────────────────────────────────────┘
```

#### **Step 2: Connection Details**
```
┌─────────────────────────────────────────────────────────────┐
│                    Connection Details                      │
│                                                             │
│  Connection Name: [My Development Server    ]             │
│  Host/IP:        [192.168.1.100            ]             │
│  Port:           [5555                     ]             │
│  Auth Token:     [*************************] [Show]       │
│                                                             │
│  [Test Connection]  [Save & Connect]  [Back]  [Cancel]    │
└─────────────────────────────────────────────────────────────┘
```

#### **Step 3: Connection Test & Validation**
```
┌─────────────────────────────────────────────────────────────┐
│                    Testing Connection...                   │
│                                                             │
│  ═══════════════════════════════════════════════════════════ │
│  Connecting to 192.168.1.100:5555...                      │
│  Authenticating with token...                             │
│  Validating Smalltalk environment...                       │
│  Loading session data...                                   │
│  ═══════════════════════════════════════════════════════════ │
│                                                             │
│  ✅ Connection successful!                                 │
│                                                             │
│  [Connect to Main Interface]  [Save Connection]  [Cancel]  │
└─────────────────────────────────────────────────────────────┘
```

### Form Validation Rules

#### **Connection Name**
- **Required**: Yes
- **Format**: Alphanumeric, spaces, hyphens, underscores
- **Length**: 3-50 characters
- **Uniqueness**: Must be unique within user's connections

#### **Host/IP Address**
- **Required**: Yes
- **Format**: Valid IPv4, IPv6, or hostname
- **Validation**: Real-time DNS resolution check
- **Default**: localhost (for local connections)

#### **Port**
- **Required**: Yes
- **Range**: 1024-65535
- **Default**: 5555 (STUI default)
- **Validation**: Port availability check

#### **Auth Token**
- **Required**: Yes
- **Format**: Alphanumeric, minimum 16 characters
- **Security**: Encrypted storage, masked display
- **Validation**: Token format validation

## Multi-Session Management

### Session Status Bar
```
┌─────────────────────────────────────────────────────────────┐
│  STUI - Session 1: Production Server (192.168.1.100)        │
│  [1] Production ●  [2] Development ○  [3] Staging ○           │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    Main Interface                        │ │
│  │                                                         │ │
│  │  Welcome to your Smalltalk development environment!     │ │
│  │                                                         │ │
│  │  Available Tools:                                       │ │
│  │  [W] Workspace    [I] Inspector    [C] Class Browser    │ │
│  │  [T] Transcript   [S] Settings     [H] Help             │ │
│  │                                                         │ │
│  │  Learning & Help:                                       │ │
│  │  [L] Smalltalk Tutorial    [D] Documentation    [E] Examples │ │
│  │                                                         │ │
│  │  Recent Commands:                                       │ │
│  │  • Object new inspect                                   │ │
│  │  • SystemVersion current                                │ │
│  │  • Smalltalk allClasses                                 │ │
│  │                                                         │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Session Switching Interface
```
┌─────────────────────────────────────────────────────────────┐
│                    Session Manager                          │
│                                                             │
│  Active Sessions:                                           │
│                                                             │
│  [1] ● Production Server (192.168.1.100) - Connected       │
│      Last activity: 2 minutes ago                          │
│      Tools: Workspace, Inspector, Class Browser            │ │
│                                                             │
│  [2] ○ Development VM (localhost:5555) - Connected          │
│      Last activity: 15 minutes ago                         │
│      Tools: Workspace, Transcript                          │ │
│                                                             │
│  [3] ○ Staging Environment (10.0.0.50) - Disconnected      │
│      Last activity: 1 hour ago                             │
│      Tools: None                                           │ │
│                                                             │
│  Actions:                                                   │
│  [S] Switch Session    [N] New Session    [D] Disconnect    │
│  [C] Close Session     [R] Reconnect      [B] Back          │
└─────────────────────────────────────────────────────────────┘
```

## Multi-Workspace & Multi-Tool Management

### Workspace Management Interface
```
┌─────────────────────────────────────────────────────────────┐
│                    Workspace Manager                        │
│                                                             │
│  Session: Production Server (192.168.1.100) - Connected    │
│                                                             │
│  Active Workspaces:                                         │
│                                                             │
│  [1] ● Main Workspace - Last: 30s ago                      │
│      Lines: 45    Cursor: Line 23                          │
│                                                             │
│  [2] ○ Debug Workspace - Last: 2m ago                      │
│      Lines: 12    Cursor: Line 8                           │
│                                                             │
│  [3] ○ Test Workspace - Last: 5m ago                       │
│      Lines: 67    Cursor: Line 34                          │
│                                                             │
│  [4] ○ Analysis Workspace - Last: 10m ago                  │
│      Lines: 23    Cursor: Line 15                          │
│                                                             │
│  [5] ○ Scratch Workspace - Last: 15m ago                   │
│      Lines: 8     Cursor: Line 3                           │
│                                                             │
│  Actions:                                                   │
│  [N] New Workspace    [S] Switch    [C] Close    [R] Rename │
│  [D] Duplicate       [M] Merge     [B] Back               │
└─────────────────────────────────────────────────────────────┘
```

### Inspector Management Interface
```
┌─────────────────────────────────────────────────────────────┐
│                    Inspector Manager                        │
│                                                             │
│  Session: Production Server (192.168.1.100) - Connected    │
│                                                             │
│  Active Inspectors:                                         │
│                                                             │
│  [1] ● Object Inspector - Array (size: 15)                │
│      Last: 1m ago    Properties: 8 visible, 3 expanded     │
│                                                             │
│  [2] ○ Collection Inspector - Dictionary (size: 42)        │
│      Last: 3m ago    Properties: 12 visible, 5 expanded    │
│                                                             │
│  [3] ○ Debug Inspector - Exception                         │
│      Last: 5m ago    Properties: 6 visible, 2 expanded     │
│                                                             │
│  [4] ○ Data Inspector - User Object                        │
│      Last: 8m ago    Properties: 15 visible, 7 expanded    │
│                                                             │
│  [5] ○ Analysis Inspector - Performance Data               │
│      Last: 12m ago   Properties: 20 visible, 10 expanded  │
│                                                             │
│  Actions:                                                   │
│  [N] New Inspector    [S] Switch    [C] Close    [R] Refresh │
│  [D] Duplicate       [E] Export    [B] Back               │
└─────────────────────────────────────────────────────────────┘
```

### Class Browser Management Interface
```
┌─────────────────────────────────────────────────────────────┐
│                    Class Browser Manager                    │
│                                                             │
│  Session: Production Server (192.168.1.100) - Connected    │
│                                                             │
│  Active Class Browsers:                                     │
│                                                             │
│  [1] ● Main Browser - Object class                         │
│      Last: 45s ago    View: Methods    Filter: "initialize" │
│                                                             │
│  [2] ○ Collection Browser - Array class                    │
│      Last: 2m ago     View: Categories    Filter: "add:"   │
│                                                             │
│  [3] ○ Debug Browser - Exception class                     │
│      Last: 4m ago     View: Methods    Filter: "signal"     │
│                                                             │
│  [4] ○ UI Browser - Widget class                           │
│      Last: 7m ago     View: Instance Variables    Filter: "color" │
│                                                             │
│  [5] ○ Utility Browser - String class                      │
│      Last: 10m ago    View: Methods    Filter: "substring"  │
│                                                             │
│  Actions:                                                   │
│  [N] New Browser    [S] Switch    [C] Close    [R] Refresh │
│  [D] Duplicate      [E] Export    [B] Back                │
└─────────────────────────────────────────────────────────────┘
```

### Centralized Transcript Console
```
┌─────────────────────────────────────────────────────────────┐
│                    Transcript Console                        │
│                                                             │
│  2024-01-15 14:23:45 [INFO] Session connected              │
│  2024-01-15 14:23:46 [DEBUG] Loading workspace data         │
│  2024-01-15 14:23:47 [INFO] 5 workspaces loaded            │
│  2024-01-15 14:23:48 [DEBUG] Inspector 1: Array created     │
│  2024-01-15 14:23:49 [DEBUG] Inspector 2: Dict created      │
│  2024-01-15 14:23:50 [INFO] Class Browser: Object          │
│  2024-01-15 14:23:51 [DEBUG] Method search: "init"         │
│  2024-01-15 14:23:52 [INFO] Workspace 1: Code executed      │
│  2024-01-15 14:23:53 [ERROR] Exception in workspace 2       │
│  2024-01-15 14:23:54 [DEBUG] Inspector 3: Exception        │
│                                                             │
│  Actions:                                                   │
│  [C] Clear    [S] Save    [F] Filter    [R] Refresh    [B] Back │
└─────────────────────────────────────────────────────────────┘
```

### Multi-Tool Navigation Shortcuts
```
┌─────────────────────────────────────────────────────────────┐
│                    Navigation Shortcuts                     │
│                                                             │
│  Workspace Navigation:                                      │
│  [Ctrl+1] Workspace 1    [Ctrl+2] Workspace 2    [Ctrl+3] Workspace 3 │
│  [Ctrl+4] Workspace 4    [Ctrl+5] Workspace 5    [Ctrl+N] New Workspace │
│                                                             │
│  Inspector Navigation:                                      │
│  [Alt+1] Inspector 1      [Alt+2] Inspector 2    [Alt+3] Inspector 3 │
│  [Alt+4] Inspector 4      [Alt+5] Inspector 5    [Alt+N] New Inspector │
│                                                             │
│  Class Browser Navigation:                                  │
│  [Shift+1] Browser 1      [Shift+2] Browser 2    [Shift+3] Browser 3 │
│  [Shift+4] Browser 4      [Shift+5] Browser 5    [Shift+N] New Browser │
│                                                             │
│  Global Navigation:                                         │
│  [T] Transcript (Always Unique)    [S] Settings    [H] Help │
│  [Tab] Next Tool    [Shift+Tab] Previous Tool              │
└─────────────────────────────────────────────────────────────┘
```

### Workspace Switching Interface
```
┌─────────────────────────────────────────────────────────────┐
│                    Quick Workspace Switch                   │
│                                                             │
│  Press [Ctrl+W] to switch workspaces anytime                │
│                                                             │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  [1] Main ●  [2] Debug ○  [3] Test ○  [4] Analysis ○   │ │
│  │  [5] Scratch ○  [N] New  [C] Close  [B] Back          │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                             │
│  Current: Main Workspace (45 lines, last: 30s ago)         │
│  Session: Production Server (192.168.1.100) - Connected    │
└─────────────────────────────────────────────────────────────┘
```

### Persistent Workspace Status Bar
```
┌─────────────────────────────────────────────────────────────┐
│  STUI - Main Workspace (Production)                         │
│  [1]● [2]○ [3]○ [4]○ [5]○  [Ctrl+W] Switch  [T] Transcript  │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    Workspace Content                      │ │
│  │                                                         │ │
│  │  Object new inspect                                     │ │
│  │  "Hello World"                                          │ │
│  │  Array with: 1, 2, 3, 4, 5                             │ │
│  │                                                         │ │
│  │  // Your Smalltalk code here...                        │ │
│  │                                                         │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Tool Switching Overlay
```
┌─────────────────────────────────────────────────────────────┐
│                    Tool Switcher                            │
│                                                             │
│  Workspaces:                                                │
│  [1] Main ●  [2] Debug ○  [3] Test ○  [4] Analysis ○  [5] Scratch ○ │
│                                                             │
│  Inspectors:                                                │
│  [Alt+1] Object ●  [Alt+2] Collection ○  [Alt+3] Debug ○  [Alt+4] Data ○  [Alt+5] Analysis ○ │
│                                                             │
│  Class Browsers:                                            │
│  [Shift+1] Object ●  [Shift+2] Array ○  [Shift+3] Exception ○  [Shift+4] Widget ○  [Shift+5] String ○ │
│                                                             │
│  Global:                                                    │
│  [T] Transcript (Centralized)  [S] Settings  [H] Help     │
│                                                             │
│  [Esc] Cancel    [Enter] Switch to highlighted            │
└─────────────────────────────────────────────────────────────┘
```

### Tool State Persistence
```
┌─────────────────────────────────────────────────────────────┐
│                    Session State                            │
│                                                             │
│  Current Session: Production Server (192.168.1.100)        │
│                                                             │
│  Active Tools:                                              │
│  • Workspaces: 5 (1 active, 4 background)                  │
│  • Inspectors: 5 (1 active, 4 background)                  │
│  • Class Browsers: 5 (1 active, 4 background)              │
│  • Transcript: 1 (centralized, always active)               │
│                                                             │
│  Memory Usage:                                              │
│  • Workspace Data: 2.3 MB                                  │
│  • Inspector Data: 1.8 MB                                  │
│  • Browser Data: 1.2 MB                                    │
│  • Total: 5.3 MB                                           │
│                                                             │
│  Auto-Save: Enabled (every 30 seconds)                     │
│  Session Recovery: Available                               │
└─────────────────────────────────────────────────────────────┘
```

## Error Handling Design

### Connection Error States

#### **Network Connection Failed**
```
┌─────────────────────────────────────────────────────────────┐
│                    Connection Error                        │
│                                                             │
│  ❌ Failed to connect to 192.168.1.100:5555               │
│                                                             │
│  Error Details:                                             │
│  • Network timeout after 30 seconds                        │
│  • Server may be offline or unreachable                    │
│  • Check your network connection and try again             │
│                                                             │
│  Possible Solutions:                                       │
│  • Verify the host address is correct                       │
│  • Check if the server is running                           │
│  • Ensure firewall allows connections on port 5555         │
│  • Try connecting from another network                     │
│                                                             │
│  [Retry]  [Edit Connection]  [Cancel]  [Help]             │
└─────────────────────────────────────────────────────────────┘
```

#### **Authentication Failed**
```
┌─────────────────────────────────────────────────────────────┐
│                    Authentication Error                    │
│                                                             │
│  ❌ Authentication failed for 192.168.1.100:5555         │
│                                                             │
│  Error Details:                                             │
│  • Invalid or expired authentication token                 │
│  • Token format validation failed                          │
│  • Server rejected the provided credentials                │
│                                                             │
│  Possible Solutions:                                       │
│  • Verify your authentication token is correct             │
│  • Check if the token has expired                          │
│  • Generate a new token from the server                    │
│  • Ensure the token has the required permissions           │
│                                                             │
│  [Update Token]  [Try Again]  [Cancel]  [Help]             │
└─────────────────────────────────────────────────────────────┘
```

#### **Server Compatibility Error**
```
┌─────────────────────────────────────────────────────────────┐
│                    Compatibility Error                      │
│                                                             │
│  ⚠️  Server compatibility issue detected                   │
│                                                             │
│  Error Details:                                             │
│  • Server is running Pharo 11, STUI requires Pharo 13+     │
│  • Missing required STUI server components                  │
│  • Protocol version mismatch                               │
│                                                             │
│  Possible Solutions:                                       │
│  • Upgrade your Pharo server to version 13 or later        │
│  • Install the STUI server package on your Pharo image    │
│  • Contact your system administrator for assistance        │
│  • Use a compatible server environment                     │
│                                                             │
│  [Continue Anyway]  [Cancel]  [Help]  [Upgrade Guide]     │
└─────────────────────────────────────────────────────────────┘
```

### Error Recovery Flows

#### **Retry Logic**
- **Automatic Retry**: 3 attempts with exponential backoff
- **Manual Retry**: User-initiated retry with updated parameters
- **Progressive Disclosure**: Show detailed error info on demand
- **Context Preservation**: Maintain user input during retry

#### **Fallback Options**
- **Alternative Connections**: Suggest similar or backup connections
- **Offline Mode**: Allow limited functionality without connection
- **Diagnostic Tools**: Built-in network and server diagnostics
- **Support Integration**: Direct access to help and documentation

## Accessibility Design

### Keyboard Navigation
- **Tab Order**: Logical flow through all interactive elements
- **Shortcuts**: Mnemonic shortcuts for common actions
- **Focus Indicators**: Clear visual focus indicators
- **Skip Links**: Quick navigation to main content areas

### Screen Reader Support
- **Semantic Markup**: Proper heading hierarchy and landmarks
- **Descriptive Labels**: Clear, descriptive labels for all elements
- **Status Announcements**: Dynamic status updates for screen readers
- **Error Descriptions**: Detailed error descriptions for accessibility

### Color and Contrast
- **High Contrast**: Minimum 4.5:1 contrast ratio
- **Color Independence**: Information not conveyed by color alone
- **Colorblind Friendly**: Accessible to users with color vision deficiencies
- **Dark Mode Support**: Optional dark theme for reduced eye strain

## Responsive Design

### Terminal Size Adaptation
- **Minimum Size**: 80x24 characters (standard terminal)
- **Optimal Size**: 120x40 characters (recommended)
- **Large Size**: 160x60 characters (high-resolution displays)
- **Dynamic Layout**: Automatic layout adjustment based on size

### Content Prioritization
- **Essential Elements**: Always visible regardless of size
- **Progressive Disclosure**: Show details based on available space
- **Scrollable Areas**: Handle overflow gracefully
- **Status Compression**: Condense status information on small screens

## Implementation Guidelines

### Component Specifications
- **Consistent Spacing**: Use standardized spacing units
- **Typography**: Monospace font with clear hierarchy
- **Color Palette**: Defined color system for consistency
- **Animation**: Subtle, purposeful animations for feedback

### State Management
- **Loading States**: Clear loading indicators for all async operations
- **Success States**: Positive feedback for completed actions
- **Error States**: Clear error messages with recovery options
- **Empty States**: Helpful guidance when no data is available

### Performance Considerations
- **Fast Loading**: Optimize for quick initial load times
- **Responsive Interactions**: Immediate feedback for user actions
- **Efficient Updates**: Minimize screen redraws and updates
- **Resource Management**: Efficient memory and network usage
