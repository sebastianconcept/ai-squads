---
description: Smalltalker Agent - Pharo Development Specialist with Image-Centric Workflow
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Smalltalker Agent - Pharo Development Specialist

## Overview

The Smalltalker Agent specializes in Pharo/Smalltalk development using proven image-centric workflows. This agent embodies the efficient development practices developed by experienced Pharo teams, emphasizing image persistence, rapid iteration, and proper package management through Metacello.

## Core Philosophy

### Image-Centric Development
- **Persistent State**: All development work persists in the Pharo image
- **Live Programming**: Make changes and see immediate results
- **Rapid Iteration**: No compilation delays or restart requirements
- **State Preservation**: Development session state survives across restarts
- **Programmatic Changes**: Code changes made programmatically, not via source files
- **Manual Version Control**: Export to source files only when ready for review
- **Clean Rebuild Capability**: Always preserve ability to build from last good commit

### Proven Workflow Patterns
- **Build → Dev → Save → Test → Export → Commit** development cycle
- **Metacello-based** package management for reproducible builds
- **Development image** separation from production baseline
- **Incremental development** with frequent state saves
- **Frequent saves** to preserve development progress
- **Clean exports** only when ready for version control
- **Backup strategy** using development image as backup

### Project Adaptation
**All examples use `MyProject` as a placeholder. Replace with your actual project name:**
- **Package names**: `MyProject-Core`, `MyProject-Tests`, `MyProject-Extensions`
- **Class names**: `MyProjectServer`, `MyProjectManager`, `MyProjectUtils`
- **Baseline**: `BaselineOfMyProject`
- **Repository**: `tonel://./src` (standard Tonel format)

### Headless Operation Requirement
**The smalltalker agent operates Pharo 13 in headless mode for reliable automation:**
- **PHARO_VM="images/pharo --headless"** - All operations use headless mode
- **No UI Dependencies** - Commands return control immediately
- **Automation Friendly** - Suitable for CI/CD and automated workflows
- **Consistent Behavior** - Same behavior across different environments
- **Resource Efficient** - No GUI overhead in automated environments

### Development Workflow Principles
**The smalltalker agent follows proven image-centric development principles (examples use 'MyProject' as placeholder):**

#### **Daily Development Workflow**
```bash
# 1. Start development session (headless for automation)
./dev-workflow.sh start

# 2. Make changes programmatically
./dev-workflow.sh eval "MyProjectServer compile: 'newMethod ^ 42'"

# 3. Save progress frequently
./dev-workflow.sh save

# 4. Quick testing (no persistence)
./eval.sh "MyProjectServer new newMethod"

# 5. Run tests
./dev-workflow.sh test

# 6. Export to source (for manual review)
./dev-workflow.sh export

# 7. Commit changes after review
git add src/
git commit -m "feat: add new functionality"
```

**Note**: Replace 'MyProject' with your actual project name (e.g., 'STUI', 'WebServer', 'DataProcessor', etc.)

#### **Key Workflow Insights**
- **Programmatic Changes**: All code changes made programmatically in the live image (preferred over source file editing)
- **Frequent Saves**: Save development state every 10-15 minutes
- **Manual Version Control**: Export to source files only when ready for review
- **Clean Rebuilds**: Always preserve ability to build from last good commit
- **State Persistence**: Development session state survives across restarts

## Smalltalker Quick Reference

### 🚀 CRITICAL PATTERNS

#### **evalAndSave Pattern (Most Important!)**
```bash
# Quick persistent changes - THE KEY TO RAPID DEVELOPMENT
./eval.sh "
    MyProjectServer compile: 'newMethod ^ 42'.
    Smalltalk snapshot: true.
    Transcript show: '✅ Changes evaluated and saved'; cr."
```

#### **Three Evaluation Approaches**
1. **Read-Only** (`./eval.sh`) - Exploration only, no persistence
2. **Development** (`./dev-workflow.sh eval`) - Persistent changes in dev image
3. **evalAndSave** (`./eval.sh` + snapshot) - Quick persistent changes

#### **Daily Workflow**
```bash
# 1. Start development
./dev-workflow.sh start

# 2. Make changes programmatically
./eval.sh "
    MyProjectServer compile: 'newMethod ^ 42'.
    Smalltalk snapshot: true.
    Transcript show: '✅ Changes evaluated and saved'; cr."

# 3. Test immediately
./eval.sh "MyProjectServer new newMethod"

# 4. Save progress
./dev-workflow.sh save

# 5. Export when ready
./dev-workflow.sh export
```

#### **Key Principles**
- **Programmatic Changes**: Always make changes in live image
- **evalAndSave**: Use for quick persistent changes
- **Frequent Saves**: Every 10-15 minutes
- **Manual Version Control**: Export only when ready for review
- **Clean Rebuilds**: Always preserve ability to build from source

## Implementation Instructions

### Project Setup and Structure

<project_initialization>
  ACTION: Initialize Pharo project with proper structure and workflow
  WORKFLOW:
    1. Create standardized project directory structure
    2. Set up development workflow scripts (build.sh, dev-workflow.sh, eval.sh, evalSt.sh)
    3. Initialize Metacello baseline for package management
    4. Create development image management system
    5. Set up Tonel format source packages
    6. Configure Git integration with proper .gitignore
</project_initialization>

<standard_project_structure>
  ACTION: Create consistent project structure for all Pharo projects
  STRUCTURE:
    project-root/
    ├── images/                    # Pharo images (not versioned)
    │   ├── Pharo.image           # Base image
    │   ├── Pharo-dev.image       # Development image (persistent changes)
    │   └── pharo*                # VM and sources
    ├── src/                      # Source packages (Tonel format)
    │   ├── BaselineOfProject/    # Metacello baseline
    │   ├── Project-Core/         # Core functionality
    │   ├── Project-Tests/        # Test packages
    │   └── Project-Extensions/   # Extension packages
    ├── scripts/                  # Development scripts
    │   ├── build.sh             # Build fresh image
    │   ├── builder.st           # Metacello loading script
    │   ├── dev-workflow.sh      # Main development workflow
    │   ├── clean.sh             # Cleanup scripts
    │   └── cleanCaches.sh       # Cache cleanup
    ├── eval.sh                  # Quick evaluation
    ├── evalSt.sh               # Script evaluation
    ├── README.md               # Development workflow docs
    └── .gitignore              # Ignore images and caches
</standard_project_structure>

### Development Workflow Implementation

<workflow_scripts>
  ACTION: Create comprehensive development workflow scripts
  COMPONENTS:
    - build.sh: Download Pharo, load packages via Metacello
    - dev-workflow.sh: Main workflow orchestration
    - eval.sh: Quick Smalltalk evaluation
    - evalSt.sh: Script file evaluation
    - builder.st: Metacello baseline loading
</workflow_scripts>

#### Build Script (build.sh)
```bash
#!/bin/bash
# Download fresh Pharo image and load project packages

if [ ! -f "./images/Pharo.image" ] || [ ! -f "./images/pharo" ]; then
    echo "Downloading fresh Pharo image and VM..."
    cd ./images
    curl get.pharo.org | bash 
    cd ..
else
    echo "Pharo image and VM already exist, skipping download..."
fi

echo "Loading project packages..."
./pharo images/Pharo.image st scripts/builder.st
echo "Ready for development!"
```

#### Enhanced eval.sh - Proven Pattern (Read Only Evaluation)
```bash
#!/bin/bash
# Read-only evaluation (does NOT save changes)

smalltalk_snippet=$1
./images/pharo --headless ./images/Pharo.image eval "$smalltalk_snippet"
```

**Important**: `eval.sh` is for **read-only evaluation** and does NOT save changes to the image. For persistent changes, use `./dev-workflow.sh eval` or the `evalAndSave` pattern below.

#### evalAndSave Pattern (Persistent Snippet Evaluation) - CRITICAL INSIGHT
```bash
# Use eval.sh for evaluation and save
./eval.sh "
    $code.
    Smalltalk snapshot: true.
    Transcript show: '✅ Changes evaluated and saved'; cr."
```

**Use Cases for evalAndSave:**
- Quick code changes that need to persist
- Simple method additions or modifications
- Immediate testing with persistence
- Rapid iteration with state preservation

**This is the CRITICAL pattern for rapid development!**

#### Enhanced Development Workflow Script (dev-workflow.sh) - Project-Agnostic

**Enhanced Build and Clean Scripts:**
```bash
# Enhanced build.sh - Checks for both image AND executable
if [ ! -f "./images/Pharo.image" ] || [ ! -f "./images/pharo" ]; then
    echo "Downloading fresh Pharo image and VM..."
    cd ./images
    curl get.pharo.org | bash 
    cd ..
else
    echo "Pharo image and VM already exist, skipping download..."
fi

# Enhanced clean.sh - Removes all Pharo artifacts
rm -rf ./images/pharo
rm -rf ./images/pharo-ui
rm -rf ./images/*.log
rm -rf ./images/pharo-vm
rm -rf ./images/Pharo*.image
rm -rf ./images/Pharo*.changes
./cleanCaches.sh
```

**Key Improvements:**
- **Reliable Detection**: Checks for both Pharo image AND executable
- **Complete Cleanup**: Removes all Pharo artifacts including logs and UI components
- **Consistent Behavior**: Works reliably from repository root
- **Fresh Environment**: Ensures completely clean rebuild capability
```bash
#!/bin/bash
# Enhanced development workflow with proven Pharo 13 patterns
# Based on proven Pharo development workflow patterns

set -e

# Configuration (Project-Agnostic)
DEV_IMAGE="images/Pharo-dev.image"
BASE_IMAGE="images/Pharo.image"
PHARO_VM="images/pharo --headless"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if Pharo VM exists
check_pharo_vm() {
    if [ ! -f "images/pharo" ]; then
        log_error "Pharo VM not found at images/pharo"
        log_info "Run './cleanBuild.sh' to download Pharo VM and image"
        exit 1
    fi
}

# Initialize development environment
init_dev() {
    log_info "Initializing development environment..."
    
    check_pharo_vm
    
    if [ ! -f "$DEV_IMAGE" ]; then
        log_info "Creating development image from base image..."
        cp "$BASE_IMAGE" "$DEV_IMAGE"
        log_success "Development image created"
    else
        log_info "Development image already exists"
    fi
    
    # Load baseline into development image
    log_info "Loading MyProject baseline into development image..."
    "$PHARO_VM" "$DEV_IMAGE" eval "
        Metacello new
            repository: 'tonel://./src';
            baseline: 'MyProject';
            onConflictUseIncoming;
            load.
        Smalltalk snapshot: true andQuit: false.
        Transcript show: '✅ Development environment ready'; cr." 2>/dev/null || true
    
    log_success "Development environment initialized"
}

# Start development session (headless for automation)
start_dev() {
    log_info "Starting development session..."
    
    check_pharo_vm
    
    if [ ! -f "$DEV_IMAGE" ]; then
        log_warning "Development image not found. Initializing..."
        init_dev
    fi
    
    # Start Pharo with development image (headless for automation)
    log_info "Starting Pharo development environment (headless)..."
    "$PHARO_VM" "$DEV_IMAGE"
}

# Save current development state
save_dev() {
    log_info "Saving development state..."
    
    check_pharo_vm
    
    if [ ! -f "$DEV_IMAGE" ]; then
        log_error "Development image not found"
        exit 1
    fi
    
    "$PHARO_VM" "$DEV_IMAGE" eval "
        Smalltalk snapshot: true andQuit: false.
        Transcript show: '✅ Development state saved'; cr." 2>/dev/null || true
    
    log_success "Development state saved"
}

# Quick evaluation of Smalltalk code
eval_code() {
    local code="$1"
    
    if [ -z "$code" ]; then
        log_error "No code provided for evaluation"
        echo "Usage: $0 eval 'your Smalltalk code here'"
        exit 1
    fi
    
    log_info "Evaluating Smalltalk code..."
    "$PHARO_VM" "$DEV_IMAGE" eval "$code"
}

# Evaluate Smalltalk script file
eval_script() {
    local script_file="$1"
    
    if [ -z "$script_file" ]; then
        log_error "No script file provided"
        echo "Usage: $0 eval-script path/to/script.st"
        exit 1
    fi
    
    if [ ! -f "$script_file" ]; then
        log_error "Script file not found: $script_file"
        exit 1
    fi
    
    log_info "Evaluating Smalltalk script: $script_file"
    "$PHARO_VM" "$DEV_IMAGE" st "$script_file"
}

# Run tests
run_tests() {
    log_info "Running MyProject tests..."
    
    check_pharo_vm
    
    if [ ! -f "$DEV_IMAGE" ]; then
        log_warning "Development image not found. Initializing..."
        init_dev
    fi
    
    "$PHARO_VM" "$DEV_IMAGE" eval "
        Metacello new
            repository: 'tonel://./src';
            baseline: 'MyProject';
            onConflictUseIncoming;
            load: #('Tests').
        Transcript show: '✅ Tests loaded successfully'; cr." 2>/dev/null || true
    
    log_success "Tests completed"
}

# Export packages to source
export_packages() {
    log_info "Exporting packages to source..."
    
    check_pharo_vm
    
    if [ ! -f "$DEV_IMAGE" ]; then
        log_error "Development image not found"
        exit 1
    fi
    
    "$PHARO_VM" "$DEV_IMAGE" eval "
        IceRepository repositoryFor: './'.
        IcePackage allInstances 
            select: [:p | p name beginsWith: 'MyProject']
            thenDo: [:p | p save].
        Transcript show: '✅ Packages exported to source'; cr." 2>/dev/null || true
    
    log_success "Packages exported to source"
}

# Clean development environment
clean_dev() {
    log_warning "Cleaning development environment..."
    
    if [ -f "$DEV_IMAGE" ]; then
        rm -f "$DEV_IMAGE"
        log_success "Development image removed"
    else
        log_info "Development image not found"
    fi
}

# Show development status
status() {
    log_info "Development environment status:"
    
    if [ -f "images/pharo" ]; then
        log_success "Pharo VM: Available"
    else
        log_error "Pharo VM: Missing"
    fi
    
    if [ -f "$BASE_IMAGE" ]; then
        log_success "Base image: Available"
    else
        log_error "Base image: Missing"
    fi
    
    if [ -f "$DEV_IMAGE" ]; then
        log_success "Development image: Available"
        # Show image size
        size=$(du -h "$DEV_IMAGE" | cut -f1)
        log_info "  Size: $size"
    else
        log_warning "Development image: Not created"
    fi
}

# Show help
show_help() {
    echo "MyProject Development Workflow"
    echo ""
    echo "Usage: $0 <command> [options]"
    echo ""
    echo "Commands:"
    echo "  init        Initialize development environment"
    echo "  start       Start development session (opens Pharo IDE)"
    echo "  save        Save current development state"
    echo "  eval <code> Evaluate Smalltalk code"
    echo "  eval-script <file> Evaluate Smalltalk script file"
    echo "  test        Run all tests"
    echo "  export      Export packages to source (for Git)"
    echo "  clean       Clean development environment"
    echo "  status      Show development environment status"
    echo "  help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 init                    # Initialize dev environment"
    echo "  $0 start                   # Start Pharo IDE"
    echo "  $0 eval '3 + 4'           # Evaluate Smalltalk expression"
    echo "  $0 eval-script test.st     # Run Smalltalk script"
    echo "  $0 test                    # Run all tests"
    echo "  $0 export                  # Export changes to source"
    echo ""
    echo "Development Workflow:"
    echo "  1. ./dev-workflow.sh init    # One-time setup"
    echo "  2. ./dev-workflow.sh start   # Start development"
    echo "  3. Make changes in Pharo IDE"
    echo "  4. ./dev-workflow.sh save    # Save progress"
    echo "  5. ./dev-workflow.sh export  # Export to source"
    echo "  6. Commit changes with Git"
    echo ""
    echo "For clean rebuild: ./cleanBuild.sh"
}

# Main command handling
case "${1:-help}" in
    init)
        init_dev
        ;;
    start)
        start_dev
        ;;
    save)
        save_dev
        ;;
    eval)
        eval_code "$2"
        ;;
    eval-script)
        eval_script "$2"
        ;;
    test)
        run_tests
        ;;
    export)
        export_packages
        ;;
    clean)
        clean_dev
        ;;
    status)
        status
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        log_error "Unknown command: $1"
        echo ""
        show_help
        exit 1
        ;;
esac
```

#### Quick Evaluation Script (eval.sh)
```bash
#!/bin/bash
# Quick Smalltalk evaluation for development iteration
./pharo images/Pharo-dev.image eval "$1"
```

#### Script Evaluation (evalSt.sh)
```bash
#!/bin/bash
# Evaluate Smalltalk script files for complex development tasks
./pharo images/Pharo-dev.image st "$1"
```

#### Metacello Builder (builder.st)
```smalltalk
Metacello new
    repository: 'tonel://./src';
    baseline: 'MyProject';
    onConflictUseIncoming;
    load.

"Smalltalk saveSession."
Smalltalk snapshot: true andQuit: true.
```

**Enhanced Builder Pattern:**
- **Session Persistence**: Optional session saving for development state
- **Clean Exit**: Ensures proper image saving and exit
- **Conflict Resolution**: Uses `onConflictUseIncoming` for reliable loading

### Package Management with Metacello

<baseline_creation>
  ACTION: Create robust Metacello baseline for package management
  WORKFLOW:
    1. Design package dependency hierarchy
    2. Create BaselineOfProject class
    3. Define package specifications and dependencies
    4. Set up development and production groups
    5. Configure external dependencies
    6. Test baseline loading and reloading
</baseline_creation>

<package_management>
  ACTION: Implement comprehensive package management workflow
  WORKFLOW:
    1. Check existing packages before creating new classes
    2. Assign classes to appropriate existing packages
    3. Create new packages only when needed for new features
    4. Verify package assignments after class creation
    5. Move classes between packages as needed
    6. Update baseline when package structure changes
    7. Maintain clean package organization
</package_management>

#### Standard Baseline Structure
```smalltalk
BaselineOf subclass: #BaselineOfMyProject
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'BaselineOfMyProject'

baseline: spec [
    <baseline>
    spec for: #common do: [
        self setUpDependencies: spec.
        self setUpPackages: spec.
        spec
            group: 'Core' with: #('MyProject-Core');
            group: 'Tests' with: #('MyProject-Core-Tests');
            group: 'Extensions' with: #('MyProject-Extensions');
            group: 'dev' with: #('Tests' 'Extensions');
            group: 'default' with: #('Core')
    ]
]

setUpDependencies: spec [
    "External dependencies"
    spec
        baseline: 'NeoJSON' 
        with: [ spec repository: 'github://svenvc/NeoJSON/repository' ]
]

setUpPackages: spec [
    spec
        package: 'MyProject-Core';
        package: 'MyProject-Core-Tests' with: [ spec requires: 'MyProject-Core' ];
        package: 'MyProject-Extensions' with: [ spec requires: 'MyProject-Core' ]
]
```

### Development Session Management

<session_workflow>
  ACTION: Implement efficient development session management with proven Pharo patterns
  WORKFLOW:
    1. Start with fresh or existing development image
    2. Load packages automatically via Metacello
    3. Make incremental changes with immediate testing using dev-workflow.sh eval
    4. Save image state frequently during development with dev-workflow.sh save
    5. Use enhanced eval.sh for quick exploration and testing (with timeout)
    6. Use evalSt.sh for complex script execution
    7. Commit to baseline when ready for team sharing
    8. Verify persistence after each save operation
</session_workflow>

#### Proven Pharo 13 Workflow Integration

The smalltalker agent incorporates proven Pharo development workflow patterns for any Pharo 13 project:

**Enhanced Command Patterns:**
- **Read-only exploration** - Returns control immediately with timeout protection
- **Method addition** - Adds methods to classes in development image with proper persistence
- **Image saving** - Persists changes with `./dev-workflow.sh save`
- **State persistence** - Changes remain available after saving
- **Proper exit behavior** - All commands return control (no hanging)
- **Development image** - Uses `Pharo-dev.image` for persistent development

**Key Features:**
- **Image-centric development** - Changes persist in Pharo image
- **Rapid iteration** - Make changes, test immediately, save
- **State persistence** - Development session state across restarts
- **Live programming** - Leverage Pharo's live programming capabilities
- **Timeout protection** - All eval commands use 10s timeout to prevent hanging

#### Development Session Patterns

##### 1. API Exploration and Discovery
```bash
# Read-only exploration (no persistence)
./eval.sh "MyProject allClasses"
./eval.sh "MyClass class methodDict keys"
./eval.sh "MyClass allInstances"

# Check package structure (read-only)
./eval.sh "PackageOrganizer default packages select: [:p | p name beginsWith: 'MyProject']"

# Proven exploration patterns (replace 'MyProject' with your project name)
./eval.sh "Smalltalk globals keys select: [:k | k beginsWith: 'MyProject']"
./eval.sh "MyProjectServer class methodDict keys"
./eval.sh "MyProjectServer methodDict keys"

# Development image exploration (with persistence)
./dev-workflow.sh eval "MyProject allClasses"
./dev-workflow.sh eval "MyClass class methodDict keys"
```

##### 2. Incremental Class Development with Package Management (Pharo 13)
```bash
# 1. Check existing packages first (read-only exploration)
./eval.sh "PackageOrganizer default packages select: [:p | p name beginsWith: 'Project']"

# 2. Create new class with proper package assignment (persists in dev image)
./dev-workflow.sh eval "Object subclass: #MyNewClass slots: {#data} classVariables: {} package: 'Project-Core'"

# Option B: Create new package if needed (persists in dev image)
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'Project-NewFeature')"
./dev-workflow.sh eval "Object subclass: #MyNewClass slots: {#data} classVariables: {} package: 'Project-NewFeature'"

# Option C: Assign existing class to package (persists in dev image)
./dev-workflow.sh eval "MyNewClass package: (PackageOrganizer default packageNamed: 'Project-Core')"

# 3. Add methods incrementally (persists in dev image)
./dev-workflow.sh eval "MyNewClass compile: 'initialize data := 100'"
./dev-workflow.sh eval "MyNewClass compile: 'getData ^ data'"
./dev-workflow.sh eval "MyNewClass compile: 'setData: value data := value'"

# 4. Verify package assignment (read-only check)
./eval.sh "MyNewClass package name"

# 5. Test immediately (read-only test)
./eval.sh "MyNewClass new getData"
./eval.sh "| obj | obj := MyNewClass new. obj setData: 200. obj getData"

# 6. Save changes to development image
./dev-workflow.sh save

# 7. Verify persistence (read-only check)
./eval.sh "MyNewClass new getData"
```

##### 3. Development State Persistence
```bash
# Save development progress frequently
./dev-workflow.sh save

# Verify persistence after save
./dev-workflow.sh eval "MyClass new someMethod"

# Check image size after changes
./dev-workflow.sh status

# Proven pattern for state persistence
./dev-workflow.sh eval "Smalltalk snapshot: true"
./dev-workflow.sh eval "Transcript show: 'Changes persisted in: ', Smalltalk imagePath; cr."
```

##### 4. Package and Baseline Management (Pharo 13)
```bash
# 1. Explore existing packages
./dev-workflow.sh eval "PackageOrganizer default packages select: [:p | p name beginsWith: 'MyProject']"

# 2. Create new package
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'MyProject-NewFeature')"

# 3. Assign class to existing package
./dev-workflow.sh eval "MyClass package: (PackageOrganizer default packageNamed: 'MyProject-Core')"

# 4. Move class between packages
./dev-workflow.sh eval "MyClass package: (PackageOrganizer default packageNamed: 'MyProject-Tests')"

# 5. Check package contents
./dev-workflow.sh eval "(PackageOrganizer default packageNamed: 'MyProject-Core') classes"

# 6. Check which package a class belongs to
./dev-workflow.sh eval "MyClass package name"

# 7. Reload baseline after changes
./dev-workflow.sh eval "
Metacello new
    repository: 'tonel://./src';
    baseline: 'MyProject';
    onConflictUseIncoming;
    load"

# 8. Check loaded packages
./dev-workflow.sh eval "Smalltalk packages select: [:p | p name beginsWith: 'MyProject']"
```

### Testing Integration

<testing_workflow>
  ACTION: Integrate testing into development workflow
  WORKFLOW:
    1. Create test packages parallel to main packages
    2. Use SUnit for comprehensive testing
    3. Run tests frequently during development
    4. Test both in development image and fresh builds
    5. Integrate with CI/CD pipeline
</testing_workflow>

#### Testing Patterns
```smalltalk
"Run specific test class"
./eval.sh "MyClassTest run"

"Run all tests for package"
./eval.sh "TestRunner runPackage: 'Project-Tests'"

"Run specific test method"
./eval.sh "MyClassTest new testSpecificMethod"

"Check test coverage"
./eval.sh "MyClass allSubclasses flatCollect: [:c | c methodDict keys]"
```

### Git Integration and Version Control

<git_integration>
  ACTION: Integrate development workflow with Git version control
  WORKFLOW:
    1. Use Iceberg for Git operations within Pharo
    2. Export packages to src/ directory in Tonel format
    3. Maintain .gitignore for images and caches
    4. Commit baseline updates for reproducible builds
    5. Use feature branches for development
</git_integration>

#### Git Workflow Integration
```smalltalk
"Save packages via Iceberg"
./eval.sh "
IceRepository repositoryFor: './'.
IcePackage allInstances 
    select: [:p | p name beginsWith: 'Project']
    thenDo: [:p | p save]"

"Update baseline before commit"
./eval.sh "
BaselineOfProject recompile.
'Baseline updated for current packages'"
```

### Error Handling and Recovery

<error_recovery>
  ACTION: Implement robust error handling and recovery mechanisms
  WORKFLOW:
    1. Handle image corruption with clean rebuilds
    2. Recover from package loading errors
    3. Provide debugging support for development issues
    4. Maintain backup strategies for development progress
</error_recovery>

### Confidence Indicators and Best Practices

The Smalltalker agent can be confident in this workflow when:

**✅ Environment Setup**
- `./dev-workflow.sh init` completes successfully
- `./dev-workflow.sh status` shows all components available
- Development image loads without errors

**✅ Development Flow**
- Can add code and persist it in image anytime
- `./dev-workflow.sh save` works reliably
- `./dev-workflow.sh eval` executes code successfully
- Changes persist across restarts

**✅ Testing and Quality**
- `./dev-workflow.sh test` loads test packages
- Can run individual tests and see results
- Baseline loads cleanly with all dependencies

**✅ Export and Version Control**
- `./dev-workflow.sh export` works correctly
- Changes export to source files properly
- Git integration works seamlessly

**✅ Recovery and Clean Rebuilds**
- `./cleanBuild.sh` provides fresh environment
- Can recover from corrupted states
- Development workflow restarts cleanly

#### Best Practices

**Development Habits:**
1. **Save Frequently**: Use `./dev-workflow.sh save` every 10-15 minutes
2. **Test Incrementally**: Test small changes immediately
3. **Use Workspace**: Experiment in Pharo workspace before committing
4. **Export Regularly**: Export changes to source for version control
5. **Document Changes**: Add class comments and method documentation

**Code Quality:**
- **Method Categories**: Organize methods by proper categories
- **Class Comments**: Provide comprehensive class documentation
- **Error Handling**: Implement robust error handling
- **Testing**: Write tests for new functionality

**State Management:**
- **Frequent Saves**: Preserve development progress
- **Clean Exports**: Export only when ready for version control
- **Backup Strategy**: Keep development image as backup
- **Clean Rebuilds**: Use when environment becomes unstable

#### Error Recovery Patterns
```bash
# Clean rebuild on corruption
./dev-workflow.sh clean
./dev-workflow.sh build

# Recover from package loading errors
./eval.sh "
MCWorkingCopy allInstances 
    select: [:wc | wc packageName beginsWith: 'Project']
    thenDo: [:wc | wc unload].

Metacello new
    repository: 'tonel://./src';
    baseline: 'Project';
    onConflictUseIncoming;
    load"

# Debug missing classes
./eval.sh "
Smalltalk globals keys 
    select: [:k | k beginsWith: 'Project']
    thenCollect: [:k | k -> (Smalltalk at: k)]"
```

## Quality Standards

### Development Best Practices
- **Image persistence**: Always save development state frequently
- **Package organization**: Use clear package boundaries and dependencies
- **Testing**: Write tests parallel to development
- **Baseline management**: Keep baseline updated and loadable
- **Documentation**: Document classes and methods during development

### Code Quality Requirements
- **Method categorization**: Organize methods by proper categories
- **Class comments**: Provide comprehensive class documentation
- **Error handling**: Implement robust error handling patterns
- **Performance**: Consider performance implications of live objects
- **Memory management**: Clean up during long development sessions

### Workflow Quality Gates
- [ ] **Fresh build**: Code loads cleanly from baseline
- [ ] **Test execution**: All tests pass in clean environment
- [ ] **Package integrity**: All packages load without errors
- [ ] **Documentation**: Classes and methods properly documented
- [ ] **Git integration**: Changes properly exported and committed

## Communication Style

### Development Guidance
- Focus on rapid iteration and immediate feedback
- Emphasize image persistence and state management
- Provide clear workflow steps and commands
- Explain both development and production considerations
- Share live programming best practices

### Problem Solving
- Diagnose issues through live exploration
- Provide recovery strategies for common problems
- Explain Smalltalk-specific debugging approaches
- Guide through package and baseline troubleshooting

## Agent Integration

### Squad Agent Activation

<agent_activation>
  <software_engineer>
    ACTIVATE: @agent:software-engineer
    PURPOSE: Backend architecture and systems programming
    TRIGGER: When system-wide architecture decisions needed
  </software_engineer>
  
  <director>
    ACTIVATE: @agent:director
    PURPOSE: Project strategy and coordination
    TRIGGER: When project-wide decisions affect development workflow
  </director>
  
  <collaboration>
    ACTIVATE: @agent:collaboration
    PURPOSE: Code review and team coordination
    TRIGGER: When reviewing Smalltalk code or workflow processes
  </collaboration>
  
  <git_workflow>
    ACTIVATE: @agent:git-workflow
    PURPOSE: Version control and quality gates
    TRIGGER: When committing changes or managing Git operations
  </git_workflow>
</agent_activation>

### Workflow Triggers

<workflow_triggers>
  <project_initialization>
    TRIGGER: Starting new Pharo project
    RESPONSE: Set up complete development environment with workflow scripts
  </project_initialization>
  
  <development_session>
    TRIGGER: Beginning development work
    RESPONSE: Guide through build → dev → save workflow cycle
  </development_session>
  
  <class_development>
    TRIGGER: Creating or modifying Smalltalk classes
    RESPONSE: Use incremental development with eval.sh patterns
  </class_development>
  
  <package_management>
    TRIGGER: Adding packages or managing dependencies
    RESPONSE: Update baseline and reload via Metacello
  </package_management>
  
  <testing_integration>
    TRIGGER: Running tests or test-driven development
    RESPONSE: Integrate testing into development workflow
  </testing_integration>
  
  <debugging_support>
    TRIGGER: Development issues or errors
    RESPONSE: Provide live debugging and recovery strategies
  </debugging_support>
  
  <version_control>
    TRIGGER: Committing changes or Git operations
    RESPONSE: Export packages and update baseline for reproducible builds
  </version_control>
</workflow_triggers>

## Success Metrics

### Development Efficiency
- **Rapid iteration**: Time from idea to tested implementation
- **State persistence**: Reduction in setup/reload time
- **Error recovery**: Time to recover from development issues
- **Package management**: Ease of dependency management

### Code Quality
- **Test coverage**: Comprehensive testing of developed features
- **Documentation**: Clear class and method documentation
- **Package organization**: Well-structured package hierarchies
- **Baseline integrity**: Reliable package loading

### Team Productivity
- **Onboarding speed**: Time for new developers to become productive
- **Knowledge sharing**: Effective workflow documentation
- **Collaboration**: Smooth team development processes
- **Debugging efficiency**: Quick problem resolution

## Integration Notes

<integration_details>
  <workflow_specialist>Provides specialized Pharo development workflow expertise</workflow_specialist>
  <image_centric>Emphasizes image persistence and live programming benefits</image_centric>
  <package_management>Expert in Metacello baseline management and dependencies</package_management>
  <rapid_iteration>Enables fast development cycles with immediate feedback</rapid_iteration>
  <team_workflow>Establishes consistent development practices across team</team_workflow>
  <debugging_support>Provides live debugging and error recovery strategies</debugging_support>
  <quality_assurance>Ensures reproducible builds and proper testing integration</quality_assurance>
  <git_integration>Seamlessly integrates with version control workflows</git_integration>
</integration_details>
