# STUI Development Workflow for Smalltalker Agent

## Overview

This document provides a comprehensive development workflow for the Smalltalker agent, enabling confident image-centric development with persistent state management and clean rebuild capabilities.

## Core Philosophy

### Image-Centric Development
- **Persistent State**: All development work persists in the Pharo image
- **Live Programming**: Make changes and see immediate results
- **Rapid Iteration**: No compilation delays or restart requirements
- **State Preservation**: Development session state survives across restarts

### Workflow Principles
- **Build → Dev → Save → Test → Export → Commit** cycle
- **Incremental Development**: Small, frequent changes with immediate feedback
- **State Management**: Frequent saves to preserve progress
- **Clean Rebuilds**: Option to start fresh when needed

## Development Environment Setup

### Initial Setup
```bash
# One-time setup - creates development environment
./dev-workflow.sh init
```

This creates:
- `images/Pharo-dev.image` - Your persistent development image
- Loads all STUI packages via Metacello baseline
- Sets up development environment

### Environment Status
```bash
# Check development environment status
./dev-workflow.sh status
```

Shows:
- Pharo VM availability
- Base image availability  
- Development image availability and size

## Proven Development Patterns

### Class Creation (Pharo 13 Builder Pattern)
```bash
# Create class with slots, tag, and package assignment
./dev-workflow.sh evalAndSave "| builder | builder := Object << #MyNewClass slots: { #data . #name }; tag: 'Core'; package: 'STUI-Tools'. builder install."
```

**Key Points:**
- Use `| builder |` to declare the builder variable
- Use `Object << #ClassName` syntax for class creation
- Specify `slots: { #slot1 . #slot2 }` for instance variables
- Use `tag: 'Category'` for method categorization
- Use `package: 'PackageName'` for package assignment
- Always use `evalAndSave` for persistent changes

### Method Addition
```bash
# Add instance method
./dev-workflow.sh evalAndSave "MyNewClass compile: 'initialize data := 100'"

# Add method with parameters
./dev-workflow.sh evalAndSave "MyNewClass compile: 'setData: value data := value'"

# Add method with local variables
./dev-workflow.sh evalAndSave "MyNewClass compile: 'complexMethod | temp | temp := data * 2. ^ temp'"
```

**Key Points:**
- Use `compile:` for method addition
- Methods are automatically categorized based on class tag
- Use `evalAndSave` for persistent changes
- Methods are immediately available after compilation

### Accessor Methods
```bash
# Add getter method
./dev-workflow.sh evalAndSave "MyNewClass compile: 'data ^ data'"

# Add setter method  
./dev-workflow.sh evalAndSave "MyNewClass compile: 'data: value data := value'"
```

### Object Instantiation and Testing
```bash
# Create instance and test methods
./dev-workflow.sh eval "| obj | obj := MyNewClass new. obj initialize. obj data"
```

## Daily Development Workflow

### 1. Start Development Session
```bash
# Start Pharo IDE with development image
./dev-workflow.sh start
```

This opens Pharo IDE with your persistent development image containing all STUI packages.

### 2. Make Changes
- **Add Classes**: Use the builder pattern with `evalAndSave`
- **Add Methods**: Use `compile:` with `evalAndSave`
- **Test Immediately**: Use `eval` for quick testing
- **Live Debugging**: Use Pharo debugger for interactive debugging

### 3. Save Progress Frequently
```bash
# Save current development state
./dev-workflow.sh save
```

**Important**: Save frequently to preserve your work. The development image persists your changes.

### 4. Quick Code Evaluation
```bash
# Evaluate Smalltalk expressions (read-only)
./dev-workflow.sh eval "3 + 4"
./dev-workflow.sh eval "STUIInspector new"
./dev-workflow.sh eval "Smalltalk packages select: [:p | p name beginsWith: 'STUI']"
```

### 5. Persistent Code Evaluation
```bash
# Evaluate and save changes
./dev-workflow.sh evalAndSave "MyClass new someMethod"
```

### 6. Run Scripts
```bash
# Execute Smalltalk script files
./dev-workflow.sh eval-script scripts/archived/test-something.st
```

### 7. Test Your Changes
```bash
# Load and run tests
./dev-workflow.sh test
```

### 8. Export to Source
```bash
# Export packages to source files (for Git)
./dev-workflow.sh export
```

This exports your changes from the image to the `src/` directory in Tonel format.

### 9. Commit Changes
```bash
# Commit exported changes
git add src/
```

## Common Development Patterns

### Creating a Complete Class
```bash
# 1. Create the class
./dev-workflow.sh evalAndSave "| builder | builder := Object << #MyClass slots: { #data . #name }; tag: 'Core'; package: 'STUI-Tools'. builder install."

# 2. Add initialize method
./dev-workflow.sh evalAndSave "MyClass compile: 'initialize data := 100. name := '''"

# 3. Add accessor methods
./dev-workflow.sh evalAndSave "MyClass compile: 'data ^ data'"
./dev-workflow.sh evalAndSave "MyClass compile: 'data: value data := value'"

# 4. Test the class
./dev-workflow.sh eval "| obj | obj := MyClass new. obj initialize. obj data"
```

### Working with Multiple Classes
```bash
# Create manager class
./dev-workflow.sh evalAndSave "| builder | builder := Object << #MyManager slots: { #items }; tag: 'Manager'; package: 'STUI-Tools'. builder install."

# Create item class
./dev-workflow.sh evalAndSave "| builder | builder := Object << #MyItem slots: { #id . #value }; tag: 'Item'; package: 'STUI-Tools'. builder install."

# Add methods to both classes
./dev-workflow.sh evalAndSave "MyManager compile: 'initialize items := OrderedCollection new'"
./dev-workflow.sh evalAndSave "MyItem compile: 'initialize id := UUID new. value := 0'"
```

### Debugging and Exploration
```bash
# Check if class exists
./dev-workflow.sh eval "Smalltalk globals keys select: [:k | k beginsWith: 'MyClass']"

# Check class methods
./dev-workflow.sh eval "MyClass methodDict keys"

# Check instance variables
./dev-workflow.sh eval "MyClass instVarNames"

# Test object creation
./dev-workflow.sh eval "MyClass new"
```

## Best Practices

### Class Creation
- Always use the builder pattern for class creation
- Declare `| builder |` at the start of the snippet
- Use descriptive tags for method categorization
- Assign classes to appropriate packages
- Use meaningful slot names

### Method Addition
- Use `compile:` for method addition
- Keep methods focused and single-purpose
- Use proper Smalltalk naming conventions
- Test methods immediately after creation
- Use `evalAndSave` for persistent changes

### State Management
- Save frequently with `evalAndSave`
- Use `eval` for read-only exploration
- Test object creation and method calls
- Verify changes persist across sessions

### Package Organization
- Assign classes to appropriate packages
- Use consistent package naming (`STUI-Core`, `STUI-Tools`, etc.)
- Keep related classes in the same package
- Use tags for method categorization within packages

## Troubleshooting

### Common Issues
1. **Class not found**: Use `evalAndSave` instead of `eval` for class creation
2. **Method not found**: Check if method was compiled with `evalAndSave`
3. **Package not found**: Ensure package exists in baseline
4. **Changes not persisting**: Use `evalAndSave` for persistent changes

### Recovery Steps
1. Check environment status: `./dev-workflow.sh status`
2. Reinitialize if needed: `./dev-workflow.sh init`
3. Export changes: `./dev-workflow.sh export`
4. Clean rebuild if necessary: `./cleanBuild.sh`
