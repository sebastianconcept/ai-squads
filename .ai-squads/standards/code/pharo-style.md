# Pharo Style Guide

## Context

Pharo-specific code style rules for SquadsAI projects using Pharo for object-oriented development. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/rust-and-smalltalk.md` and includes important updates for Pharo 13 compatibility.

**Project-Agnostic Design**: This guide is designed to work with any Pharo 13 project. Examples use `MyProject` as a placeholder and may include STUI-specific examples for illustration purposes. Replace `MyProject` with your actual project name.

## Pharo 13 API Changes and Compatibility

### Critical Pharo 13 Updates

#### Spec Framework Changes (v2.0.3)
- **Breaking Change**: Spec framework updated to version 2.0.3 with API modifications
- **Action Required**: Update UI components to use new Spec API patterns
- **Migration**: Review existing Spec-based interfaces for compatibility

```smalltalk
"NEW Pharo 13 Spec 2.0.3 pattern"
SpApplication new
    run: (MyWindow newApplication: application)

"Instead of deprecated patterns from earlier versions"
```

#### Iceberg Git Integration (v2.4.1)
- **Update**: Iceberg updated to version 2.4.1
- **Improved**: Better Git repository management and performance
- **Action**: Leverage enhanced Iceberg features for version control

#### Tools Framework (v0.10.4)
- **Update**: New tools framework version 0.10.4
- **Enhanced**: Development tool improvements and bug fixes
- **Compatibility**: Review custom tool integrations

#### Trait Initialization Robustness
- **Fix**: Robust handling of `#initialize` message sent to Traits
- **Impact**: Safer trait usage and reduced initialization errors
- **Best Practice**: Avoid sending `#initialize` directly to traits

```smalltalk
"CORRECT: Initialize trait users, not traits themselves"
MyClass new initialize.

"AVOID: Direct trait initialization (now safely handled)"
MyTrait initialize. "This won't cause errors but isn't recommended"
```

#### Roassal Visualization Engine
- **Update**: Latest Roassal version with enhanced visualization capabilities
- **Features**: Improved graphical representations and performance
- **Migration**: Update visualization code to leverage new Roassal features

### Quality Gate Requirements for Pharo 13

#### Pre-commit Checks
- [ ] **Pharo 13 Compatibility**: Ensure code runs on Pharo 13 without deprecation warnings
- [ ] **Spec 2.0.3 Compliance**: Verify UI components use current Spec API
- [ ] **Iceberg Integration**: Test Git operations with Iceberg 2.4.1
- [ ] **Tool Framework**: Validate custom tools work with new framework
- [ ] **Trait Usage**: Verify trait initialization patterns follow best practices

#### Testing Requirements
```smalltalk
"Run comprehensive tests for Pharo 13 compatibility"
TestRunner runTests: 'YourProject-Tests'.

"Verify no deprecation warnings"
DeprecationLog reset.
"Run your code"
DeprecationLog ifNotEmpty: [ self error: 'Deprecation warnings found' ].
```

## Code Formatting

- Use 4 spaces for indentation
- One statement per line
- Use blank lines to separate logical sections
- Keep lines under 80 characters when possible
- Use consistent spacing around operators and keywords
- **Pre-commit Requirement**: Ensure code follows Pharo formatting conventions

## Naming Conventions

- **Classes**: `PascalCase` (e.g., `BankAccount`, `UserManager`)
- **Methods**: `camelCase` (e.g., `calculateInterest`, `validateInput`)
- **Variables**: `camelCase` (e.g., `accountBalance`, `userName`)
- **Constants**: `SCREAMING_SNAKE_CASE` (e.g., `MAX_RETRY_COUNT`)
- **Private methods**: Start with lowercase (e.g., `privateHelper`)
- **Public methods**: Start with lowercase (e.g., `publicMethod`)

## Method Definitions

- Place opening bracket on the same line as method signature
- Use descriptive method names that indicate their purpose
- Group related methods together in the class
- Use proper method categorization (instance methods, class methods)
- Do not use abbreviations when naming

```smalltalk
calculateInterest: principal rate: rate years: years
    "Calculate compound interest for given principal, rate, and years"
    | compoundRate |
    compoundRate := (1 + rate) raisedTo: years.
    ^principal * (compoundRate - 1)
```

## Class Structure

- Organize methods by category (accessor, mutator, testing, etc.)
- Use proper class comments to describe purpose
- Preserve good Separation of Concerns for flexibility
- Follow Pharo 13 best practices for class organization

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'balance accountNumber owner'
    classVariableNames: 'NextAccountNumber'
    poolDictionaries: 'BANK_CODES'
    category: 'Banking'

"BankAccount represents a bank account with basic operations.
Compatible with Pharo 13 and follows current best practices."
```

## Pharo 13 Specific UI Development

### Spec 2.0.3 Components

```smalltalk
"Modern Spec 2.0.3 application structure"
SpApplication subclass: #MyApplication
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyApp-Application'

initialize
    super initialize.
    self useBackend: #Morphic.
```

### Window Management

```smalltalk
"Improved window management in Pharo 13"
openWindow
    | window |
    window := self newWindow.
    window
        title: 'My Application';
        initialExtent: 400@300;
        centerOnScreen;
        openWithSpec.
    ^window
```

## Error Handling and Robustness

### Exception Handling
```smalltalk
"Robust exception handling for Pharo 13"
performOperation
    [ self doSomething ]
        on: Error
        do: [ :error |
            self logError: error.
            self notifyUser: 'Operation failed: ', error messageText ]
```

### Trait Usage Best Practices
```smalltalk
"Proper trait composition in Pharo 13"
Object subclass: #MyClass
    uses: TMyTrait + TAnotherTrait
    instanceVariableNames: 'data'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyApp-Core'

"Initialize the class instance, not the traits"
initialize
    super initialize.
    self initializeTraitBehavior.
```

## Testing Framework

### SUnit Testing
```smalltalk
"Modern SUnit testing patterns for Pharo 13"
TestCase subclass: #MyClassTest
    instanceVariableNames: 'instance'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyApp-Tests'

setUp
    super setUp.
    instance := MyClass new.

testBasicFunctionality
    | result |
    result := instance performSomeOperation.
    self assert: result isNotNil.
    self assert: result > 0.
```

## Package Management

### Metacello and Baseline
```smalltalk
"Pharo 13 compatible baseline structure"
BaselineOf subclass: #BaselineOfMyProject
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'BaselineOfMyProject'

baseline: spec
    <baseline>
    spec for: #common do: [
        self declarePackagesOn: spec.
        self declareGroupsOn: spec ].
```

## Performance Considerations

### Memory Management
```smalltalk
"Efficient memory usage patterns for Pharo 13"
processLargeDataSet: dataSet
    "Process data in chunks to manage memory efficiently"
    dataSet groupsOf: 1000 do: [ :chunk |
        self processChunk: chunk.
        Smalltalk garbageCollect ].
```

### Collection Optimization
```smalltalk
"Use appropriate collection types for Pharo 13"
buildOptimizedCollection
    | result |
    result := OrderedCollection new: 1000. "Pre-size for performance"
    data do: [ :item |
        result add: (self transformItem: item) ].
    ^result asArray "Convert to array if read-only access needed"
```

## Documentation Standards

### Class Comments
```smalltalk
"Comprehensive class documentation for Pharo 13"
"BankAccount represents a financial account with standard banking operations.

This class is compatible with Pharo 13 and follows current Smalltalk best practices.
It uses modern Spec 2.0.3 components for UI interactions when needed.

Key responsibilities:
- Maintain account balance and transaction history
- Validate all banking operations
- Provide secure access to account information

Example usage:
  account := BankAccount new
    accountNumber: '12345';
    owner: 'John Doe';
    initialBalance: 1000;
    yourself.
  account deposit: 500.
  account withdraw: 200.

See also: Transaction, BankingValidation"
```

## Integration with Modern Tools

### Iceberg Git Workflow
```smalltalk
"Working with Iceberg 2.4.1 in Pharo 13"
"1. Use Iceberg for all Git operations"
"2. Create feature branches for development"
"3. Use proper commit messages"
"4. Leverage improved repository management"
```

### Roassal Visualization
```smalltalk
"Updated Roassal patterns for Pharo 13"
createVisualization
    | canvas |
    canvas := RSCanvas new.
    data do: [ :item |
        canvas add: (self createShapeFor: item) ].
    canvas @ RSCanvasController.
    ^canvas
```

## Quality Assurance

### Code Review Checklist
- [ ] Pharo 13 compatibility verified
- [ ] No deprecation warnings
- [ ] Spec 2.0.3 API compliance
- [ ] Proper trait usage patterns
- [ ] Comprehensive test coverage
- [ ] Clear documentation
- [ ] Performance considerations addressed
- [ ] Error handling implemented
- [ ] Git integration through Iceberg 2.4.1

### Continuous Integration
```smalltalk
"CI/CD considerations for Pharo 13 projects"
"- Ensure all tests pass on Pharo 13"
"- Verify no deprecation warnings"
"- Check Spec UI components load correctly"
"- Validate Iceberg Git operations"
"- Test visualization components with new Roassal"
```

## Migration Guidelines

### Upgrading to Pharo 13
1. **Update Spec Components**: Migrate to Spec 2.0.3 API
2. **Test Trait Usage**: Verify trait initialization patterns
3. **Update Tools**: Adapt to new tools framework 0.10.4
4. **Validate Visualizations**: Update Roassal-based components
5. **Git Integration**: Leverage Iceberg 2.4.1 improvements

### Deprecation Handling
```smalltalk
"Handle deprecations gracefully"
handleDeprecatedAPI
    "Use modern API when available"
    Smalltalk version >= 13
        ifTrue: [ self useModernAPI ]
        ifFalse: [ self useCompatibilityAPI ]
```

## Proven Pharo 13 Development Workflow Integration

### Enhanced Development Workflow Commands

Based on proven Pharo development workflow patterns, we've integrated successful patterns into our standard Pharo 13 development approach for any project:

#### **Read-Only Exploration (Returns Control Properly)**

```bash
# Quick evaluation with base image
./eval.sh "3 + 4"
./eval.sh "Transcript show: 'Hello from Pharo!'; cr. 3 + 4"

# Development environment evaluation
./dev-workflow.sh eval "3 + 4"
./dev-workflow.sh eval "Transcript show: 'Testing development workflow'; cr. 3 + 4"
```

#### **Adding Code and Saving Image (Persistent Development)**

```bash
# Create a class
./dev-workflow.sh eval "Object subclass: #MyClass slots: {#data} classVariables: {} package: 'MyProject-Core'"

# Add methods to the class
./dev-workflow.sh eval "MyClass compile: 'initialize data := 100'"
./dev-workflow.sh eval "MyClass compile: 'getData ^ data'"

# Test the methods
./dev-workflow.sh eval "MyClass new getData"

# Save to development image (persists changes)
./dev-workflow.sh save

# Verify persistence (changes remain after save)
./dev-workflow.sh eval "MyClass new getData"
```

### Enhanced Development Workflow Scripts

#### **dev-workflow.sh - Enhanced Development Orchestration**

```bash
#!/bin/bash
# Enhanced development workflow with proven Pharo patterns
# Based on comprehensive Pharo development workflow

# Commands:
#   init        - Initialize development environment
#   start       - Start development session (opens Pharo IDE)
#   save        - Save current development state
#   eval <code> - Evaluate Smalltalk code
#   eval-script <file> - Evaluate Smalltalk script file
#   test        - Run all tests
#   export      - Export packages to source (for Git)
#   clean       - Clean development environment
#   status      - Show development environment status
#   help        - Show help message

# Features:
#   - Colored output with status indicators
#   - Error handling and validation
#   - Persistent development image management
#   - Environment status checking
#   - Comprehensive help system
```

#### **eval.sh - Read Only Evaluation (Headless, No Persistence)**

```bash
#!/bin/bash
# Read-only evaluation (headless mode, does NOT save changes)

smalltalk_snippet=$1
./images/pharo --headless ./images/Pharo.image eval "$smalltalk_snippet"
```

**Important**: `eval.sh` is for **read-only evaluation** and does NOT save changes to the image. For persistent changes, use `./dev-workflow.sh eval` or the `evalAndSave` pattern below.

#### **evalAndSave Pattern (Persistent Snippet Evaluation)**
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

### Proven Command Patterns

#### **Class Creation and Method Addition with Proper Package Management (Pharo 13)**

```bash
# 1. Check existing packages first
./dev-workflow.sh eval "PackageOrganizer default packages select: [:p | p name beginsWith: 'MyProject']"

# 2. Create class with proper package assignment
# Option A: Assign to existing package (Pharo 13 syntax)
./dev-workflow.sh eval "Object subclass: #DemoClass slots: {#data} classVariables: {} package: 'MyProject-Core'"

# Option B: Create new package if needed
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'MyProject-NewFeature')"
./dev-workflow.sh eval "Object subclass: #DemoClass slots: {#data} classVariables: {} package: 'MyProject-NewFeature'"

# Option C: Assign existing class to package
./dev-workflow.sh eval "DemoClass package: (PackageOrganizer default packageNamed: 'MyProject-Core')"

# 3. Add methods to the class
./dev-workflow.sh eval "DemoClass compile: 'initialize data := 100'"
./dev-workflow.sh eval "DemoClass compile: 'getData ^ data'"
./dev-workflow.sh eval "DemoClass compile: 'setData: value data := value'"

# 4. Verify package assignment
./dev-workflow.sh eval "DemoClass package name"

# 5. Test methods
./dev-workflow.sh eval "DemoClass new getData"
./dev-workflow.sh eval "| obj | obj := DemoClass new. obj setData: 200. obj getData"

# 6. Save changes
./dev-workflow.sh save
```

#### **Package Management and Organization (Pharo 13)**

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

# 6. Remove package (if empty)
./dev-workflow.sh eval "PackageOrganizer default removePackage: (PackageOrganizer default packageNamed: 'MyProject-EmptyPackage')"
```

#### **API Exploration and Discovery**

```bash
# Explore existing classes
./dev-workflow.sh eval "Smalltalk globals keys select: [:k | k beginsWith: 'MyProject']"

# Check class methods
./dev-workflow.sh eval "MyProjectServer class methodDict keys"

# Check instance methods
./dev-workflow.sh eval "MyProjectServer methodDict keys"

# Check package structure
./dev-workflow.sh eval "PackageOrganizer default packages select: [:p | p name beginsWith: 'MyProject']"

# Check which package a class belongs to
./dev-workflow.sh eval "MyProjectServer package name"
```

#### **Development State Persistence**

```bash
# Save development progress frequently
./dev-workflow.sh save

# Verify persistence after save
./dev-workflow.sh eval "MyClass new someMethod"

# Check image size after changes
./dev-workflow.sh status
```

### Key Features from Proven Pharo Workflow

#### **✅ Verified Working Commands**

- **Read-only exploration** - Returns control immediately
- **Method addition** - Adds methods to classes in development image
- **Image saving** - Persists changes with `./dev-workflow.sh save`
- **State persistence** - Changes remain available after saving
- **Proper exit behavior** - All commands return control (no hanging)
- **Development image** - Uses `Pharo-dev.image` for persistent development

#### **✅ What Gets Saved**

- New classes created
- Methods added to classes
- Instance variables
- Package assignments
- All development state

#### **✅ Workflow Benefits**

- **Persistent State** - All development work persists in the Pharo image
- **Live Programming** - Make changes and see immediate results
- **Rapid Iteration** - No compilation delays or restart requirements
- **State Preservation** - Development session state survives across restarts
- **Programmatic Changes** - Code changes made programmatically, not via source files
- **Manual Version Control** - Export to source files only when ready for review
- **Clean Rebuild Capability** - Always preserve ability to build from last good commit

### Troubleshooting Enhanced Workflow

#### **Common Issues and Solutions**

1. **Command hangs and doesn't return control**
   - Solution: Use the updated scripts with temporary files and timeout
   - All commands now use `timeout 10s` and proper exit handling

2. **Changes not persisting after save**
   - Solution: Use `./dev-workflow.sh save` (not just eval commands)
   - Verify with `./dev-workflow.sh eval "MyClass new someMethod"`

3. **Development image not found**
   - Solution: Run `./dev-workflow.sh build` to create development image

4. **Package loading errors**
   - Solution: Use `./dev-workflow.sh clean` then `./dev-workflow.sh build`

### Package Naming Conventions and Organization

#### **Standard Package Structure**
```bash
# Core packages (main functionality)
Project-Core          # Main application logic
Project-Model         # Domain models and business logic
Project-UI            # User interface components
Project-Network       # Network and communication

# Extension packages (optional features)
Project-Extensions    # Optional extensions and plugins
Project-Integrations  # Third-party integrations

# Test packages (parallel to main packages)
Project-Core-Tests    # Tests for Project-Core
Project-Model-Tests   # Tests for Project-Model
Project-UI-Tests      # Tests for Project-UI

# Development packages
Project-Dev           # Development tools and utilities
Project-Debug         # Debugging and profiling tools
```

#### **Package Assignment Guidelines (Pharo 13)**
```bash
# 1. Always assign classes to packages during creation
./dev-workflow.sh eval "Object subclass: #MyClass slots: {#data} classVariables: {} package: 'MyProject-Core'"

# 2. Use existing packages when possible
./dev-workflow.sh eval "MyClass package: (PackageOrganizer default packageNamed: 'MyProject-Core')"

# 3. Create new packages only for distinct features
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'MyProject-NewFeature')"

# 4. Verify package assignment immediately
./dev-workflow.sh eval "MyClass package name"

# 5. Move classes to appropriate packages as needed
./dev-workflow.sh eval "MyClass package: (PackageOrganizer default packageNamed: 'MyProject-Tests')"
```

### Project Adaptation Guide

**To use these patterns with your own project, replace `MyProject` with your project name:**

```bash
# Replace 'MyProject' with your actual project name
# Examples: 'STUI', 'MyApp', 'WebServer', 'DataProcessor', etc.

# Package exploration
./dev-workflow.sh eval "PackageOrganizer default packages select: [:p | p name beginsWith: 'YourProject']"

# Class creation
./dev-workflow.sh eval "Object subclass: #YourClass slots: {#data} classVariables: {} package: 'YourProject-Core'"

# Package creation
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'YourProject-NewFeature')"

# Baseline loading
./dev-workflow.sh eval "
Metacello new
    repository: 'tonel://./src';
    baseline: 'YourProject';
    onConflictUseIncoming;
    load"
```

**Note**: All examples in this guide use `MyProject` as a placeholder. The STUI examples shown are for illustration purposes only - replace with your actual project name.

### Headless Operation Requirements

**The smalltalker agent requires headless operation for reliable automation:**

```bash
# ✅ CORRECT: Headless mode for automation
PHARO_VM="images/pharo --headless"

# ❌ INCORRECT: GUI mode (not suitable for automation)
PHARO_VM="images/pharo"
```

**Benefits of Headless Operation:**
- **Immediate Return Control** - Commands return control immediately
- **No UI Dependencies** - Works in server environments without display
- **Automation Friendly** - Suitable for CI/CD pipelines
- **Consistent Behavior** - Same behavior across different environments
- **Resource Efficient** - No GUI overhead in automated workflows

### Evaluation Strategy: Quick vs Development

**Two distinct evaluation approaches for different purposes:**

#### **Read-Only Evaluation (`./eval.sh`)**
```bash
# ✅ Read-only exploration and testing (no persistence)
./eval.sh "3 + 4"
./eval.sh "MyProjectServer allInstances"
./eval.sh "PackageOrganizer default packages"
```

**Characteristics:**
- **No Persistence** - Changes are NOT saved to the image
- **Read-Only Operations** - Perfect for exploration and testing
- **Fast Execution** - Quick feedback without overhead
- **Safe** - Cannot accidentally modify the development state

#### **Development Evaluation (`./dev-workflow.sh eval`)**
```bash
# ✅ Development operations (with persistence)
./dev-workflow.sh eval "MyProjectServer compile: 'newMethod ^ 42'"
./dev-workflow.sh eval "Object subclass: #MyNewClass slots: {#data} classVariables: {} package: 'MyProject-Core'"
./dev-workflow.sh eval "PackageOrganizer default addPackage: (Package new name: 'MyProject-NewFeature')"
```

**Characteristics:**
- **With Persistence** - Changes ARE saved to the development image
- **Development Operations** - Creating classes, adding methods, modifying packages
- **State Management** - Requires explicit save with `./dev-workflow.sh save`
- **Development Workflow** - Part of the build → dev → save → test → export cycle

### Enhanced Workflow Patterns (Headless Mode)

**Daily Development Workflow:**
```bash
# 1. Initialize development environment (one-time setup)
./dev-workflow.sh init

# 2. Start development session (headless for automation)
./dev-workflow.sh start

# 3. Make changes programmatically
./dev-workflow.sh eval "MyProjectServer compile: 'newMethod ^ 42'"

# 4. Save progress frequently (every 10-15 minutes)
./dev-workflow.sh save

# 5. Read-only testing (headless, no persistence)
./eval.sh "MyProjectServer new newMethod"

# 6. Run tests (headless)
./dev-workflow.sh test

# 7. Export to source (for manual review)
./dev-workflow.sh export

# 8. Review changes manually
git status
git diff

# 9. Commit changes after review
git add src/
git commit -m "feat: add new functionality"
```

**Key Workflow Insights:**
- **Programmatic Changes**: All code changes made programmatically in the live image (preferred over source file editing)
- **Frequent Saves**: Save development state every 10-15 minutes
- **Manual Version Control**: Export to source files only when ready for review
- **Clean Rebuilds**: Always preserve ability to build from last good commit
- **State Persistence**: Development session state survives across restarts

**Advanced Development Patterns:**
```bash
# Incremental class development
./dev-workflow.sh eval "
Object subclass: #MyNewClass
    slots: {#data}
    classVariables: {}
    package: 'MyProject-Core'"

./dev-workflow.sh eval "
MyNewClass compile: 'initialize
    super initialize.
    data := Dictionary new'"

./dev-workflow.sh eval "MyNewClass new class name"

# Package management
./dev-workflow.sh eval "
Metacello new
    repository: 'tonel://./src';
    baseline: 'MyProject';
    onConflictUseIncoming;
    load"

# Debugging and exploration
./dev-workflow.sh eval "MyProjectServer allMethods"
./dev-workflow.sh eval "MyProjectServer allInstances"
```

### Integration with Smalltalker Agent

This enhanced workflow is integrated into the Smalltalker agent's standard development flow for any Pharo 13 project:

- **Project initialization** - Sets up complete development environment with proven scripts
- **Development sessions** - Guides through init → start → save → test → export workflow cycle
- **Class development** - Uses incremental development with eval patterns and proper exit handling
- **Package management** - Updates baseline and reloads via Metacello with proper package assignment
- **Testing integration** - Integrates testing into development workflow
- **Debugging support** - Provides live debugging and recovery strategies
- **Version control** - Exports packages and updates baseline for reproducible builds
- **Error handling** - Robust error recovery and environment validation
- **Status monitoring** - Environment status checking and validation
- **Headless operation** - All operations use `--headless` mode for reliable automation

### Development Habits and Best Practices

#### **Development Habits**
1. **Save Frequently**: Use `./dev-workflow.sh save` every 10-15 minutes
2. **Test Incrementally**: Test small changes immediately
3. **Use Workspace**: Experiment in Pharo workspace before committing
4. **Export Regularly**: Export changes to source for version control
5. **Document Changes**: Add class comments and method documentation

#### **Code Quality**
- **Method Categories**: Organize methods by proper categories
- **Class Comments**: Provide comprehensive class documentation
- **Error Handling**: Implement robust error handling
- **Testing**: Write tests for new functionality

#### **State Management**
- **Frequent Saves**: Preserve development progress
- **Clean Exports**: Export only when ready for version control
- **Backup Strategy**: Keep development image as backup
- **Clean Rebuilds**: Use when environment becomes unstable

### Success Metrics

- **Rapid iteration** - Time from idea to tested implementation
- **State persistence** - Reduction in setup/reload time
- **Error recovery** - Time to recover from development issues
- **Package management** - Ease of dependency management
- **Team productivity** - Consistent development practices across team

## Incremental Development Workflow

### Modern Pharo Development Setup

```bash
# Project structure for modern Pharo development
project/
├── src/                     # Source packages (Tonel format) - MAIN CODE
│   ├── BaselineOfProject/   # Metacello baseline
│   ├── Project-Core/        # Core functionality packages
│   ├── Project-Network/     # Network layer packages
│   └── Project-Tests/       # Test packages
├── scripts/
│   ├── build.sh            # Fresh image setup with packages loaded
│   ├── builder.st          # Metacello loading script
│   ├── eval.sh             # Quick evaluation (returns immediately)
│   └── evalSt.sh          # Interactive development (keeps image open)
├── images/                 # Pharo images (auto-generated, not versioned)
└── README.md              # Development workflow documentation
```

### Baseline Configuration

```smalltalk
"Modern baseline structure for incremental development"
BaselineOf subclass: #BaselineOfProject
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'BaselineOfProject'

baseline: spec [
    <baseline>
    spec for: #common do: [
        self setUpDependencies: spec.
        self setUpPackages: spec.
        spec
            group: 'Core' with: #('Project-Core');
            group: 'Network' with: #('Project-Network');
            group: 'Tests' with: #('Project-Core-Tests' 'Project-Network-Tests');
            group: 'dev' with: #('Tests');
            group: 'default' with: #('Network')
    ]
]
```

### Development Scripts

#### Build Script (`scripts/build.sh`)
```bash
#!/bin/bash
# Gets a fresh Pharo image and loads project with proper packages

if [ ! -f "images/Pharo.image" ]; then
    echo "Downloading fresh Pharo image and VM..."
    cd images
    curl get.pharo.org | bash 
    cd ..
else
    echo "Pharo image already exists, skipping download..."
fi

echo "Loading project packages..."
./pharo images/Pharo.image st scripts/builder.st
echo "Ready for development!"
```

#### Builder Script (`scripts/builder.st`)
```smalltalk
Metacello new
    repository: 'tonel://./src';
    baseline: 'Project';
    onConflictUseIncoming;
    load.

"Save the loaded state"
Smalltalk snapshot: true andQuit: true.
```

### Interactive Development Commands

#### Quick Evaluation (`eval.sh`)
```bash
# For API exploration and quick tests - returns immediately
./eval.sh "MyClass allInstances"
./eval.sh "MyServer defaultPort"
./eval.sh "MyClass class methods select: [:m | m selector beginsWith: 'create']"
```

#### Interactive Development (`evalSt.sh`)
```bash
# For complex development work - keeps image open for continued interaction
./evalSt.sh development_script.st
# Perfect for developing, testing, iterating, then saving progress
```

### Incremental Development Workflow

#### 1. **API Discovery**
```smalltalk
"Explore existing classes and methods"
./eval.sh "MyProject allClasses select: [:c | c name beginsWith: 'My']"
./eval.sh "MyServer class methodDict keys select: [:k | k beginsWith: 'start']"
```

#### 2. **Develop and Test**
```smalltalk
"Add new methods and test immediately in single evaluation"
./eval.sh "
MyServer class compile: 'defaultPort ^5555'.
MyServer class compile: 'startOnDefaultPort ^self startOnPort: self defaultPort'.
{MyServer defaultPort. MyServer canUnderstand: #startOnDefaultPort}
"
```

#### 3. **Save Progress**
```smalltalk
"Persist changes to image for continued development"
./eval.sh "
MyServer class compile: 'newFeature ^self processData'.
Smalltalk snapshot: true andQuit: false.
'Changes saved to image!'
"
```

#### 4. **Verify Persistence**
```smalltalk
"Test that saved methods are available"
./eval.sh "MyServer defaultPort"
./eval.sh "MyServer class methodDict includesKey: #newFeature"
```

### Package Management with Tonel Format

#### Creating New Classes in Packages
```smalltalk
"Tonel format class definition in src/Project-Core/MyNewClass.class.st"
Class {
    #name : 'MyNewClass',
    #superclass : 'Object',
    #instVars : [
        'instanceVar1',
        'instanceVar2'
    ],
    #category : 'Project-Core',
    #package : 'Project-Core'
}

{ #category : 'initialization' }
MyNewClass >> initialize [
    super initialize.
    instanceVar1 := Dictionary new.
    instanceVar2 := nil
]
```

#### Reloading Packages
```smalltalk
"Reload baseline to pick up new classes"
./eval.sh "
Metacello new
   repository: 'tonel://./src';
   baseline: 'Project';
   onConflictUseIncoming;
   load.
'Baseline reloaded with new classes'
"
```

### Git Integration with Iceberg

#### Using Git Repositories Browser
1. **Open Iceberg**: `World Menu > Tools > Iceberg`
2. **Select repository**: Auto-detects project repository
3. **Review changes**: See modified packages and classes
4. **Commit workflow**:
   - Click "Commit"
   - Review changed packages
   - Write descriptive commit message
   - Click "Commit"
5. **Push changes**: Click "Push" after committing

#### Package Saving Best Practices
```smalltalk
"Iceberg automatically saves packages in Tonel format to src/"
"Changes made in Pharo IDE are reflected in filesystem"
"Use meaningful commit messages when saving through Iceberg"
```

### Development Session Management

#### Session State Preservation
```smalltalk
"Save working state during development"
developmentWorkflow [
    "1. Load fresh image with build.sh"
    "2. Make changes incrementally with eval.sh"
    "3. Save progress: Smalltalk snapshot: true andQuit: false"
    "4. Continue development from saved state"
    "5. Use Iceberg to commit source changes to Git"
]
```

#### Error Recovery
```smalltalk
"Handle development errors gracefully"
./eval.sh "
| result |
[ 
    result := self riskyOperation.
    self logSuccess: result 
] on: Error do: [ :error |
    self logError: error.
    'Error handled: ', error messageText 
]
"
```

### Class Loading Troubleshooting

#### Missing Class Dependencies
```smalltalk
"Check what classes are loaded vs. expected"
./eval.sh "Smalltalk allClasses select: [:c | c name beginsWith: 'MyProject'] thenCollect: #name"

"Identify missing classes causing UndeclaredVariableRead errors"
"Add missing classes to appropriate src/ packages"
"Reload baseline to include new classes"
```

#### Package Verification
```smalltalk
"Verify package contents after baseline reload"
./eval.sh "((Smalltalk packages detect: [:p | p name = 'MyProject-Core']) definedClasses) collect: #name"
```

### Development Environment Tips

#### Image Management
- **Fresh start**: Use `scripts/build.sh` for clean development environment
- **Incremental work**: Use `eval.sh` for quick iterations
- **Complex changes**: Use `evalSt.sh` for interactive sessions
- **State preservation**: Save images with `Smalltalk snapshot: true andQuit: false`

#### Code Organization
- **Source of truth**: `src/` directory contains versioned code
- **Package structure**: Follow dependency hierarchy in baseline
- **File format**: Use Tonel format for Git-friendly diffs
- **Class creation**: Add to appropriate packages, reload baseline

#### Version Control Integration
- **Iceberg workflow**: Use Git Repositories Browser for commits
- **File organization**: Tonel format enables clean Git diffs
- **Commit strategy**: Commit frequently with descriptive messages
- **Branch management**: Use feature branches for development

## Incremental Development Workflow

### Command-Line Development Setup

Pharo excels at incremental, interactive development. Set up your project with proper scripts for seamless workflow:

#### Essential Scripts for Development
```bash
# eval.sh - Quick evaluation with immediate return
./eval.sh "MyProjectServer allInstances"

# evalSt.sh - Interactive session for complex development
./evalSt.sh script.st

# build.sh - Fresh image with baseline loaded
./scripts/build.sh
```

#### Project Structure for Modern Workflow
```
your-project/
├── src/                     # Tonel format source packages
│   ├── BaselineOfProject/   # Metacello baseline
│   ├── Project-Core/        # Core classes
│   └── Project-Tests/       # Test packages
├── scripts/
│   ├── build.sh            # Fresh image setup
│   └── builder.st          # Metacello loading script
├── eval.sh                 # Quick evaluation
├── evalSt.sh              # Interactive development
└── README.md              # Workflow documentation
```

### Creating New Classes and Methods

#### 1. Creating Classes Programmatically
```bash
# Create a new class using modern Pharo syntax
./eval.sh "
Object subclass: #MyNewClass
    slots: {#instanceVar1. #instanceVar2}
    classVariables: {}
    package: 'MyProject-Core'.

MyNewClass name"

# Alternative approach for simple classes
./eval.sh "
Object subclass: #SimpleClass
    instanceVariableNames: 'data status'
    classVariableNames: ''
    package: 'MyProject-Core'.

SimpleClass name"
```

#### 2. Adding Methods to Classes
```bash
# Add instance methods
./eval.sh "
MyNewClass compile: 'initialize
    super initialize.
    instanceVar1 := nil.
    instanceVar2 := OrderedCollection new'.

MyNewClass compile: 'getData
    \"Return the current data\"
    ^ instanceVar1'.

MyNewClass compile: 'setData: newData
    \"Set new data with validation\"
    newData ifNil: [ ^ self error: ''Data cannot be nil'' ].
    instanceVar1 := newData'.

'Methods added successfully'"

# Add class methods
./eval.sh "
MyNewClass class compile: 'defaultInstance
    \"Return a default instance\"
    ^ self new
        setData: ''default'';
        yourself'.

MyNewClass class compile: 'createWithData: data
    \"Factory method to create instance with data\"
    ^ self new
        setData: data;
        yourself'.

'Class methods added successfully'"
```

#### 3. Testing New Classes Immediately
```bash
# Test the new class
./eval.sh "
| instance |
instance := MyNewClass new.
instance setData: 'test data'.
{
    instance getData.
    MyNewClass defaultInstance getData.
    MyNewClass createWithData: 'factory data'
}"
```

#### 4. Creating Classes in Packages
```bash
# Ensure package exists first
./eval.sh "
RPackageOrganizer default 
    packageNamed: 'MyProject-Core' 
    ifAbsent: [ RPackageOrganizer default createPackageNamed: 'MyProject-Core' ].

'Package ready for classes'"

# Then create class in the package
./eval.sh "
Object subclass: #PackagedClass
    slots: {#data}
    classVariables: {}
    package: 'MyProject-Core'"
```

#### 5. Creating Subclasses
```bash
# Create a subclass of existing class
./eval.sh "
MyNewClass subclass: #SpecializedClass
    slots: {#specialFeature}
    classVariables: {}
    package: 'MyProject-Extensions'.

SpecializedClass compile: 'initialize
    super initialize.
    specialFeature := true'.

SpecializedClass compile: 'processSpecially
    specialFeature ifTrue: [ 
        ^ self getData, '' - processed specially'' 
    ] ifFalse: [ 
        ^ super getData 
    ]'.

SpecializedClass new processSpecially"
```

#### 6. Method Categories and Organization
```bash
# Add methods with proper categorization
./eval.sh "
MyNewClass compile: 'getData
    \"Access the stored data\"
    ^ instanceVar1' classified: 'accessing'.

MyNewClass compile: 'setData: newData
    \"Set the stored data\"
    instanceVar1 := newData' classified: 'accessing'.

MyNewClass compile: 'hasData
    \"Test if data is present\"
    ^ instanceVar1 isNotNil' classified: 'testing'.

MyNewClass compile: 'resetInternalState
    \"Reset internal state - private method\"
    instanceVar1 := nil.
    instanceVar2 removeAll' classified: 'private'"
```

#### 7. Common Class Creation Patterns
```bash
# Pattern 1: Data Container Class
./eval.sh "
Object subclass: #DataContainer
    slots: {#data. #timestamp. #metadata}
    classVariables: {}
    package: 'MyProject-Core'.

DataContainer compile: 'initialize
    super initialize.
    data := Dictionary new.
    timestamp := DateAndTime now.
    metadata := Dictionary new'.

DataContainer compile: 'at: key put: value
    data at: key put: value.
    timestamp := DateAndTime now'.

DataContainer compile: 'at: key
    ^ data at: key ifAbsent: [ nil ]'"

# Pattern 2: Service Class with Singleton
./eval.sh "
Object subclass: #MyService
    slots: {#configuration. #isRunning}
    classVariables: {#DefaultInstance}
    package: 'MyProject-Services'.

MyService class compile: 'default
    ^ DefaultInstance ifNil: [ 
        DefaultInstance := self new initialize 
    ]'.

MyService compile: 'initialize
    super initialize.
    configuration := Dictionary new.
    isRunning := false'.

MyService compile: 'start
    isRunning ifFalse: [
        self performStartup.
        isRunning := true 
    ]'"

# Pattern 3: Error Class
./eval.sh "
Error subclass: #MyProjectError
    slots: {#context. #errorCode}
    classVariables: {}
    package: 'MyProject-Errors'.

MyProjectError class compile: 'code: errorCode message: message
    ^ self new
        errorCode: errorCode;
        messageText: message;
        yourself'.

MyProjectError compile: 'errorCode: code
    errorCode := code'.

MyProjectError compile: 'fullDescription
    ^ self messageText, '' (Code: '', errorCode asString, '')'''
```

#### 8. Troubleshooting Class Creation
```bash
# Check if class was created successfully
./eval.sh "
(Smalltalk globals includesKey: #MyNewClass)
    ifTrue: [ 'Class exists: ', MyNewClass name ]
    ifFalse: [ 'Class not found' ]"

# Check class package assignment
./eval.sh "MyNewClass package name"

# Verify class structure
./eval.sh "
{
    'Superclass:' -> MyNewClass superclass name.
    'Instance variables:' -> MyNewClass instVarNames.
    'Class variables:' -> MyNewClass classVarNames.
    'Methods count:' -> MyNewClass methodDict size
}"

# List all methods in the class
./eval.sh "MyNewClass methodDict keys sorted"

# Check method compilation
./eval.sh "
(MyNewClass canUnderstand: #getData)
    ifTrue: [ 'Method getData exists' ]
    ifFalse: [ 'Method getData missing' ]"
```

#### 9. Class Removal and Cleanup
```bash
# Remove a class if needed (be careful!)
./eval.sh "
MyNewClass removeFromSystem.
'Class removed'"

# Remove specific methods
./eval.sh "
MyNewClass removeSelector: #obsoleteMethod.
'Method removed'"

# Check what would be affected before removal
./eval.sh "MyNewClass subclasses"
./eval.sh "MyNewClass allInstances size"
```

### Instant Feedback Development Cycle

#### 1. Quick API Exploration
```bash
# Explore class methods and structure
./eval.sh "MyClass class methodDict keys"
./eval.sh "MyClass allInstVarNames"
./eval.sh "MyClass organization categories"

# Check what's loaded
./eval.sh "Smalltalk allClasses select: [:c | c name beginsWith: 'MyProject']"

# Inspect instances
./eval.sh "MyClass allInstances"
```

#### 2. Method Development and Testing
```bash
# Add methods and test immediately
./eval.sh "
MyClass compile: 'newMethod ^42'.
MyClass new newMethod"

# Test with complex logic
./eval.sh "
MyClass compile: 'calculateSomething: input
    | result |
    result := input * 2 + 10.
    ^ result'.
MyClass new calculateSomething: 5"
```

#### 3. Class Structure Exploration
```smalltalk
"Discover class hierarchy and methods"
MyClass superclass.
MyClass allSubclasses.
MyClass methodDict size.
MyClass selectors sorted.

"Find methods by pattern"
MyClass class methodDict keys select: [:k | k beginsWith: 'default'].

"Explore packages"
MyClass package definedClasses collect: #name.
```

### Code Persistence Strategies

#### 1. Image Snapshots for Development Progress
```bash
# Save current development state
./eval.sh "
'Your development progress here'.
Smalltalk snapshot: true andQuit: false.
'Development state saved'"
```

#### 2. Incremental Method Development
```smalltalk
"Develop methods incrementally"
MyClass compile: 'step1: input
    "First implementation"
    ^ input + 1'.

"Test and refine"
result := MyClass new step1: 5.

"Improve the method"
MyClass compile: 'step1: input
    "Improved implementation with validation"
    input ifNil: [ ^ nil ].
    ^ input + 1'.
```

#### 3. State Preservation During Development
```smalltalk
"Save development context"
| myWorkspace |
myWorkspace := Dictionary new
    at: 'current_objects' put: MyClass allInstances;
    at: 'test_data' put: #(1 2 3 4 5);
    at: 'development_notes' put: 'Working on feature X';
    yourself.

"Later restore context"
testData := myWorkspace at: 'test_data'.
```

### Advanced Exploration Techniques

#### 1. Method Source Investigation
```smalltalk
"Read method source code"
(MyClass >> #methodName) sourceCode.

"Find senders and implementors"
#methodName senders.
#methodName implementors.

"Browse method history"
(MyClass >> #methodName) timeStamp.
```

#### 2. Package and Baseline Management

##### Understanding BaselineOf Structure
```bash
# Check current baseline structure
./eval.sh "BaselineOfSTUI new baseline: (MetacelloBaselineSpec new)"

# Examine existing packages in baseline
./eval.sh "(BaselineOfSTUI >> #baseline:) sourceCode"
```

##### Adding New Package Categories to Baseline
```bash
# 1. First create the new package directory
mkdir -p src/STUI-NewCategory

# 2. Create the package.st file
echo 'Package { #name : "STUI-NewCategory" }' > src/STUI-NewCategory/package.st

# 3. Update the BaselineOfSTUI to include the new package
./eval.sh "
BaselineOfSTUI compile: 'setUpCorePackages: spec
    spec package: ''STUI-Core''.
    spec package: ''STUI-Core-Tests'' with: [ spec requires: ''STUI-Core'' ].

    spec package: ''STUI-ZeroMQ'' with: [ spec requires: ''STUI-Core'' ].
    spec package: ''STUI-Network'' with: [ spec requires: ''STUI-ZeroMQ'' ].
    spec package: ''STUI-Session'' with: [ spec requires: ''STUI-Network'' ].
    spec package: ''STUI-Server'' with: [ spec requires: ''STUI-Session'' ].
    spec package: ''STUI-Server-Tests'' with: [ spec requires: ''STUI-Server'' ].
    
    \"Add your new package here\"
    spec package: ''STUI-NewCategory'' with: [ spec requires: ''STUI-Core'' ].'

'Baseline updated with new package'"

# 4. Update the groups if needed
./eval.sh "
BaselineOfSTUI compile: 'baseline: spec
    <baseline>
    spec for: #common do: [
        self setUpDependencies: spec.
        self setUpPackages: spec.
        spec
            group: ''Core'' with: #( ''STUI-Core'' );
            group: ''Server'' with: #( ''STUI-Server'' );
            group: ''NewFeatures'' with: #( ''STUI-NewCategory'' );
            group: ''Tests''
            with:
                #( ''STUI-Core-Tests'' ''STUI-Server-Tests'' );
            group: ''dev'' with: #( ''Tests'' );
            group: ''default'' with: #( ''Server'' ) ]'

'Baseline groups updated'"
```

##### Reloading Baseline After Changes
```bash
# Reload the entire baseline to pick up new packages
./eval.sh "
Metacello new
   repository: 'tonel://./src';
   baseline: 'STUI';
   onConflictUseIncoming;
   load.

'Baseline reloaded with new packages'"

# Load specific group with new packages
./eval.sh "
Metacello new
   repository: 'tonel://./src';
   baseline: 'STUI';
   onConflictUseIncoming;
   load: #('NewFeatures').

'New features group loaded'"
```

##### Adding Dependencies Between Packages
```bash
# Update baseline to add dependencies
./eval.sh "
BaselineOfSTUI compile: 'setUpCorePackages: spec
    \"Core packages\"
    spec package: ''STUI-Core''.
    spec package: ''STUI-Core-Tests'' with: [ spec requires: ''STUI-Core'' ].

    \"Infrastructure packages\"
    spec package: ''STUI-ZeroMQ'' with: [ spec requires: ''STUI-Core'' ].
    spec package: ''STUI-Network'' with: [ spec requires: ''STUI-ZeroMQ'' ].
    
    \"Business logic packages\"
    spec package: ''STUI-Session'' with: [ spec requires: ''STUI-Network'' ].
    spec package: ''STUI-Server'' with: [ spec requires: ''STUI-Session'' ].
    
    \"Extension packages with multiple dependencies\"
    spec package: ''STUI-Extensions'' with: [ 
        spec requires: #(''STUI-Core'' ''STUI-Server'') 
    ].
    
    \"Test packages\"
    spec package: ''STUI-Server-Tests'' with: [ spec requires: ''STUI-Server'' ].'

'Package dependencies updated'"
```

##### Baseline Development Workflow
```bash
# 1. Create new package structure
./eval.sh "
RPackageOrganizer default 
    packageNamed: 'STUI-NewCategory' 
    ifAbsent: [ RPackageOrganizer default createPackageNamed: 'STUI-NewCategory' ].

'Package created in image'"

# 2. Add classes to the new package
./eval.sh "
Object subclass: #STUINewFeature
    slots: {#data}
    classVariables: {}
    package: 'STUI-NewCategory'.

STUINewFeature compile: 'initialize
    super initialize.
    data := Dictionary new'.

'Class added to new package'"

# 3. Test the new package locally
./eval.sh "
STUINewFeature new class package name = 'STUI-NewCategory'"

# 4. Update baseline to include the package
# (Use the baseline update code from above)

# 5. Test baseline loading
./eval.sh "
Metacello new
   repository: 'tonel://./src';
   baseline: 'STUI';
   onConflictUseIncoming;
   load: #('NewFeatures').

'New package loaded via baseline'"
```

##### Managing External Dependencies
```bash
# Add external dependencies to baseline
./eval.sh "
BaselineOfSTUI compile: 'setUpDependencies: spec
    \"External dependencies\"
    spec
        baseline: ''NeoJSON'' 
        with: [ spec repository: ''github://svenvc/NeoJSON/repository'' ];
        
        baseline: ''Zinc'' 
        with: [ spec repository: ''github://svenvc/zinc/repository'' ].
        
    \"Configuration for older projects\"
    spec
        configuration: ''Seaside3''
        with: [ 
            spec
                repository: ''http://smalltalkhub.com/mc/Seaside/MetacelloConfigurations/main'';
                version: #stable 
        ].'

'External dependencies added'"

# Reference external dependencies in packages
./eval.sh "
BaselineOfSTUI compile: 'setUpCorePackages: spec
    \"... existing packages ...\"
    
    spec package: ''STUI-WebAPI'' with: [ 
        spec requires: #(''STUI-Core'' ''NeoJSON'' ''Zinc'') 
    ].'

'Package with external dependencies added'"
```

##### Baseline Troubleshooting
```bash
# Check what packages are currently loaded
./eval.sh "
(RPackageOrganizer default packages 
    select: [:p | p name beginsWith: 'STUI']) 
    collect: #name"

# Verify baseline structure
./eval.sh "
| spec |
spec := MetacelloBaselineSpec new.
BaselineOfSTUI new baseline: spec.
spec packages collect: [:p | p name -> p requires]"

# Check for baseline compilation errors
./eval.sh "
[ BaselineOfSTUI new baseline: (MetacelloBaselineSpec new) ]
    on: Error 
    do: [ :error | 'Baseline error: ', error messageText ]"

# Reset and reload if needed
./eval.sh "
\"Clear package cache and reload\"
MCWorkingCopy allInstances 
    select: [:wc | wc packageName beginsWith: 'STUI']
    thenDo: [:wc | wc unload].

\"Reload everything\"
Metacello new
   repository: 'tonel://./src';
   baseline: 'STUI';
   onConflictUseIncoming;
   load"
```

##### Package Organization Best Practices
```bash
# Create logical package structure
# Core/
#   STUI-Core              # Essential classes
#   STUI-Core-Tests        # Core functionality tests
# Infrastructure/
#   STUI-ZeroMQ           # ZeroMQ integration
#   STUI-Network          # Network abstraction
# Domain/
#   STUI-Session          # Session management
#   STUI-Server           # Server implementation
# Extensions/
#   STUI-WebAPI           # Web API extensions
#   STUI-Tools            # Development tools
# Tests/
#   STUI-*-Tests          # Test packages for each domain

# Update baseline to reflect this structure
./eval.sh "
BaselineOfSTUI compile: 'setUpPackages: spec
    \"Core packages\"
    self setUpCorePackages: spec.
    
    \"Infrastructure packages\"  
    self setUpInfrastructurePackages: spec.
    
    \"Domain packages\"
    self setUpDomainPackages: spec.
    
    \"Extension packages\"
    self setUpExtensionPackages: spec.
    
    \"Test packages\"
    self setUpTestPackages: spec.'

'Organized baseline structure implemented'"
```

#### 3. Live Debugging and Inspection
```smalltalk
"Insert breakpoints for live debugging"
MyClass compile: 'debugMethod: input
    self halt. "Breakpoint here"
    ^ input processedSomehow'.

"Inspect objects in detail"
myObject inspect.
myObject class browse.
```

### Error Recovery and Problem Solving

#### 1. Handle Missing Classes During Development
```smalltalk
"Check if class exists before using"
Smalltalk globals at: #MyClass ifAbsent: [
    "Class not loaded, handle gracefully"
    ^ 'MyClass not available'
].
```

#### 2. Incremental Class Loading
```smalltalk
"Load classes step by step when baseline fails"
"First create minimal stub"
Object subclass: #MyClass
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyProject-Core'.

"Then add methods incrementally"
MyClass compile: 'initialize
    super initialize.
    "Basic initialization"'.
```

#### 3. Development State Recovery
```bash
# If something breaks, restart from known good state
./scripts/build.sh
# Then restore your development progress incrementally
```

### IDE Integration Best Practices

#### 1. Iceberg Workflow Integration
```smalltalk
"Save packages to Git through Iceberg"
"1. Develop in image using eval scripts"
"2. Save image snapshots for checkpoints"
"3. When ready, save packages via Iceberg"
"4. Use Git Repositories Browser for commits"
```

#### 2. Testing Integration
```smalltalk
"Run tests during development"
TestRunner runSuite: 'MyProject-Tests'.

"Quick test specific methods"
MyClassTest new testSpecificMethod.
```

#### 3. Documentation During Development
```smalltalk
"Document as you develop"
MyClass compile: 'newFeature: input
    "This method implements the new feature X.
    
    Arguments:
      input - The data to process
    
    Returns:
      Processed result
    
    Example:
      MyClass new newFeature: ''test data''
    "
    ^ input processedSomehow'.
```

### Workflow Integration Commands

#### Development Session Workflow
```bash
# 1. Start fresh development session
./scripts/build.sh

# 2. Explore and develop incrementally
./eval.sh "exploration commands here"

# 3. Save progress frequently
./eval.sh "Smalltalk snapshot: true andQuit: false"

# 4. Test complex scenarios
./evalSt.sh complex_development.st

# 5. Final persistence and commit via Iceberg
```

#### Common Development Patterns
```smalltalk
"Pattern 1: Explore-Develop-Test-Save"
"Explore existing code"
ExistingClass methodDict keys.

"Develop new functionality"
ExistingClass compile: 'newMethod ^''hello'''.

"Test immediately"
ExistingClass new newMethod.

"Save progress"
Smalltalk snapshot: true andQuit: false.

"Pattern 2: Incremental Debugging"
"Add debug output"
MyClass compile: 'problematicMethod: input
    Transcript show: ''Input: '', input printString; cr.
    result := self processInput: input.
    Transcript show: ''Result: '', result printString; cr.
    ^ result'.

"Test and observe"
MyClass new problematicMethod: testInput.

"Remove debug code when fixed"
MyClass compile: 'problematicMethod: input
    ^ self processInput: input'.
```

### Performance Tips for Development

#### 1. Efficient Exploration
```smalltalk
"Cache frequently used objects"
myInstances := MyClass allInstances.
myMethods := MyClass methodDict keys sorted.

"Use lazy evaluation for large datasets"
largeResult := [ expensiveComputation ] value.
```

#### 2. Memory Management During Development
```smalltalk
"Clean up during long development sessions"
Smalltalk garbageCollect.
Smalltalk cleanUp: true.

"Reset development state when needed"
MyClass allInstances do: #resetState.
```

## Best Practices Summary

1. **Always target Pharo 13** for new development
2. **Use incremental development** with eval.sh for instant feedback
3. **Save image snapshots frequently** during development sessions
4. **Explore APIs thoroughly** before implementing
5. **Use Spec 2.0.3** for UI components
6. **Leverage Iceberg 2.4.1** for Git operations
7. **Follow trait initialization** best practices
8. **Test incrementally** as you develop
9. **Document during development**, not after
10. **Use modern Roassal** features for visualizations
11. **Handle errors robustly** with proper exception management
12. **Structure projects** with src/ packages and proper baselines
13. **Use Tonel format** for Git-friendly source code management
14. **Adopt explore-develop-test-save cycle** for efficient development
15. **Persist development state** through image snapshots
16. **Make programmatic changes** in the live image
17. **Export to source files** only when ready for review
18. **Preserve clean rebuild capability** from last good commit
19. **Save development state** every 10-15 minutes
20. **Use manual version control** with proper review process

## Git Workflow Integration

### Export and Commit Cycle
```bash
# 1. Make changes programmatically in live image
./dev-workflow.sh eval "MyProjectServer compile: 'newMethod ^ 42'"

# 2. Save progress
./dev-workflow.sh save

# 3. Export to source (for manual review)
./dev-workflow.sh export

# 4. Review changes
git status
git diff

# 5. Commit changes after review
git add src/
git commit -m "feat: add new functionality"

# 6. Push to remote
git push
```

### Preferred Development Approach
**The smalltalker agent prefers programmatic changes in the live image over source file editing:**

#### **✅ Preferred: Programmatic Changes**
```bash
# Make changes programmatically in the live image
./dev-workflow.sh eval "MyProjectServer compile: 'newMethod ^ 42'"
./eval.sh "
    MyProjectServer compile: 'newMethod ^ 42'.
    Smalltalk snapshot: true.
    Transcript show: '✅ Changes evaluated and saved'; cr."
```

#### **❌ Avoid: Direct Source File Editing**
```bash
# Don't edit source files during development
# Source files are for version control, not primary development
```

### Version Control Strategy
- **Source Files**: Used for version control, not primary development
- **Development Image**: Primary development environment with persistent state
- **Manual Review**: Export to source files only when ready for review
- **Clean Rebuilds**: Always preserve ability to build from last good commit
