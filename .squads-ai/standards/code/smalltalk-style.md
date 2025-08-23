# Smalltalk Style Guide

## Code Formatting
- Use 4 spaces for indentation
- One statement per line
- Use blank lines to separate logical sections
- Keep lines under 80 characters when possible
- Use consistent spacing around operators and keywords

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
- Use proper class comments to describe purpose, focus on what is it first and why does it exist second
- Preserve always good Separations of Concerns for flexibility

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'balance accountNumber owner'
    classVariableNames: 'NextAccountNumber'
    poolDictionaries: 'BANK_CODES'
    category: 'Banking'

"BankAccount represents a bank account with basic operations"
```

## Method Implementation
- Use temporary variables for clarity without abbreviations
- Break complex operations into smaller, readable steps
- Use meaningful variable names without abbreviations
- Include proper error handling with custom Error subclasses
- Prefer lazy accessors to call an initializer function of that instVar

```smalltalk
withdraw: amount
    "Withdraw specified amount from account"
    | newBalance |
    (amount <= 0) ifTrue: [ ^ NonPositiveAmount signal: 'Amount must be positive' ].
    (amount > balance) ifTrue: [ ^ InsufficientFunds signal: 'Insufficient funds' ].
    newBalance := balance - amount.
    balance := newBalance.
    self logTransaction: 'Withdrawal' amount: amount.
    ^amount
```

```smalltalk
balance
    "Answer the current balance"
    ^ amount ifNil: [ self initializeBalance ]
```

```smalltalk
initializeBalance
    ^ amount := self getCurrentBalance
```


## Control Structures
- Use proper Smalltalk control structures
- Prefer `ifTrue:ifFalse:` over `ifTrue:` when both branches are needed
- Use `collect:`, `select:`, `detect:` for collections
- Use `do:` for iteration when side effects are needed

```smalltalk
"Good: Using proper Smalltalk control structures"
accounts do: [:account |
    (account balance > 1000) ifTrue: [
        account applyPremiumRate
    ] ifFalse: [
        account applyStandardRate
    ]
]

"Good: Using collection methods"
highValueAccounts := accounts select: [:account | account balance > 1000].
accountNames := accounts collect: [:account | account owner name].
```

## Error Handling
- Use `self error:` for raising errors
- Include descriptive error messages
- Use `halt` for debugging when needed
- Implement proper validation methods

```smalltalk
validateAccountNumber: accountNumber
    "Validate account number format"
    (accountNumber isKindOf: String) ifFalse: [
        ^InvalidAccountNumberFormat signal: 'Account number must be a string'
    ].
    (accountNumber size = 10) ifFalse: [
        ^InvalidAccountNumberLength signal: 'Account number must be 10 digits'
    ].
    ^true
```

## Documentation and Comments
- Use class comments to describe the class purpose
- Use method comments to describe what the method does
- Include examples in comments when helpful
- Document parameters and return values

```smalltalk
"BankAccount class
Represents a bank account with basic operations including
deposits, withdrawals, and balance inquiries.

Instance Variables:
    balance - Current account balance
    accountNumber - Unique account identifier
    owner - Account owner name

Class Variables:
    NextAccountNumber - Counter for generating account numbers"
```

## Testing
- Write unit tests for all public methods
- Use descriptive test method names
- Test both positive and negative cases
- Group related tests together

```smalltalk
testWithdrawValidAmount
    "Test withdrawing a valid amount"
    | account initialBalance |
    account := BankAccount new.
    initialBalance := 1000.
    account deposit: initialBalance.
    account withdraw: 500.
    self assert: account balance equals: 500
```

## Performance Considerations
- Use appropriate collection classes (`Array`, `OrderedCollection`, `Dictionary`)
- Avoid creating unnecessary temporary objects
- Use `at:put:` for array access when performance is critical
- Profile code when performance issues arise

## Debugging and Development
- Use `Transcript show:` for output during development
- Use `self halt` for setting breakpoints
- Implement `printOn:` method for custom object display
- Use workspace for testing and experimentation

## Best Practices
- Follow the principle of least surprise
- Keep methods small and focused
- Use composition over inheritance when appropriate
- Implement proper equality and comparison methods
- Override `hash` when overriding `=` method
- Use proper access control (public, private, protected)

## Project Structure for CI/CD

### Directory Layout
```
YourProject/
├── .smalltalk.ston              # Main SmalltalkCI configuration
├── .smalltalkci/
│   └── .unit-tests.ston         # Test-specific configuration
├── .github/
│   └── workflows/
│       └── build.yml            # GitHub Actions workflow
├── src/                         # Source code (tonel format)
│   ├── BaselineOfYourProject/
│   ├── YourProject-Core/
│   └── YourProject-Tests/
├── build                        # Build script
├── builder.st                   # Smalltalk build script
├── clean                        # Clean script
└── cleanCaches                  # Cache cleanup script
```

### File Naming Conventions
- **`.smalltalk.ston`** - Main project configuration for SmalltalkCI
- **`.smalltalkci/`** - Directory for CI-specific configurations
- **`src/`** - Source code in tonel format (not git repository)
- **`build`** - Executable build script
- **`builder.st`** - Smalltalk code for building
- **`clean*`** - Cleanup scripts for different purposes

### Project Configuration Files

#### `.project` - IDE Configuration
```json
{
    "srcDirectory": "src"
}
```

#### `.filetree` - File Tree Configuration
```json
{
    "packageExtension": ".package",
    "propertyFileExtension": ".json"
}
```

#### `codecov.yml` - Coverage Configuration
```yaml
ignore:
  - "**/Manifest*.st"
  - "**/*ReplicaSet*.st"
```

#### `.gitignore` - Version Control Exclusions
```gitignore
# Pharo/Smalltalk specific
pharo-vm/
pharo
pharo-ui
*.mcz
*.changes
*.image
*.sources
*.dmp
package-cache/
github-cache/
*-cache/

# Project specific
site/_site/
_site

# Compiled source
*.com
*.dll
*.exe
*.o
*.so

# Packages
*.7z
*.dmg
*.gz
*.iso
*.jar
*.rar
*.tar
*.zip

# Logs and databases
*.log
*.sql
*.sqlite

# OS generated files
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db
```

### GitHub Repository Setup

#### Required Directories and Files
```
.github/
├── workflows/
│   └── build.yml              # CI/CD workflow
├── ISSUE_TEMPLATE/            # Issue templates (optional)
└── PULL_REQUEST_TEMPLATE.md   # PR template (optional)
```

#### Repository Settings
- **Actions**: Enable GitHub Actions in repository settings
- **Secrets**: Add `CODECOV_TOKEN` for coverage reporting
- **Branches**: Protect main branch and require status checks
- **Environments**: Configure deployment environments if needed

#### Status Badges for README
```markdown
[![Release](https://img.shields.io/github/v/tag/username/repo?label=release)](https://github.com/username/repo/releases)
[![Unit Tests](https://github.com/username/repo/actions/workflows/build.yml/badge.svg)](https://github.com/username/repo/actions/workflows/build.yml)
[![Coverage Status](https://codecov.io/gh/username/repo/branch/main/graph/badge.svg)](https://codecov.io/gh/username/repo)
![Tests](https://img.shields.io/badge/tests-193-green)

[![Pharo 11](https://img.shields.io/badge/Pharo-11-%23383932.svg)](https://pharo.org/download)
[![Pharo 10](https://img.shields.io/badge/Pharo-10-%23383932.svg)](https://pharo.org/download)

[![License](https://img.shields.io/badge/license-MIT-green)](./LICENSE.txt)
[![Forks](https://img.shields.io/github/forks/username/repo?style=social)]()
[![Social](https://img.shields.io/github/stars/username/repo?style=social)]()

[![Technology Badges](https://img.shields.io/badge/YourTech-Color?logo=tech&logoColor=white)](https://tech-url.com)
```

### Tonel Format and Baseline Structure

#### Baseline File (`src/BaselineOfYourProject/BaselineOfYourProject.class.st`)
```smalltalk
BaselineOfYourProject subclass: #BaselineOfYourProject
    slots: {}
    classVariables: {}
    package: 'BaselineOfYourProject'

BaselineOfYourProject class
    #baseline: [
        <baseline>
        spec
            for: #pharo
            do: [
                spec
                    package: 'YourProject-Core'
                    package: 'YourProject-Tests' with: [ spec requires: 'YourProject-Core' ]
            ]
    ]
```

#### Package Structure
- **`YourProject-Core`** - Main application code
- **`YourProject-Tests`** - Test code (depends on Core)
- **`BaselineOfYourProject`** - Dependency and package specification
- **`src/`** - Contains all packages in tonel format for CI/CD

### Smalltalk-Specific Release Management

**Note:** For complete Changelog and Architecture Decision Records standards, see the main [Code Style Guide](../code-style.md#project-documentation-standards) which contains universal guidelines applicable to all projects.

#### README Structure
- **Project header** with hero image
- **Status badges** for CI, coverage, and versions
- **Technology badges** for supported backends/features
- **Clear description** of project purpose and features
- **Installation instructions** with Metacello examples
- **Usage examples** with practical code snippets
- **Dependency information** for including in other projects

#### Dependency Management Patterns

##### Loading from GitHub
```smalltalk
"Load latest version with default backends"
Metacello new
  baseline: 'YourProject';
  repository: 'github://username/YourProject:latest/src';
  load.

"Load specific backends explicitly"
Metacello new
  baseline: 'YourProject';
  repository: 'github://username/YourProject:latest/src';
  load: #('Core' 'Feature1' 'Feature2')
```

##### Including as Dependency
```smalltalk
"In BaselineOf or ConfigurationOf"
spec
  baseline: 'YourProject'
    with: [ spec
    repository: 'github://username/YourProject:latest/src';
    load: #('Core' 'Feature1' 'Feature2') ]
```

##### Version Pinning
- **Pin dependency versions** for reproducible builds
- **Use semantic versioning** (v0.7.0, v0.6.0)
- **Document breaking changes** in changelog
- **Support multiple Pharo versions** when possible

## Pharo Headless Execution

### Overview
Pharo Smalltalk can be run in headless mode from the command line, making it perfect for automation, CI/CD, and server-side execution.

### Core Principles for Headless Execution
- **Always use `eval` for automation scripts** - never use `st` command
- **Design code to run without user interaction**
- **Ensure automatic exit after execution**
- **Use Transcript for all output in headless mode**
- **Implement proper error handling for automation**

### Installation
- **Location**: `/Applications/PharoLauncher.app/Contents/MacOS/Pharo`
- **Image**: `/Applications/PharoLauncher.app/Contents/Resources/PharoLauncher.image`
- **Launcher Script**: `/Applications/PharoLauncher.app/Contents/Resources/pharo-launcher`

### Working Command Structure
```bash
"/Applications/PharoLauncher.app/Contents/MacOS/Pharo" \
  --headless "/Applications/PharoLauncher.app/Contents/Resources/PharoLauncher.image" \
  --no-default-preferences \
  eval "your Smalltalk code here"
```

### Key Commands

#### ✅ `eval` - Recommended for Headless Execution
- **Automatically exits** after execution
- **Perfect for automation/CI/CD**
- **Can run complex expressions**
- **No hanging processes**

#### ⚠️ `st` - Problematic for Headless
- **Keeps image running indefinitely**
- **Requires manual termination (Ctrl+C)**
- **Not suitable for automation**

### Working Smalltalk Features

#### Basic Operations
```smalltalk
"Arithmetic"
2 + 3 = 5

"String operations"
'Hello' size = 5

"Arrays"
#(1 2 3 4 5)

"Class introspection"
5 class = SmallInteger
String methods size = 356

"Date/time"
DateAndTime now
```

#### Collection Operations with Blocks
```smalltalk
"Mapping with collect:"
#(1 2 3 4 5) collect: [ :x | x * x ]
"Result: #(1 4 9 16 25)"

"Filtering with select:"
#(1 2 3 4 5 6 7 8 9 10) select: [ :x | x even ]
"Result: #(2 4 6 8 10)"

"Reducing with inject:into:"
(#(1 2 3 4 5 6 7 8 9 10) select: [ :x | x even ]) inject: 0 into: [ :sum :x | sum + x ]
"Result: 30"
```

#### File System
```smalltalk
"Working directory"
FileSystem workingDirectory
"Result: File @ /Users/seb/code/stui"
```

### Non-Working Methods
- `take:` method doesn't exist on collections
- Some file system methods like `homeDirectory`
- Some meta-programming methods

### Headless Execution Best Practices

#### 1. Use `eval` for Inline Code
- Runs and exits automatically
- Perfect for automation

#### 2. For Complex Scripts
- Use `eval` with multiple statements separated by semicolons
- Avoid `st` command unless you need the image to stay open

#### 3. Perfect Use Cases
- CI/CD pipelines
- Automated testing
- Server-side Smalltalk execution
- Data processing scripts

### Example Use Cases

#### Simple Calculation
```bash
"/Applications/PharoLauncher.app/Contents/MacOS/Pharo" \
  --headless "/Applications/PharoLauncher.app/Contents/Resources/PharoLauncher.image" \
  --no-default-preferences \
  eval "Transcript show: 'Result: '; show: (1 to: 100) sum; cr."
```

#### Collection Processing
```bash
"/Applications/PharoLauncher.app/Contents/MacOS/Pharo" \
  --headless "/Applications/PharoLauncher.app/Contents/Resources/PharoLauncher.image" \
  --no-default-preferences \
  eval "Transcript show: 'Squared: '; show: (#(1 2 3 4 5) collect: [ :x | x * x ]); cr."
```

#### Multiple Operations
```bash
"/Applications/PharoLauncher.app/Contents/MacOS/Pharo" \
  --headless "/Applications/PharoLauncher.app/Contents/Resources/PharoLauncher.image" \
  --no-default-preferences \
  eval "Transcript show: 'Time: '; show: DateAndTime now; cr. Transcript show: 'Random: '; show: (1 to: 100) atRandom; cr."
```

### Troubleshooting

#### Common Issues
1. **Method not found**: Check if method exists in this Pharo version
2. **Hanging process**: Use `eval` instead of `st`
3. **Quote issues**: Ensure proper escaping in complex expressions

#### Debugging
- Start with simple expressions
- Test individual operations before combining
- Use `Transcript show:` for output
- Check method availability with `class methods size`

### Alternative Installation
If you prefer command-line installation:
```bash
curl -L https://get.pharo.org | bash
```

### Notes
- Pharo Launcher 3.2 installed
- macOS Intel (x86_64) compatible
- Headless mode works perfectly for automation
- Blocks and functional programming fully supported

## Summary

This unified style guide covers both general Smalltalk development and Pharo-specific headless execution patterns. Key principles include:

1. **General Smalltalk Best Practices**
   - Clear naming conventions and code structure
   - Proper use of blocks and functional programming
   - Comprehensive documentation and testing

2. **Headless Execution Compatibility**
   - All code must work without user interaction
   - Use `eval` command for automation (never `st`)
   - Implement proper error handling and logging

3. **Automation Readiness**
   - Code suitable for CI/CD and automated execution
   - Proper exit conditions and process management
   - Comprehensive logging for debugging

4. **Maintainability**
   - Clear separation of concerns
   - Consistent formatting and structure
   - Robust error handling and validation

Following these guidelines ensures your Smalltalk code will work reliably in both interactive and headless modes, making it suitable for long-term projects and automation workflows.

## Anti-Patterns to Avoid

### ❌ Don't Use `st` Command
```bash
"Never use this for automation"
pharo image st "code"
```

### ❌ Don't Create Hanging Processes
```smalltalk
"Don't create infinite loops or waiting processes"
"❌ This will hang"
[ true ] whileTrue: [ "something" ]

"✅ Use proper exit conditions"
[ self hasMoreWork ] whileTrue: [ self processNextItem ]
```

### ❌ Don't Rely on User Input
```smalltalk
"Don't use UI-dependent code in headless mode"
"❌ This won't work headless"
UIManager default request: 'Enter value'

"✅ Use configuration or parameters"
self getValueFromConfiguration
```

## Integration with CI/CD

### Modern CI/CD with SmalltalkCI

#### Project Configuration (`.smalltalk.ston`)
```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'YourProject',
      #directory : 'src',
      #load : [ 'Core', 'Tests' ],
      #platforms : [ #pharo ]
    }
  ]
}
```

#### Test Configuration (`.smalltalkci/.unit-tests.ston`)
```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'YourProject',
      #directory : '../src',
      #load : [ 'Core', 'Tests' ],
      #platforms : [ #pharo ]
    }
  ],
  #testing : {
    #coverage : {
      #packages : [ 'YourProject-Base', 'YourProject-Tests' ],
      #format: #lcov
    }
  }
}
```

#### GitHub Actions Workflow (`.github/workflows/build.yml`)
```yaml
name: Unit Tests

on: [push]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        smalltalk: [Pharo64-11]
        redis-version: [7]
        mongodb-version: ['7.0']
    name: ${{ matrix.smalltalk }}
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_USER: postgres
          POSTGRES_HOST_AUTH_METHOD: trust
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432    
    steps:
      - name: Start Redis
        uses: supercharge/redis-github-action@1.7.0
        with:
          redis-version: ${{ matrix.redis-version }}
      - name: Start MongoDB
        uses: supercharge/mongodb-github-action@1.10.0
        with:
          mongodb-version: ${{ matrix.mongodb-version }}
          mongodb-replica-set: test-rs          
      - uses: actions/checkout@v2
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: ${{ matrix.smalltalk }}
      - name: Load Image and Run Tests
        run: smalltalkci -s ${{ matrix.smalltalk }} .smalltalkci/.unit-tests.ston
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        timeout-minutes: 15
      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v1
        with:
          name: ${{matrix.os}}-${{matrix.smalltalk}}
          token: ${{ secrets.CODECOV_TOKEN }}
```

### Build Scripts

#### Main Build Script (`build`)
```bash
#!/bin/bash
# Gets a fresh Pharo image and loads your project

echo "Downloading fresh Pharo image and VM ..."
curl get.pharo.org | bash 

echo "Opening Pharo to load project ..."
./pharo-ui Pharo.image builder.st

echo "Ready!"
```

#### Smalltalk Builder Script (`builder.st`)
```smalltalk
Metacello new
    repository: 'tonel://./src';
    baseline: 'YourProject';
    onConflictUseIncoming;
    load.

Smalltalk saveSession.
```

#### Clean Scripts
```bash
# clean - Removes Pharo installation
rm -rf pharo pharo-ui pharo-vm *.log
./cleanCaches

# cleanCaches - Removes package caches
rm -rf package-cache github-cache
```

### CI/CD Best Practices

#### 1. Use SmalltalkCI for Automated Testing
- **Standardized testing** across different Smalltalk platforms
- **Coverage reporting** with lcov format
- **Matrix testing** for multiple Pharo versions
- **Timeout management** to prevent hanging builds

#### 2. Metacello for Dependency Management
- **Baseline-based loading** for reproducible builds
- **Repository specification** with tonel:// protocol
- **Conflict resolution** with onConflictUseIncoming
- **Platform-specific loading** for different Smalltalk dialects

#### 3. GitHub Actions Integration
- **Automatic testing** on every push
- **Multiple Pharo versions** support
- **Coverage upload** to Codecov
- **Service containers** for databases (PostgreSQL, MongoDB, Redis)

#### GitHub-Specific Configuration Details

##### Required Secrets
```yaml
# Add these in your GitHub repository settings
secrets:
  CODECOV_TOKEN: "your-codecov-token-here"
  # GITHUB_TOKEN is automatically provided by GitHub
```

##### Service Containers
- **PostgreSQL**: Built-in service with health checks
- **Redis**: Uses supercharge/redis-github-action@1.7.0
- **MongoDB**: Uses supercharge/mongodb-github-action@1.10.0

##### Matrix Strategy
- **Multiple Smalltalk versions**: Test against different Pharo releases
- **Database versions**: Test with different Redis and MongoDB versions
- **Cross-platform testing**: Can extend to different operating systems

##### Health Checks
- **PostgreSQL**: Uses `pg_isready` with configurable intervals
- **Timeout management**: 15-minute timeout for test execution
- **Port configuration**: Standard database ports (5432 for PostgreSQL)

#### 4. Build Process
- **Fresh image download** for each build
- **Automated loading** via Smalltalk scripts
- **Session persistence** for debugging
- **Clean environment** for reproducible results

### Script Structure for Custom CI/CD
```smalltalk
"Standard structure for CI/CD scripts"
main
    Transcript show: 'Starting CI/CD process'; cr.
    
    self runTests.
    self buildApplication.
    self runIntegrationTests.
    self deployIfSuccessful.
    
    Transcript show: 'CI/CD process complete'; cr.
```

### Exit Codes
```smalltalk
"Use proper exit codes for CI/CD"
exitWithSuccess
    Transcript show: 'SUCCESS: All operations completed successfully'; cr.
    Smalltalk exitSuccess

exitWithFailure: reason
    Transcript show: 'FAILURE: '; show: reason; cr.
    Smalltalk exitFailure
```

## Common Patterns for Headless Execution

### Data Pipeline
```smalltalk
"Standard data processing pipeline"
processPipeline: input
    ^input
        select: [ :item | item isValid ]
        collect: [ :item | item transform ]
        inject: OrderedCollection new into: [ :collection :item |
            collection add: item.
            collection
        ]
```

### Configuration Management
```smalltalk
"Configuration management for headless operation"
defaultConfiguration
    ^Dictionary new
        at: 'timeout' put: 30;
        at: 'retries' put: 3;
        at: 'outputPath' put: 'results/';
        yourself
```

### Logging for Automation
```smalltalk
"Comprehensive logging for automation"
log: message level: level
    | timestamp |
    timestamp := DateAndTime now.
    Transcript 
        show: '['; show: timestamp; show: '] ';
        show: level; show: ': '; show: message; cr
```
