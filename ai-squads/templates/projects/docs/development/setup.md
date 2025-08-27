---
description: Development Setup Guide - Getting started with project development
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Development Setup Guide

> Last Updated: [DATE]
> Version: [VERSION]

## Prerequisites

### Required Software
- [Software 1] - [Version requirement]
- [Software 2] - [Version requirement]
- [Software 3] - [Version requirement]

### Required Accounts
- [Account/Service 1] - [Purpose]
- [Account/Service 2] - [Purpose]

## Environment Setup

### 1. Clone Repository
```bash
git clone [REPOSITORY_URL]
cd [PROJECT_NAME]
```

### 2. Install Dependencies
```bash
# Install system dependencies
[SYSTEM_INSTALL_COMMAND]

# Install project dependencies
[PROJECT_INSTALL_COMMAND]
```

### 3. Configuration
```bash
# Copy configuration template
cp config/config.template.yaml config/config.yaml

# Update configuration with your settings
[EDIT_COMMAND] config/config.yaml
```

### 4. Environment Variables
```bash
# Copy environment template
cp .env.template .env

# Set required environment variables
export [VARIABLE_1]=[VALUE_1]
export [VARIABLE_2]=[VALUE_2]
```

## Development Workflow

### Running the Project
```bash
# Development mode
[DEV_COMMAND]

# Production mode
[PROD_COMMAND]

# Test mode
[TEST_COMMAND]
```

### Code Quality
```bash
# Linting
[LINT_COMMAND]

# Formatting
[FORMAT_COMMAND]

# Testing
[TEST_COMMAND]
```

### Git Workflow
```bash
# Create feature branch
git checkout -b feature/[feature-name]

# Make changes and commit
git add .
git commit -m "feat: [description]"

# Push and create PR
git push origin feature/[feature-name]
```

## IDE Setup

### Recommended Extensions
- [Extension 1] - [Purpose]
- [Extension 2] - [Purpose]
- [Extension 3] - [Purpose]

### Configuration Files
- `.vscode/settings.json` - Editor settings
- `.vscode/extensions.json` - Required extensions
- `.editorconfig` - Code formatting rules

## Troubleshooting

### Common Issues
1. **[Issue 1]**
   - **Symptoms**: [Description]
   - **Solution**: [Steps to resolve]

2. **[Issue 2]**
   - **Symptoms**: [Description]
   - **Solution**: [Steps to resolve]

### Getting Help
- **Documentation**: Check other docs in this directory
- **Issues**: [Issue tracker URL]
- **Discussions**: [Discussion forum URL]
- **Team Chat**: [Chat platform URL]

## Next Steps

- Complete the [Architecture Overview](../architecture/overview.md)
- Review [Best Practices](../best-practices/)
- Set up [Development Environment](../environment/)
