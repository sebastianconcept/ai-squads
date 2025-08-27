---
description: Documentation Directory Template - Standard docs structure for all projects
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Project Documentation

This directory contains all project documentation organized by category for easy reference and knowledge base usage.

## Directory Structure

```
docs/
├── README.md                    # This file - documentation overview
├── api/                         # API documentation and specifications
├── architecture/                # System architecture and design documents
├── deployment/                  # Deployment guides and procedures
├── development/                 # Development setup and guidelines
├── environment/                 # Environment configuration and setup
├── operations/                  # Operations and maintenance guides
├── performance/                 # Performance analysis and optimization
├── training/                    # Training materials and onboarding
├── lessons-learned/             # Lessons learned and retrospectives
├── best-practices/              # Best practices and guidelines
├── troubleshooting/             # Common issues and solutions
├── metrics/                     # Success metrics and KPIs
├── stakeholder/                 # Stakeholder communications and reports
└── next-phase/                  # Next phase planning and preparation
```

## Documentation Standards

### File Naming
- Use kebab-case for file names
- Include date prefix for time-sensitive documents (YYYY-MM-DD-)
- Use descriptive, action-oriented names

### Content Structure
- Start with a clear title and purpose
- Include last updated date and version
- Use consistent markdown formatting
- Include table of contents for long documents
- End with next steps or related documents

### Maintenance
- Review and update documentation monthly
- Archive outdated documents to `docs/archive/`
- Link related documents for easy navigation
- Keep documentation close to code when possible

## Quick Reference

### For Developers
- **Getting Started**: `docs/development/setup.md`
- **Architecture**: `docs/architecture/overview.md`
- **API Reference**: `docs/api/`

### For Operations
- **Deployment**: `docs/deployment/`
- **Monitoring**: `docs/operations/monitoring.md`
- **Troubleshooting**: `docs/troubleshooting/`

### For Stakeholders
- **Project Status**: `docs/stakeholder/status-reports.md`
- **Metrics**: `docs/metrics/`
- **Roadmap**: `docs/next-phase/`

## Integration with Workflows

This documentation structure integrates with:
- **Agent Knowledge Base**: All agents can reference these documents
- **Project Templates**: New projects automatically include this structure
- **Workflow Automation**: Scripts can generate and update documentation
- **Quality Assurance**: Documentation completeness is tracked

## Template Usage

When creating new projects:
1. Copy this entire `docs/` directory structure
2. Customize content for your specific project
3. Update references in project templates
4. Ensure all workflows reference appropriate documentation
