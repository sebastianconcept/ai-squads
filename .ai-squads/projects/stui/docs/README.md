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

### Project Status & Planning
- **Current Status**: [`CURRENT_STATUS_SUMMARY.md`](./CURRENT_STATUS_SUMMARY.md)
- **Team Action Plan**: [`TEAM_ACTION_PLAN.md`](./TEAM_ACTION_PLAN.md)
- **Roadmap 2025**: [`ROADMAP_2025.md`](./ROADMAP_2025.md)

### Development & Integration
- **Protocol Documentation**: [`PROTOCOL.md`](./PROTOCOL.md)
- **Integration Guide**: [`INTEGRATION.md`](./INTEGRATION.md)
- **ZeroMQ Integration**: [`ZEROMQ_INTEGRATION.md`](./ZEROMQ_INTEGRATION.md)
- **Demo Strategy**: [`DEMO_STRATEGY.md`](./DEMO_STRATEGY.md)

### Production & Operations
- **Production Readiness**: [`PRODUCTION_READINESS_ASSESSMENT.md`](./PRODUCTION_READINESS_ASSESSMENT.md)
- **Production Checklist**: [`PRODUCTION_CHECKLIST.md`](./PRODUCTION_CHECKLIST.md)
- **Terminal Process Management**: [`TERMINAL_PROCESS_MANAGEMENT.md`](./TERMINAL_PROCESS_MANAGEMENT.md)

### Phase Documentation
- **Phase 2 Roadmap**: [`PHASE_2_CONTINUATION_ROADMAP.md`](./PHASE_2_CONTINUATION_ROADMAP.md)
- **Phase 3 Summary**: [`PHASE_3_COMPLETION_SUMMARY.md`](./PHASE_3_COMPLETION_SUMMARY.md)
- **Integration Test Plan**: [`INTEGRATION_TEST_FIX_PLAN.md`](./INTEGRATION_TEST_FIX_PLAN.md)

### For Developers
- **Getting Started**: `development/setup.md`
- **Architecture**: `architecture/overview.md`
- **API Reference**: `api/`

### For Operations
- **Deployment**: `deployment/`
- **Monitoring**: `operations/monitoring.md`
- **Troubleshooting**: `troubleshooting/`

### For Stakeholders
- **Project Status**: `stakeholder/status-reports.md`
- **Metrics**: `metrics/`
- **Next Phase**: `next-phase/`

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
