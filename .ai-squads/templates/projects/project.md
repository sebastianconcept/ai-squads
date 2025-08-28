---
description: Project Template - Complete project structure and documentation
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Project Templates

This directory contains templates for creating new projects in the SquadAI system.

## Template Files

### Core Project Files
- **`mission.md`** - Project mission, vision, and purpose
- **`mission-lite.md`** - Condensed project overview for quick reference
- **`roadmap.md`** - Development phases and feature roadmap
- **`tech-stack.md`** - Technical architecture and technology choices
- **`decisions.md`** - Decision log and architectural decisions
- **`README.md`** - Project overview and getting started guide

## Usage

### Creating a New Project
1. **Copy template files** to `.ai-squads/projects/[PROJECT_NAME]/`
2. **Customize content** for your specific project
3. **Update references** to match your project structure
4. **Integrate with squads** by updating squad definitions

### Project Structure

## Documentation

All project documentation is organized in the `docs/` directory:

- **`docs/README.md`** - Documentation overview and structure
- **`docs/development/`** - Development setup and guidelines
- **`docs/architecture/`** - System architecture and design
- **`docs/api/`** - API documentation and specifications
- **`docs/deployment/`** - Deployment guides and procedures
- **`docs/operations/`** - Operations and maintenance
- **`docs/performance/`** - Performance analysis and optimization
- **`docs/training/`** - Training materials and onboarding
- **`docs/lessons-learned/`** - Lessons learned and retrospectives
- **`docs/best-practices/`** - Best practices and guidelines
- **`docs/troubleshooting/`** - Common issues and solutions
- **`docs/metrics/`** - Success metrics and KPIs
- **`docs/stakeholder/`** - Stakeholder communications
- **`docs/next-phase/`** - Next phase planning

For detailed documentation, see the [docs/](docs/) directory.
```
.ai-squads/projects/[PROJECT_NAME]/
├── mission.md          # Project vision and purpose
├── roadmap.md          # Development phases and features
├── tech-stack.md       # Technical architecture
├── decisions.md        # Decision log
└── README.md           # Project overview
```

## Template Customization

### Mission Template
- **Project Overview**: What the project is and why it exists
- **Target Users**: Who will use this project
- **Key Features**: Core functionality and capabilities
- **Success Metrics**: How success will be measured

### Roadmap Template
- **Development Phases**: Logical grouping of features
- **Feature Prioritization**: Must-have vs. nice-to-have
- **Timeline Estimates**: Rough development timeframes
- **Dependencies**: What needs to be built first

### Tech Stack Template
- **Frontend Technologies**: UI frameworks and libraries
- **Backend Technologies**: Server-side technologies
- **Database Choices**: Data storage and management
- **Infrastructure**: Hosting, deployment, and operations

### Decisions Template
- **Architectural Decisions**: Key technical choices
- **Rationale**: Why decisions were made
- **Alternatives Considered**: What other options were evaluated
- **Impact**: Consequences and trade-offs

## Integration with Squads

### Squad Assignment
- **Identify appropriate squad** for project type
- **Update squad definition** to include project
- **Assign squad agents** to project tasks
- **Define handoff protocols** between agents

### Agent Coordination
- **Director Agent**: Project coordination and oversight
- **Specialized Agents**: Domain-specific implementation
- **Collaboration Agent**: Team coordination and handoffs
- **Quality Gates**: Ensure deliverables meet standards

## Best Practices

### Project Setup
- **Start with mission**: Define clear project purpose
- **Plan roadmap**: Break work into manageable phases
- **Choose tech stack**: Select appropriate technologies
- **Document decisions**: Record key choices and rationale

### Ongoing Maintenance
- **Update roadmap**: Reflect progress and changes
- **Track decisions**: Document new architectural choices
- **Review mission**: Ensure alignment with goals
- **Iterate quickly**: Learn and adapt based on feedback

## Example Projects

### Web Application
- **Mission**: Modern web app for task management
- **Squad**: Web Development Squad
- **Agents**: UX Expert, UI Implementor, Backend Engineer

### Mobile App
- **Mission**: Cross-platform mobile productivity tool
- **Squad**: Mobile Development Squad
- **Agents**: UX Expert, UI Implementor, Backend Engineer

### API Service
- **Mission**: RESTful API for data management
- **Squad**: Backend Development Squad
- **Agents**: Backend Engineer, API Designer, DevOps Engineer
