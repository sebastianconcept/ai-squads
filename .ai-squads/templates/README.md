# SquadAI Templates

This directory contains templates for creating new squads and agents in the SquadAI system.

## Available Templates

### Squad Templates
- **`squad-template.md`** - Template for creating new squads
  - Defines squad structure with Overview, Members, and Team Preferences
  - Lean and focused format for easy maintenance
  - Used by `scripts/create-squad.sh` for squad creation

### Agent Templates
- **`agent-template.md`** - Template for creating new agents
  - Comprehensive agent structure with all standard sections
  - Includes Core Capabilities, Implementation Instructions, and Agent Integration
  - Used by `scripts/create-resource.sh` for agent creation

### Project Templates
- **`project.md`** - Overview of project template structure
- **`mission.md`** - Project mission, vision, and purpose template
- **`mission-lite.md`** - Condensed project overview for quick reference
- **`roadmap.md`** - Development phases and feature roadmap template
- **`tech-stack.md`** - Technical architecture and technology choices template
- **`decisions.md`** - Decision log and architectural decisions template
- **`README.md`** - Project overview and getting started guide template

## Usage

### Creating a New Squad
```bash
./scripts/create-resource.sh squad "Web Development"
# Creates: .ai-squads/squads/web-development.md
# Creates: .cursor/rules/web-development-squad.md
```

### Creating a New Agent
```bash
./scripts/create-resource.sh agent "Data Analyst"
# Creates: .ai-squads/agents/data-analyst.md
# Creates: .cursor/rules/data-analyst-agent.md
```

### Creating a New Project
```bash
# Copy project templates to new project directory
cp -r templates/projects .ai-squads/projects/[PROJECT_NAME]
# Then customize each template file for your specific project
# Start with project.md for the overview, then customize each template
```

## Template Structure

### Squad Template Sections
- **Overview**: One sentence description of squad purpose
- **Members**: List of agents in the squad
- **Team Preferences**: Technology focus, architecture, development style, quality standards, workflow

### Agent Template Sections
- **Overview**: Agent specialization and main purpose
- **Core Capabilities**: Key areas of expertise
- **Implementation Instructions**: Workflows and processes
- **Communication Style**: How the agent communicates
- **Agent Integration**: How it works with other agents
- **Workflow Triggers**: When and how it's activated
- **Deliverables**: What it produces
- **Success Metrics**: How success is measured
- **Integration Notes**: Technical integration details

### Project Template Sections
- **Mission**: Project vision, purpose, user personas, growth strategy, design direction, and branding requirements
- **Mission-Lite**: Condensed project overview for quick reference and AI context
- **Roadmap**: Development phases, features, timelines, and major achievements tracking
- **Tech Stack**: Technical architecture, critical dependencies, security considerations, and architecture patterns
- **Decisions**: Decision log and architectural decisions with implementation tracking
- **README**: Project overview and getting started guide

## Customization

After creating a new squad or agent from a template:

1. **Edit the main file** (`.ai-squads/squads/[name].md` or `.ai-squads/agents/[name].md`)
2. **Update the reference file** (`.cursor/rules/[name]-squad.md` or `.cursor/rules/[name]-agent.md`)
3. **Customize content** to match the specific squad or agent purpose
4. **Test integration** with the Director Agent

After creating a new project from templates:

1. **Copy project templates** to `.ai-squads/projects/[PROJECT_NAME]/`
2. **Customize each template file** for your specific project
3. **Update project references** in squad definitions
4. **Test project integration** with the Director Agent

## Template Updates

When updating templates:
1. **Modify the template file** in this directory
2. **Test with existing resources** to ensure compatibility
3. **Update documentation** if structure changes significantly
4. **Consider backward compatibility** for existing squads/agents
