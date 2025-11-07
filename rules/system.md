# AI Squads System Rules

## System Overview

AI Squads is a config-oriented system for managing AI agent teams in software projects. It provides specialist agents, code style standards, and workflows for project planning and code review.

## Core Principles

1. **Configuration-Driven**: All agents, standards, and workflows are defined in `.md` files for easy adjustment
2. **Global Installation**: ai-squads is installed once globally to `~/.cursor/` and used across all projects
3. **Agent Specialization**: Agents are specialists in specific technologies or domains
4. **Style Consistency**: Agents reference code style standards for consistent guidance
5. **Developer-Oriented**: Documentation and templates are brief, clear, and actionable

## File Structure

- Global installation at `~/.cursor/`:
  - `commands/` - Command workflow definitions
  - `templates/` - Document and command templates
  - `scripts/` - Shell scripts for file operations
  - `rules/` - System rules (applied globally to all prompts)
- Source repository contains:
  - `agents/` - Agent definitions
  - `standards/code/` - Code style standards
  - `commands/` - Command workflow definitions
  - `templates/` - Document and command templates
  - `scripts/` - Shell scripts for file operations

## Agent System

- Agents are defined in the ai-squads source repository (`agents/`)
- Each agent has specialization, rules, capabilities, and style guide references
- Agents can be combined into teams per project
- Team configuration stored in `docs/team.md`
- Commands reference agents from the source repository

## Command System

- Global commands installed to `~/.cursor/commands/`
- Commands reference workflow rules from global installation
- Workflows define step-by-step processes

## Project Structure

When a project adopts ai-squads:
- `docs/` - Project planning documentation
- `docs/feature/{feature_name}/` - Feature-specific documentation (PRD, specs, tasks)

## Feature Context

When working with a user, they are likely working in the scope of a planned feature:
- Check for feature documentation in `docs/feature/{feature_name}/`
- The feature name typically matches the current git branch name
- Feature documentation includes:
  - `PRD.md` - Product requirements and user stories
  - `specs.md` - Technical specifications and architecture
  - `tasks.md` - Implementation tasks and breakdown
- Reference this context when providing guidance to ensure alignment with planned work

## Documentation Standards

- Brief and clear
- Developer-oriented
- Actionable
- Use markdown format
- Include examples where helpful

## Style Guides

- Located in the ai-squads source repository (`standards/code/`)
- Referenced by relevant agents
- Applied during code review
- Updated as needed for consistency

## Workflow Execution

- Commands trigger workflows defined in global installation (`~/.cursor/commands/`)
- Workflows reference agents and style guides from the source repository
- Scripts handle file operations (installed globally at `~/.cursor/scripts/`)
- AI handles interactive prompts and document generation

## Best Practices

- Keep agent rules succinct and focused
- Update style guides based on team feedback
- Document decisions in `docs/DECISIONS.md`
- Review and refine workflows regularly
- Maintain consistency across projects
