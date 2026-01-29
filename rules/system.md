# AI Squads System Rules

## System Overview

AI Squads is a config-oriented system for managing AI agent teams in software projects. It provides specialist agents, code style standards, and workflows for project planning and code review.

## Core Principles

1. **Configuration-Driven**: All agents, standards, and workflows are defined in `.md` files for easy adjustment
2. **Global Installation**: ai-squads is installed once globally to `~/.cursor/` and used across all projects
3. **Agent Specialization**: Agents are specialists in specific technologies or domains; they are global by default, with market-specific context (e.g. Brazilian) applied when requested
4. **Style Consistency**: Agents reference code style standards for consistent guidance
5. **Developer-Oriented**: Documentation and templates are brief, clear, and actionable
6. **Evidence-Based Approach**: All agents use a scientific, methodic spirit that is creative but grounded in reality

## Evidence-Based Methodology

All agents must follow an evidence-based approach when investigating, diagnosing, or making decisions:

### Evidence Collection Priority

**Preferred Evidence Sources** (in order of preference):
1. **Direct Instrumentation**: Reading logs, metrics, test results, code analysis
2. **Visual/Audio Capture**: Screenshots, screen recordings, audio recordings of behavior
3. **Experimental Data**: Controlled inputs with observed outputs, test runs
4. **Code Inspection**: Reading source code, configuration files, documentation
5. **User Testimony**: User reports, descriptions (use when direct evidence unavailable)

**Evidence Hierarchy**:
- **Strongest**: Direct observation from logs, metrics, or instruments
- **Strong**: Visual/audio capture of actual behavior
- **Moderate**: Experimental data from controlled tests
- **Weaker**: User testimony or descriptions (may be incomplete or biased)

### Scientific Method Application

When investigating issues or making decisions:

1. **Observe**: Collect direct evidence first (logs, metrics, visual capture)
2. **Hypothesize**: Form hypotheses based on evidence, not assumptions
3. **Experiment**: Design small, controlled experiments to test hypotheses
4. **Collect Evidence**: Document experimental inputs and observed outputs
5. **Validate or Discard**: Confirm hypotheses with evidence or discard if contradicted
6. **Document**: Record findings, evidence, and reasoning for future reference

### Hypothesis Generation for Mysteries

When resolving mysteries (investigations, bug diagnosis, system issues):

- **Generate Multiple Hypotheses**: Don't fixate on the first explanation
- **Design Small Experiments**: Create minimal tests to validate or refute each hypothesis
- **Collect Evidence Systematically**: Document what you observe, when, and from what source
- **Reduce Ambiguity**: Use experiments to narrow down possibilities
- **Unlock Progress**: Discard hypotheses that are contradicted by evidence to focus on viable paths

### Evidence Documentation

- Document evidence immediately when gathered
- Include timestamps, sources, and context
- For experimental data: document controlled inputs and observed outputs
- Reference evidence in hypotheses (supporting or contradicting)
- Use commit hashes to scope evidence validity to specific codebase versions

### When Direct Evidence is Unavailable

If direct evidence cannot be obtained:
- Clearly state what evidence is missing
- Document assumptions explicitly (mark as "assumed" not "observed")
- Design experiments to gather missing evidence
- Prefer asking for specific evidence (e.g., "Please provide logs from X") over general questions

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
- Team configuration stored in `docs/TEAM.md`
- Commands reference agents from the source repository

## Command System

- Global commands installed to `~/.cursor/commands/`
- Commands reference workflow rules from global installation
- Workflows define step-by-step processes

## Project Structure

When a project adopts ai-squads:
- `~/docs/{project-name}/` - Project planning documentation (where {project-name} is derived from git repository name)
- `~/docs/{project-name}/feature/{feature_name}/` - Feature-specific documentation (PRD, specs, prd.json)

## Feature Context

When working with a user, they are likely working in the scope of a planned feature:
- Check for feature documentation in `~/docs/{project-name}/feature/{feature_name}/`
- The feature name typically matches the current git branch name
- Feature documentation includes:
  - `PRD.md` - Product requirements and user stories
  - `SPECS.md` - Technical specifications and architecture
  - `prd.json` - Machine-readable execution format (replaces deprecated tasks.md)
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
- Document decisions in `~/docs/{project-name}/DECISIONS.md`
- Review and refine workflows regularly
- Maintain consistency across projects
