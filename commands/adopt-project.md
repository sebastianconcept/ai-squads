---
name: adopt-project
alwaysApply: false
---

# Adopt Project Workflow

This workflow guides the adoption of ai-squads in a project.

## Prerequisites

1. Global ai-squads installation must be present (run `./scripts/install.sh` from ai-squads directory)
2. Must be run from a git repository root

## Steps

### 1. Verify Global Installation
- Check if `~/.cursor/templates/` directory exists
- Verify it contains project templates
- If not found, prompt user to run global installation first

### 2. Create Project Structure
- Run `~/.cursor/scripts/create-project-docs.sh` from project root
- This creates:
  - `docs/` directory
  - Copies all templates

### 3. Gather Project Information
Ask the user interactive questions:

**Project Mission:**
- What is the main purpose of this project?
- What problem does it solve?
- Who is the target audience?

**Tech Stack:**
- What technologies are used? (Backend, Frontend, Database, etc.)
- What frameworks/libraries?
- Any specific versions or constraints?

**Roadmap:**
- What are the immediate priorities?
- What are the long-term goals?
- Any known upcoming features?

**Key Decisions:**
- What important architectural decisions have been made?
- What trade-offs were considered?

### 4. Select Agent Team
Based on tech stack and project type, suggest relevant agents:
- Rust backend → Rust Specialist
- Smalltalk/Pharo → Smalltalk Specialist
- JavaScript/Alpine/htmx → JavaScript Specialist, UI Developer
- SaaS products (especially Brazilian market) → Clovis (Copywriting & Growth)
- Product strategy → Strategic Designer (Rian)
- All projects → Jobs to be Done, UI/UX

Allow user to:
- Select from suggested agents
- Add additional agents
- Remove suggested agents
- Specify roles for each agent

### 5. Customize Templates
Fill in generated templates with gathered information:
- `docs/mission.md` - Project mission
- `docs/tech-stack.md` - Technology details
- `docs/roadmap.md` - Project roadmap
- `docs/DECISIONS.md` - Key decisions
- `docs/README.md` - Project overview
- `docs/team.md` - Selected agent team

### 6. Finalize
- Review created structure
- Optionally create initial git commit
- Confirm installation complete

## Output

After completion, the project should have:
- `docs/` with all project documentation
- `docs/team.md` with agent team configuration

## Notes

- All paths should be relative to project root
- Keep documentation brief and developer-oriented
