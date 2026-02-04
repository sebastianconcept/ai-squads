# Platform-Agnostic Definitions

This directory contains the single source of truth for agent and command definitions in a platform-agnostic format.

## Purpose

Instead of maintaining separate definitions per platform (Cursor, Claude, Gemini, Codex), we define agents and commands once in a platform-agnostic format. Each platform has a sync script (`scripts/{platform}-cli/sync-definitions.sh`) that transforms these definitions to the platform's expected format/location.

## Directory Structure

```
definitions/
├── agents/          # Agent definitions (*.md)
└── commands/        # Command definitions (*.md)
```

## Convention: Platform-Agnostic Definitions

### Commands

Command definitions describe **what to do** and **how to behave**, not **how to invoke**. Invocation is handled by a thin layer per platform.

**What commands describe:**
- Purpose and goals
- Prerequisites
- Step-by-step workflow
- Expected outputs
- Quality criteria

**What commands do NOT describe:**
- Platform-specific invocation syntax (e.g., "slash-command", "@-mention")
- Platform-specific tool names (use generic terms like "AI CLI" instead of "Cursor CLI")
- Platform-specific file paths (use installation-agnostic paths like `~/.cursor/` as default, but note it's configurable)

**Example:**
- ✅ "AI CLI must be available" (platform-agnostic)
- ❌ "Cursor CLI must be available" (platform-specific)

### Agents

Agent definitions describe **capabilities**, **specialization**, and **behavior**, not platform-specific features.

**What agents describe:**
- Domain expertise
- Skills and capabilities
- Code style references
- Workflow preferences

**What agents do NOT describe:**
- Platform-specific tool usage
- Platform-specific invocation patterns

## Schema: Platform-Agnostic Markdown Format

### Command Schema

Commands use markdown with frontmatter:

```markdown
---
name: command-name
alwaysApply: false
---

# Command Title

## Prerequisites
- Prerequisite 1
- Prerequisite 2

## Steps

### 1. Step Name
- Action item
- Another action

### 2. Next Step
...
```

**Frontmatter fields:**
- `name`: Command identifier (used for discovery)
- `alwaysApply`: Boolean indicating if command applies automatically

**Content structure:**
- Title: Command name
- Prerequisites: What must be true before running
- Steps: Sequential workflow steps
- Optional sections: Examples, Notes, References

### Agent Schema

Agents use markdown with frontmatter:

```markdown
---
name: agent-name
specialization: Domain or Technology
---

# Agent Name

## Specialization
Brief description of what this agent specializes in.

## Capabilities
- Capability 1
- Capability 2

## Code Style References
- Reference to standards/code/{language}-style.md

## Workflow Preferences
- Preference 1
- Preference 2
```

**Frontmatter fields:**
- `name`: Agent identifier
- `specialization`: Domain or technology focus

**Content structure:**
- Specialization: What the agent knows
- Capabilities: What the agent can do
- Code Style References: Links to style guides
- Workflow Preferences: How the agent prefers to work

## Platform Sync Scripts

Each platform has a sync script that transforms definitions to the platform's expected format and location:

- **Cursor**: `scripts/cursor-cli/sync-definitions.sh` → `~/.cursor/commands/`, `~/.cursor/agents/`
- **Claude**: `scripts/claude-cli/sync-definitions.sh` → `~/.claude/commands/` (slash commands, e.g. /plan-feature)
- **Gemini**: `scripts/gemini-cli/sync-definitions.sh` → `~/.gemini/skills/ai-squads/` (skills)
- **Codex**: `scripts/codex-cli/sync-definitions.sh` → `~/.agents/skills/ai-squads/` (skills)

You can also include the ai-squads repo at runtime via `--add-dir` or `--include-directories` where the CLI supports it. Run `scripts/install_or_update.sh` to sync definitions for all detected platforms.

## Source of Truth

Agents and commands live in `definitions/agents/` and `definitions/commands/`. Install and update scripts copy from these directories to each platform (e.g. `~/.cursor/` for Cursor). Platform sync scripts (`scripts/{platform}-cli/sync-definitions.sh`) use definitions as the sole source when syncing.
