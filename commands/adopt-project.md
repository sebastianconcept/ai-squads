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

### 6. Create Initial Progress Tracking

**Purpose**: Create `docs/progress.md` - project-level progress digest that provides instant context restoration.

**Implementation Method**:
- **Recommended**: Call `scripts/update-progress.sh` after project structure is created
  - Script automatically creates initial progress.md if it doesn't exist
  - Script is idempotent (safe to call multiple times)
  - Script location: `scripts/update-progress.sh` (project-local) or `~/.cursor/scripts/update-progress.sh` (global)
  - **Command**: Run `bash scripts/update-progress.sh` from project root
  - Script will:
    1. Read `docs/README.md` to extract core value (one-liner summary)
    2. Create `docs/progress.md` with initialized structure
    3. Set all metrics to 0 or N/A
    4. Set position to "No active features"
    5. Initialize empty context sections

**Alternative Methods** (if script not available):
- Use `@take-note` command to create progress.md manually
- Create progress.md directly using file operations (use template from `docs/feature/autonomous-execution/PROGRESS-MD-DESIGN.md`)

**Initialization Steps**:

1. **Read Project Information**:
   - Read `docs/README.md` to extract core value (one-liner summary)
   - If README.md doesn't exist or is empty, use placeholder: "[Project description]"
   - Get current timestamp for "Last updated" field

2. **Create File Structure**:
   - Location: `docs/progress.md` (project root level)
   - Format: Markdown with structured sections
   - Template: Use structure from `docs/feature/autonomous-execution/PROGRESS-MD-DESIGN.md`

3. **Initialize Sections**:

   **Project Reference**:
   - `See: docs/README.md`
   - `Core value: [One-liner from README.md or placeholder]`
   - `Current focus: No active features`

   **Current Position**:
   - `Active Feature: None`
   - `Current Story: None`
   - `Status: No active features`
   - `Last activity: [Current timestamp] — Project initialized`
   - `Overall Progress: [░░░░░░░░░░] 0%`
   - `Features planned: 0`
   - `Features complete: 0`
   - `Stories total: 0`
   - `Stories complete: 0`

   **Performance Metrics**:
   - **Velocity**: 
     - Stories completed: 0
     - Average duration: N/A
     - Total execution time: 0 hours
     - Average iterations per story: N/A
     - Success rate: N/A
   - **By Feature**: Empty table with header, shows "*No features yet*"
   - **By Story Type**: Empty table with header, shows "*No stories yet*"
   - **By Agent**: Empty table with header, shows "*No stories yet*"
   - **Recent Trend**: N/A
   - **Gap Closure**: 
     - Gap closure stories: 0
     - Gap closure rate: 0%
     - Most common gap type: N/A

   **Accumulated Context**:
   - **Recent Decisions**: Empty, shows "*No decisions yet. See `docs/DECISIONS.md` when available.*"
   - **Active Blockers**: Empty, shows "*No active blockers*"
   - **Pending Todos**: "Total pending: 0 todos across 0 features"
   - **Active Investigations**: Empty, shows "*No active investigations*"

   **Quality Metrics**:
   - **Quality Check Performance**: All N/A (typecheck, lint, test, format pass rates)
   - **Common Quality Issues**: "*No quality issues tracked yet*"

   **Session Continuity**:
   - `Last session: [Current timestamp]`
   - `Stopped at: Project initialization`
   - `Resume context: Status: No active features`

   **Next Actions**:
   - Immediate: "[ ] Plan first feature"
   - Upcoming: "*No upcoming actions*"

4. **File Format Example**:
   ```markdown
   # Project Progress
   
   **Last updated:** 2024-01-15 10:00 UTC
   **Generated from:** prd.json files, feature notes, DECISIONS.md, git history
   
   ## Project Reference
   
   See: `docs/README.md`
   **Core value:** [One-liner from README.md]
   **Current focus:** No active features
   
   ## Current Position
   
   **Active Feature:** None
   **Current Story:** None
   **Status:** No active features
   **Last activity:** 2024-01-15 10:00 UTC — Project initialized
   
   **Overall Progress:** [░░░░░░░░░░] 0%
   - Features planned: 0
   - Features complete: 0
   - Stories total: 0
   - Stories complete: 0
   
   [Rest of structure...]
   ```

5. **Verification**:
   - Verify file was created at `docs/progress.md`
   - Verify all sections are present
   - Verify file is readable and properly formatted
   - Verify timestamp is current

**Template Reference**: See `docs/feature/autonomous-execution/PROGRESS-MD-DESIGN.md` for complete structure.

**Script Reference**: See `scripts/update-progress.sh` for implementation.

### 7. Create Project Notes (Optional but Recommended)

**Purpose**: Document codebase discoveries and architecture analysis for future reference.

**When to Create**: After completing project analysis (steps 1-5), create project notes to preserve deep analysis.

**Note Structure**: Create notes in `docs/notes/{project-name}/` (category: "projects" stored in frontmatter/metadata, not in directory path)

**Note Files to Create**:

1. **context.md** (category: "projects" in frontmatter):
   - Project scope and analysis objectives
   - What was analyzed and why
   - Success criteria for analysis

2. **evidence.md** (category: "projects" in frontmatter):
   - Codebase facts discovered (file structures, dependencies, technologies used)
   - Key files and their purposes
   - Architecture patterns observed
   - Technology versions and constraints

3. **insights.json** (category: "projects" in metadata):
   - Architecture discoveries with implications
   - Code patterns found and their significance
   - Design decisions identified
   - Trade-offs and rationale discovered
   - Mark insights as `evidenceBased: true` when supported by codebase analysis
   - Include `evidence: {}` object documenting where patterns were found

**Implementation**:
- Use note tools (documented APIs) to create notes - see "Available Tools" section in execution prompts
- See `docs/feature/self-notes/specs.md` for complete format specifications
- Notes can be in flat structure (`docs/notes/{project-name}-*.md`) or grouped structure (`docs/notes/{project-name}/*.md`)
- Category is always stored in frontmatter/metadata, not in directory path
- Use standard file operations (`write_file`, `read_file`) to implement note tools

**Example**: After analyzing a Rust project, create:
- `docs/notes/payment-service-analysis-context.md` (category: "projects" in frontmatter)
- `docs/notes/payment-service-analysis-evidence.md` (category: "projects" in frontmatter)
- `docs/notes/payment-service-analysis-insights.json` (category: "projects" in metadata)

### 8. Finalize
- Review created structure
- Optionally create initial git commit
- Confirm installation complete

## Output

After completion, the project should have:
- `docs/` with all project documentation
- `docs/team.md` with agent team configuration
- `docs/progress.md` - Initial project progress tracking file
- `docs/notes/` directory (if project notes were created) - Project analysis notes

## Notes

- All paths should be relative to project root
- Keep documentation brief and developer-oriented
