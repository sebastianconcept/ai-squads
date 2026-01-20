# Execution Prompt Template

This template shows how execution prompts are constructed for each agent iteration during autonomous feature execution.

## Prompt Structure

The execution prompt is constructed programmatically by combining the following components in order:

---

## 1. Agent Definition

**Source**: `agents/{agent-name}.md`

**Content**: Full agent definition including:
- Specialization
- Style Guide references
- Rules
- Capabilities
- Skills (if applicable, e.g., browser-verification for UI developer)
- When to Use

**Example**:
```
# UI Developer Agent

## Specialization
Frontend development, UI implementation, JavaScript, Alpine.js, htmx

## Rules
- Implement UI designs accurately
- Write maintainable frontend code
[... full agent definition ...]

## Skills
- browser-verification: Use ~/.cursor/skills/browser-verification/skill.md when working on frontend user stories
```

---

## 2. Project Context

### 2.1 Project Progress

**Source**: `docs/progress.md`

**Content**: Project-level progress digest providing instant context restoration

**Purpose**: Read FIRST to understand:
- Where you are in the project overall (active feature, current story, overall progress)
- Performance metrics (velocity, iterations, success rate, quality metrics)
- Accumulated context (recent decisions, active blockers, pending todos, active investigations)
- Session continuity (last session, resume context)

**Format**:
```
## Project Progress

[Full content from docs/progress.md if exists]

**Key Information:**
- Current Position: [Active feature, current story, status]
- Overall Progress: [X]%
- Performance: [Velocity metrics, trends]
- Blockers: [Active blockers affecting progress]
- Recent Decisions: [Last 5 decisions]
```

**When Available:**
- File is read automatically if it exists
- If file doesn't exist, this section is omitted (no error)
- File is updated after each story completion

### 2.2 Project Notes

**Source**: `docs/notes.md` (single append-only file)

**Content**: Full project notes file (no truncation - all entries are preserved)
- All entries from `docs/notes.md` are included
- Codebase Patterns section (if present) is included
- Since entries are selective and code standards are handled elsewhere, the file won't grow excessively

**Format** (append-only):
```
## Project Notes

Full project notes from `docs/notes.md` (all entries preserved, no truncation):

[Full content from docs/notes.md]
```

**Notes Format** (what agents append):
```
## [Date/Time] - [Story ID]
- What was implemented
- Files changed
- **Learnings for future iterations:**
  - Patterns discovered
  - Gotchas encountered
  - Useful context
---
```

### 2.3 Project Decisions

**Source**: `docs/DECISIONS.md`

**Content**: Architectural and design decisions that inform implementation choices

**Format**:
```
## Project Decisions

[Content from docs/DECISIONS.md]
```

### 2.4 Dev Notes

**Source**: `dev-notes.md` files found in:
- Project root (global notes)
- Feature directory (feature-specific notes)
- Current working directory (directory-specific notes)

**Content**: All relevant dev-notes.md content, clearly labeled by scope

**Format**:
```
## Dev Notes

### Global Project Notes (from root)
[Content from project-root/dev-notes.md if exists]

### Feature Notes (from docs/feature/{name}/dev-notes.md)
[Content from feature-specific dev-notes.md if exists]

### Directory Notes (from current directory)
[Content from current-directory/dev-notes.md if exists]
```

### 2.5 Feature Notes (if available)

**Source**: `docs/notes/features/{feature-name}/` (if feature notes exist)

**Content**: Notes from the current feature's note directory:
- `context.md`: Feature goals, scope, and success criteria
- `todos.md`: Current task status, what needs to be done, what's in progress
- `insights.json`: Relevant learnings and decisions (prioritize evidence-based insights)

**Format**:
```
## Feature Notes

**Source**: `docs/notes/features/{feature-name}/`

### Context
[Content from context.md if exists]

### Current Tasks
[Content from todos.md if exists]

### Insights
[Relevant content from insights.json if exists, prioritized by evidence-based flag]
```

**Behavior**:
- Notes are read automatically before each story execution
- If notes don't exist, this section is omitted
- Agent can read notes proactively during execution using note tools
- Agent can update `todos.md` to track progress during execution

---

## 3. Current Story Details

**Source**: `prd.json` (current story being executed)

**Content**: Full story details from prd.json

**Format**:
```
## Current User Story

**Story ID**: US-001
**Title**: Add user login form
**Type**: frontend
**Priority**: 1
**Dependencies**: [US-000] (all dependencies must be completed)

**Description**: 
As a user, I want to log in to the application so that I can access my account.

**Acceptance Criteria**:
- Login form with email and password fields
- Form validation works correctly
- Error messages display for invalid credentials
- Success redirects to dashboard

**Notes**: [Any existing notes from prd.json]
```

---

## 4. Quality Check Commands

**Source**: `prd.json.quality` object

**Content**: Object with key-value pairs (label → command) that must all pass

**Format**:
```
## Quality Checks

Before marking this story as complete, you must run all of the following quality check commands. **All commands must pass** for the story to be considered complete:

- **typecheck**: `npm run typecheck`
- **lint**: `npm run lint`
- **format**: `npm run format:check`
- **test**: `npm test`

**Important**: 
- Run these commands in the project root directory
- All commands must exit with code 0 (success)
- If any command fails, fix the issues before marking the story complete
- Do not commit code that fails quality checks
- Each key is a label describing the type of check (e.g., "style", "unit", "typecheck", "lint", "format", "test")
```

**Examples for Different Tech Stacks**:
- **Rust**: `{"style": "cargo clippy", "unit": "cargo test"}`
- **JavaScript**: `{"typecheck": "npm run typecheck", "lint": "npm run lint", "format": "npm run format:check", "test": "npm test"}`
- **Python**: `{"lint": "ruff check", "format": "ruff format --check", "test": "pytest"}`

---

## 5. Available Tools

**Content**: Documentation of note management tools available to agents

**Format**:
```
## Available Tools

You have access to the following note management tools for maintaining persistent memory across conversation boundaries:

### Note Tools

1. **write_note(category, name, content, metadata?)**
   - Write a new note file
   - **Parameters**:
     - `category` (string): Category for organization (e.g., "investigations", "features", "projects", "general")
     - `name` (string): Note name (used as filename, sanitized automatically)
     - `content` (string): Markdown content to write
     - `metadata` (optional object): Additional metadata (agent, command, context, commit)
   - **Returns**: `{success: boolean, path: string, error?: string}`
   - **Location**: `docs/notes/{category}/{name}.md`
   - **Example**: `write_note("features", "user-auth", "# Feature: User Authentication\n\nGoals: ...", {agent: "uidev", command: "execute-feature"})`

2. **read_note(path | {category, name})**
   - Read an existing note file
   - **Parameters**: Either full `path` (string) or `{category, name}` (object)
   - **Returns**: `{content: string, metadata: object, error?: string}`
   - **Example**: `read_note({category: "features", name: "user-auth"})` or `read_note("docs/notes/features/user-auth/context.md")`

3. **list_notes(category?)**
   - List available notes
   - **Parameters**: `category` (optional string): Filter by category, or null for all
   - **Returns**: `Array<{name: string, path: string, modified: string, size: number, category: string}>`
   - **Example**: `list_notes("features")` or `list_notes()` for all notes

4. **append_note(path | {category, name}, content)**
   - Append content to an existing note (creates note if it doesn't exist)
   - **Parameters**: Either full `path` (string) or `{category, name}` (object), plus `content` (string)
   - **Returns**: `{success: boolean, path: string, error?: string}`
   - **Example**: `append_note({category: "features", name: "user-auth"}, "## Update\n\nCompleted login form validation.")`

5. **search_notes(query, category?)**
   - Search note content by keyword/phrase
   - **Parameters**: `query` (string): Search term, `category` (optional string): Limit search to category
   - **Returns**: `Array<{path: string, snippet: string, matches: number, lineNumbers: number[]}>`
   - **Example**: `search_notes("authentication", "features")`

### Semantic Note Types

Use these standard note names for structured organization:

- **context.md**: Scope and goals (what the work is for, what problem it solves, success criteria)
- **evidence.md**: Facts gathered (logs, metrics, test results, experimental data)
- **todos.md**: Current tasks (what needs to be done, in progress, blocked)
- **insights.json**: Discoveries with origin and impact (patterns found, decisions made, implications), stored as JSON for better actionability

**Note Location**: `docs/notes/{category}/{name}.md`

**When to Use Notes**:

- **At Feature Start**: Create `context.md` when beginning a new feature (if it doesn't exist)
- **During Task Execution**: Update `todos.md` when task status changes
- **When Discoveries Are Made**: Write to `insights.json` when patterns or decisions are found (mark as evidence-based when supported by first-hand evidence)
- **When Gathering Facts**: Write to `evidence.md` when collecting data (investigations)

**Note Naming Examples**:
- Feature notes: `docs/notes/features/user-authentication/context.md`
- Investigation notes: `docs/notes/memory-leak-2024-01-15/evidence.md` (category: "investigations" in frontmatter)
- Use kebab-case, descriptive names

**Error Handling**: Note operations are non-fatal - if note reading/writing fails, execution continues (notes are helpful but not critical)
```

---

## 6. Execution Instructions

**Content**: Step-by-step instructions for implementing the story

**Format**:
```
## Execution Instructions

You are implementing user story **US-001: Add user login form**.

### Steps:

1. **Read Context First**:
   - Review project notes for relevant patterns and learnings
   - Check dev-notes.md for project-specific guidance
   - **Review feature notes (context.md, todos.md, insights.json) if available above**
   - **Learn from Previous Attempts**: If `insights.json` contains execution attempts, read them to understand (prioritize evidence-based insights):
     - What approaches were tried before
     - What failed and why
     - What worked
     - How to adjust your approach based on previous learnings
   - Understand the current story requirements

2. **Implement the Story**:
   - Follow the agent's rules and style guides
   - Implement all acceptance criteria
   - Write clean, maintainable code
   - Handle edge cases and errors gracefully

3. **For Frontend Stories (if type is "frontend")**:
   - Use the browser-verification skill: `~/.cursor/skills/browser-verification/skill.md`
   - Verify the UI works correctly in a browser
   - Document verification results
   - **Browser verification is required before marking frontend stories as complete**

4. **Run Quality Checks**:
   - Execute all quality check commands from the Quality Checks section
   - Fix any issues found
   - Ensure all commands pass before proceeding

5. **Update Project Documentation**: Before marking story complete, update project docs if needed:
   - **DECISIONS.md** (REQUIRED if architectural/design decisions were made):
     - Add entries for any architectural decisions, design choices, or trade-offs made
     - Use format: Date, Context, Decision, Consequences
     - Document why decisions were made and their implications
     - Location: `docs/DECISIONS.md`
   - **Other project docs** (update as needed):
     - Update `docs/tech-stack.md` if new technologies or dependencies were added
     - Update `docs/roadmap.md` if priorities or plans changed
     - Update any other relevant documentation

6. **Update Notes** (if applicable):
   - Update `todos.md` in `docs/notes/features/{feature-name}/` when task status changes
   - **Document Execution Attempt**: After execution (success or failure), append to `insights.json`:
     - What approach was tried
     - What worked and what didn't
     - What errors or issues were encountered (if any)
     - What was learned from this attempt
     - How the next attempt should differ (if the story didn't complete)
     - Mark insights as `evidenceBased: true` when supported by first-hand evidence (logs, test results, metrics)
     - Include `evidence: {}` object when evidence-based
   - Write to `insights.json` when discoveries are made or decisions are documented
   - Use note tools (write_note, append_note) to maintain persistent memory
   - **Note**: If this is the first story in a feature, create `context.md` with feature goals and scope
   - **Iterative Learning**: Your documentation helps the next execution iteration learn from this attempt

7. **Update Project State**:
   - After successful implementation, quality checks, and documentation updates:
     - Set `passes: true` for this story in `docs/feature/{feature-name}/prd.json`
     - Append progress entry to `docs/notes.md` using our format (see Progress Report Format below)
     - Update `dev-notes.md` in directories where you edited files (only if you discovered NEW patterns not in coding standards - see dev-notes.md guidance)

8. **Commit** (if quality checks pass):
   - Create a commit with message: `feat: [Story ID] - [Story Title]`
   - Example: `feat: US-001 - Add user login form`

### Important Notes:

- **Do not mark the story as complete** (`passes: true`) until:
  - All acceptance criteria are met
  - All quality checks pass
  - Browser verification passes (for frontend stories)
  - Project documentation is updated (especially DECISIONS.md for architectural decisions)
  - Code is committed (if applicable)

- **If you encounter issues**:
  - Document them in the story's `notes` field in prd.json
  - Add details to project notes
  - Do not set `passes: true` if the story is incomplete

- **Dependencies**: Ensure all story dependencies (listed in `dependencies` array) have `passes: true` before starting this story. If dependencies are not complete, pause and wait.

- **Agent Rules**: Follow all rules and capabilities defined in your agent definition above.
```

---

## Complete Example Prompt

Here's how a complete prompt would look for a frontend story:

```
# UI Developer Agent

[Full agent definition from agents/ui-developer.md]

---

## Project Notes

[Full content from docs/notes.md - all entries preserved]

---

## Dev Notes

### Global Project Notes
[Content from project-root/dev-notes.md if exists]

---

## Current User Story

**Story ID**: US-001
**Title**: Add user login form
**Type**: frontend
**Priority**: 1
**Dependencies**: [] (none)

**Description**: 
As a user, I want to log in to the application so that I can access my account.

**Acceptance Criteria**:
- Login form with email and password fields
- Form validation works correctly
- Error messages display for invalid credentials
- Success redirects to dashboard

---

## Quality Checks

Before marking this story as complete, you must run all of the following quality check commands. **All commands must pass**:

- **typecheck**: `npm run typecheck`
- **lint**: `npm run lint`
- **format**: `npm run format:check`
- **test**: `npm test`

---

## Execution Instructions

[Full execution instructions as shown above]
```

---

## Implementation Notes

- The prompt is constructed programmatically by the execution loop script
- Each component is loaded from its source file/directory
- Components are combined in the order shown above
- The complete prompt is passed to Cursor CLI via the `agent -p` command
- Each iteration gets a fresh prompt with updated context
