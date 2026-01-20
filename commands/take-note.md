---
name: take-note
alwaysApply: false
---

# Take Note Command

This command provides a standalone interface for creating and managing notes in the AI Self-Notes System. It can be called by agents, users, or the execution loop to maintain persistent memory across conversation boundaries.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. `~/docs/{project-name}/notes/` directory exists (created automatically if missing)

## When to Use

Invoke this command when you need to:
- Create a new note for an investigation, feature, or project
- Append to an existing note
- Read notes to restore context
- List available notes
- Search notes by content

**Use Cases:**
- **Agents**: During feature execution, investigation, or project analysis
- **Users**: Manually documenting findings or context
- **Execution Loop**: Automatically reading notes before story execution
- **Commands**: diagnose-issue, adopt-project, execute-feature can call this

## How It Works

### Command Syntax

```bash
# Create a new note
@take-note write category="features" name="user-auth" content="# Feature: User Authentication\n\nGoals: ..."

# Append to existing note
@take-note append category="features" name="user-auth" content="## Update\n\nCompleted login form."

# Read a note
@take-note read category="features" name="user-auth"

# List notes
@take-note list category="features"

# Search notes
@take-note search query="authentication" category="features"
```

### Note Operations

#### 1. Write Note

**Purpose**: Create a new note file with content

**Parameters**:
- `category` (required): Category for organization ("investigations", "features", "projects", "general")
- `name` (required): Note name (used as filename, sanitized automatically)
- `content` (required): Markdown content to write
- `metadata` (optional): Additional metadata (agent, command, context, commit)

**Example**:
```markdown
@take-note write category="features" name="user-login-form" content="# Feature: User Login Form

## Goals
- Secure user authentication
- Password-based login
- Session management

## Scope
- Login form UI
- Form validation
- API integration"
```

**Creates**: `~/docs/{project-name}/notes/features/user-login-form-CONTEXT.md` (or in subdirectory if name includes path)

**File Format**:
```markdown
---
created: 2024-01-15T10:00:00Z
updated: 2024-01-15T10:00:00Z
category: features
agent: uidev
command: take-note
context: Feature planning
commit: abc123def456
---

[Content here]
```

#### 2. Append Note

**Purpose**: Add content to an existing note (creates note if it doesn't exist)

**Parameters**:
- `category` (required): Category name
- `name` (required): Note name
- `content` (required): Markdown content to append

**Example**:
```markdown
@take-note append category="features" name="user-login-form" content="## Update 2024-01-15T14:30:00Z

Completed login form validation. All acceptance criteria met."
```

**Appends with timestamp separator**:
```markdown
---
## 2024-01-15T14:30:00Z
[New content]
```

#### 3. Read Note

**Purpose**: Read an existing note file

**Parameters**:
- `category` (required): Category name
- `name` (required): Note name
- OR `path` (required): Full path to note file

**Example**:
```markdown
@take-note read category="features" name="user-login-form"
```

**Returns**: Full note content with metadata

#### 4. List Notes

**Purpose**: List available notes, optionally filtered by category

**Parameters**:
- `category` (optional): Filter by category, or omit for all notes

**Example**:
```markdown
@take-note list category="features"
```

**Returns**: Array of note metadata (name, path, modified time, size, category)

#### 5. Search Notes

**Purpose**: Search note content by keyword/phrase

**Parameters**:
- `query` (required): Search term or phrase
- `category` (optional): Limit search to category

**Example**:
```markdown
@take-note search query="authentication" category="features"
```

**Returns**: Array of matches with context snippets

## Semantic Note Types

Use these standard note names for structured organization:

- **CONTEXT.md**: Scope and goals (what the work is for, what problem it solves, success criteria)
- **EVIDENCE.md**: Facts gathered (logs, metrics, test results, experimental data)
- **TODOS.md**: Current tasks (what needs to be done, in progress, blocked)
- **insights.json**: Discoveries with origin and impact (patterns found, decisions made, implications), stored as JSON for better actionability

**Note**: `insights.json` uses JSON format, not markdown. Use `write_note` with JSON content.

## Note Naming Conventions

### Feature Notes
- Format: `{user-story-id}-{description}-{type}.md` or `{feature-name}/{type}.md`
- Example: `US-001-user-login-form-CONTEXT.md` or `user-login-form/CONTEXT.md`
- Category: "features" (stored in frontmatter)

### Investigation Notes
- Format: `{issue-id}-{description}-{type}.md` or `{issue-id}/{type}.md`
- Example: `memory-leak-2024-01-15-investigating-memory-growth-EVIDENCE.md` or `memory-leak-2024-01-15/EVIDENCE.md`
- Category: "investigations" (stored in frontmatter)

### Project Notes
- Format: `{project-name}-{description}-{type}.md` or `{project-name}/{type}.md`
- Example: `payment-service-architecture-analysis-insights.json`
- Category: "projects" (stored in metadata)

## Implementation

**Tool Implementation**: This command documents APIs that agents implement using standard Cursor file operations (`read_file`, `write_file`, `list_dir`, `grep`). No custom tool implementations needed.

**File Operations**:
- Use `read_file` to read note content
- Use `write_file` to create/update notes with YAML frontmatter
- Use `list_dir` to list notes in directories
- Use `grep` or text search for searching note content

**YAML Parsing**: Agents parse YAML frontmatter using standard parsing (or include as-is in markdown)

**JSON Parsing**: Agents parse `insights.json` using standard JSON parsing

## Integration with Other Commands

### execute-feature
- Automatically reads feature notes before each story execution
- Can call `take-note` to update `TODOS.md` or `insights.json` during execution

### diagnose-issue
- Creates investigation notes when starting investigation
- Updates `EVIDENCE.md` as evidence is gathered
- Documents hypotheses in `insights.json`

### adopt-project
- Creates project notes during project analysis
- Documents architecture discoveries in `insights.json`

## Examples

### Creating Feature Context Note
```markdown
@take-note write category="features" name="user-login-form/context" content="# Feature: User Login Form

## Problem Statement
Users need to authenticate to access their accounts.

## Goals
- Secure user authentication
- Password-based login
- Session management

## Success Criteria
- Users can log in with valid credentials
- Invalid credentials show appropriate errors
- Sessions persist across page reloads"
```

### Appending to Todos
```markdown
@take-note append category="features" name="user-login-form/todos" content="## 2024-01-15T14:30:00Z

- [x] Implement login form UI
- [x] Add form validation
- [ ] Connect to authentication API
- [ ] Add error handling"
```

### Creating Insights JSON
```markdown
@take-note write category="features" name="user-login-form/insights" content='{
  "metadata": {
    "created": "2024-01-15T10:00:00Z",
    "updated": "2024-01-15T15:30:00Z",
    "category": "features",
    "commit": "abc123def456"
  },
  "insights": [
    {
      "id": "insight-001",
      "timestamp": "2024-01-15T13:30:00Z",
      "type": "discovery",
      "title": "API Endpoint Structure",
      "description": "Authentication endpoint is /api/v1/auth/login",
      "evidenceBased": true,
      "evidence": {
        "source": "api-documentation",
        "observation": "API docs specify /api/v1/auth/login endpoint"
      }
    }
  ]
}'
```

## Error Handling

- **Missing files**: Return empty result (not an error) for read operations
- **Invalid paths**: Return error with path validation details
- **Note writing failures**: Log error but continue execution (notes are helpful but not critical)
- **Non-fatal**: All note operations are non-fatal - execution continues even if notes fail

## Related Commands

- `diagnose-issue` - Creates investigation notes
- `adopt-project` - Creates project notes
- `execute-feature` - Reads and updates feature notes
