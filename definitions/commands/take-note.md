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

**Tool Implementation**: This command documents APIs that agents implement using standard file operations (`read_file`, `write_file`, `list_dir`, `grep`). No custom tool implementations needed.

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

### Creating Feature Context Note with Introspection

Markdown notes can also include introspection sections:

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
- Sessions persist across page reloads

---

## Introspection & Retrospection

### What Went Well
- Iterative learning approach worked well - each attempt built on previous learnings
- Evidence-based insights helped prioritize correct approaches
- Note-taking system preserved context across memory resets

### What Could Be Improved
- First attempt could have been avoided by reading project-level lessons
- API error format discovery took 4 iterations - could have tested API earlier
- Should have checked localStorage pattern in codebase before first attempt

### Recommendations
- Always read project-level lessons before starting new features
- Check codebase for existing patterns (localStorage, API endpoints, error formats) first
- Test external dependencies (APIs, localStorage) early in implementation"
```

**Alternative**: Store introspection in YAML frontmatter:

```yaml
---
created: 2024-01-15T10:00:00Z
updated: 2024-01-16T16:00:00Z
category: features
commit: ghi789jkl012
introspection:
  whatWentWell:
    - "Iterative learning approach worked well"
    - "Evidence-based insights helped prioritize correct solutions"
  whatCouldBeImproved:
    - "Should have read project-level lessons before starting"
    - "Could have tested API endpoints earlier"
  recommendations:
    - "Always read project-level lessons before starting"
    - "Check codebase for existing patterns first"
---

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

### Creating Insights JSON with Introspection

**REQUIRED**: Every execution attempt MUST include introspection. Here's an example:

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
      "id": "insight-006",
      "timestamp": "2024-01-15T15:30:00Z",
      "type": "execution-attempt",
      "story": "US-001",
      "attempt": 3,
      "title": "Login Form Implementation - Attempt 3",
      "description": "Used localStorage with null check, proper error handling",
      "result": "success",
      "learning": "localStorage + null checks + error handling = working solution",
      "evidenceBased": true,
      "lesson": true,
      "iterations": 3,
      "evidence": {
        "source": "quality-check",
        "observation": "All quality checks passed",
        "files": ["src/components/LoginForm.tsx"],
        "commit": "ghi789jkl012"
      },
      "introspection": {
        "whatWentWell": [
          "Reading previous insights.json helped avoid repeating the localStorage vs cookies mistake",
          "Null check pattern from attempt 2 was the key missing piece",
          "Systematic approach of trying one thing at a time made debugging easier"
        ],
        "whatCouldBeImproved": [
          "Should have checked codebase patterns (localStorage) before first attempt",
          "Could have tested localStorage access patterns earlier",
          "Should have read project-level lessons before starting"
        ],
        "recommendations": [
          "Future agents: Read project-level lessons before implementing auth features",
          "Check localStorage.getItem('token') pattern in existing codebase first",
          "Always include null checks when reading from localStorage",
          "Test localStorage access early in the implementation process"
        ]
      }
    }
  ]
}'
```

### Adding Introspection to Existing insights.json

When updating an existing `insights.json` file, follow this pattern:

1. **Read existing file**: Use `read_file("~/docs/{project-name}/notes/{feature-name}/insights.json")`
2. **Parse JSON structure**: Extract the `insights` array
3. **Create new insight with introspection**:
   ```json
   {
     "id": "insight-XXX",
     "timestamp": "2024-01-15T16:00:00Z",
     "type": "execution-attempt",
     "story": "US-001",
     "attempt": 4,
     "title": "Login Form Implementation - Attempt 4",
     "description": "...",
     "result": "failed",
     "introspection": {
       "whatWentWell": ["observation 1", "observation 2"],
       "whatCouldBeImproved": ["observation 1"],
       "recommendations": ["recommendation 1", "recommendation 2"]
     }
   }
   ```
4. **Append to insights array**: Add the new insight to the existing array
5. **Update metadata.updated**: Set to current timestamp
6. **Write back**: Use `write_file` to save the updated JSON

**Important**: The `introspection` object is REQUIRED for every execution attempt. It must include:
- `whatWentWell: string[]` - At least one observation
- `whatCouldBeImproved: string[]` - At least one observation  
- `recommendations: string[]` - At least one actionable recommendation

## Error Handling

- **Missing files**: Return empty result (not an error) for read operations
- **Invalid paths**: Return error with path validation details
- **Note writing failures**: Log error but continue execution (notes are helpful but not critical)
- **Non-fatal**: All note operations are non-fatal - execution continues even if notes fail

## Related Commands

- `diagnose-issue` - Creates investigation notes
- `adopt-project` - Creates project notes
- `execute-feature` - Reads and updates feature notes
