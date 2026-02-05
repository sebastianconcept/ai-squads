---
name: execute-feature
alwaysApply: false
---

# Execute Feature Workflow

This workflow executes a planned feature autonomously using the execution loop.

## Prerequisites

1. Feature must have been planned (have `prd.json` in one of the locations below)
2. Project must be a git repository
3. AI CLI must be available and authenticated (Cursor CLI, Claude CLI, or Gemini CLI)
4. Project must have quality checks defined in `prd.json.quality`

### Where feature docs live

The script looks for feature docs in this order:

- **In-repo (preferred)**: `docs/feature/{feature_name}/prd.json` — if your repo has a `docs/` directory at the project root, this path is used (e.g. `docs/feature/input-service/prd.json`).
- **Home docs**: `~/docs/{project-name}/feature/{feature_name}/prd.json` — otherwise the script uses `$HOME/docs/` and the project name from the git repo (e.g. `~/docs/asset-radar/feature/input-service/prd.json`).

### Bash 4+ (macOS)

The script uses Bash 4+ (associative arrays). macOS’s default `/bin/bash` is 3.2. The script will re-exec with Homebrew’s bash if installed (`/opt/homebrew/bin/bash` or `/usr/local/bin/bash`). If you see `declare: -A: invalid option`, install Bash 4+ and run with it:

```bash
brew install bash
/opt/homebrew/bin/bash ~/.cursor/scripts/execute-feature.sh input-service
```

## Steps

### 1. Get Feature Name
- Prompt user for feature name (or scan `docs/feature/` or `~/docs/{project-name}/feature/` for `prd.json` files in automatic mode)
- Validate feature exists and has `prd.json`
- Read `prd.json` from `docs/feature/{feature_name}/prd.json` (in-repo) or `~/docs/{project-name}/feature/{feature_name}/prd.json`
- Check for `--dry-run` flag (preview mode without execution)

### 2. Validate prd.json
- Check all required fields are present
- Validate user stories structure
- Validate dependencies reference valid story IDs
- Verify quality check commands exist in `prd.json.quality` object
- Validate agent names exist in `definitions/agents/` directory

### 3. Initialize Execution State
- Check git status (must be clean or on feature branch)
- Create feature branch from `prd.json.branchName` if it doesn't exist
- Switch to feature branch
- Verify execution environment (AI CLI available)

### 4. Run Execution Loop
- **Cursor**: Execute `~/.cursor/scripts/execute-feature.sh {feature_name}` from project root
- **Claude/Gemini**: Execute `<ai-squads>/scripts/execute-feature.sh {feature_name}` from project root (where `<ai-squads>` is your local clone path)
- Or with `--dry-run` flag: `{script_path} {feature_name} --dry-run`
- Script handles:
  - Dependency resolution
  - Story selection
  - Agent routing
  - Quality checks
  - Commits
  - Progress tracking
  - Archiving
- **Dry-run mode**: Preview prompts without executing (saves prompts to `~/docs/{project-name}/feature/{feature_name}/dry-run-prompt-{story_id}.md`)

### 5. Monitor Progress
- Execution loop provides progress updates
- Pauses for user intervention if needed
- Shows current story being worked on
- Displays quality check results

## Automatic Mode

If no feature name is provided, the script can:
- Scan `~/docs/{project-name}/feature/` directory for all `prd.json` files
- Automatically pick features with stories that have `passes: false`
- Execute features in priority order (or feature creation order)
- Continue until all planned features are archived
- User can still intervene at any pause point

## Resume Mechanism

If execution pauses (for any reason):
- Current state is preserved in `prd.json`
- Stories with `passes: false` remain in the feature
- Feature directory remains in `~/docs/{project-name}/feature/{name}/` (not archived)
- User can review code, fix issues, adjust `prd.json`
- To resume: Run `/execute-feature` again on the same feature
- Execution loop reads `prd.json` and continues from current progress

## Output

After completion (all stories pass):
- Feature is automatically archived to `~/docs/{project-name}/archived/YYYY-MM-DD-{feature_name}/`
- All commits are on feature branch
- Project notes updated in `~/docs/{project-name}/NOTES.md` (append-only format)
- Quality gates passed for all stories

## Dry-Run Mode

Use `--dry-run` flag to preview execution without actually running it:

```bash
/execute-feature {feature_name} --dry-run
```

**What dry-run does:**
- Builds the prompt for the next story to be executed
- Saves the prompt to `~/docs/{project-name}/feature/{feature_name}/dry-run-prompt-{story_id}.md`
- Shows prompt size (bytes, KB, lines)
- Displays what would happen in real execution
- **Does not**: Execute AI CLI, run quality checks, commit changes, or modify `prd.json`

**Use cases:**
- Debugging prompt construction
- Understanding what context is sent to agents
- Verifying prompt size before execution
- Reviewing execution plan

## Notes

- Execution uses AI CLI in headless mode (platform-specific invocation, e.g. Cursor: `agent -p --force --workspace`, Claude: `--print`, Gemini: `-p`, Codex: `codex exec`)
- Each iteration spawns a fresh AI instance
- Quality checks must pass before commits
- Frontend stories require browser verification
- All learnings are captured in project notes
- Use `--dry-run` to preview prompts without execution

## Security Considerations

### Quality Check Commands

**Important**: Quality check commands from `prd.json.quality` are executed directly in your project directory. 

**Trust Model**:
- Commands in `prd.json.quality` should be trusted - you control this file
- Commands are executed with the same permissions as the user running the script
- Commands run in the project root directory context

**Safety Measures**:
- Commands are executed with a timeout (default: 300 seconds, configurable via `QUALITY_CHECK_TIMEOUT`)
- Commands use `bash -c` for safer execution (compared to `eval`)
- Output is logged to `/tmp/quality-check-{label}.log` for debugging

**Best Practices**:
- Only include trusted quality check commands in `prd.json.quality`
- Review quality check commands before planning features
- Use project-specific commands (e.g., `npm run lint` not raw shell commands)
- Consider validating commands during `/plan-feature` if needed

**Configuration**:
- Set `QUALITY_CHECK_TIMEOUT` environment variable to adjust timeout (in seconds)
- Default timeout: 300 seconds (5 minutes)
