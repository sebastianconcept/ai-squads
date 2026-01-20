---
name: execute-feature
alwaysApply: false
---

# Execute Feature Workflow

This workflow executes a planned feature autonomously using the execution loop.

## Prerequisites

1. Feature must have been planned (have `~/docs/{project-name}/feature/{feature_name}/prd.json`)
2. Project must be a git repository
3. Cursor CLI must be available and authenticated
4. Project must have quality checks defined in `prd.json.quality`

## Steps

### 1. Get Feature Name
- Prompt user for feature name (or scan `~/docs/{project-name}/feature/` for `prd.json` files in automatic mode)
- Validate feature exists and has `prd.json`
- Read `prd.json` from `~/docs/{project-name}/feature/{feature_name}/prd.json`
- Check for `--dry-run` flag (preview mode without execution)

### 2. Validate prd.json
- Check all required fields are present
- Validate user stories structure
- Validate dependencies reference valid story IDs
- Verify quality check commands exist in `prd.json.quality` object
- Validate agent names exist in `agents/` directory

### 3. Initialize Execution State
- Check git status (must be clean or on feature branch)
- Create feature branch from `prd.json.branchName` if it doesn't exist
- Switch to feature branch
- Verify execution environment (Cursor CLI available)

### 4. Run Execution Loop
- Execute `~/.cursor/scripts/execute-feature.sh {feature_name}` from project root
- Or with `--dry-run` flag: `~/.cursor/scripts/execute-feature.sh {feature_name} --dry-run`
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
- **Does not**: Execute Cursor CLI, run quality checks, commit changes, or modify `prd.json`

**Use cases:**
- Debugging prompt construction
- Understanding what context is sent to agents
- Verifying prompt size before execution
- Reviewing execution plan

## Notes

- Execution uses Cursor CLI in headless mode (`agent -p --force`)
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
