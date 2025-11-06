# Review Merge Request

Review a merge request or pull request using your project's agent team.

This command will:
- Analyze git diff or selected files
- Load project's agent team from `ai-squads-docs/team.md`
- Invoke relevant agents based on file types and changes
- Apply code style standards from ai-squads
- Generate comprehensive review feedback

## Usage

Run this command from your project root directory.

You can review:
- Current git diff (uncommitted changes)
- Specific files you've selected
- A branch comparison

The command uses agents assigned to your project to provide specialized feedback.

See `ai-squads/commands/review-merge-request.mdc` for detailed workflow.

