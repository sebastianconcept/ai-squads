---
name: update-docs
alwaysApply: false
---

# Update Docs Workflow

This workflow maintains documentation consistency by tracking when each doc was last updated, analyzing code changes via git diffs, and automatically updating docs to reflect current system behavior.

## Prerequisites

1. Project must be a git repository
2. Project must have `~/docs/{project-name}/` directory
3. Cursor CLI must be available and authenticated (for AI updates)

## Steps

### 1. Verify Prerequisites
- Check if project is a git repository
- Verify `~/docs/{project-name}/` directory exists
- Check if Cursor CLI is available
- Check if `jq` is installed (for JSON handling)

### 2. Initialize or Load Metadata
- Check if `~/docs/{project-name}/docs-metadata.json` exists
- If missing: Initialize metadata for all docs in `~/docs/{project-name}/` (excluding `~/docs/{project-name}/archived/`)
  - Use `git log` to find last-modified commit for each doc
  - Create metadata file with commit hashes
- If exists: Load metadata and scan for new docs to add

### 3. Scan Documentation Files
- Recursively scan `docs/` for all `.md` files (excluding `docs/archived/`)
- Compare found docs with metadata entries
- Identify:
  - Docs already in metadata (will be checked for updates)
  - New docs (will be added to metadata)
  - Docs in metadata that no longer exist (will be removed from metadata)

### 4. Ask About Uncommitted Changes
- Prompt user: "Include uncommitted changes in diff analysis? (y/n)"
- This determines whether to diff against HEAD or working directory

### 5. Process Each Document
For each doc in metadata:

1. **Get Diff**:
   - Get tracked commit hash from metadata
   - Run `git diff {tracked_commit}..{target}` (HEAD or working directory)
   - If diff is empty: Skip doc (idempotency - no changes to process)

2. **Update Document** (if diff exists):
   - Load current doc content
   - Build AI prompt with:
     - Current doc content
     - Git diff showing changes
     - Project context (mission, tech-stack, team)
     - Instructions to update only relevant sections
   - Call Cursor CLI to generate updated doc
   - Normalize content and compute SHA256 hash
   - Compare hash with current doc hash
   - If hashes differ: Write updated doc, update metadata
   - If hashes identical: Skip write (idempotency - doc unchanged)

3. **Update Metadata**:
   - Update `lastChecked` timestamp for all processed docs
   - Only update `commitHash` and `lastUpdated` if doc was actually modified

### 6. Present Summary
- Show total docs processed
- Show docs updated (content changed)
- Show docs skipped (no changes needed)
- Show docs added to metadata (new docs)
- Show docs removed from metadata (deleted docs)
- Show any errors encountered

## Output

After completion:
- `~/docs/{project-name}/docs-metadata.json` - Updated with current commit hashes and timestamps
- Updated documentation files (if changes were needed)
- Summary of what was updated

## Notes

- **Idempotency**: Running the command multiple times with no code changes will not modify any docs
- **Archived Docs**: `~/docs/{project-name}/archived/` is excluded from all operations (historical records)
- **Backward Compatibility**: Works on older projects without metadata (auto-initializes)
- **Content Hashing**: Uses SHA256 hash comparison to prevent unnecessary writes
- **Atomic Updates**: Metadata only updated after successful doc writes

## Usage

Run `/update-docs` command in Cursor from your project root.

The command will:
1. Scan all docs in `~/docs/{project-name}/` (excluding `archived/`)
2. Check which docs need updates based on git diffs
3. Update docs that have changed
4. Update metadata with new commit hashes
