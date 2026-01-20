#!/bin/bash

# Update Docs Script
# Maintains documentation consistency by tracking commit hashes and updating docs via AI

set -e

# Source common functions
. "$HOME/.cursor/scripts/common.sh"

DOCS_DIR="$(get_docs_dir)"
METADATA_FILE="$DOCS_DIR/docs-metadata.json"
MODEL="${MODEL:-auto}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[update-docs]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        error "Not a git repository. Please run from a git repository root."
        exit 1
    fi
    
    # DOCS_DIR is created by get_docs_dir(), so it should always exist
    if [ ! -d "$DOCS_DIR" ]; then
        error "Docs directory not found: $DOCS_DIR"
        exit 1
    fi
    
    if ! command -v agent &> /dev/null; then
        error "Cursor CLI not found. Please install Cursor CLI first."
        exit 1
    fi
    
    if ! command -v jq &> /dev/null; then
        error "jq not found. Please install jq for JSON parsing."
        exit 1
    fi
    
    success "Prerequisites check passed"
}

# Normalize content for comparison
normalize_content() {
    local content="$1"
    # Strip trailing whitespace, normalize line endings, remove multiple blank lines
    echo "$content" | sed 's/[[:space:]]*$//' | tr -d '\r' | awk 'NF || prev {print; prev=NF} {prev=NF}'
}

# Compute SHA256 hash of content
compute_content_hash() {
    local content="$1"
    local normalized
    normalized=$(normalize_content "$content")
    
    if command -v sha256sum >/dev/null 2>&1; then
        echo -n "$normalized" | sha256sum | cut -d' ' -f1
    elif command -v shasum >/dev/null 2>&1; then
        echo -n "$normalized" | shasum -a 256 | cut -d' ' -f1
    else
        error "Neither sha256sum nor shasum found. Cannot compute content hash."
        exit 1
    fi
}

# Read metadata file
read_metadata() {
    if [ -f "$METADATA_FILE" ]; then
        # Validate JSON before reading
        if ! jq empty "$METADATA_FILE" 2>/dev/null; then
            warn "Metadata file is corrupted, backing up and re-initializing..."
            mv "$METADATA_FILE" "${METADATA_FILE}.backup.$(date +%s)"
            echo '{"version":"1.0","lastChecked":"","docs":{}}'
        else
            cat "$METADATA_FILE"
        fi
    else
        echo '{"version":"1.0","lastChecked":"","docs":{}}'
    fi
}

# Write metadata file (atomic)
write_metadata() {
    local metadata="$1"
    local temp_file="${METADATA_FILE}.tmp"
    
    echo "$metadata" | jq . > "$temp_file"
    
    # Validate the JSON we just wrote
    if ! jq empty "$temp_file" 2>/dev/null; then
        error "Failed to write valid JSON metadata"
        rm -f "$temp_file"
        return 1
    fi
    
    mv "$temp_file" "$METADATA_FILE"
    return 0
}

# Get current commit hash
get_current_commit() {
    git rev-parse HEAD
}

# Check for uncommitted changes
has_uncommitted_changes() {
    ! git diff-index --quiet HEAD -- 2>/dev/null
}

# Get commit hash where doc was last modified
get_doc_last_commit() {
    local doc_path="$1"
    local full_path="$DOCS_DIR/$doc_path"
    
    if [ ! -f "$full_path" ]; then
        echo ""
        return
    fi
    
    # Try to find last commit that modified this file
    local commit_hash
    commit_hash=$(git log -1 --format=%H -- "$full_path" 2>/dev/null || echo "")
    
    if [ -z "$commit_hash" ]; then
        # File not in git history, use current HEAD
        get_current_commit
    else
        echo "$commit_hash"
    fi
}

# Scan docs directory (excluding archived)
scan_docs_directory() {
    find "$DOCS_DIR" -name "*.md" -type f ! -path "*/archived/*" | while read -r full_path; do
        # Get relative path from docs/
        local rel_path="${full_path#$DOCS_DIR/}"
        echo "$rel_path"
    done
}

# Get git diff for a doc
get_diff_for_doc() {
    local tracked_commit="$1"
    local target_commit="$2"
    
    if [ "$target_commit" = "working" ]; then
        git diff "$tracked_commit" -- .
    else
        git diff "$tracked_commit" "$target_commit" -- .
    fi
}

# Update doc with AI
update_doc_with_ai() {
    local doc_path="$1"
    local full_path="$DOCS_DIR/$doc_path"
    local tracked_commit="$2"
    local target_commit="$3"
    local git_diff="$4"
    
    # Load current doc content
    local current_content
    current_content=$(cat "$full_path")
    
    # Load project context
    local mission_content=""
    local tech_stack_content=""
    local team_content=""
    
    [ -f "$DOCS_DIR/MISSION.md" ] && mission_content=$(cat "$DOCS_DIR/MISSION.md")
    [ -f "$DOCS_DIR/TECH-STACK.md" ] && tech_stack_content=$(cat "$DOCS_DIR/TECH-STACK.md")
    [ -f "$DOCS_DIR/TEAM.md" ] && team_content=$(cat "$DOCS_DIR/TEAM.md")
    
    # Build AI prompt
    local prompt_file
    prompt_file=$(mktemp)
    
    cat > "$prompt_file" <<EOF
## Document Update Task

**Document**: $doc_path
**Purpose**: Update this document to reflect current system behavior based on code changes
**Last Updated**: At commit $tracked_commit

## Current Document Content

\`\`\`markdown
$current_content
\`\`\`

## Code Changes Since Last Update

**Git Diff** (from commit $tracked_commit to $target_commit):

\`\`\`
$git_diff
\`\`\`

## Project Context

**Mission**:
\`\`\`markdown
$mission_content
\`\`\`

**Tech Stack**:
\`\`\`markdown
$tech_stack_content
\`\`\`

**Team**:
\`\`\`markdown
$team_content
\`\`\`

## Update Instructions

Analyze the git diff and update the document to reflect current system behavior. Ensure:
- Documentation accurately describes the current system
- Changes are explained clearly for junior developers
- Information is structured for AI agents to understand
- Preserve existing document structure and format where possible
- **Only update sections that need changes based on the diff**
- **Preserve unchanged sections exactly as they are**
- Maintain consistency with other project documentation

Generate the complete updated document content. Return only the markdown content, no explanations or code blocks around it.
EOF
    
    # Call Cursor CLI
    log "Updating $doc_path via AI..."
    local updated_content_raw
    local project_root
    project_root="$(git rev-parse --show-toplevel)"
    updated_content_raw=$(agent -p --force --workspace "$project_root" --model "$MODEL" "$(cat "$prompt_file")" 2>&1)
    
    # Clean up prompt file
    rm -f "$prompt_file"
    
    # Validate updated content
    if [ -z "$updated_content_raw" ]; then
        error "AI returned empty content for $doc_path"
        return 1
    fi
    
    # Clean up AI response - remove markdown code blocks if present
    local updated_content
    updated_content="$updated_content_raw"
    # Remove markdown code block markers if present
    updated_content=$(echo "$updated_content" | sed '/^```markdown$/,/^```$/d' | sed '/^```$/,/^```$/d')
    # Remove leading/trailing whitespace
    updated_content=$(echo "$updated_content" | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}')
    
    # Final validation
    if [ -z "$updated_content" ]; then
        error "AI returned empty content after cleanup for $doc_path"
        return 1
    fi
    
    # Check if content actually changed
    local current_hash
    local updated_hash
    current_hash=$(compute_content_hash "$current_content")
    updated_hash=$(compute_content_hash "$updated_content")
    
    if [ "$current_hash" = "$updated_hash" ]; then
        log "Doc unchanged (content hash identical), skipping write for $doc_path"
        echo "unchanged"
        return 0
    fi
    
    # Write updated content
    echo "$updated_content" > "$full_path"
    echo "updated"
    return 0
}

# Initialize metadata for older projects
initialize_metadata() {
    log "Initializing metadata for older project..."
    
    local metadata
    metadata=$(read_metadata)
    local current_commit
    current_commit=$(get_current_commit)
    local current_date
    current_date=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    
    # Scan all docs
    local doc_count=0
    while IFS= read -r doc_path; do
        [ -z "$doc_path" ] && continue
        
        local doc_commit
        doc_commit=$(get_doc_last_commit "$doc_path")
        
        if [ -z "$doc_commit" ]; then
            doc_commit="$current_commit"
        fi
        
        # Get commit date in ISO8601 format directly from git
        local commit_date
        commit_date=$(git log -1 --date=iso8601-strict --format=%cd "$doc_commit" 2>/dev/null | sed 's/$/Z/' || echo "$current_date")
        if [ -z "$commit_date" ] || [ "$commit_date" = "Z" ]; then
            commit_date="$current_date"
        fi
        
        # Add to metadata
        metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
            --arg commit "$doc_commit" \
            --arg updated "$commit_date" \
            --arg checked "$current_date" \
            '.docs[$path] = {
                "lastUpdated": $updated,
                "commitHash": $commit,
                "lastChecked": $checked
            }')
        
        doc_count=$((doc_count + 1))
    done < <(scan_docs_directory)
    
    metadata=$(echo "$metadata" | jq --arg date "$current_date" '.lastChecked = $date')
    
    write_metadata "$metadata"
    success "Initialized metadata for $doc_count docs"
}

# Update metadata with new/missing docs
update_metadata_with_new_docs() {
    local metadata
    metadata=$(read_metadata)
    local current_commit
    current_commit=$(get_current_commit)
    local current_date
    current_date=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local added_count=0
    
    # Scan all docs
    while IFS= read -r doc_path; do
        [ -z "$doc_path" ] && continue
        
        # Check if already in metadata (idempotency check)
        local exists
        exists=$(echo "$metadata" | jq -r --arg path "$doc_path" '.docs[$path] // empty')
        
        if [ -n "$exists" ]; then
            continue  # Skip existing entries
        fi
        
        # New doc, add to metadata
        local doc_commit
        doc_commit=$(get_doc_last_commit "$doc_path")
        
        if [ -z "$doc_commit" ]; then
            doc_commit="$current_commit"
        fi
        
        # Get commit date in ISO8601 format directly from git
        local commit_date
        commit_date=$(git log -1 --date=iso8601-strict --format=%cd "$doc_commit" 2>/dev/null | sed 's/$/Z/' || echo "$current_date")
        if [ -z "$commit_date" ] || [ "$commit_date" = "Z" ]; then
            commit_date="$current_date"
        fi
        
        metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
            --arg commit "$doc_commit" \
            --arg updated "$commit_date" \
            --arg checked "$current_date" \
            '.docs[$path] = {
                "lastUpdated": $updated,
                "commitHash": $commit,
                "lastChecked": $checked
            }')
        
        added_count=$((added_count + 1))
        log "Added new doc to metadata: $doc_path"
    done < <(scan_docs_directory)
    
    # Remove docs from metadata that no longer exist (excluding archived)
    local removed_count=0
    local doc_paths
    doc_paths=$(echo "$metadata" | jq -r '.docs | keys[]')
    
    while IFS= read -r doc_path; do
        [ -z "$doc_path" ] && continue
        
        local full_path="$DOCS_DIR/$doc_path"
        
        # Check if file exists and is not in archived
        if [ ! -f "$full_path" ] || [[ "$doc_path" == archived/* ]]; then
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" 'del(.docs[$path])')
            removed_count=$((removed_count + 1))
            log "Removed doc from metadata: $doc_path"
        fi
    done <<< "$doc_paths"
    
    metadata=$(echo "$metadata" | jq --arg date "$current_date" '.lastChecked = $date')
    
    if [ $added_count -gt 0 ] || [ $removed_count -gt 0 ]; then
        write_metadata "$metadata"
        if [ $added_count -gt 0 ]; then
            success "Added $added_count new docs to metadata"
        fi
        if [ $removed_count -gt 0 ]; then
            log "Removed $removed_count docs from metadata"
        fi
    fi
    
    echo "$metadata"
}

# Main execution
main() {
    check_prerequisites
    
    log "Starting documentation update process..."
    
    # Initialize or update metadata
    local metadata
    if [ ! -f "$METADATA_FILE" ]; then
        initialize_metadata
        metadata=$(read_metadata)
    else
        metadata=$(update_metadata_with_new_docs)
    fi
    
    # Ask about uncommitted changes
    local include_uncommitted="n"
    if has_uncommitted_changes; then
        echo ""
        read -p "Include uncommitted changes in diff analysis? (y/n): " -n 1 -r include_uncommitted
        echo ""
    fi
    
    local target_commit
    if [ "$include_uncommitted" = "y" ] || [ "$include_uncommitted" = "Y" ]; then
        target_commit="working"
    else
        target_commit=$(get_current_commit)
    fi
    
    # Process each doc
    local updated_count=0
    local skipped_count=0
    local error_count=0
    local current_date
    current_date=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    
    local doc_paths
    doc_paths=$(echo "$metadata" | jq -r '.docs | keys[]')
    local total_docs
    total_docs=$(echo "$doc_paths" | wc -l | tr -d ' ')
    local processed=0
    
    log "Processing $total_docs docs..."
    echo ""
    
    while IFS= read -r doc_path; do
        [ -z "$doc_path" ] && continue
        
        processed=$((processed + 1))
        log "[$processed/$total_docs] Processing $doc_path..."
        
        local full_path="$DOCS_DIR/$doc_path"
        
        # Check if file still exists
        if [ ! -f "$full_path" ]; then
            warn "Doc no longer exists: $doc_path (will be removed from metadata)"
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" 'del(.docs[$path])')
            continue
        fi
        
        # Get tracked commit
        local tracked_commit
        tracked_commit=$(echo "$metadata" | jq -r --arg path "$doc_path" '.docs[$path].commitHash // empty')
        
        if [ -z "$tracked_commit" ]; then
            warn "No tracked commit for $doc_path, skipping"
            continue
        fi
        
        # Check if commit still exists
        if ! git cat-file -e "$tracked_commit" 2>/dev/null; then
            warn "Tracked commit $tracked_commit no longer exists for $doc_path, re-initializing..."
            local new_commit
            new_commit=$(get_doc_last_commit "$doc_path")
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
                --arg commit "$new_commit" \
                --arg date "$current_date" \
                '.docs[$path].commitHash = $commit | .docs[$path].lastUpdated = $date')
            tracked_commit="$new_commit"
        fi
        
        # Get diff
        local git_diff
        git_diff=$(get_diff_for_doc "$tracked_commit" "$target_commit")
        
        # Idempotency check: skip if diff is empty
        if [ -z "$git_diff" ]; then
            log "  No changes detected, skipping $doc_path"
            # Update lastChecked timestamp
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
                --arg date "$current_date" \
                '.docs[$path].lastChecked = $date')
            skipped_count=$((skipped_count + 1))
            continue
        fi
        
        # Update doc with AI
        local update_result
        update_result=$(update_doc_with_ai "$doc_path" "$tracked_commit" "$target_commit" "$git_diff" 2>&1)
        
        if [ $? -ne 0 ] || [ -z "$update_result" ]; then
            error "Failed to update $doc_path"
            error_count=$((error_count + 1))
            continue
        fi
        
        if [ "$update_result" = "unchanged" ]; then
            log "  Doc unchanged after AI analysis, skipping write"
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
                --arg date "$current_date" \
                '.docs[$path].lastChecked = $date')
            skipped_count=$((skipped_count + 1))
        elif [ "$update_result" = "updated" ]; then
            # Update metadata with new commit hash
            local new_commit
            new_commit=$(get_current_commit)
            metadata=$(echo "$metadata" | jq --arg path "$doc_path" \
                --arg commit "$new_commit" \
                --arg date "$current_date" \
                '.docs[$path].commitHash = $commit | .docs[$path].lastUpdated = $date | .docs[$path].lastChecked = $date')
            success "  Updated $doc_path"
            updated_count=$((updated_count + 1))
        fi
    done <<< "$doc_paths"
    
    # Write final metadata
    metadata=$(echo "$metadata" | jq --arg date "$current_date" '.lastChecked = $date')
    write_metadata "$metadata"
    
    # Summary
    echo ""
    log "=========================================="
    log "Summary"
    log "=========================================="
    log "Total docs processed: $total_docs"
    log "Docs updated: $updated_count"
    log "Docs skipped (no changes): $skipped_count"
    log "Errors: $error_count"
    
    if [ $updated_count -eq 0 ] && [ $error_count -eq 0 ]; then
        success "All docs are up to date!"
    fi
}

# Run main function
main "$@"
