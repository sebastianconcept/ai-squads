#!/bin/bash

# Execute Feature Script
# Autonomous execution loop for features with prd.json
# Requires Bash 4+ (for associative arrays). On macOS, re-execs with Homebrew bash if needed.

set -e

# macOS: system bash is 3.x and doesn't support declare -A; re-exec with Homebrew bash if available
if [[ "${OSTYPE:-}" == darwin* ]] && [[ "${BASH_VERSINFO[0]:-0}" -lt 4 ]]; then
    if [[ -x /opt/homebrew/bin/bash ]]; then
        exec /opt/homebrew/bin/bash "$0" "$@"
    elif [[ -x /usr/local/bin/bash ]]; then
        exec /usr/local/bin/bash "$0" "$@"
    else
        echo "This script requires Bash 4+. On macOS install with: brew install bash" >&2
        echo "Then run: /opt/homebrew/bin/bash $0 $*" >&2
        exit 1
    fi
fi

# Source common functions (prefer repo scripts/common.sh when run from a repo that has it)
SCRIPT_PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -n "$SCRIPT_PROJECT_ROOT" && -f "$SCRIPT_PROJECT_ROOT/scripts/common.sh" ]]; then
    . "$SCRIPT_PROJECT_ROOT/scripts/common.sh"
else
    . "$HOME/.cursor/scripts/common.sh"
fi

# Parse command-line arguments
FEATURE_NAME=""
DRY_RUN=false
NON_INTERACTIVE="${NON_INTERACTIVE:-false}"
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --non-interactive)
            NON_INTERACTIVE=true
            shift
            ;;
        --help|-h)
            echo "Usage: execute-feature.sh [feature_name] [--dry-run] [--non-interactive]"
            echo ""
            echo "Execute a planned feature autonomously using the execution loop."
            echo ""
            echo "Arguments:"
            echo "  feature_name      Name of the feature to execute (optional, will scan if not provided)"
            echo "  --dry-run         Preview the prompt without executing (saves prompt to file)"
            echo "  --non-interactive Skip confirmation prompts (e.g. for MCP allowlist)"
            echo "  --help, -h        Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  MAX_ITERATIONS         Maximum iterations per feature (default: 20)"
            echo "  MODEL                  Cursor CLI model to use (default: auto)"
            echo "  QUALITY_CHECK_TIMEOUT  Timeout for quality checks in seconds (default: 300)"
            echo "  NON_INTERACTIVE        Set to true to skip confirmation prompts"
            exit 0
            ;;
        *)
            if [ -z "$FEATURE_NAME" ]; then
                FEATURE_NAME="$1"
            else
                error "Unknown argument: $1"
                error "Use --help for usage information"
                exit 1
            fi
            shift
            ;;
    esac
done

DOCS_DIR="$(get_docs_dir)"
# Prefer in-repo docs/ when present (e.g. docs/feature/input-service/prd.json)
PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -n "$PROJECT_ROOT" && -d "$PROJECT_ROOT/docs" ]]; then
    DOCS_DIR="$PROJECT_ROOT/docs"
fi
CURSOR_DIR="$HOME/.cursor"
AGENTS_DIR="$CURSOR_DIR/agents"
# When running from ai-squads repo (e.g. ./scripts/execute-feature.sh), use repo agents if host agents missing
if [[ ! -d "$AGENTS_DIR" && -n "$SCRIPT_PROJECT_ROOT" && -d "$SCRIPT_PROJECT_ROOT/definitions/agents" ]]; then
    AGENTS_DIR="$SCRIPT_PROJECT_ROOT/definitions/agents"
fi
SKILLS_DIR="$CURSOR_DIR/skills"
MAX_ITERATIONS="${MAX_ITERATIONS:-20}"
MODEL="${MODEL:-auto}"
QUALITY_CHECK_TIMEOUT="${QUALITY_CHECK_TIMEOUT:-300}"  # 5 minutes default timeout
MAX_PARALLEL_STORIES="${MAX_PARALLEL_STORIES:-5}"  # Max stories to run in parallel
INTROSPECTION_THRESHOLD="${INTROSPECTION_THRESHOLD:-20}"  # Default: 20 iterations before enhanced introspection

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${BLUE}[$timestamp][execute-feature]${NC} $1"
}

error() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${RED}[$timestamp][ERROR]${NC} $1" >&2
}

success() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${GREEN}[$timestamp][SUCCESS]${NC} $1"
}

warn() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${YELLOW}[$timestamp][WARN]${NC} $1"
}

# Safe jq execution with error handling
safe_jq() {
    local query="$1"
    local file="$2"
    local result
    local exit_code
    
    result=$(jq -r "$query" "$file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -ne 0 ]; then
        error "jq query failed: $query"
        error "Error: $result"
        return 1
    fi
    
    echo "$result"
    return 0
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    if [ "$DRY_RUN" = "false" ]; then
        if ! command -v agent &> /dev/null; then
            error "Cursor CLI not found. Please install Cursor CLI first."
            exit 1
        fi
        
        # Check Cursor CLI version (Phase 1)
        if ! check_cursor_cli_version; then
            # Non-fatal warning if version check fails, but continue
            warn "Cursor CLI version check failed. Some new features may not be available."
        fi
    fi
    
    if ! command -v jq &> /dev/null; then
        error "jq not found. Please install jq for JSON parsing."
        exit 1
    fi
    
    if [ ! -d "$AGENTS_DIR" ]; then
        error "Agents directory not found: $AGENTS_DIR"
        error "Run ./scripts/install_or_update.sh from the ai-squads repo to install agents from definitions/agents to ~/.cursor/agents"
        exit 1
    fi
    
    if [ "$DRY_RUN" = "true" ]; then
        warn "DRY-RUN MODE: No actual execution will occur"
    fi
    
    success "Prerequisites check passed"
}

# Check Cursor CLI version (Phase 1)
check_cursor_cli_version() {
    local min_version="2026.01.16"
    local current_version
    current_version=$(agent --version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    
    if [ -z "$current_version" ]; then
        warn "Could not determine Cursor CLI version."
        return 1
    fi
    
    # Compare versions using sort -V
    if [ "$(printf '%s\n' "$min_version" "$current_version" | sort -V | head -1)" != "$min_version" ]; then
        warn "Cursor CLI version $current_version is older than required minimum $min_version"
        warn "Mode selection features (--mode=plan, --mode=ask) require version $min_version or later"
        warn "Please update Cursor CLI if you encounter errors."
        return 1
    fi
    
    success "Cursor CLI version verified: $current_version"
    return 0
}

# Select mode based on story complexity (Phase 1)
select_mode() {
    local story_id=$1
    local deps_count
    deps_count=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .dependencies | length" "$PRD_JSON" 2>/dev/null || echo "0")
    local acc_criteria
    acc_criteria=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .acceptanceCriteria | join(\" \")" "$PRD_JSON" 2>/dev/null || echo "")
    local story_type
    story_type=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .type" "$PRD_JSON" 2>/dev/null || echo "feature")
    local acc_length=${#acc_criteria}
    
    # Complex story indicators
    if [ "$deps_count" -gt 2 ] || [ "$acc_length" -gt 500 ] || [[ "$story_type" == "infrastructure" || "$story_type" == "integration" ]]; then
        echo "plan"  # Start with planning for complex stories
    else
        echo "default"
    fi
}

# Select model based on story characteristics (Phase 1)
select_model() {
    local story_id=$1
    local story_type
    story_type=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .type" "$PRD_JSON" 2>/dev/null || echo "feature")
    local deps_count
    deps_count=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .dependencies | length" "$PRD_JSON" 2>/dev/null || echo "0")
    local acc_criteria
    acc_criteria=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .acceptanceCriteria | join(\" \")" "$PRD_JSON" 2>/dev/null || echo "")
    local acc_length=${#acc_criteria}
    
    # Check for model mapping in PRD (Phase 3 Improvement)
    # format: "modelMapping": {"infrastructure": "o3-mini", "feature": "sonnet-4.5"}
    local mapped_model
    mapped_model=$(jq -r ".modelMapping.\"$story_type\" // empty" "$PRD_JSON" 2>/dev/null)
    if [ -n "$mapped_model" ] && [ "$mapped_model" != "null" ]; then
        echo "$mapped_model"
        return
    fi

    # Use environment variable MODEL as default if provided
    local default_model="${MODEL:-auto}"
    
    # If specific model is set in env, respect it unless it's 'auto'
    if [ "$default_model" != "auto" ]; then
        echo "$default_model"
        return
    fi

    # Dynamic selection logic
    # Fast path: simple tasks
    # COMMENTING DUE TO not being so necessary right now and overspending otherwise
    # if [ "$deps_count" -eq 0 ] && [ "$acc_length" -lt 200 ] && [[ "$story_type" == "config" || "$story_type" == "library" ]]; then
    #     echo "composer-1"
    # # Quality path: complex stories
    # elif [ "$deps_count" -gt 2 ] || [ "$acc_length" -gt 500 ] || [[ "$story_type" == "infrastructure" || "$story_type" == "integration" ]]; then
    #     echo "sonnet-4.5"
    # else
    #     echo "auto"
    # fi
}

# Parse structured output for completion status (Phase 1)
parse_agent_output() {
    local output_file=$1
    local story_id=$2
    
    # Try to parse JSON output
    # Verified structure: {"type":"result","subtype":"success","is_error":false,"result":"..."}
    local json_output
    json_output=$(jq -r '.' "$output_file" 2>/dev/null)
    
    if [ $? -eq 0 ] && [ -n "$json_output" ] && [ "$json_output" != "null" ]; then
        local type
        type=$(echo "$json_output" | jq -r '.type // empty' 2>/dev/null)
        local subtype
        subtype=$(echo "$json_output" | jq -r '.subtype // empty' 2>/dev/null)
        local is_error
        is_error=$(echo "$json_output" | jq -r '.is_error // empty' 2>/dev/null)
        
        if [ "$type" = "result" ]; then
            if [ "$subtype" = "success" ] && [ "$is_error" = "false" ]; then
                return 0  # Success
            else
                return 1  # Failure
            fi
        fi
    fi
    
    # Fallback to log parsing if JSON invalid or not found
    if grep -qiE "(completed|success|done|finished)" "$output_file" >/dev/null 2>&1; then
        # Check if files were actually changed (git diff)
        if git diff --quiet; then
            # No changes might mean it didn't do anything or it's a no-op
            # But sometimes agents just analyze. However for user stories we expect changes.
            return 0
        else
            return 0
        fi
    fi
    
    return 1 # Default to failure
}

# Find all features with incomplete stories
find_incomplete_features() {
    local features_dir="$DOCS_DIR/feature"
    local incomplete_features=""
    
    if [ ! -d "$features_dir" ]; then
        echo ""
        return
    fi
    
    # Find all prd.json files and check if they have incomplete stories
    while IFS= read -r prd_file; do
        local feature_name
        feature_name=$(basename "$(dirname "$prd_file")")
        
        # Check if feature has incomplete stories
        local incomplete_count
        incomplete_count=$(jq -r '.userStories[] | select(.passes == false) | .id' "$prd_file" 2>/dev/null | wc -l | tr -d ' ')
        
        if [ "$incomplete_count" -gt 0 ]; then
            if [ -n "$incomplete_features" ]; then
                incomplete_features="$incomplete_features"$'\n'
            fi
            incomplete_features="$incomplete_features$feature_name|$prd_file"
        fi
    done < <(find "$features_dir" -name "prd.json" 2>/dev/null | sort)
    
    echo "$incomplete_features"
}

# Find feature or scan for prd.json files
find_feature() {
    if [ -z "$FEATURE_NAME" ]; then
        log "No feature name provided, scanning for prd.json files..."
        FEATURES=$(find "$DOCS_DIR/feature" -name "prd.json" 2>/dev/null | head -1)
        if [ -z "$FEATURES" ]; then
            error "No prd.json files found in ~/docs/{project-name}/feature/"
            exit 1
        fi
        FEATURE_NAME=$(basename "$(dirname "$FEATURES")")
        log "Found feature: $FEATURE_NAME"
    fi
    
    PRD_JSON="$DOCS_DIR/feature/$FEATURE_NAME/prd.json"
    
    if [ ! -f "$PRD_JSON" ]; then
        error "prd.json not found: $PRD_JSON"
        exit 1
    fi
    
    log "Using feature: $FEATURE_NAME"
}

# Get runnable quality commands only (one "label|command" per line).
# Supports: (1) quality.commands = array of command strings — run each; (2) legacy quality = object
# (key -> command string or array). quality.notes is documentation only and is never run.
get_quality_command_lines() {
    jq -r '
      if .quality then
        if ((.quality.commands | type?) == "array") and ((.quality.commands | length) > 0) then
          [.quality.commands[] | select(. != null and (. | tostring) != "")] | to_entries[] | "quality-\(.key + 1)|\(.value)"
        else
          .quality | to_entries[] | select(.key != "notes") | .key as $k | (if (.value | type) == "array" then .value[] else .value end) | select(. != null and (. | tostring) != "") | "\($k)|\(.)"
        end
      else
        empty
      end
    ' "$PRD_JSON" 2>/dev/null
}

# Normalize agent alias to canonical agent name
normalize_agent_name() {
    local agent_name="$1"
    case "$agent_name" in
        "steve"|"uidev")
            echo "ui-developer"
            ;;
        "rusty")
            echo "rust-specialist"
            ;;
        "bob")
            echo "jobs-to-be-done"
            ;;
        *)
            # Return as-is if not an alias
            echo "$agent_name"
            ;;
    esac
}

# Validate prd.json
validate_prd() {
    log "Validating prd.json..."
    
    if ! jq empty "$PRD_JSON" 2>/dev/null; then
        error "Invalid JSON in prd.json"
        exit 1
    fi
    
    # Check required fields (project is optional; used for display only)
    local required_fields=("featureName" "branchName" "description" "quality" "userStories")
    for field in "${required_fields[@]}"; do
        if ! jq -e ".$field" "$PRD_JSON" > /dev/null 2>&1; then
            error "Missing required field: $field"
            exit 1
        fi
    done
    
    # Validate agents exist and paths are safe
    local agents
    agents=$(jq -r '.userStories[].agent' "$PRD_JSON" 2>/dev/null | sort -u)
    if [ $? -ne 0 ]; then
        error "Failed to extract agents from prd.json"
        exit 1
    fi
    
    for agent in $agents; do
        if [ -z "$agent" ]; then
            error "Empty agent name found in prd.json"
            exit 1
        fi
        
        # Normalize agent alias to canonical name
        local canonical_agent
        canonical_agent=$(normalize_agent_name "$agent")
        
        # Validate path to prevent directory traversal
        local agent_path="$AGENTS_DIR/$canonical_agent.md"
        if ! validate_path "$agent_path" "$AGENTS_DIR"; then
            error "Invalid agent path detected (possible security issue): $agent"
            exit 1
        fi
        
        if [ ! -f "$agent_path" ]; then
            error "Agent not found: $agent (normalized to: $canonical_agent, expected $agent_path)"
            exit 1
        fi
    done
    
    # Detect dependency cycles
    log "Checking for dependency cycles..."
    if ! detect_dependency_cycles "$PRD_JSON"; then
        error "Dependency cycles detected in prd.json. Please fix dependencies before execution."
        exit 1
    fi
    
    # Validate quality: at least one runnable command (quality.commands array or legacy key->command; notes are not run)
    local quality_checks
    quality_checks=$(get_quality_command_lines)
    if [ -z "$quality_checks" ]; then
        error "No quality checks defined in prd.json.quality (at least one runnable command required; use quality.commands array or legacy key->command; quality.notes is documentation only)"
        exit 1
    fi
    local has_empty=false
    while IFS='|' read -r label command; do
        if [ -z "$label" ] || [ -z "$command" ]; then
            error "Quality check has empty label or command"
            has_empty=true
        fi
    done <<< "$quality_checks"
    if [ "$has_empty" = "true" ]; then
        error "Quality checks must have non-empty labels and commands"
        exit 1
    fi
    
    success "prd.json validation passed"
}

# Initialize git branch
init_git_branch() {
    local branch_name
    branch_name=$(jq -r '.branchName' "$PRD_JSON")
    
    log "Initializing git branch: $branch_name"
    
    # Check for uncommitted changes before branch operations
    if [ -n "$(git status --porcelain)" ]; then
        error "Working directory has uncommitted changes"
        error "Please commit or stash your changes before executing a feature"
        error "Current changes:"
        git status --short
        exit 1
    fi
    
    # Check if branch exists
    if git rev-parse --verify "$branch_name" >/dev/null 2>&1; then
        log "Branch exists, checking out: $branch_name"
        git checkout "$branch_name"
    else
        log "Creating new branch: $branch_name"
        git checkout -b "$branch_name"
    fi
}

# Get notes file (single append-only file: NOTES.md)
get_notes_file() {
    echo "$DOCS_DIR/NOTES.md"
}

# Append to project notes (append-only format)
append_notes() {
    local notes_file
    notes_file=$(get_notes_file)
    local message="$1"
    
    mkdir -p "$(dirname "$notes_file")"
    
    # Initialize file if it doesn't exist
    if [ ! -f "$notes_file" ]; then
        echo "# Project Notes" > "$notes_file"
        echo "" >> "$notes_file"
        echo "Started: $(date)" >> "$notes_file"
        echo "---" >> "$notes_file"
        echo "" >> "$notes_file"
    fi
    
    # Append in format: ## [Date/Time] - [Story ID/Feature]
    echo "## $(date '+%Y-%m-%d %H:%M:%S') - $message" >> "$notes_file"
    echo "" >> "$notes_file"
}

# Validate path to prevent directory traversal attacks
validate_path() {
    local path="$1"
    local base_dir="$2"
    
    # Resolve to absolute path
    local abs_path
    abs_path=$(cd "$(dirname "$path")" 2>/dev/null && pwd)/$(basename "$path") 2>/dev/null || echo ""
    
    if [ -z "$abs_path" ]; then
        return 1
    fi
    
    # Check if path is within base directory
    case "$abs_path" in
        "$base_dir"|"$base_dir"/*)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Detect cycles in dependency graph using DFS
detect_dependency_cycles() {
    local prd_json="$1"
    
    # Get all story IDs
    local all_stories
    all_stories=$(jq -r '.userStories[].id' "$prd_json" 2>/dev/null || echo "")
    
    if [ -z "$all_stories" ]; then
        return 0  # No stories, no cycles
    fi
    
    # Use a temporary file to track visited and recursion stack (bash arrays are tricky to pass)
    local visited_file
    visited_file=$(mktemp)
    local rec_stack_file
    rec_stack_file=$(mktemp)
    
    # Helper function for DFS cycle detection
    check_cycle_dfs() {
        local story_id="$1"
        
        # Check if in recursion stack (cycle detected)
        if grep -q "^${story_id}$" "$rec_stack_file" 2>/dev/null; then
            error "Dependency cycle detected involving story: $story_id"
            rm -f "$visited_file" "$rec_stack_file"
            return 1
        fi
        
        # Check if already visited
        if grep -q "^${story_id}$" "$visited_file" 2>/dev/null; then
            return 0  # Already processed, no cycle from here
        fi
        
        # Mark as visited and add to recursion stack
        echo "$story_id" >> "$visited_file"
        echo "$story_id" >> "$rec_stack_file"
        
        # Get dependencies and check each
        local deps
        deps=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .dependencies[]?" "$prd_json" 2>/dev/null || echo "")
        
        for dep in $deps; do
            if [ -n "$dep" ]; then
                if ! check_cycle_dfs "$dep"; then
                    rm -f "$visited_file" "$rec_stack_file"
                    return 1  # Cycle found in dependency
                fi
            fi
        done
        
        # Remove from recursion stack (last line only)
        sed -i '' '$d' "$rec_stack_file" 2>/dev/null || sed -i '$d' "$rec_stack_file" 2>/dev/null || true
        
        return 0
    }
    
    # Check each story for cycles
    local cycle_found=false
    for story_id in $all_stories; do
        if ! grep -q "^${story_id}$" "$visited_file" 2>/dev/null; then
            if ! check_cycle_dfs "$story_id"; then
                cycle_found=true
                break
            fi
        fi
    done
    
    rm -f "$visited_file" "$rec_stack_file"
    
    if [ "$cycle_found" = "true" ]; then
        return 1
    fi
    
    return 0
}

# Create worktree for a specific story (Phase 2)
create_story_worktree() {
    local story_id="$1"
    local branch_name="$2"
    local project_root="$3"
    
    # Worktree path: outside project to avoid conflicts
    local worktree_base
    worktree_base="$(dirname "$project_root")/worktrees"
    mkdir -p "$worktree_base"
    
    # Unique worktree name with timestamp to avoid collisions
    local worktree_name="${FEATURE_NAME}-${story_id}-$(date +%s)"
    local worktree_path="$worktree_base/$worktree_name"
    
    log "Creating worktree for story $story_id at $worktree_path"
    
    # Create worktree from current branch state
    if ! git worktree add "$worktree_path" "$branch_name" > /dev/null 2>&1; then
        error "Failed to create worktree for story $story_id"
        return 1
    fi
    
    echo "$worktree_path"
    return 0
}

# Cleanup worktree (Phase 2)
cleanup_worktree() {
    local worktree_path="$1"
    local story_id="$2"
    local force="${3:-false}"
    
    if [ ! -d "$worktree_path" ]; then
        return 0
    fi
    
    log "Cleaning up worktree for story $story_id: $worktree_path"
    
    local force_flag=""
    if [ "$force" = "true" ]; then
        force_flag="--force"
    fi
    
    if git worktree remove "$worktree_path" $force_flag > /dev/null 2>&1; then
        success "Worktree removed for story $story_id"
        return 0
    else
        warn "Failed to remove worktree for story $story_id using git command. Attempting manual cleanup..."
        rm -rf "$worktree_path"
        git worktree prune
        return 0
    fi
}

# Find current wave of stories (Phase 2)
find_current_wave() {
    local prd_json="$1"
    # Find the lowest wave number that has available stories (passes == false)
    local min_wave
    min_wave=$(jq -r '.userStories[] | select(.passes == false) | .wave' "$prd_json" 2>/dev/null | sort -n | head -1)
    echo "$min_wave"
}

# Find stories that can run in parallel in the current wave (Phase 2)
find_parallel_stories() {
    local prd_json="$1"
    local wave="$2"
    
    # Find all stories in the same wave that:
    # - Don't pass yet
    # - Have all dependencies satisfied (all dependent stories have passes == true)
    
    jq -r ".userStories as \$all | .userStories[] | select(.wave == $wave and .passes == false) | .id as \$id | .dependencies as \$deps | if (\$deps | length == 0) or (\$deps | all(. as \$d | (\$all[] | select(.id == \$d) | .passes))) then \$id else empty end" "$prd_json" 2>/dev/null
}

# Global variable to track active worktrees for cleanup on exit
declare -A ACTIVE_WORKTREES

# Cleanup on script exit (Phase 2 - Safety)
cleanup_on_exit() {
    local exit_code=$?
    
    if [ ${#ACTIVE_WORKTREES[@]} -gt 0 ]; then
        echo ""
        warn "Script interrupted or failed. Cleaning up active worktrees..."
        for story_id in "${!ACTIVE_WORKTREES[@]}"; do
            local worktree_path="${ACTIVE_WORKTREES[$story_id]}"
            cleanup_worktree "$worktree_path" "$story_id" "true"
        done
    fi
    
    # Final cleanup of any rules
    if [ -n "$FEATURE_NAME" ]; then
        cleanup_feature_rules "$FEATURE_NAME"
    fi
    
    exit $exit_code
}

# Register trap
trap cleanup_on_exit EXIT INT TERM

# Analyze worktree diff for coherence (Phase 2 - Gate 3)
analyze_worktree_diff() {
    local story_id="$1"
    local worktree_path="$2"
    
    log "Story $story_id: Analyzing diff for coherence (Gate 3)..."
    
    local changed_files
    changed_files=$(cd "$worktree_path" && git diff --name-only HEAD)
    
    if [ -z "$changed_files" ]; then
        warn "Story $story_id: No files changed in worktree."
        return 1
    fi
    
    # Gate 4: No Unrelated Changes (System Files)
    if echo "$changed_files" | grep -qE "execute-feature\.sh|prd\.json|\.git/config"; then
        # Check if it's an infrastructure story
        local story_type
        story_type=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .type" "$PRD_JSON")
        if [ "$story_type" != "infrastructure" ]; then
            error "Story $story_id: Unauthorized modification of system files detected: $changed_files"
            return 1
        fi
    fi
    
    # Gate 3: File Scope Validation (Agent Role)
    local agent_name
    agent_name=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .agent" "$PRD_JSON")
    
    case "$agent_name" in
        "ui-developer"|"steve"|"uidev")
            # UI agents shouldn't touch core backend or scripts unless specified
            if echo "$changed_files" | grep -qE "scripts/|server/|backend/"; then
                warn "Story $story_id: UI agent modified non-UI files. Proceeding with caution (quality checks will be final arbiter)."
            fi
            ;;
        "backend-developer"|"bob"|"rusty")
            if echo "$changed_files" | grep -qE "static/|public/|frontend/|\.css$"; then
                warn "Story $story_id: Backend agent modified UI files. Proceeding with caution."
            fi
            ;;
    esac
    
    success "Story $story_id: Diff analysis passed."
    return 0
}

# Run quality checks in a specific worktree (Phase 2)
run_quality_checks_in_worktree() {
    local worktree_path="$1"
    local story_id="$2"
    
    log "Running quality checks in worktree for story $story_id"
    
    # Run from worktree root (repo root for this checkout) so scripts/quality.sh etc. resolve correctly
    local quality_checks
    quality_checks=$(get_quality_command_lines)
    
    local failed=false
    while IFS='|' read -r label command; do
        [ -z "$label" ] && continue
        log "Running quality check in worktree: $label"
        
        # One command per line (quality.commands array); never run quality.notes; CWD = worktree root
        if ! (cd "$worktree_path" && timeout "$QUALITY_CHECK_TIMEOUT" bash -c "$command" > /tmp/quality-check-${label}-${story_id}.log 2>&1); then
            error "Quality check failed in worktree: $label"
            failed=true
        else
            success "Quality check passed in worktree: $label"
        fi
    done <<< "$quality_checks"
    
    [ "$failed" = "false" ]
}

# Resolve merge conflict using an agent (Phase 2 - Conflict Resolver)
resolve_merge_conflict_with_agent() {
    local story_id="$1"
    local worktree_path="$2"
    local conflict_files="$3"
    
    log "Summoning Conflict Resolver agent for story $story_id..."
    
    local story_title
    story_title=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .title" "$PRD_JSON")
    local story_desc
    story_desc=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .description" "$PRD_JSON")
    
    # Create a specialized prompt for conflict resolution
    local resolver_prompt_file=$(mktemp)
    cat > "$resolver_prompt_file" <<EOF
# Conflict Resolver Agent

You are an expert Senior Integrator agent. Your task is to resolve git merge conflicts between the current main branch and a new feature implementation.

## Context
- **Feature**: $FEATURE_NAME
- **Story ID**: $story_id
- **Story Title**: $story_title
- **Story Description**: $story_desc

## Task
1. Analyze the merge conflicts in the following files:
$conflict_files

2. Resolve the conflicts such that the requirements of story $story_id are satisfied while preserving the existing functionality in the main branch.

3. **IMPORTANT**: You must remove all conflict markers (<<<<<<<, =======, >>>>>>>) and provide a clean, working implementation.

4. Run the quality checks for this project after resolution to ensure everything still works.

## Quality Checks to Run:
$(get_quality_command_lines | while IFS='|' read -r label cmd; do echo "- $label: $cmd"; done)

Resolve the conflicts now.
EOF

    # Run the agent in the main project directory (where the conflict is)
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    
    if agent -p --force --workspace "$project_root" --model "sonnet-4.5" "$(cat "$resolver_prompt_file")" > /tmp/conflict-resolver-${story_id}.log 2>&1; then
        success "Conflict Resolver agent completed its task"
        rm -f "$resolver_prompt_file"
        return 0
    else
        error "Conflict Resolver agent failed to resolve conflicts"
        cat /tmp/conflict-resolver-${story_id}.log
        rm -f "$resolver_prompt_file"
        return 1
    fi
}

# Merge worktree result sequentially (Phase 2)
merge_worktree_sequential() {
    local story_id="$1"
    local worktree_path="$2"
    local project_root="$3"
    
    log "Merging worktree for story $story_id into main branch"
    
    # Get story title for commit message
    local story_title
    story_title=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .title" "$PRD_JSON")
    
    # Commit changes in worktree first
    cd "$worktree_path" || return 1
    
    if [ -z "$(git status --porcelain)" ]; then
        warn "No changes to commit in worktree for story $story_id"
        cd - > /dev/null
        return 1
    fi
    
    # Commit in worktree
    local commit_msg="feat: $story_id - $story_title"
    if ! git add -A && git commit -m "$commit_msg" > /dev/null 2>&1; then
        error "Failed to commit changes in worktree for story $story_id"
        cd - > /dev/null
        return 1
    fi
    
    # Get commit hash
    local commit_hash
    commit_hash=$(git rev-parse HEAD)
    
    cd - > /dev/null
    
    # Now merge into main branch
    cd "$project_root" || return 1
    
    # Try to cherry-pick the commit from the worktree
    if git cherry-pick "$commit_hash" > /tmp/merge-${story_id}.log 2>&1; then
        success "Successfully merged worktree for story $story_id via cherry-pick"
    else
        warn "Cherry-pick conflict detected for story $story_id"
        
        # Check for unmerged paths
        local conflict_files
        conflict_files=$(git status --short | grep "^UU" | awk '{print $2}')
        
        if [ -n "$conflict_files" ]; then
            if resolve_merge_conflict_with_agent "$story_id" "$worktree_path" "$conflict_files"; then
                # Agent claimed success, check if markers are gone
                if grep -rE "<<<<<<<|=======|>>>>>>>" $conflict_files >/dev/null 2>&1; then
                    error "Conflict markers still present after agent resolution"
                    
                    # Phase 3 Improvement: Manual Fallback
                    if [ "$NON_INTERACTIVE" != "true" ]; then
                        warn "Conflict resolution agent failed. Pausing for manual intervention..."
                        warn "Conflict in: $conflict_files"
                        warn "Please resolve the conflict manually in the project root and then press ENTER."
                        read -r
                    else
                        git cherry-pick --abort
                        return 1
                    fi
                fi
                
                # Markers gone, try to continue
                git add .
                if git cherry-pick --continue --no-edit > /dev/null 2>&1; then
                    success "Conflict resolved by agent and merge completed"
                else
                    error "Failed to continue cherry-pick even after agent resolution"
                    git cherry-pick --abort
                    return 1
                fi
            else
                # Phase 3 Improvement: Manual Fallback
                if [ "$NON_INTERACTIVE" != "true" ]; then
                    warn "Conflict Resolver agent failed. Pausing for manual intervention..."
                    warn "Please resolve the conflict manually in the project root and then press ENTER."
                    read -r
                    if [ -z "$(git status --short | grep "^UU")" ]; then
                        git add .
                        if git cherry-pick --continue --no-edit > /dev/null 2>&1; then
                            success "Conflict resolved manually and merge completed"
                        else
                            error "Failed to continue cherry-pick after manual resolution"
                            return 1
                        fi
                    else
                        error "Conflict still exists. Aborting."
                        git cherry-pick --abort
                        return 1
                    fi
                else
                    error "Conflict resolution failed"
                    git cherry-pick --abort
                    return 1
                fi
            fi
        else
            error "Cherry-pick failed but no unmerged paths found"
            cat /tmp/merge-${story_id}.log
            git cherry-pick --abort
            return 1
        fi
    fi
    
    return 0
}

# Update PROGRESS.md current position (Phase 3 Improvement)
update_progress_position() {
    local status="$1"
    local focus="$2"
    local progress_file="$DOCS_DIR/PROGRESS.md"
    
    if [ ! -f "$progress_file" ]; then
        return
    fi
    
    # Update Status and Current Position using sed
    # Use | as delimiter so values (e.g. story IDs, paths) can contain /
    # Matches lines like "**Status:** ..." or "**Current Position:** ..."
    sed -i '' "s|\*\*Status:\*\*.*|\*\*Status:\*\* $status|" "$progress_file"
    sed -i '' "s|\*\*Active Feature:\*\*.*|\*\*Active Feature:\*\* $FEATURE_NAME|" "$progress_file"
    sed -i '' "s|\*\*Current Story:\*\*.*|\*\*Current Story:\*\* $focus|" "$progress_file"
    sed -i '' "s|\*\*Last activity:\*\*.*|\*\*Last activity:\*\* $(date -u +'%Y-%m-%d %H:%M UTC') — $status|" "$progress_file"
}

# Display parallel execution progress (Phase 3 Improvement)
show_parallel_progress() {
    local wave="$1"
    local story_ids=("$@")
    # Remove wave from array
    story_ids=("${story_ids[@]:1}")
    
    echo ""
    log "┌─────────────────────────────────────────────────────────────────┐"
    log "│ PARALLEL EXECUTION WAVE: $wave                                  "
    log "├─────────────────────────────────────────────────────────────────┤"
    log "│ STORIES:                                                        "
    for id in "${story_ids[@]}"; do
        local title
        title=$(jq -r ".userStories[] | select(.id == \"$id\") | .title" "$PRD_JSON")
        local agent
        agent=$(jq -r ".userStories[] | select(.id == \"$id\") | .agent" "$PRD_JSON")
        log "│ - $id: $title ($agent)"
    done
    log "└─────────────────────────────────────────────────────────────────┘"
    echo ""
    
    update_progress_position "Parallel Wave $wave in progress" "Executing ${#story_ids[@]} stories concurrently"
}

# Execute parallel stories in a wave (Phase 2)
execute_parallel_stories() {
    local wave="$1"
    
    log "Executing parallel stories for wave $wave"
    
    local parallel_stories
    parallel_stories=$(find_parallel_stories "$PRD_JSON" "$wave")
    
    if [ -z "$parallel_stories" ]; then
        log "No parallel stories found for wave $wave"
        return 0
    fi
    
    # Limit number of parallel stories
    local story_ids=($parallel_stories)
    local total_available=${#story_ids[@]}
    
    if [ "$total_available" -gt "$MAX_PARALLEL_STORIES" ]; then
        warn "Found $total_available parallel stories, but limiting to $MAX_PARALLEL_STORIES"
        story_ids=("${story_ids[@]:0:$MAX_PARALLEL_STORIES}")
    fi
    
    # Show visual progress (Phase 3 Improvement)
    show_parallel_progress "$wave" "${story_ids[@]}"
    
    log "Starting parallel execution for ${#story_ids[@]} stories"
    
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    local branch_name
    branch_name=$(jq -r '.branchName' "$PRD_JSON")
    
    declare -A worktree_paths
    declare -A worktree_pids
    
    # Launch all stories in background
    for story_id in "${story_ids[@]}"; do
        local worktree_path
        worktree_path=$(create_story_worktree "$story_id" "$branch_name" "$project_root")
        
        if [ $? -ne 0 ] || [ -z "$worktree_path" ]; then
            continue
        fi
        
        worktree_paths["$story_id"]="$worktree_path"
        ACTIVE_WORKTREES["$story_id"]="$worktree_path" # Track for global cleanup
        
        # Build prompt
        local prompt_file=$(mktemp)
        build_prompt "$story_id" > "$prompt_file"
        
        # Get mode and model
        local current_mode=$(select_mode "$story_id")
        local current_model=$(select_model "$story_id")
        local mode_flag=""
        [ "$current_mode" != "default" ] && mode_flag="--mode $current_mode"
        
        # Execute in background
        (
            log "Story $story_id: Starting agent execution..."
            if agent -p --force --workspace "$worktree_path" --model "$current_model" $mode_flag --output-format json "$(cat "$prompt_file")" > "/tmp/cursor-output-${story_id}.log" 2>&1; then
                if parse_agent_output "/tmp/cursor-output-${story_id}.log" "$story_id"; then
                    # Phase 2 - Multi-Gate Validation
                    # Gate 1: Quality checks (already implemented)
                    if run_quality_checks_in_worktree "$worktree_path" "$story_id"; then
                        # Gate 3: Diff Coherence Check
                        if analyze_worktree_diff "$story_id" "$worktree_path"; then
                            exit 0
                        fi
                    fi
                fi
            fi
            exit 1
        ) &
        worktree_pids["$story_id"]=$!
    done
    
    # Wait for all to complete
    local successful_stories=()
    for story_id in "${!worktree_pids[@]}"; do
        wait "${worktree_pids[$story_id]}"
        if [ $? -eq 0 ]; then
            successful_stories+=("$story_id")
        else
            error "Story $story_id failed in parallel execution or validation gates"
            
            # Phase 1: Failure Diagnosis for parallel failures
            # We can't easily distinguish transient from permanent here without checking logs
            # For simplicity, we'll run diagnosis if logs exist
            local agent_log="/tmp/cursor-output-${story_id}.log"
            if [ -f "$agent_log" ]; then
                # Only diagnose if it's not a transient issue
                if ! grep -qi "timeout\|network\|rate limit\|429\|503\|502" "$agent_log" 2>/dev/null; then
                    run_failure_diagnosis "$story_id" "$(cat "$agent_log")"
                fi
            fi
        fi
    done
    
    # Merge successful ones sequentially
    local merged_count=0
    for story_id in "${successful_stories[@]}"; do
        if merge_worktree_sequential "$story_id" "${worktree_paths[$story_id]}" "$project_root"; then
            update_story_status "$story_id" "true" "Completed via parallel execution in wave $wave"
            append_notes "Story $story_id completed (parallel, wave $wave)"
            merged_count=$((merged_count + 1))
        fi
        # Remove from active tracking after merge attempt
        unset ACTIVE_WORKTREES["$story_id"]
    done
    
    # Cleanup all worktrees
    for story_id in "${!worktree_paths[@]}"; do
        cleanup_worktree "${worktree_paths[$story_id]}" "$story_id" "true"
        unset ACTIVE_WORKTREES["$story_id"]
    done
    
    [ "$merged_count" -gt 0 ]
}

# Governance-based MCP management (Phase 3)
manage_mcp_servers() {
    local action=$1 # "enable" or "disable"
    local allowlist="mcp-allowlist.json"
    
    # Project-root mcp.json is the primary source
    if [ -f "mcp.json" ]; then
        local servers=$(jq -r '.mcpServers | keys[]' mcp.json 2>/dev/null)
        for server in $servers; do
            if [ "$action" = "enable" ]; then
                # Check allowlist
                if ! grep -q "\"$server\"" "$allowlist" 2>/dev/null; then
                    warn "MCP Server '$server' is not in the allowlist."
                    
                    local should_allow=false
                    if [ "$NON_INTERACTIVE" = "true" ]; then
                        success "Non-interactive mode: Automatically allowing server '$server'."
                        should_allow=true
                    else
                        echo -n "Allow this server for autonomous use? (y/n): "
                        read -r response
                        if [ "$response" = "y" ]; then
                            should_allow=true
                        fi
                    fi
                    
                    if [ "$should_allow" = "true" ]; then
                        # Add to allowlist
                        if [ ! -f "$allowlist" ]; then echo "[]" > "$allowlist"; fi
                        local tmp=$(mktemp)
                        jq ". += [\"$server\"]" "$allowlist" > "$tmp" && mv "$tmp" "$allowlist"
                        success "Added '$server' to allowlist."
                    else
                        warn "Skipping '$server'."
                        continue
                    fi
                fi
                log "Enabling MCP server: $server"
                agent mcp enable "$server" > /dev/null 2>&1
            else
                log "Disabling MCP server: $server"
                agent mcp disable "$server" > /dev/null 2>&1
            fi
        done
    fi
}

# Synthesize active rules from PRD and Notes (Phase 3)
synthesize_feature_rules() {
    local feature_name=$1
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    local rules_dir="$project_root/.cursor/rules"
    mkdir -p "$rules_dir"
    local rules_file="$rules_dir/${feature_name}.md"
    
    log "Synthesizing active memory for feature $feature_name..."
    
    # 1. Start with PRD constraints
    echo "# Active Rules: $feature_name" > "$rules_file"
    echo "" >> "$rules_file"
    echo "## PRD Constraints" >> "$rules_file"
    # Extract rules from prd.json if they exist
    if jq -e '.rules' "$PRD_JSON" >/dev/null 2>&1; then
        jq -r '.rules[] | "- \(.)"' "$PRD_JSON" >> "$rules_file"
    else
        echo "- Follow general project standards." >> "$rules_file"
    fi
    echo "" >> "$rules_file"
    
    # 2. Add recent learnings from insights.json (Note System Integration)
    local insights_file="$DOCS_DIR/feature/${feature_name}/insights.json"
    if [ -f "$insights_file" ]; then
        echo "## Recent Learnings (from Note System)" >> "$rules_file"
        # Extract lessons from insights.json
        if jq -e '.[] | select(.lesson == true)' "$insights_file" >/dev/null 2>&1; then
            jq -r '.[] | select(.lesson == true) | "- \(.content)"' "$insights_file" >> "$rules_file"
        else
            echo "- No specific lessons captured yet." >> "$rules_file"
        fi
        echo "" >> "$rules_file"
    fi
    
    # 3. Add project-level patterns from NOTES.md
    local notes_file="$DOCS_DIR/NOTES.md"
    if [ -f "$notes_file" ]; then
        echo "## Project Patterns" >> "$rules_file"
        # Extract the '## Codebase Patterns' section using sed
        sed -n '/## Codebase Patterns/,/##/p' "$notes_file" | grep "^-" >> "$rules_file" 2>/dev/null || true
        echo "" >> "$rules_file"
    fi

    # 4. Consolidated Project Memory (Phase 3 - Refined)
    local notes_root="$DOCS_DIR/notes"
    if [ -d "$notes_root" ]; then
        echo "## Consolidated Project Memory" >> "$rules_file"
        
        # Pull from recent Chapters (the most senior level of memory)
        # Scan specifically in docs/{project-name}/notes/*/chapters/
        local chapters
        chapters=$(find "$notes_root" -path "*/chapters/*" -name "[0-9][0-9]-*.md" 2>/dev/null | sort -r | head -n 3)
        if [ -n "$chapters" ]; then
            for chapter in $chapters; do
                local chap_name=$(basename "$chapter")
                local chap_summary=$(grep -m 1 "^# " "$chapter" | sed 's/^# //')
                echo "- **From Chapter $chap_name**: $chap_summary" >> "$rules_file"
            done
        fi

        # Pull explicit lessons found in any note (recursive search)
        local lesson_notes
        lesson_notes=$(grep -rl "lesson: true" "$notes_root" --include="*.md" 2>/dev/null | head -n 5)
        if [ -n "$lesson_notes" ]; then
            for note in $lesson_notes; do
                local note_name=$(basename "$note")
                # Extract the first few lines of the Learnings section
                local learning=$(sed -n '/## Learnings/,/##/p' "$note" | grep "^-" | head -n 2)
                if [ -n "$learning" ]; then
                    echo "- **Learning from $note_name**: $learning" >> "$rules_file"
                fi
            done
        fi
        echo "" >> "$rules_file"
    fi
    
    success "Feature rules synthesized at .cursor/rules/${feature_name}.md"
}

# Cleanup synthesized feature rules (Phase 3)
cleanup_feature_rules() {
    local feature_name=$1
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    local rules_file="$project_root/.cursor/rules/${feature_name}.md"
    
    if [ -f "$rules_file" ]; then
        rm -f "$rules_file"
        log "Cleaned up feature-specific rules: $rules_file"
    fi
}

# Find next available story
find_next_story() {
    local prd_json="$1"
    
    # Get all stories that don't pass yet
    local available_stories
    available_stories=$(jq -r '.userStories[] | select(.passes == false) | .id' "$prd_json")
    
    if [ -z "$available_stories" ]; then
        echo ""
        return
    fi
    
    # Check dependencies for each story
    for story_id in $available_stories; do
        local deps
        deps=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .dependencies[]?" "$prd_json" 2>/dev/null || echo "")
        
        if [ -z "$deps" ]; then
            # No dependencies, can work on this
            echo "$story_id"
            return
        fi
        
        # Check if all dependencies pass
        local all_deps_pass=true
        for dep in $deps; do
            local dep_passes
            dep_passes=$(jq -r ".userStories[] | select(.id == \"$dep\") | .passes" "$prd_json")
            if [ "$dep_passes" != "true" ]; then
                all_deps_pass=false
                break
            fi
        done
        
        if [ "$all_deps_pass" = "true" ]; then
            echo "$story_id"
            return
        fi
    done
    
    # No available stories (dependencies not satisfied)
    echo ""
}

# Get dependency resolution details for error messages
get_dependency_details() {
    local prd_json="$1"
    local blocked_stories=""
    
    # Get all stories that don't pass yet
    local available_stories
    available_stories=$(jq -r '.userStories[] | select(.passes == false) | .id' "$prd_json")
    
    for story_id in $available_stories; do
        local deps
        deps=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .dependencies[]?" "$prd_json" 2>/dev/null || echo "")
        
        if [ -n "$deps" ]; then
            local missing_deps=""
            for dep in $deps; do
                local dep_passes
                dep_passes=$(jq -r ".userStories[] | select(.id == \"$dep\") | .passes" "$prd_json")
                if [ "$dep_passes" != "true" ]; then
                    local dep_title
                    dep_title=$(jq -r ".userStories[] | select(.id == \"$dep\") | .title" "$prd_json")
                    if [ -n "$missing_deps" ]; then
                        missing_deps="$missing_deps, "
                    fi
                    missing_deps="$missing_deps$dep ($dep_title)"
                fi
            done
            
            if [ -n "$missing_deps" ]; then
                local story_title
                story_title=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .title" "$prd_json")
                if [ -n "$blocked_stories" ]; then
                    blocked_stories="$blocked_stories\n"
                fi
                blocked_stories="${blocked_stories}- **$story_id**: $story_title (waiting for: $missing_deps)"
            fi
        fi
    done
    
    echo -e "$blocked_stories"
}

# Add agent definition to prompt
add_agent_definition() {
    local story_id="$1"
    local agent_name
    agent_name=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .agent" "$PRD_JSON" 2>/dev/null)
    
    if [ -z "$agent_name" ] || [ "$agent_name" = "null" ]; then
        error "Failed to extract agent name for story: $story_id"
        return 1
    fi
    
    # Normalize agent alias to canonical name
    local canonical_agent
    canonical_agent=$(normalize_agent_name "$agent_name")
    
    # Validate agent name format (should match filename pattern: lowercase, hyphens allowed)
    if [[ ! "$canonical_agent" =~ ^[a-z0-9-]+$ ]]; then
        error "Invalid agent name format: $canonical_agent (expected lowercase alphanumeric with hyphens)"
        return 1
    fi
    
    local agent_file="$AGENTS_DIR/$canonical_agent.md"
    
    # Validate path
    if ! validate_path "$agent_file" "$AGENTS_DIR"; then
        error "Invalid agent path detected: $agent_file"
        return 1
    fi
    
    if [ ! -f "$agent_file" ]; then
        error "Agent file not found: $agent_file (for agent alias: $agent_name)"
        return 1
    fi
    
    echo "# Agent Definition"
    echo ""
    cat "$agent_file"
    echo ""
    echo "---"
    echo ""
}

# Add project progress context to prompt (read FIRST for project-level context)
add_progress_context() {
    if [ -f "$DOCS_DIR/PROGRESS.md" ]; then
        echo "## Project Progress"
        echo ""
        echo "**Source**: \`~/docs/{project-name}/PROGRESS.md\`"
        echo ""
        echo "**Purpose**: Provides instant project-level context restoration. Read first to understand:"
        echo "- Where you are in the project overall (active feature, current story, overall progress)"
        echo "- Performance metrics (velocity, iterations, success rate, quality metrics)"
        echo "- Accumulated context (recent decisions, active blockers, pending todos, active investigations)"
        echo "- Session continuity (last session, resume context)"
        echo ""
        cat "$DOCS_DIR/PROGRESS.md"
        echo ""
        echo "---"
        echo ""
        log "Reading project progress from ~/docs/{project-name}/PROGRESS.md"
    fi
}

# Add project context to prompt
add_project_context() {
    # Add project preferences (if exists) - interaction language and other preferences
    if [ -f "$DOCS_DIR/PREFERENCES.md" ]; then
        echo "## Project Preferences"
        echo ""
        echo "**Source**: \`~/docs/{project-name}/PREFERENCES.md\`"
        echo ""
        cat "$DOCS_DIR/PREFERENCES.md"
        echo ""
        echo "---"
        echo ""
    fi

    # Add project notes (if exists)
    local notes_file
    notes_file=$(get_notes_file)
    if [ -f "$notes_file" ]; then
        echo "## Project Notes"
        echo ""
        echo "Full project notes from \`~/docs/{project-name}/NOTES.md\` (all entries preserved, no truncation):"
        echo ""
        cat "$notes_file"
        echo ""
        echo "---"
        echo ""
    fi
    
    # Add DECISIONS.md if exists
    if [ -f "$DOCS_DIR/DECISIONS.md" ]; then
        echo "## Project Decisions"
        echo ""
        echo "Architectural and design decisions from \`~/docs/{project-name}/DECISIONS.md\`:"
        echo ""
        cat "$DOCS_DIR/DECISIONS.md"
        echo ""
        echo "---"
        echo ""
    fi
    
    # Add DEV-NOTES.md if exists (root level)
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    if [ -f "$project_root/DEV-NOTES.md" ]; then
        echo "## Dev Notes (Global)"
        echo ""
        cat "$project_root/DEV-NOTES.md"
        echo ""
        echo "---"
        echo ""
    fi
    
    # Add feature-specific DEV-NOTES.md if exists
    if [ -f "$DOCS_DIR/feature/$FEATURE_NAME/DEV-NOTES.md" ]; then
        echo "## Dev Notes (Feature: $FEATURE_NAME)"
        echo ""
        cat "$DOCS_DIR/feature/$FEATURE_NAME/DEV-NOTES.md"
        echo ""
        echo "---"
        echo ""
    fi
}

# Helper function to extract category from markdown frontmatter
extract_category_from_frontmatter() {
    local file="$1"
    if [ ! -f "$file" ]; then
        return 1
    fi
    
    # Extract category from YAML frontmatter (between --- markers)
    # Handles both "category: features" and "category: 'features'" formats
    local category
    category=$(awk '
        /^---$/ { in_frontmatter = !in_frontmatter; next }
        in_frontmatter && /^category:/ {
            gsub(/^category:[[:space:]]*/, "")
            gsub(/^["'\'']|["'\'']$/, "")  # Remove quotes
            gsub(/[[:space:]]*$/, "")      # Remove trailing spaces
            print
            exit
        }
    ' "$file" 2>/dev/null)
    
    if [ -n "$category" ]; then
        echo "$category"
        return 0
    fi
    return 1
}

# Add feature notes to prompt (from ~/docs/{project-name}/notes/{feature-name}/ or flat files matching {feature-name}-*)
add_feature_notes() {
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    local notes_base_dir="$DOCS_DIR/notes"
    local has_notes=false
    
    # First, try grouped structure: ~/docs/{project-name}/notes/{feature-name}/
    # Note: Category is stored in frontmatter (category: "features"), not in directory path
    if [ -d "$feature_notes_dir" ]; then
        # Check for CONTEXT.md
        if [ -f "$feature_notes_dir/CONTEXT.md" ]; then
            if [ "$has_notes" = "false" ]; then
                echo "## Feature Notes"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/$FEATURE_NAME/\`"
                echo ""
                has_notes=true
            fi
            echo "### Context"
            echo ""
            cat "$feature_notes_dir/CONTEXT.md"
            echo ""
            echo "---"
            echo ""
        fi
        
        # Check for TODOS.md
        if [ -f "$feature_notes_dir/TODOS.md" ]; then
            if [ "$has_notes" = "false" ]; then
                echo "## Feature Notes"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/$FEATURE_NAME/\`"
                echo ""
                has_notes=true
            fi
            echo "### Current Tasks"
            echo ""
            cat "$feature_notes_dir/TODOS.md"
            echo ""
            echo "---"
            echo ""
        fi
        
        # Check for chapter index (read chapters before full notes for quick overview)
        local chapters_dir="$feature_notes_dir/chapters"
        if [ -d "$chapters_dir" ] && [ -f "$chapters_dir/index.md" ]; then
            if [ "$has_notes" = "false" ]; then
                echo "## Feature Notes"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/$FEATURE_NAME/\`"
                echo ""
                has_notes=true
            fi
            echo "### Chapters"
            echo ""
            echo "**Note**: This feature is organized into chapters for better navigation. Read the chapter index for a quick overview before diving into full notes."
            echo ""
            cat "$chapters_dir/index.md"
            echo ""
            echo "---"
            echo ""
        fi
        
        # Check for insights.json (prioritize evidence-based insights)
        if [ -f "$feature_notes_dir/insights.json" ]; then
            if [ "$has_notes" = "false" ]; then
                echo "## Feature Notes"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/$FEATURE_NAME/\`"
                echo ""
                has_notes=true
            fi
            echo "### Insights"
            echo ""
            echo "**Note**: Evidence-based insights (marked with \`evidenceBased: true\`) are prioritized as they are supported by first-hand evidence (logs, test results, metrics)."
            echo ""
            # Parse and format insights.json, prioritizing evidence-based insights
            if command -v jq >/dev/null 2>&1; then
                # Sort insights: evidence-based first, then by timestamp (newest first)
                jq -r '.insights | sort_by(.evidenceBased == false, .timestamp) | reverse | .[] | 
                    "#### \(.title) (\(.type))\n" +
                    "- **Timestamp**: \(.timestamp)\n" +
                    "- **Evidence-Based**: \(.evidenceBased)\n" +
                    (if .description then "- **Description**: \(.description)\n" else "" end) +
                    (if .impact then "- **Impact**: \(.impact)\n" else "" end) +
                    (if .decision then "- **Decision**: \(.decision)\n" else "" end) +
                    (if .result then "- **Result**: \(.result)\n" else "" end) +
                    (if .error then "- **Error**: \(.error)\n" else "" end) +
                    (if .learning then "- **Learning**: \(.learning)\n" else "" end) +
                    (if .nextAttempt then "- **Next Attempt**: \(.nextAttempt)\n" else "" end) +
                    (if .evidence and .evidenceBased == true then 
                        "- **Evidence**:\n" +
                        "  - Source: \(.evidence.source // "unknown")\n" +
                        "  - Observation: \(.evidence.observation // "N/A")\n" +
                        (if .evidence.files then "  - Files: \(.evidence.files | join(", "))\n" else "" end) +
                        (if .evidence.commit then "  - Commit: \(.evidence.commit)\n" else "" end)
                    else "" end) +
                    (if .introspection then
                        "- **Introspection**:\n" +
                        (if .introspection.whatWentWell and (.introspection.whatWentWell | length) > 0 then
                            "  - **What Went Well**:\n" +
                            (.introspection.whatWentWell[] | "    - \(.)\n")
                        else "" end) +
                        (if .introspection.whatCouldBeImproved and (.introspection.whatCouldBeImproved | length) > 0 then
                            "  - **What Could Be Improved**:\n" +
                            (.introspection.whatCouldBeImproved[] | "    - \(.)\n")
                        else "" end) +
                        (if .introspection.recommendations and (.introspection.recommendations | length) > 0 then
                            "  - **Recommendations**:\n" +
                            (.introspection.recommendations[] | "    - \(.)\n")
                        else "" end) +
                        (if .introspection.whatShouldIDoDifferentlyNow and (.introspection.whatShouldIDoDifferentlyNow | length) > 0 then
                            "  - **What Should I Do Differently Now**:\n" +
                            (.introspection.whatShouldIDoDifferentlyNow[] | "    - \(.)\n")
                        else "" end)
                    else "" end) +
                    "\n---\n"' "$feature_notes_dir/insights.json" 2>/dev/null || {
                    # Fallback: if jq parsing fails, show raw JSON
                    echo "**Note**: Unable to parse insights.json. Raw content:"
                    echo "\`\`\`json"
                    cat "$feature_notes_dir/insights.json"
                    echo "\`\`\`"
                }
            else
                # Fallback: if jq not available, show raw JSON
                echo "**Note**: jq not available. Raw insights.json content:"
                echo "\`\`\`json"
                cat "$feature_notes_dir/insights.json"
                echo "\`\`\`"
            fi
            echo ""
            echo "---"
            echo ""
        fi
    fi
    
    # If grouped structure didn't have notes, try flat structure: ~/docs/{project-name}/notes/{feature-name}-*.md
    # Now with full frontmatter parsing to verify category
    if [ "$has_notes" = "false" ] && [ -d "$notes_base_dir" ]; then
        # Check for flat files matching feature name pattern
        local flat_context="$notes_base_dir/${FEATURE_NAME}-CONTEXT.md"
        local flat_todos="$notes_base_dir/${FEATURE_NAME}-TODOS.md"
        local flat_insights="$notes_base_dir/${FEATURE_NAME}-insights.json"
        
        # Verify files exist and have correct category in frontmatter
        local found_valid_notes=false
        
        # Check markdown files (CONTEXT.md, TODOS.md)
        for file in "$flat_context" "$flat_todos"; do
            if [ -f "$file" ]; then
                local file_category
                file_category=$(extract_category_from_frontmatter "$file")
                if [ "$file_category" = "features" ]; then
                    found_valid_notes=true
                    break
                elif [ -z "$file_category" ]; then
                    # No category found in frontmatter - treat as feature note (for notes created before category was required)
                    found_valid_notes=true
                    break
                fi
            fi
        done
        
        # Check insights.json (has different structure - category in metadata)
        if [ -f "$flat_insights" ] && command -v jq >/dev/null 2>&1; then
            local json_category
            json_category=$(jq -r '.metadata.category // "unknown"' "$flat_insights" 2>/dev/null)
            if [ "$json_category" = "features" ] || [ "$json_category" = "null" ] || [ "$json_category" = "unknown" ]; then
                found_valid_notes=true
            fi
        elif [ -f "$flat_insights" ]; then
            # jq not available - include file (category verification skipped when jq unavailable)
            found_valid_notes=true
        fi
        
        if [ "$found_valid_notes" = "true" ]; then
            echo "## Feature Notes"
            echo ""
            echo "**Source**: \`~/docs/{project-name}/notes/\` (flat structure, matching \`${FEATURE_NAME}-*\`)"
            echo ""
            echo "**Note**: Flat structure notes verified with category filtering (frontmatter parsed to confirm \`category: features\`)."
            echo ""
            has_notes=true
            
            if [ -f "$flat_context" ]; then
                echo "### Context"
                echo ""
                cat "$flat_context"
                echo ""
                echo "---"
                echo ""
            fi
            
            if [ -f "$flat_todos" ]; then
                echo "### Current Tasks"
                echo ""
                cat "$flat_todos"
                echo ""
                echo "---"
                echo ""
            fi
            
            if [ -f "$flat_insights" ]; then
                echo "### Insights"
                echo ""
                if command -v jq >/dev/null 2>&1; then
                    jq -r '.insights | sort_by(.evidenceBased == false, .timestamp) | reverse | .[] | 
                        "#### \(.title) (\(.type))\n" +
                        "- **Timestamp**: \(.timestamp)\n" +
                        "- **Evidence-Based**: \(.evidenceBased)\n" +
                        (if .description then "- **Description**: \(.description)\n" else "" end) +
                        (if .impact then "- **Impact**: \(.impact)\n" else "" end) +
                        (if .learning then "- **Learning**: \(.learning)\n" else "" end) +
                        (if .introspection then
                            "- **Introspection**:\n" +
                            (if .introspection.whatWentWell and (.introspection.whatWentWell | length) > 0 then
                                "  - **What Went Well**:\n" +
                                (.introspection.whatWentWell[] | "    - \(.)\n")
                            else "" end) +
                            (if .introspection.whatCouldBeImproved and (.introspection.whatCouldBeImproved | length) > 0 then
                                "  - **What Could Be Improved**:\n" +
                                (.introspection.whatCouldBeImproved[] | "    - \(.)\n")
                            else "" end) +
                            (if .introspection.recommendations and (.introspection.recommendations | length) > 0 then
                                "  - **Recommendations**:\n" +
                                (.introspection.recommendations[] | "    - \(.)\n")
                            else "" end)
                        else "" end) +
                        "\n---\n"' "$flat_insights" 2>/dev/null || cat "$flat_insights"
                else
                    cat "$flat_insights"
                fi
                echo ""
                echo "---"
                echo ""
            fi
        fi
    fi
    
    if [ "$has_notes" = "true" ]; then
        log "Included feature notes from ~/docs/{project-name}/notes/$FEATURE_NAME/ or matching flat files"
    fi
}

# Add project-level lessons to prompt (from project-level insights.json)
add_project_lessons() {
    local project_insights_file="$DOCS_DIR/insights.json"
    local has_lessons=false
    
    # Read project-level insights.json at docs root
    if [ -f "$project_insights_file" ]; then
        if command -v jq >/dev/null 2>&1; then
            # Check if file has category: "projects" in metadata and has lessons
            local category
            category=$(jq -r '.metadata.category // "unknown"' "$project_insights_file" 2>/dev/null)
            local has_project_lessons
            has_project_lessons=$(jq -r '.insights[] | select(.lesson == true) | .id' "$project_insights_file" 2>/dev/null | head -1)
            
            if [ "$category" = "projects" ] && [ -n "$has_project_lessons" ]; then
                echo "## Project-Level Lessons"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/insights.json\` (category: projects)"
                echo ""
                echo "**Purpose**: Lessons learned from past work (bug fixes, feature completion, high-iteration tasks). Read these to inform your approach and avoid repeating mistakes."
                echo ""
                echo "**Note**: These lessons are prioritized alongside evidence-based insights. They represent learnings that should influence future work."
                echo ""
                
                # Format lessons, prioritizing evidence-based ones
                jq -r '.insights | 
                    map(select(.lesson == true)) | 
                    sort_by(.evidenceBased == false, .timestamp) | 
                    reverse | 
                    .[] | 
                    "#### \(.title) (\(.type))\n" +
                    "- **Timestamp**: \(.timestamp)\n" +
                    "- **Evidence-Based**: \(.evidenceBased)\n" +
                    (if .description then "- **Description**: \(.description)\n" else "" end) +
                    (if .learning then "- **Learning**: \(.learning)\n" else "" end) +
                    (if .impact then "- **Impact**: \(.impact)\n" else "" end) +
                    (if .iterations then "- **Iterations**: \(.iterations)\n" else "" end) +
                    (if .evidence and .evidenceBased == true then 
                        "- **Evidence**:\n" +
                        "  - Source: \(.evidence.source // "unknown")\n" +
                        "  - Observation: \(.evidence.observation // "N/A")\n" +
                        (if .evidence.files then "  - Files: \(.evidence.files | join(", "))\n" else "" end) +
                        (if .evidence.commit then "  - Commit: \(.evidence.commit)\n" else "" end)
                    else "" end) +
                    "\n---\n"' "$project_insights_file" 2>/dev/null || {
                    echo "**Note**: Unable to parse project lessons. Raw content:"
                    echo "\`\`\`json"
                    cat "$project_insights_file"
                    echo "\`\`\`"
                }
                echo ""
                echo "---"
                echo ""
                has_lessons=true
                log "Reading project-level lessons from ~/docs/{project-name}/insights.json"
            fi
        fi
    fi
    
    # Also search for any insights.json files in notes/ with category: "projects" (fallback for backward compatibility)
    local notes_base_dir="$DOCS_DIR/notes"
    if [ "$has_lessons" = "false" ] && [ -d "$notes_base_dir" ]; then
        # Find all insights.json files and check for project-level lessons
        while IFS= read -r insights_file; do
            if command -v jq >/dev/null 2>&1; then
                local file_category
                file_category=$(jq -r '.metadata.category // "unknown"' "$insights_file" 2>/dev/null)
                local file_has_lessons
                file_has_lessons=$(jq -r '.insights[] | select(.lesson == true) | .id' "$insights_file" 2>/dev/null | head -1)
                
                if [ "$file_category" = "projects" ] && [ -n "$file_has_lessons" ]; then
                    echo "## Project-Level Lessons"
                    echo ""
                    echo "**Source**: \`$(basename "$insights_file")\` (category: projects)"
                    echo ""
                    echo "**Purpose**: Lessons learned from past work. Read these to inform your approach and avoid repeating mistakes."
                    echo ""
                    
                    # Format lessons
                    jq -r '.insights | 
                        map(select(.lesson == true)) | 
                        sort_by(.evidenceBased == false, .timestamp) | 
                        reverse | 
                        .[] | 
                        "#### \(.title) (\(.type))\n" +
                        "- **Timestamp**: \(.timestamp)\n" +
                        "- **Evidence-Based**: \(.evidenceBased)\n" +
                        (if .description then "- **Description**: \(.description)\n" else "" end) +
                        (if .learning then "- **Learning**: \(.learning)\n" else "" end) +
                        (if .impact then "- **Impact**: \(.impact)\n" else "" end) +
                        (if .iterations then "- **Iterations**: \(.iterations)\n" else "" end) +
                        "\n---\n"' "$insights_file" 2>/dev/null || true
                    echo ""
                    echo "---"
                    echo ""
                    has_lessons=true
                    log "Reading project-level lessons from $(basename "$insights_file")"
                    break  # Only use first matching file
                fi
            fi
        done < <(find "$notes_base_dir" -name "insights.json" -type f 2>/dev/null)
    fi
}

# Check and create chapter if quantitative triggers are met
check_and_create_chapter_if_needed() {
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    
    # Only check if feature notes directory exists
    if [ ! -d "$feature_notes_dir" ]; then
        return 0
    fi
    
    # Source create-chapter script functions
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # Check quantitative triggers using create-chapter.sh
    if [ -f "$script_dir/create-chapter.sh" ]; then
        local trigger_result
        if trigger_result=$("$script_dir/create-chapter.sh" "$feature_notes_dir" "features" "auto" 2>/dev/null); then
            if [ -n "$trigger_result" ] && [ -f "$trigger_result" ]; then
                log "Chapter created automatically: $(basename "$trigger_result")"
                log "Trigger detected: Quantitative threshold met"
            fi
        fi
    fi
}

# Create feature completion chapter (qualitative trigger)
create_feature_completion_chapter() {
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    
    # Only create if feature notes directory exists
    if [ ! -d "$feature_notes_dir" ]; then
        return 0
    fi
    
    # Source create-chapter script
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # Create feature completion chapter
    if [ -f "$script_dir/create-chapter.sh" ]; then
        local chapter_file
        if chapter_file=$("$script_dir/create-chapter.sh" "$feature_notes_dir" "features" "feature-completion" 2>/dev/null); then
            if [ -n "$chapter_file" ] && [ -f "$chapter_file" ]; then
                log "Feature completion chapter created: $(basename "$chapter_file")"
            fi
        fi
    fi
}

create_max_iterations_retro() {
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    
    # Create feature notes directory if it doesn't exist
    mkdir -p "$feature_notes_dir"
    
    # Source create-chapter script
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # Create max-iterations retro chapter
    if [ -f "$script_dir/create-chapter.sh" ]; then
        local chapter_file
        if chapter_file=$("$script_dir/create-chapter.sh" "$feature_notes_dir" "features" "max-iterations" 2>/dev/null); then
            if [ -n "$chapter_file" ] && [ -f "$chapter_file" ]; then
                log "Max iterations retro chapter created: $(basename "$chapter_file")"
                
                # Populate the chapter with retro template content
                local template_file="$script_dir/../templates/feature/max-iterations-retro.md"
                if [ -f "$template_file" ]; then
                    # Read template and replace placeholders
                    local prd_json="$DOCS_DIR/feature/$FEATURE_NAME/prd.json"
                    local total_stories=0
                    local completed_stories=0
                    local incomplete_stories=""
                    
                    if [ -f "$prd_json" ] && command -v jq >/dev/null 2>&1; then
                        total_stories=$(jq -r '.userStories | length' "$prd_json" 2>/dev/null || echo "0")
                        completed_stories=$(jq -r '[.userStories[] | select(.passes == true)] | length' "$prd_json" 2>/dev/null || echo "0")
                        incomplete_stories=$(jq -r '[.userStories[] | select(.passes != true)] | .[] | "- \(.id): \(.title)"' "$prd_json" 2>/dev/null || echo "")
                    fi
                    
                    # Get timestamp
                    local timestamp
                    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
                    
                    # Append template content to chapter (after frontmatter)
                    {
                        # Keep frontmatter
                        head -n 10 "$chapter_file"
                        echo ""
                        # Add template content
                        sed '1,/^---$/d' "$template_file" | sed -e "s/{FEATURE_NAME}/$FEATURE_NAME/g" \
                            -e "s/{MAX_ITERATIONS}/$MAX_ITERATIONS/g" \
                            -e "s/{TOTAL_STORIES}/$total_stories/g" \
                            -e "s/{COMPLETED_STORIES}/$completed_stories/g" \
                            -e "s/{TIMESTAMP}/$timestamp/g"
                        echo ""
                        echo "## Incomplete Stories"
                        echo ""
                        if [ -n "$incomplete_stories" ]; then
                            echo "$incomplete_stories"
                        else
                            echo "No incomplete stories found."
                        fi
                    } > "${chapter_file}.tmp" && mv "${chapter_file}.tmp" "$chapter_file"
                fi
                
                echo "$chapter_file"
            fi
        fi
    fi
}

# Add available tools section to prompt
add_available_tools() {
    echo "## Available Tools"
    echo ""
    echo "You have access to note management tools for maintaining persistent memory across conversation boundaries."
    echo ""
    echo "**Important**: These are **documented APIs** that you implement using standard Cursor file operations (\`read_file\`, \`write_file\`, \`list_dir\`, \`grep\`). No custom tool implementations exist - you use standard file operations to implement these APIs."
    echo ""
    echo "### Note Tools (Documented APIs)"
    echo ""
    echo "1. **write_note(category, name, content, metadata?)**"
    echo "   - Write a new note file"
    echo "   - **Parameters**:"
    echo "     - \`category\` (string): Category stored in frontmatter (e.g., \"investigations\", \"features\", \"projects\", \"general\")"
    echo "     - \`name\` (string): Note name/filename (sanitized: alphanumeric, hyphens, underscores)"
    echo "     - \`content\` (string): Markdown content to write"
    echo "     - \`metadata\` (optional): Additional metadata (agent, command, context, commit, tags)"
    echo "   - **Returns**: \`{success: boolean, path: string, error?: string}\`"
    echo "   - **Implementation**: Use \`write_file\` to create note at \`~/docs/{project-name}/notes/{name}.md\` (flat) or \`~/docs/{project-name}/notes/{id}/{type}.md\` (grouped)"
    echo "   - **Frontmatter**: Include YAML frontmatter with \`category\`, \`created\`, \`updated\`, and optional metadata"
    echo "   - **Tags**: **Always include minimum required tags** in frontmatter, plus any additional relevant tags:"
    echo "     - **Required minimum**:"
    echo "       - Domain tag: \"backend\" or \"frontend\" based on story type"
    echo "       - Agent name tag: your agent name (e.g., \"uidev\", \"rusty\", \"steve\")"
    echo "     - **Optional additional tags**: Add any relevant tags using your specialized knowledge:"
    echo "       - Technology tags: \"react\", \"rust\", \"postgresql\", \"api\", etc."
    echo "       - Feature area tags: \"authentication\", \"payment\", \"user-management\", etc."
    echo "       - Pattern tags: \"migration\", \"testing\", \"optimization\", etc."
    echo "     - Example: \`tags: [\"frontend\", \"uidev\", \"react\", \"authentication\", \"form-validation\"]\`"
    echo "     - **Note**: Minimum tags are required, but you should add more tags when they provide value for discovery"
    echo "   - **Git Commit**: Auto-detect commit hash using \`git rev-parse HEAD\` if in repository (unless provided)"
    echo ""
    echo "2. **read_note(path | {category, name})**"
    echo "   - Read an existing note file"
    echo "   - **Parameters**: Either full \`path\` (string) or \`{category, name}\` (object)"
    echo "   - **Returns**: \`{content: string, metadata: object, error?: string}\`"
    echo "   - **Implementation**: Use \`read_file\` to read note, parse YAML frontmatter to extract category and metadata"
    echo "   - **Category Filtering**: If category provided, read frontmatter to verify category matches"
    echo ""
    echo "3. **list_notes(category?)**"
    echo "   - List available notes"
    echo "   - **Parameters**: \`category\` (optional string): Filter by category, or null for all"
    echo "   - **Returns**: \`Array<{name: string, path: string, modified: string, size: number, category: string}>\`"
    echo "   - **Implementation**: Use \`list_dir\` to scan \`~/docs/{project-name}/notes/\` (recursively), read frontmatter from each file to extract category, filter by category if provided"
    echo "   - **Sorting**: Sort by modification time (newest first)"
    echo ""
    echo "4. **append_note(path | {category, name}, content)**"
    echo "   - Append content to an existing note (creates note if it doesn't exist)"
    echo "   - **Parameters**: Either full \`path\` (string) or \`{category, name}\` (object), plus \`content\` (string)"
    echo "   - **Returns**: \`{success: boolean, path: string, error?: string}\`"
    echo "   - **Implementation**: Use \`read_file\` to read existing note, append timestamp separator and new content, use \`write_file\` to save"
    echo "   - **Timestamp Separator**: Add \`---\\n## [ISO 8601 timestamp]\\n[content]\` before appended content"
    echo "   - **Update Metadata**: Update \`updated\` timestamp in frontmatter"
    echo ""
    echo "5. **search_notes(query, category?)**"
    echo "   - Search note content by keyword/phrase"
    echo "   - **Parameters**: \`query\` (string): Search term, \`category\` (optional string): Limit search to category"
    echo "   - **Returns**: \`Array<{path: string, snippet: string, matches: number, lineNumbers: number[]}>\`"
    echo "   - **Implementation**: Use \`grep\` or text search across \`~/docs/{project-name}/notes/\` directory, read frontmatter to filter by category if provided"
    echo "   - **Context Snippets**: Return 3 lines of context around each match"
    echo ""
    echo "6. **search_by_tag(tag, category?)**"
    echo "   - Search notes and features by tag (case-insensitive)"
    echo "   - **Parameters**: \`tag\` (string): Tag to search for, \`category\` (optional string): Limit search to category"
    echo "   - **Returns**: \`{notes: Array<{path, name, category, tags, modified, snippet?}>, features: Array<{featureName, path, tags, description, status}>, totalMatches: number}\`"
    echo "   - **Implementation**:"
    echo "     - Search all notes in \`~/docs/{project-name}/notes/\` (read frontmatter/metadata to extract tags)"
    echo "     - Search all \`prd.json\` files in \`~/docs/{project-name}/feature/*/prd.json\` (read feature-level tags)"
    echo "     - Case-insensitive matching (e.g., \"Authentication\" matches \"authentication\")"
    echo "     - If category provided, filter notes by reading category from frontmatter/metadata"
    echo "     - Sort results by modification time (newest first)"
    echo "   - **Tags Format**: Tags stored as array in frontmatter (\`tags: [\"authentication\", \"security\"]\`) or JSON metadata (\`metadata.tags\`)"
    echo "   - **Use Case**: Find related work across different entities (notes, features) using shared tags"
    echo "   - **Recommended Usage**: Before starting a task, use \`search_by_tag(\"backend\")\` or \`search_by_tag(\"frontend\")\` to find related notes from similar work"
    echo "   - **Agent-Specific Learning**: Use \`search_by_tag(\"your-agent-name\")\` to find notes from your previous work and learn from your own patterns"
    echo ""
    echo "7. **read_tags_index()**"
    echo "   - Read tags index to discover existing tags and maintain tag consistency"
    echo "   - **Parameters**: None"
    echo "   - **Returns**: \`{tags: {[tagName: string]: {count: number, category: string, required: boolean, description: string, relatedTags: string[]}}, tagCategories: {[category: string]: string[]}, usageStats: {totalNotes: number, totalTags: number, mostUsedTags: string[]}, error?: string}\`"
    echo "   - **Implementation**: Read \`~/docs/{project-name}/notes/tags.json\` and parse JSON"
    echo "   - **Use Case**: Before creating notes, read tags index to discover existing tags and use them for consistency"
    echo "   - **Recommended Usage**: Read tags index before creating notes to see what tags exist and which are commonly used together"
    echo ""
    echo "8. **add_tag(tag, category?, description?, relatedTags?)**"
    echo "   - Add a tag to the tags index (idempotent - safe to call multiple times)"
    echo "   - **Parameters**:"
    echo "     - \`tag\` (string, required): Tag name to add (case-insensitive, normalized to lowercase)"
    echo "     - \`category\` (string, optional): Tag category (\"domain\", \"agent\", \"technology\", \"feature\", \"pattern\")"
    echo "     - \`description\` (string, optional): Human-readable description of the tag"
    echo "     - \`relatedTags\` (array, optional): Array of related tag names (tags commonly used together)"
    echo "   - **Returns**: \`{success: boolean, tag: string, created: boolean, error?: string}\`"
    echo "     - \`created\`: true if tag was newly created, false if it already existed"
    echo "   - **Implementation**:"
    echo "     - Read \`~/docs/{project-name}/notes/tags.json\` (create if doesn't exist)"
    echo "     - Normalize tag to lowercase for consistency"
    echo "     - Check if tag already exists in tags object"
    echo "     - If tag exists: Update metadata (category, description, relatedTags) if provided, keep existing count"
    echo "     - If tag doesn't exist: Add new tag entry with count: 0, provided metadata, and default values"
    echo "     - Write updated \`tags.json\` back to file"
    echo "   - **Idempotent**: Safe to call multiple times with same tag - won't create duplicates"
    echo "   - **Use Case**: When creating notes, add new tags to the index so they're discoverable by other agents"
    echo "   - **Recommended Usage**: Call \`add_tag()\` for any new tags you're using in your notes (the tool handles uniqueness)"
    echo ""
    echo "9. **update_tags_index()**"
    echo "   - Update tags index by scanning all notes and features"
    echo "   - **Parameters**: None"
    echo "   - **Returns**: \`{success: boolean, tagsUpdated: number, error?: string}\`"
    echo "   - **Implementation**:"
    echo "     - Scan all notes in \`~/docs/{project-name}/notes/\` (recursively) and extract tags from frontmatter/metadata"
    echo "     - Scan all \`prd.json\` files in \`~/docs/{project-name}/feature/*/prd.json\` and extract feature-level tags"
    echo "     - Count tag usage, build related tags (tags used together), calculate statistics"
    echo "     - Write updated \`tags.json\` to \`~/docs/{project-name}/notes/tags.json\`"
    echo "   - **Use Case**: After creating or updating notes, update tags index to refresh tag usage statistics"
    echo "   - **Recommended Usage**: Call after creating/updating notes to keep tags index current"
    echo ""
    echo "10. **create_chapter(entity_id, chapter_name, reason, notes_scope?)**"
    echo "   - Create a chapter manually for organizing notes into logical sections"
    echo "   - **Parameters**:"
    echo "     - \`entity_id\` (string, required): Entity identifier (e.g., \"user-login-form\", \"memory-leak-2024-01-15\")"
    echo "     - \`chapter_name\` (string, required): Descriptive name for the chapter (e.g., \"Initial Investigation\", \"Feature Completion\")"
    echo "     - \`reason\` (string, required): Why this chapter is being created (e.g., \"Feature completed\", \"20 notes threshold reached\")"
    echo "     - \`notes_scope\` (object, optional): Specific notes to include in chapter:"
    echo "       - \`evidence_lines\` (string, optional): Line range (e.g., \"1-150\")"
    echo "       - \`insights_ids\` (array, optional): Specific insight IDs to include"
    echo "       - \`time_span\` (object, optional): Time range with \`start\` and \`end\` (ISO 8601)"
    echo "   - **Returns**: \`{success: boolean, chapter_path: string, chapter_number: number, error?: string}\`"
    echo "   - **Implementation**:"
    echo "     - Call \`scripts/create-chapter.sh\` with entity directory, category, and trigger type"
    echo "     - Chapter is created in \`~/docs/{project-name}/notes/{entity_id}/chapters/XX-{chapter-name}.md\`"
    echo "     - Chapter index (\`chapters/index.md\`) is automatically updated"
    echo "     - Main \`context.md\` is updated with chapter table of contents"
    echo "   - **Use Case**: Manually organize notes into chapters when needed, or when automatic triggers don't apply"
    echo "   - **Recommended Usage**: Create chapters for important milestones or when notes become unwieldy"
    echo "   - **Note**: Chapters are summaries, not replacements - original notes are preserved"
    echo ""
    echo "### Note Storage Structure"
    echo ""
    echo "**Category is stored in frontmatter/metadata, NOT in directory path.**"
    echo ""
    echo "**Flat Structure (Recommended)**:"
    echo "- \`~/docs/{project-name}/notes/{id}-{description}-{type}.md\`"
    echo "  - Example: \`~/docs/my-project/notes/US-001-user-login-form-CONTEXT.md\` (category: \"features\" in frontmatter)"
    echo "  - Example: \`~/docs/my-project/notes/memory-leak-2024-01-15-investigating-memory-growth-EVIDENCE.md\` (category: \"investigations\" in frontmatter)"
    echo ""
    echo "**Grouped Structure (Optional, for complex entities)**:"
    echo "- \`~/docs/{project-name}/notes/{id}-{description}/{type}.md\`"
    echo "  - Example: \`~/docs/my-project/notes/US-001-user-login-form/CONTEXT.md\` (category: \"features\" in frontmatter)"
    echo "  - Example: \`~/docs/my-project/notes/memory-leak-2024-01-15/EVIDENCE.md\` (category: \"investigations\" in frontmatter)"
    echo ""
    echo "**Key Points**:"
    echo "- Category is always in frontmatter (\`category: investigations\`) or JSON metadata (\`metadata.category\`), never in directory path"
    echo "- Tools filter by reading category property from frontmatter, not from directory structure"
    echo "- Agents have freedom to use flat or grouped structure based on task complexity"
    echo ""
    echo "### Semantic Note Types"
    echo ""
    echo "Use these standard note names for structured organization:"
    echo ""
    echo "- **CONTEXT.md**: Scope and goals (what the work is for, what problem it solves, success criteria)"
    echo "- **EVIDENCE.md**: Facts gathered (logs, metrics, test results, experimental data)"
    echo "- **TODOS.md**: Current tasks (what needs to be done, in progress, blocked)"
    echo "- **insights.json**: Discoveries with origin and impact (patterns found, decisions made, implications), stored as JSON with evidence-based tracking"
    echo ""
    echo "### When to Use Notes (Trigger-Based)"
    echo ""
    echo "**REQUIRED Triggers** (always do these):"
    echo "- **First story in feature**: Create \`CONTEXT.md\` when beginning a new feature (REQUIRED for first story)"
    echo "- **After execution**: Document execution attempt in \`insights.json\` after every execution (success or failure) - REQUIRED"
    echo ""
    echo "**Conditional Triggers** (do when condition occurs):"
    echo "- **Task status changes**: Update \`TODOS.md\` when task status changes (REQUIRED when status changes)"
    echo "- **Discoveries made**: Write to \`insights.json\` when patterns or decisions are found, marking as evidence-based when supported by first-hand evidence"
    echo "- **Bug fixes**: Capture lessons in project-level \`insights.json\` (REQUIRED when fixing bugs)"
    echo "  - Location: \`~/docs/{project-name}/insights.json\`"
    echo "- **Feature completion**: Capture lessons in project-level \`insights.json\` (REQUIRED when completing features)"
    echo "  - Location: \`~/docs/{project-name}/insights.json\`"
    echo "- **>3 iterations needed**: Capture lessons in project-level \`insights.json\` (REQUIRED when >3 iterations)"
    echo "  - Location: \`~/docs/{project-name}/insights.json\`"
    echo "- **When gathering facts**: Write to \`EVIDENCE.md\` when collecting data (investigations)"
    echo ""
    echo "### Implementation Examples"
    echo ""
    echo "**Example: Writing a note with tags**"
    echo "\`\`\`"
    echo "# Use write_file to create note"
    echo "# Path: ~/docs/{project-name}/notes/US-001-user-login-form-CONTEXT.md"
    echo "# Content includes YAML frontmatter with category and tags:"
    echo "---"
    echo "created: 2024-01-15T10:00:00Z"
    echo "updated: 2024-01-15T10:00:00Z"
    echo "category: features"
    echo "agent: uidev"
    echo "tags: [\"frontend\", \"uidev\", \"authentication\"]"
    echo "commit: abc123def456"
    echo "---"
    echo ""
    echo "# Feature: User Login Form"
    echo ""
    echo "## Problem Statement"
    echo "Users need to authenticate..."
    echo "\`\`\`"
    echo ""
    echo "**Note**: Always include **minimum required tags** (domain: \"backend\" or \"frontend\", agent name), plus any additional relevant tags using your specialized knowledge"
    echo ""
    echo "**Example: Searching notes by tag**"
    echo "\`\`\`"
    echo "# Before starting a frontend task, search for related work:"
    echo "# 1. Use search_by_tag(\"frontend\") to find all frontend-related notes"
    echo "# 2. Use search_by_tag(\"uidev\") to find notes from your previous work"
    echo "# 3. Use search_by_tag(\"authentication\") to find authentication-related notes"
    echo "# 4. Read the most relevant notes to learn from previous work"
    echo "#"
    echo "# Implementation:"
    echo "# - Use grep to search frontmatter for tags: [\"frontend\"]"
    echo "# - Use list_dir to scan ~/docs/{project-name}/notes/"
    echo "# - For each file, use read_file to read frontmatter"
    echo "# - Extract tags array from frontmatter: tags: [\"frontend\", \"uidev\"]"
    echo "# - Filter files where tags array contains the search tag (case-insensitive)"
    echo "\`\`\`"
    echo ""
    echo "**Error Handling**: Note operations are non-fatal - if note reading/writing fails, execution continues (notes are helpful but not critical)"
    echo ""
    echo "---"
    echo ""
}

# Add story details to prompt
add_story_details() {
    local story_id="$1"
    local story_details
    story_details=$(jq -r '
        .userStories[] | select(.id == "'"$story_id"'") |
        "**Story ID**: " + .id + "\n**Title**: " + .title + "\n**Type**: " + .type + "\n**Priority**: " + (.priority | tostring) +
        "\n**Dependencies**: " + ((.dependencies // []) | join(", ")) + "\n\n**Description**:\n" + .description +
        "\n\n**Acceptance Criteria**:\n" + ((.acceptanceCriteria // []) | map("- " + .) | join("\n")) + "\n\n**Notes**: " + (.notes // "")
    ' "$PRD_JSON" 2>/dev/null)
    
    if [ -z "$story_details" ]; then
        error "Failed to extract story details for: $story_id"
        return 1
    fi
    
    echo "## Current User Story"
    echo ""
    echo "$story_details"
    echo ""
    echo "---"
    echo ""
}

# Add browser verification skill if needed
add_browser_verification() {
    local story_id="$1"
    local story_type
    story_type=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .type" "$PRD_JSON" 2>/dev/null)
    
    if [ "$story_type" = "frontend" ]; then
        local skill_file="$SKILLS_DIR/browser-verification/skill.md"
        
        # Validate path
        if ! validate_path "$skill_file" "$SKILLS_DIR"; then
            warn "Invalid skill path detected: $skill_file"
            return 0
        fi
        
        if [ -f "$skill_file" ]; then
            echo "## Browser Verification Skill"
            echo ""
            echo "**Required for frontend stories**: You must use the browser-verification skill to verify UI changes work correctly in a browser before marking this story as complete."
            echo ""
            cat "$skill_file"
            echo ""
            echo "---"
            echo ""
        else
            warn "Browser-verification skill not found at $skill_file (frontend story requires browser verification)"
        fi
    fi
}

# Add quality checks to prompt (runnable commands only; quality.notes is shown as documentation, not run)
add_quality_checks() {
    local quality_checks
    quality_checks=$(get_quality_command_lines | while IFS='|' read -r label cmd; do echo "- **$label**: \`$cmd\`"; done)
    
    if [ -z "$quality_checks" ]; then
        warn "No quality checks found in prd.json"
    else
        echo "## Quality Checks"
        echo ""
        echo "Before marking this story as complete, you must run all of the following quality check commands. **All commands must pass**:"
        echo ""
        echo "$quality_checks"
        echo ""
    fi
    
    # quality.notes is documentation only — display if present, never run as a command
    local quality_notes
    quality_notes=$(jq -r '.quality.notes // empty' "$PRD_JSON" 2>/dev/null)
    if [ -n "$quality_notes" ]; then
        echo "**Quality notes (documentation)**: $quality_notes"
        echo ""
    fi
    
    if [ -n "$quality_checks" ] || [ -n "$quality_notes" ]; then
        echo "---"
        echo ""
    fi
}

# Add execution instructions to prompt
add_execution_instructions() {
    local story_id="$1"
    local story_title
    story_title=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .title" "$PRD_JSON" 2>/dev/null || echo "Unknown")
    
    if [ -z "$story_title" ] || [ "$story_title" = "null" ]; then
        story_title="Unknown"
    fi
    
    local year
    year=$(date +%Y)
    
    echo "## Execution Instructions"
    echo ""
    echo "You are implementing user story **$story_id: $story_title**."
    echo ""
    echo "### Steps:"
    echo ""
    echo "1. **Read Context First**:"
    echo "   - Review project notes and dev-notes for relevant patterns"
    echo "   - Review feature notes (CONTEXT.md, TODOS.md, insights.json) if available above"
    echo "   - **Discover Existing Tags**: Read tags index using \`read_tags_index()\` to discover existing tags and maintain consistency:"
    echo "     - See what tags have been used before (avoid creating duplicate tags like \"auth\" vs \"authentication\")"
    echo "     - Find related tags that might help discover similar work"
    echo "     - Check tag usage counts to see which tags are most common"
    echo "   - **Search Related Notes**: Use \`search_by_tag\` to find related work before starting:"
    echo "     - \`search_by_tag(\"backend\")\` or \`search_by_tag(\"frontend\")\` to find notes from similar domain work"
    echo "     - \`search_by_tag(\"your-agent-name\")\` to find notes from your previous work and learn from your own patterns"
    echo "     - Use tags discovered from tags index to find related work (e.g., if tags index shows \"authentication\" is used, search for it)"
    echo "     - Read related notes to understand patterns, learnings, and approaches from similar work"
    echo "   - **Review Project-Level Lessons**: Project-level lessons (if available above) contain learnings from past work - read these to inform your approach and avoid repeating mistakes"
    echo "   - **Learn from Previous Attempts**: If \`insights.json\` contains execution attempts, read them to understand:"
    echo "     - What approaches were tried before"
    echo "     - What failed and why (prioritize evidence-based insights)"
    echo "     - What worked"
    echo "     - How to adjust your approach based on previous learnings"
    echo "   - Understand the current story requirements"
    echo "2. **Implement the Story**: Follow the agent's rules and style guides"
    echo "3. **For Frontend Stories**: Use browser-verification skill if type is \"frontend\""
    echo "4. **Run Quality Checks**: Execute all quality check commands, all must pass"
    echo "5. **Update Notes** (Required based on triggers below):"
    echo ""
    echo "   **Note-Taking Triggers** (create/update notes when these conditions are met):"
    echo ""
    echo "   **REQUIRED - Always do these:**"
    echo "   - **First story in feature**: If this is the first story in the feature, you MUST create \`CONTEXT.md\` with feature goals, scope, and success criteria"
    echo "     - **How to detect first story**: Check \`prd.json\` to see if any other stories have \`passes: true\`. If no other stories have passed, this is the first story."
    echo "     - Location: \`~/docs/{project-name}/notes/$FEATURE_NAME/CONTEXT.md\` or \`~/docs/{project-name}/notes/$FEATURE_NAME-CONTEXT.md\`"
    echo "     - Category: \"features\" in frontmatter"
    echo "   - **REQUIRED - After EVERY execution attempt (success or failure)**: You MUST document introspection in feature \`insights.json\`:"
    echo "     - Create or update execution-attempt insight with \`introspection\` object"
    echo "     - Include \`whatWentWell: string[]\` (at least one observation about what worked)"
    echo "     - Include \`whatCouldBeImproved: string[]\` (at least one observation about what could improve)"
    echo "     - Include \`recommendations: string[]\` (at least one actionable recommendation for next attempt)"
    echo "     - This is mandatory, not optional - every attempt must have introspection"
    echo "     - What approach was tried"
    echo "     - What worked and what didn't"
    echo "     - What errors or issues were encountered (if any)"
    echo "     - What was learned from this attempt"
    echo "     - How the next attempt should differ (if the story didn't complete)"
    echo "     - Whether the insight is evidence-based (\`evidenceBased: true/false\`)"
    echo "     - If evidence-based, document evidence in \`evidence: {}\` object"
    echo "     - Location: \`~/docs/{project-name}/notes/$FEATURE_NAME/insights.json\` or \`~/docs/{project-name}/notes/$FEATURE_NAME-insights.json\`"
    echo ""
    echo "   **How to Add Introspection to Existing insights.json**:"
    echo "   1. Read existing file: \`read_file(\"~/docs/{project-name}/notes/$FEATURE_NAME/insights.json\")\`"
    echo "   2. Parse JSON structure"
    echo "   3. Create new insight object with required \`introspection\` object:"
    echo "      \`\`\`json"
    echo "      {"
    echo "        \"id\": \"insight-XXX\","
    echo "        \"timestamp\": \"ISO8601\","
    echo "        \"type\": \"execution-attempt\","
    echo "        \"story\": \"$story_id\","
    echo "        \"attempt\": N,"
    echo "        \"title\": \"...\","
    echo "        \"description\": \"...\","
    echo "        \"result\": \"success\" | \"failed\" | \"partial\","
    echo "        \"introspection\": {"
    echo "          \"whatWentWell\": [\"observation 1\", \"observation 2\"],"
    echo "          \"whatCouldBeImproved\": [\"observation 1\"],"
    echo "          \"recommendations\": [\"recommendation 1\", \"recommendation 2\"]"
    echo "        }"
    echo "      }"
    echo "      \`\`\`"
    echo "   4. Append to \`insights\` array"
    echo "   5. Update \`metadata.updated\` timestamp"
    echo "   6. Write back using \`write_file\`"
    echo ""
    echo "   **REQUIRED - When these conditions occur:**"
    echo "   - **Task status changes**: Update \`TODOS.md\` when task status changes (mark tasks complete, add new tasks, update blockers)"
    echo "     - Location: \`~/docs/{project-name}/notes/$FEATURE_NAME/TODOS.md\` or \`~/docs/{project-name}/notes/$FEATURE_NAME-TODOS.md\`"
    echo "   - **Discoveries made**: Write to feature \`insights.json\` when patterns are found, decisions are made, or important discoveries occur"
    echo "   - **Bug fixes**: ALWAYS capture lessons in project-level \`insights.json\` (category: \"projects\") when fixing bugs"
    echo "     - What was learned from diagnosing/fixing the bug (mark as \`lesson: true\`)"
    echo "     - Location: \`~/docs/{project-name}/insights.json\`"
    echo "   - **Feature completion**: ALWAYS capture lessons in project-level \`insights.json\` when completing a feature"
    echo "     - Implementation learnings, patterns discovered, gotchas (mark as \`lesson: true\`)"
    echo "     - Location: \`~/docs/{project-name}/insights.json\`"
    echo "   - **>3 iterations needed**: ALWAYS capture lessons when more than 3 iterations were needed"
    echo "     - Document complexity and what made it difficult (mark as \`lesson: true\`, include \`iterations\` count)"
    echo "     - Location: \`~/docs/{project-name}/insights.json\`"
    echo ""
    echo "   **Tag Requirements** (apply to all notes):"
    echo "   - **Always include minimum required tags** in frontmatter:"
    echo "     - Domain tag: \"backend\" or \"frontend\" (based on story type) - REQUIRED"
    echo "     - Agent name tag: your agent name (e.g., \"uidev\", \"rusty\", \"steve\") - REQUIRED"
    echo "   - **Tag Validation**: Before marking story complete, verify that all notes you created/updated include the minimum required tags (domain tag + agent name tag)"
    echo "   - **Optional additional tags**: Add any relevant tags using your specialized knowledge (technology, feature area, patterns, etc.)"
    echo "   - **Tag Consistency**: Before adding new tags, check tags index using \`read_tags_index()\` to see if similar tags already exist (e.g., prefer \"authentication\" over \"auth\" if \"authentication\" is already used)"
    echo "   - **Add Tags to Index**: When using tags in your notes, call \`add_tag(\"tag-name\", category?, description?)\` to add them to the tags index (idempotent - safe to call multiple times)"
    echo "     - Example: \`add_tag(\"react\", \"technology\", \"React framework\")\`"
    echo "     - Example: \`add_tag(\"authentication\", \"feature\", \"Authentication and authorization features\")\`"
    echo "   - **Update Tags Index**: After creating/updating notes, call \`update_tags_index()\` to refresh tag usage statistics"
    echo "   - Example frontmatter: \`tags: [\"frontend\", \"uidev\", \"react\", \"authentication\", \"form-validation\"]\`"
    echo ""
    echo "   **Lesson Format** (for project-level lessons):"
    echo "   - Clear description of what was learned"
    echo "   - Why it matters"
    echo "   - How it should influence future work"
    echo "   - Mark as \`lesson: true\`"
    echo "   - Include \`iterations\` count if applicable"
    echo "   - Lessons are automatically read before starting new tasks to inform approach and avoid repeating mistakes"
    echo "6. **Verify Notes Updated**: Before proceeding, verify you have updated notes according to the triggers above:"
    echo "   - If this is the first story in the feature, verify \`CONTEXT.md\` was created (check \`prd.json\` to confirm no other stories have \`passes: true\`)"
    echo "   - Verify execution attempt was documented in \`insights.json\`"
    echo "   - If task status changed, verify \`TODOS.md\` was updated"
    echo "   - If discoveries were made, verify they were added to \`insights.json\`"
    echo "   - If this was a bug fix, feature completion, or required >3 iterations, verify lessons were captured in project-level \`insights.json\` at \`~/docs/{project-name}/insights.json\`"
    echo "   - **Tag Validation**: Verify all notes you created/updated include minimum required tags (domain tag + agent name tag) in frontmatter"
    echo "7. **Update Project Documentation**: Before marking story complete, update project docs if needed:"
    echo "   - **DECISIONS.md** (REQUIRED if architectural/design decisions were made):"
    echo "     - Add entries for any architectural decisions, design choices, or trade-offs made"
    echo "     - Use format: Date, Context, Decision, Consequences"
    echo "     - Document why decisions were made and their implications"
    echo "     - Location: \`docs/DECISIONS.md\`"
    echo "   - **Other project docs** (update as needed):"
    echo "     - Update \`docs/TECH-STACK.md\` if new technologies or dependencies were added"
    echo "     - Update \`docs/ROADMAP.md\` if priorities or plans changed"
    echo "     - Update any other relevant documentation"
    echo "8. **Update Project State**: After successful implementation and documentation updates:"
    echo "   - Set \`passes: true\` for this story in \`docs/feature/$FEATURE_NAME/prd.json\`"
    echo "   - Append progress entry to \`docs/NOTES.md\` using our format (see below)"
    echo "   - Update \`DEV-NOTES.md\` in directories where you edited files (if you discovered reusable patterns)"
    echo "9. **Commit**: Create commit with message: \`feat: $story_id - $story_title\`"
    echo ""
    echo "### Progress Report Format (docs/NOTES.md)"
    echo ""
    echo "APPEND to \`docs/NOTES.md\` (never replace, always append):"
    echo "\`\`\`"
    echo "## [Date/Time] - [Story ID]"
    echo "- What was implemented"
    echo "- Files changed"
    echo "- **Learnings for future iterations:**"
    echo "  - Patterns discovered (e.g., \"this codebase uses X for Y\")"
    echo "  - Gotchas encountered (e.g., \"don't forget to update Z when changing W\")"
    echo "  - Useful context (e.g., \"the evaluation panel is in component X\")"
    echo "---"
    echo "\`\`\`"
    echo ""
    echo "### Consolidate Patterns"
    echo ""
    echo "If you discover a **reusable pattern** that future iterations should know, add it to the \`## Codebase Patterns\` section at the TOP of \`~/docs/{project-name}/NOTES.md\` (create it if it doesn't exist). This section should consolidate the most important learnings:"
    echo ""
    echo "\`\`\`"
    echo "## Codebase Patterns"
    echo "- Example: Use \`sql<number>\` template for aggregations"
    echo "- Example: Always use \`IF NOT EXISTS\` for migrations"
    echo "- Example: Export types from actions.ts for UI components"
    echo "\`\`\`"
    echo ""
    echo "Only add patterns that are **general and reusable**, not story-specific details."
    echo ""
    echo "### Update DEV-NOTES.md Files (Directory-Level Learnings)"
    echo ""
    echo "Before committing, check if any edited files have learnings worth preserving in nearby \`DEV-NOTES.md\` files:"
    echo ""
    echo "1. **Identify directories with edited files** - Look at which directories you modified"
    echo "2. **Check for existing DEV-NOTES.md** - Look for \`DEV-NOTES.md\` in those directories or parent directories"
    echo "3. **Check coding standards first** - Before adding to DEV-NOTES.md, verify the knowledge is NOT already in:"
    echo "   - Your agent's style guide (referenced in agent definition, e.g., \`rust-style.md\`, \`javascript-style.md\`)"
    echo "   - Your agent's rules and capabilities (in the agent definition above)"
    echo "   - General coding standards and best practices"
    echo "4. **Only add NEW learnings** - Only append to DEV-NOTES.md if:"
    echo "   - The knowledge is NOT already covered in coding standards"
    echo "   - It's a project-specific pattern, gotcha, or convention"
    echo "   - It's a bugfix learning (especially valuable after diagnosing an issue)"
    echo "   - It's a module-specific requirement or dependency"
    echo ""
    echo "**Examples of good DEV-NOTES.md additions (NEW learnings only):**"
    echo "- \"When modifying X, also update Y to keep them in sync\" (project-specific dependency)"
    echo "- \"This module uses pattern Z for all API calls\" (project-specific convention)"
    echo "- \"Tests require the dev server running on PORT 3000\" (project-specific requirement)"
    echo "- \"Field names must match the template exactly\" (project-specific gotcha)"
    echo "- \"Fixed bug: Component X crashes if Y is null - always check Y before rendering\" (bugfix learning)"
    echo ""
    echo "**Do NOT add:**"
    echo "- General coding standards (already in style guides)"
    echo "- Best practices covered in agent rules"
    echo "- Story-specific implementation details"
    echo "- Temporary debugging notes"
    echo "- Information already in ~/docs/{project-name}/NOTES.md"
    echo "- Knowledge already in coding standards"
    echo ""
    echo "**Special emphasis on bugfix learnings:**"
    echo "- Learnings from diagnosing and fixing bugs are especially valuable"
    echo "- These often reveal project-specific gotchas not covered in general standards"
    echo "- Document the bug, the fix, and why it happened to prevent recurrence"
    echo ""
    echo "Only update DEV-NOTES.md if you have **genuinely NEW, reusable knowledge** that would help future work in that directory."
    echo ""
    echo "### Important Notes:"
    echo ""
    echo "- **Do not mark the story as complete** (\`passes: true\`) until:"
    echo "  - All acceptance criteria are met"
    echo "  - All quality checks pass"
    echo "  - Browser verification passes (for frontend stories)"
    echo "  - Project documentation is updated (especially DECISIONS.md for architectural decisions)"
    echo "  - Code is committed (if applicable)"
    echo ""
    echo "- **If you encounter issues**:"
    echo "  - Document them in the story's \`notes\` field in prd.json"
    echo "  - Add details to project notes"
    echo "  - Do not set \`passes: true\` if the story is incomplete"
    echo ""
    echo "- **Dependencies**: Ensure all story dependencies (listed in \`dependencies\` array) have \`passes: true\` before starting this story. If dependencies are not complete, pause and wait."
    echo ""
    echo "- **Agent Rules**: Follow all rules and capabilities defined in your agent definition above."
}

# Build execution prompt
# Run failure diagnosis loop (Phase 1)
run_failure_diagnosis() {
    local story_id="$1"
    local failure_logs="$2"
    local diagnosis_file="$DOCS_DIR/feature/$FEATURE_NAME/diagnosis/${story_id}.md"
    
    mkdir -p "$(dirname "$diagnosis_file")"
    
    log "Story $story_id: Initiating failure diagnosis loop..."
    
    # 1. Diagnosis Step (/ask mode)
    local diagnosis_prompt=$(cat <<EOF
# Failure Diagnosis Request

The execution of story **$story_id** has failed. 

## Failure Context
$failure_logs

## Task
1. Analyze the logs and the current state of the codebase.
2. Identify the root cause of the failure.
3. Determine if it's a logic error, missing context, or environment issue.
4. Provide a clear diagnosis of the problem.
EOF
)
    
    local diagnosis_output_file="/tmp/diagnosis-${story_id}.log"
    log "Story $story_id: Running diagnosis (/ask mode)..."
    
    if ! agent -p --force --workspace "$(git rev-parse --show-toplevel)" --mode ask "$diagnosis_prompt" > "$diagnosis_output_file" 2>&1; then
        warn "Failure diagnosis agent failed. Proceeding with manual iteration."
        return 1
    fi
    
    # 2. Solution Ideation Step
    log "Story $story_id: Running solution ideation..."
    local diagnosis_content=$(cat "$diagnosis_output_file")
    local ideation_prompt=$(cat <<EOF
# Solution Ideation Request

Based on the following diagnosis of a failed execution for story **$story_id**, please run the \`ideate-solution\` command to propose at least 2 approaches to fix it.

## Diagnosis
$diagnosis_content

## Task
Invoke \`ideate-solution\` and present the options.
EOF
)
    
    local ideation_output_file="/tmp/ideation-${story_id}.log"
    if ! agent -p --force --workspace "$(git rev-parse --show-toplevel)" "$ideation_prompt" > "$ideation_output_file" 2>&1; then
        warn "Solution ideation agent failed. Proceeding with diagnosis only."
        echo "# Diagnosis (no ideation)" > "$diagnosis_file"
        echo "$diagnosis_content" >> "$diagnosis_file"
    else
        echo "# Failure Diagnosis & Proposed Solutions" > "$diagnosis_file"
        echo "## Diagnosis" >> "$diagnosis_file"
        echo "$diagnosis_content" >> "$diagnosis_file"
        echo "" >> "$diagnosis_file"
        echo "## Proposed Solutions" >> "$diagnosis_file"
        cat "$ideation_output_file" >> "$diagnosis_file"
    fi
    
    success "Story $story_id: Failure diagnosis loop completed. Results saved to $diagnosis_file"
    return 0
}

# Add diagnosis context to prompt
add_diagnosis_context() {
    local story_id="$1"
    local diagnosis_file="$DOCS_DIR/feature/$FEATURE_NAME/diagnosis/${story_id}.md"
    
    if [ -f "$diagnosis_file" ]; then
        echo "## Previous Failure Diagnosis"
        echo ""
        echo "The previous attempt for this story failed. Here is the diagnosis and proposed solutions:"
        echo ""
        cat "$diagnosis_file"
        echo ""
        echo "---"
        echo ""
        log "Added failure diagnosis context for story $story_id"
    fi
}

# Add previous introspection from feature notes
add_previous_introspection() {
    local story_id="$1"
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    local notes_base_dir="$DOCS_DIR/notes"
    local has_introspection=false
    
    # Try grouped structure first
    if [ -f "$feature_notes_dir/insights.json" ] && command -v jq >/dev/null 2>&1; then
        # Check if there are any execution-attempt insights with introspection for this story
        local has_story_introspection
        has_story_introspection=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection) | .id" "$feature_notes_dir/insights.json" 2>/dev/null | head -1)
        
        if [ -n "$has_story_introspection" ]; then
            echo "## Previous Introspection"
            echo ""
            echo "**Source**: Previous execution attempts for this story from \`~/docs/{project-name}/notes/$FEATURE_NAME/insights.json\`"
            echo ""
            echo "**Purpose**: Learn from previous introspection to inform your approach and avoid repeating mistakes."
            echo ""
            
            # Extract introspection from execution-attempt insights for this story
            local what_went_well
            local what_could_improve
            local recommendations
            
            what_went_well=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatWentWell) | .introspection.whatWentWell[]" "$feature_notes_dir/insights.json" 2>/dev/null | sort -u)
            what_could_improve=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatCouldBeImproved) | .introspection.whatCouldBeImproved[]" "$feature_notes_dir/insights.json" 2>/dev/null | sort -u)
            recommendations=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.recommendations) | .introspection.recommendations[]" "$feature_notes_dir/insights.json" 2>/dev/null | sort -u)
            
            if [ -n "$what_went_well" ] || [ -n "$what_could_improve" ] || [ -n "$recommendations" ]; then
                has_introspection=true
                
                if [ -n "$what_went_well" ]; then
                    echo "### What Went Well (from previous attempts)"
                    echo "$what_went_well" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                if [ -n "$what_could_improve" ]; then
                    echo "### What Could Be Improved (from previous attempts)"
                    echo "$what_could_improve" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                if [ -n "$recommendations" ]; then
                    echo "### Recommendations (from previous attempts)"
                    echo "$recommendations" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                echo "**Use this introspection to inform your approach and avoid repeating mistakes.**"
                echo ""
                echo "---"
                echo ""
            fi
        fi
    fi
    
    # Try flat structure if no introspection found
    if [ "$has_introspection" = "false" ] && [ -f "$notes_base_dir/${FEATURE_NAME}-insights.json" ] && command -v jq >/dev/null 2>&1; then
        local flat_insights="$notes_base_dir/${FEATURE_NAME}-insights.json"
        local has_story_introspection
        has_story_introspection=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection) | .id" "$flat_insights" 2>/dev/null | head -1)
        
        if [ -n "$has_story_introspection" ]; then
            echo "## Previous Introspection"
            echo ""
            echo "**Source**: Previous execution attempts for this story"
            echo ""
            
            local what_went_well
            local what_could_improve
            local recommendations
            
            what_went_well=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatWentWell) | .introspection.whatWentWell[]" "$flat_insights" 2>/dev/null | sort -u)
            what_could_improve=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatCouldBeImproved) | .introspection.whatCouldBeImproved[]" "$flat_insights" 2>/dev/null | sort -u)
            recommendations=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.recommendations) | .introspection.recommendations[]" "$flat_insights" 2>/dev/null | sort -u)
            
            if [ -n "$what_went_well" ] || [ -n "$what_could_improve" ] || [ -n "$recommendations" ]; then
                has_introspection=true
                
                if [ -n "$what_went_well" ]; then
                    echo "### What Went Well (from previous attempts)"
                    echo "$what_went_well" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                if [ -n "$what_could_improve" ]; then
                    echo "### What Could Be Improved (from previous attempts)"
                    echo "$what_could_improve" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                if [ -n "$recommendations" ]; then
                    echo "### Recommendations (from previous attempts)"
                    echo "$recommendations" | while IFS= read -r line; do
                        echo "- $line"
                    done
                    echo ""
                fi
                
                echo "---"
                echo ""
            fi
        fi
    fi
}

# Add enhanced introspection prompt when threshold is reached
add_introspection_threshold_prompt() {
    local story_id="$1"
    local story_iterations="$2"
    local feature_notes_dir="$DOCS_DIR/notes/$FEATURE_NAME"
    local notes_base_dir="$DOCS_DIR/notes"
    local insights_file=""
    
    # Find insights.json file
    if [ -f "$feature_notes_dir/insights.json" ]; then
        insights_file="$feature_notes_dir/insights.json"
    elif [ -f "$notes_base_dir/${FEATURE_NAME}-insights.json" ]; then
        insights_file="$notes_base_dir/${FEATURE_NAME}-insights.json"
    fi
    
    if [ -z "$insights_file" ] || [ ! -f "$insights_file" ] || ! command -v jq >/dev/null 2>&1; then
        return 0  # No insights file or jq not available, skip
    fi
    
    # Check if there are execution-attempt insights for this story
    local has_attempts
    has_attempts=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\") | .id" "$insights_file" 2>/dev/null | head -1)
    
    if [ -z "$has_attempts" ]; then
        return 0  # No previous attempts found
    fi
    
    echo "## ⚠️ Enhanced Introspection Required - Threshold Reached"
    echo ""
    echo "This story has reached $INTROSPECTION_THRESHOLD iterations without completion."
    echo ""
    echo "**Action Required**: Enhanced introspection - question the original approach and reconceive the task."
    echo ""
    
    # Aggregate introspection from all attempts
    local all_what_went_well
    local all_what_could_improve
    local all_recommendations
    
    all_what_went_well=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatWentWell) | .introspection.whatWentWell[]" "$insights_file" 2>/dev/null | sort -u)
    all_what_could_improve=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.whatCouldBeImproved) | .introspection.whatCouldBeImproved[]" "$insights_file" 2>/dev/null | sort -u)
    all_recommendations=$(jq -r ".insights[] | select(.type == \"execution-attempt\" and .story == \"$story_id\" and .introspection.recommendations) | .introspection.recommendations[]" "$insights_file" 2>/dev/null | sort -u)
    
    echo "### Aggregated Introspection from All Previous Attempts"
    echo ""
    echo "Below is introspection aggregated from ALL $story_iterations previous attempts. Use this to identify patterns:"
    echo ""
    
    if [ -n "$all_what_went_well" ]; then
        echo "#### Patterns Across All Attempts - What Went Well:"
        echo "$all_what_went_well" | while IFS= read -r line; do
            echo "- $line"
        done
        echo ""
    fi
    
    if [ -n "$all_what_could_improve" ]; then
        echo "#### Patterns Across All Attempts - What Could Be Improved:"
        echo "$all_what_could_improve" | while IFS= read -r line; do
            echo "- $line"
        done
        echo ""
    fi
    
    if [ -n "$all_recommendations" ]; then
        echo "#### Patterns Across All Attempts - Previous Recommendations:"
        echo "$all_recommendations" | while IFS= read -r line; do
            echo "- $line"
        done
        echo ""
    fi
    
    echo "### Enhanced Reflection Questions:"
    echo ""
    echo "1. **Pattern Analysis**: What patterns emerge from the aggregated introspection above?"
    echo "   - What has been tried repeatedly? (Review all previous approaches)"
    echo "   - What consistently went wrong across attempts?"
    echo "   - What (if anything) consistently worked?"
    echo ""
    echo "2. **Root Cause**: Why is the current approach not working? (Based on pattern analysis)"
    echo ""
    echo "3. **Approach Questioning**:"
    echo "   - Are the acceptance criteria realistic given what we've learned?"
    echo "   - Should the approach be fundamentally changed?"
    echo "   - Are there alternative ways to solve this problem?"
    echo ""
    echo "4. **Alternative Approaches**: Based on all previous attempts, what fundamentally different approaches should be considered?"
    echo ""
    echo "### Required Enhanced Actions:"
    echo ""
    echo "1. **Document enhanced introspection** in insights.json with:"
    echo "   - Summary of all approaches tried (from aggregated introspection above)"
    echo "   - Pattern analysis: What consistently worked vs. what consistently failed"
    echo "   - Root cause analysis: Why current approach isn't working (based on patterns)"
    echo "   - Alternative approaches to consider (with rationale based on pattern insights)"
    echo "   - Recommendations for reconceiving the task (specific, actionable, informed by all attempts)"
    echo ""
    echo "2. **Propose approach change** if needed (update CONTEXT.md or create new approach document)"
    echo ""
    echo "3. **Update recommendations** with actionable next steps for new approach"
    echo ""
    echo "**Goal**: Break out of iteration loop by finding a fundamentally different approach or adjusting requirements, informed by comprehensive pattern analysis from all previous attempts."
    echo ""
    echo "---"
    echo ""
}

build_prompt() {
    local story_id="$1"
    local story_iterations="${2:-0}"
    
    # Add all components
    add_agent_definition "$story_id" || return 1
    add_progress_context  # Read FIRST for project-level context
    add_project_context
    add_feature_notes
    add_previous_introspection "$story_id"  # Add previous introspection after feature notes
    add_project_lessons  # Read project-level lessons after feature notes
    add_diagnosis_context "$story_id" # Add diagnosis if it exists
    
    # Add enhanced introspection prompt if threshold reached
    if [ "$story_iterations" -ge "$INTROSPECTION_THRESHOLD" ]; then
        local story_passes
        story_passes=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .passes" "$PRD_JSON" 2>/dev/null)
        if [ "$story_passes" != "true" ]; then
            add_introspection_threshold_prompt "$story_id" "$story_iterations"
        fi
    fi
    
    add_story_details "$story_id" || return 1
    add_browser_verification "$story_id"
    add_quality_checks
    add_available_tools
    add_execution_instructions "$story_id"
}


# Run quality checks
run_quality_checks() {
    log "Running quality checks..."
    
    # Run from repository root so paths like scripts/quality.sh resolve correctly (not from docs/ submodule)
    local project_root
    project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    
    local quality_checks
    quality_checks=$(get_quality_command_lines)
    
    local failed=false
    while IFS='|' read -r label command; do
        [ -z "$label" ] && continue
        log "Running check: $label (timeout: ${QUALITY_CHECK_TIMEOUT}s)"
        # Run each command from repo root; quality.commands = one command per element; never run quality.notes
        if ! (cd "$project_root" && timeout "$QUALITY_CHECK_TIMEOUT" bash -c "$command" > /tmp/quality-check-$label.log 2>&1); then
            local exit_code=$?
            if [ $exit_code -eq 124 ]; then
                error "Quality check timed out after ${QUALITY_CHECK_TIMEOUT}s: $label"
                echo "Command exceeded timeout limit. Consider increasing QUALITY_CHECK_TIMEOUT or optimizing the command." >> /tmp/quality-check-$label.log
            else
                error "Quality check failed: $label"
            fi
            cat "/tmp/quality-check-$label.log"
            failed=true
        else
            success "Quality check passed: $label"
        fi
    done <<< "$quality_checks"
    
    if [ "$failed" = "true" ]; then
        return 1
    fi
    
    success "All quality checks passed"
    return 0
}

# Update story status in prd.json
update_story_status() {
    local story_id="$1"
    local passes="$2"
    local notes="$3"
    local increment_iterations="${4:-false}"
    
    local tmp_file
    tmp_file=$(mktemp)
    
    if [ "$increment_iterations" = "true" ]; then
        # Increment iterations counter
        jq ".userStories[] |= if .id == \"$story_id\" then .iterations = ((.iterations // 0) + 1) | .passes = $passes | .notes = \"$notes\" else . end" "$PRD_JSON" > "$tmp_file"
    else
        # Just update passes and notes
        jq ".userStories[] |= if .id == \"$story_id\" then .passes = $passes | .notes = \"$notes\" else . end" "$PRD_JSON" > "$tmp_file"
    fi
    mv "$tmp_file" "$PRD_JSON"
}

# Check if all stories pass
all_stories_pass() {
    local all_pass
    all_pass=$(jq -r '.userStories[] | select(.passes == false) | .id' "$PRD_JSON" 2>/dev/null)
    
    if [ -z "$all_pass" ]; then
        return 0
    else
        return 1
    fi
}

# Calculate progress statistics
calculate_progress() {
    local prd_json="$1"
    local total_stories
    total_stories=$(jq -r '.userStories | length' "$prd_json" 2>/dev/null || echo "0")
    local completed_stories
    completed_stories=$(jq -r '.userStories[] | select(.passes == true) | .id' "$prd_json" 2>/dev/null | wc -l | tr -d ' ')
    local remaining_stories
    remaining_stories=$((total_stories - completed_stories))
    
    if [ "$total_stories" -eq 0 ]; then
        echo "0|0|0|0"
        return
    fi
    
    local percentage
    if command -v awk >/dev/null 2>&1; then
        percentage=$(awk "BEGIN {printf \"%.1f\", ($completed_stories / $total_stories) * 100}")
    else
        percentage=$((completed_stories * 100 / total_stories))
    fi
    
    echo "${completed_stories}|${total_stories}|${remaining_stories}|${percentage}"
}

# Display progress information
show_progress() {
    local prd_json="$1"
    local current_story="$2"
    local progress_stats
    progress_stats=$(calculate_progress "$prd_json")
    
    IFS='|' read -r completed total remaining percentage <<< "$progress_stats"
    
    echo ""
    log "═══════════════════════════════════════════════════════════"
    log "Progress: $completed/$total stories completed (${percentage}%)"
    log "Remaining: $remaining stories"
    if [ -n "$current_story" ]; then
        log "Current: $current_story"
        # Phase 3 Improvement: Sync with PROGRESS.md
        local title
        title=$(jq -r ".userStories[] | select(.id == \"$current_story\") | .title" "$prd_json")
        update_progress_position "Executing $current_story" "$title"
    fi
    log "═══════════════════════════════════════════════════════════"
    echo ""
}

# Archive feature
archive_feature() {
    log "Archiving feature: $FEATURE_NAME"
    
    local date_prefix
    date_prefix=$(date +%Y-%m-%d)
    local archived_dir="$DOCS_DIR/archived/$date_prefix-$FEATURE_NAME"
    local feature_dir="$DOCS_DIR/feature/$FEATURE_NAME"
    
    mkdir -p "$archived_dir"
    
    # Move entire feature directory
    mv "$feature_dir"/* "$archived_dir/" 2>/dev/null || true
    rmdir "$feature_dir" 2>/dev/null || true
    
    success "Feature archived to: $archived_dir"
    
    append_notes "Feature completed and archived: $FEATURE_NAME"
}

# Execute a single feature
execute_single_feature() {
    local feature_name="$1"
    FEATURE_NAME="$feature_name"
    PRD_JSON="$DOCS_DIR/feature/$FEATURE_NAME/prd.json"
    
    log "Starting execution for feature: $FEATURE_NAME"
    
    validate_prd
    init_git_branch
    
    local iteration=0
    
    while [ $iteration -lt $MAX_ITERATIONS ]; do
        iteration=$((iteration + 1))
        log "Iteration $iteration/$MAX_ITERATIONS"
        
        # Phase 3: Synthesize active memory rules
        synthesize_feature_rules "$FEATURE_NAME"
        
        # Phase 3: Enable allowed MCP servers
        manage_mcp_servers "enable"
        
        # Show progress
        show_progress "$PRD_JSON" ""
        
        # Check if all stories pass
        if all_stories_pass; then
            success "All stories completed!"
            
            if [ "$DRY_RUN" = "true" ]; then
                log "DRY-RUN: All stories would be marked as complete"
                log "In real execution, final quality gate would run, then feature would be archived"
                return 0
            fi
            
            # Create feature completion chapter (qualitative trigger)
            create_feature_completion_chapter
            
            # Run final quality gate
            log "Running final quality gate..."
            if run_quality_checks; then
                archive_feature
                # Phase 3: Final cleanup
                manage_mcp_servers "disable"
                cleanup_feature_rules "$FEATURE_NAME"
                success "Feature execution complete!"
                return 0
            else
                # Phase 3: Cleanup on failure
                manage_mcp_servers "disable"
                cleanup_feature_rules "$FEATURE_NAME"
                error "Final quality gate failed"
                return 1
            fi
        fi

        # Phase 2: Parallel Execution
        # Find current wave
        local current_wave
        current_wave=$(find_current_wave "$PRD_JSON")
        
        if [ -n "$current_wave" ] && [ "$current_wave" != "null" ]; then
            # Find stories that can run in parallel
            local parallel_ids
            parallel_ids=$(find_parallel_stories "$PRD_JSON" "$current_wave")
            
            # If we have multiple stories that can run in parallel, try it
            local parallel_count
            parallel_count=$(echo "$parallel_ids" | grep -v "^$" | wc -l | tr -d ' ')
            if [ "$parallel_count" -gt 1 ]; then
                log "Phase 2: Found $parallel_count parallel stories in wave $current_wave"
                if [ "$DRY_RUN" = "true" ]; then
                    log "DRY-RUN: Would execute stories in parallel: $(echo $parallel_ids | xargs)"
                else
                    if execute_parallel_stories "$current_wave"; then
                        log "Parallel execution for wave $current_wave completed some stories"
                        # Phase 3: Disable MCP servers before continuing
                        manage_mcp_servers "disable"
                        continue # Move to next iteration to re-evaluate state
                    else
                        warn "Parallel execution for wave $current_wave failed or had no results, falling back to sequential"
                    fi
                fi
            fi
        fi
        
        # Fallback to sequential execution (existing logic)
        # Find next available story
        local story_id
        story_id=$(find_next_story "$PRD_JSON")
        
        if [ -z "$story_id" ]; then
            error "No available stories - all remaining stories have unsatisfied dependencies"
            echo ""
            local dep_details
            dep_details=$(get_dependency_details "$PRD_JSON")
            if [ -n "$dep_details" ]; then
                echo "Blocked stories:"
                echo "$dep_details"
                echo ""
            fi
            warn "To resolve:"
            warn "  1. Complete the missing dependencies (mark them as passes: true in prd.json)"
            warn "  2. Or adjust dependencies in prd.json if they're incorrect"
            warn "  3. Then run /execute-feature again to continue"
            return 1
        fi
        
        log "Working on story: $story_id"
        show_progress "$PRD_JSON" "$story_id"
        
        # Increment iteration count for this story (track attempts - increment before starting work)
        # Skip in dry-run mode to avoid modifying prd.json
        if [ "$DRY_RUN" = "false" ]; then
            update_story_status "$story_id" "false" "" "true"
        fi
        local story_iterations
        story_iterations=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .iterations" "$PRD_JSON")
        if [ "$DRY_RUN" = "true" ]; then
            log "Story $story_id (would be attempt #$((story_iterations + 1)))"
        else
            log "Story $story_id attempt #$story_iterations"
        fi
        
        # Build prompt (pass story_iterations for threshold check)
        log "Building prompt for $story_id..."
        local prompt_file
        prompt_file=$(mktemp)
        if ! build_prompt "$story_id" "$story_iterations" > "$prompt_file"; then
            error "Build prompt failed for story $story_id (check agent file exists and prd.json story details above)"
            rm -f "$prompt_file"
            return 1
        fi
        
        # Log prompt size for visibility
        local prompt_size_bytes
        prompt_size_bytes=$(wc -c < "$prompt_file" | tr -d ' ')
        local prompt_size_lines
        prompt_size_lines=$(wc -l < "$prompt_file" | tr -d ' ')
        # Calculate KB (using awk for portability, fallback to simple division)
        local prompt_size_kb
        if command -v awk >/dev/null 2>&1; then
            prompt_size_kb=$(awk "BEGIN {printf \"%.1f\", $prompt_size_bytes / 1024}")
        else
            # Fallback: simple integer division
            prompt_size_kb=$((prompt_size_bytes / 1024))
        fi
        log "Prompt size: ${prompt_size_bytes} bytes (${prompt_size_kb} KB, ${prompt_size_lines} lines)"
        
        # Get story title for command
        local story_title
        story_title=$(jq -r ".userStories[] | select(.id == \"$story_id\") | .title" "$PRD_JSON")
        
        # Dry-run mode: save prompt and exit
        if [ "$DRY_RUN" = "true" ]; then
            local dry_run_output_file
            dry_run_output_file="$DOCS_DIR/feature/$FEATURE_NAME/dry-run-prompt-${story_id}.md"
            mkdir -p "$(dirname "$dry_run_output_file")"
            cp "$prompt_file" "$dry_run_output_file"
            
            echo ""
            success "DRY-RUN: Prompt saved to: $dry_run_output_file"
            echo ""
            log "What would happen next (if not in dry-run mode):"
            echo "  1. Spawn Cursor CLI agent with the prompt above"
            echo "  2. Agent would implement story: $story_id - $story_title"
            echo "  3. Run quality checks from prd.json.quality"
            echo "  4. If quality checks pass: commit changes and mark story as complete"
            echo "  5. Continue to next story or complete feature"
            echo ""
            log "To actually execute, run without --dry-run flag:"
            echo "  /execute-feature $FEATURE_NAME"
            echo ""
            
            rm -f "$prompt_file"
            return 0
        fi
        
        # Run Cursor CLI
        log "Spawning Cursor CLI agent..."
        
        # Phase 1: Dynamic Mode and Model Selection
        local current_mode
        current_mode=$(select_mode "$story_id")
        local current_model
        current_model=$(select_model "$story_id")
        
        local mode_flag=""
        if [ "$current_mode" != "default" ]; then
            mode_flag="--mode $current_mode"
            log "Using mode: $current_mode (complex story)"
        fi
        
        if [ "$current_model" != "auto" ]; then
            log "Using model: $current_model (dynamic selection)"
        else
            log "Using model: auto"
        fi
        
        local project_root
        project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
        
        # Phase 1: Use --output-format json and dynamic flags
        if agent -p --force --workspace "$project_root" --model "$current_model" $mode_flag --output-format json "$(cat "$prompt_file")" > /tmp/cursor-output.log 2>&1; then
            # Phase 1: Use verified JSON parsing
            if parse_agent_output "/tmp/cursor-output.log" "$story_id"; then
                success "Agent execution completed successfully"
            else
                warn "Agent execution finished but status check failed"
                # Fallback check: if quality checks pass later, we might still be okay
            fi
        else
            local agent_exit_code=$?
            error "Agent execution failed (exit code: $agent_exit_code)"
            cat /tmp/cursor-output.log
            
            # Categorize failure type
            local failure_type="permanent"
            local failure_reason="Agent execution failed"
            
            # Check for transient failures (network issues, rate limits, etc.)
            if grep -qi "timeout\|network\|rate limit\|429\|503\|502" /tmp/cursor-output.log 2>/dev/null; then
                failure_type="transient"
                failure_reason="Agent execution failed (likely transient: network/timeout/rate limit)"
                warn "This appears to be a transient failure - will retry on next iteration"
            elif grep -qi "authentication\|auth\|unauthorized\|401\|403" /tmp/cursor-output.log 2>/dev/null; then
                failure_type="permanent"
                failure_reason="Agent execution failed (authentication error - check Cursor CLI auth)"
                error "Authentication error detected - please check Cursor CLI authentication"
            fi
            
            update_story_status "$story_id" "false" "$failure_reason in iteration $iteration (attempt $story_iterations, type: $failure_type)"
            append_notes "Story $story_id agent execution failed (type: $failure_type, attempt $story_iterations)"
            
            # Phase 1: Failure Diagnosis for permanent failures
            if [ "$failure_type" = "permanent" ]; then
                run_failure_diagnosis "$story_id" "$(cat /tmp/cursor-output.log)"
            fi

            # For permanent failures after multiple attempts, pause for user intervention
            if [ "$failure_type" = "permanent" ] && [ "$story_iterations" -ge 3 ]; then
                error "Story $story_id has failed $story_iterations times with permanent errors"
                warn "Pausing execution for user intervention. Please review and fix issues."
                warn "To resume: Run /execute-feature $FEATURE_NAME again after fixing issues"
                return 1
            fi
            
            warn "Continuing to next iteration..."
            continue
        fi
        
        # Run quality checks
        local quality_check_result
        if run_quality_checks; then
            quality_check_result="passed"
            # Commit changes
            local commit_msg="feat: $story_id - $story_title"
            if git add -A && git commit -m "$commit_msg" > /dev/null 2>&1; then
                success "Committed: $commit_msg"
            else
                warn "No changes to commit"
            fi
            
            # Update story status (mark as complete)
            update_story_status "$story_id" "true" "Completed in iteration $iteration (total attempts: $story_iterations)"
            
            # Append to notes
            append_notes "Story $story_id completed: $story_title (completed in $story_iterations iteration(s))"
            
            # Check for chapter creation triggers (quantitative triggers)
            check_and_create_chapter_if_needed
            
            success "Story $story_id completed successfully in $story_iterations iteration(s)"
        else
            quality_check_result="failed"
            error "Quality checks failed for story $story_id"
            
            # Categorize quality check failure
            local failure_type="permanent"
            local failure_reason="Quality checks failed"
            
            # Check log files for transient issues
            local has_transient_issue=false
            for log_file in /tmp/quality-check-*.log; do
                if [ -f "$log_file" ]; then
                    if grep -qi "timeout\|network\|connection\|temporary" "$log_file" 2>/dev/null; then
                        has_transient_issue=true
                        failure_type="transient"
                        failure_reason="Quality checks failed (likely transient: timeout/network issue)"
                        break
                    fi
                fi
            done
            
            # If quality checks fail due to actual code issues (not transient), it's permanent
            if [ "$failure_type" != "transient" ]; then
                failure_type="permanent"
                failure_reason="Quality checks failed (code issues need fixing)"
            fi
            
            update_story_status "$story_id" "false" "$failure_reason in iteration $iteration (attempt $story_iterations, type: $failure_type)"
            append_notes "Story $story_id failed quality checks (type: $failure_type, attempt $story_iterations)"
            
            # Phase 1: Failure Diagnosis for permanent quality check failures
            if [ "$failure_type" = "permanent" ]; then
                # Collect all failed check logs
                local quality_failure_context=""
                for log_file in /tmp/quality-check-*.log; do
                    if [ -f "$log_file" ]; then
                        local check_label=$(basename "$log_file" .log | sed 's/quality-check-//')
                        quality_failure_context="$quality_failure_context\n\n### Check: $check_label\n$(cat "$log_file")"
                    fi
                done
                run_failure_diagnosis "$story_id" "Quality checks failed:$quality_failure_context"
            fi

            # For permanent failures after multiple attempts, provide guidance
            if [ "$failure_type" = "permanent" ] && [ "$story_iterations" -ge 3 ]; then
                warn "Story $story_id has failed quality checks $story_iterations times"
                warn "This suggests code issues that need manual review"
                warn "Consider:"
                warn "  1. Reviewing the quality check logs in /tmp/quality-check-*.log"
                warn "  2. Manually fixing the issues"
                warn "  3. Running quality checks manually to verify fixes"
                warn "  4. Resuming execution with /execute-feature $FEATURE_NAME"
            fi
            
            warn "Continuing to next iteration..."
        fi
        
        # Phase 3: Disable MCP servers for this iteration
        manage_mcp_servers "disable"
        
        rm -f "$prompt_file"
    done
    
    error "Max iterations reached ($MAX_ITERATIONS)"
    
    # Create max iterations retro before exiting
    if [ "$DRY_RUN" = "false" ]; then
        log "Creating max iterations retrospective..."
        create_max_iterations_retro
    fi
    
    return 1
}

# Main execution loop
main() {
    check_prerequisites
    
    if [ "$DRY_RUN" = "true" ]; then
        echo ""
        warn "=========================================="
        warn "  DRY-RUN MODE ENABLED"
        warn "=========================================="
        warn "No actual execution will occur."
        warn "Prompts will be saved to feature directory for review."
        echo ""
    fi
    
    # If feature name provided, execute single feature
    if [ -n "$FEATURE_NAME" ]; then
        find_feature
        execute_single_feature "$FEATURE_NAME"
        return $?
    fi
    
    # Automatic mode: find and execute all incomplete features
    log "Automatic mode: scanning for incomplete features..."
    local incomplete_features
    incomplete_features=$(find_incomplete_features)
    
    if [ -z "$incomplete_features" ]; then
        success "No incomplete features found. All features are complete or archived."
        return 0
    fi
    
    local feature_count
    feature_count=$(echo "$incomplete_features" | grep -c '|' || echo "0")
    log "Found $feature_count feature(s) with incomplete stories"
    
    local executed=0
    local completed=0
    local failed=0
    
    while IFS='|' read -r feature_name prd_file; do
        [ -z "$feature_name" ] && continue
        
        executed=$((executed + 1))
        echo ""
        log "=========================================="
        log "Processing feature $executed/$feature_count: $feature_name"
        log "=========================================="
        echo ""
        
        FEATURE_NAME="$feature_name"
        PRD_JSON="$prd_file"
        
        if execute_single_feature "$feature_name"; then
            completed=$((completed + 1))
            success "Feature '$feature_name' completed successfully"
        else
            failed=$((failed + 1))
            warn "Feature '$feature_name' execution failed or paused"
            warn "You can resume by running: /execute-feature $feature_name"
        fi
        
        echo ""
    done <<< "$incomplete_features"
    
    echo ""
    log "=========================================="
    log "Automatic execution summary:"
    log "  Total features processed: $executed"
    log "  Completed: $completed"
    log "  Failed/Paused: $failed"
    log "=========================================="
    
    if [ $failed -gt 0 ]; then
        return 1
    fi
    return 0
}

# Run main function
main "$@"
