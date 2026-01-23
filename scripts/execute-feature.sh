#!/bin/bash

# Execute Feature Script
# Autonomous execution loop for features with prd.json

set -e

# Source common functions
. "$HOME/.cursor/scripts/common.sh"

# Parse command-line arguments
FEATURE_NAME=""
DRY_RUN=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --help|-h)
            echo "Usage: execute-feature.sh [feature_name] [--dry-run]"
            echo ""
            echo "Execute a planned feature autonomously using the execution loop."
            echo ""
            echo "Arguments:"
            echo "  feature_name    Name of the feature to execute (optional, will scan if not provided)"
            echo "  --dry-run       Preview the prompt without executing (saves prompt to file)"
            echo "  --help, -h      Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  MAX_ITERATIONS         Maximum iterations per feature (default: 10)"
            echo "  MODEL                  Cursor CLI model to use (default: auto)"
            echo "  QUALITY_CHECK_TIMEOUT  Timeout for quality checks in seconds (default: 300)"
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
CURSOR_DIR="$HOME/.cursor"
AGENTS_DIR="$CURSOR_DIR/agents"
SKILLS_DIR="$CURSOR_DIR/skills"
MAX_ITERATIONS="${MAX_ITERATIONS:-10}"
MODEL="${MODEL:-auto}"
QUALITY_CHECK_TIMEOUT="${QUALITY_CHECK_TIMEOUT:-300}"  # 5 minutes default timeout

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[execute-feature]${NC} $1"
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
    fi
    
    if ! command -v jq &> /dev/null; then
        error "jq not found. Please install jq for JSON parsing."
        exit 1
    fi
    
    if [ ! -d "$AGENTS_DIR" ]; then
        error "Agents directory not found: $AGENTS_DIR"
        exit 1
    fi
    
    if [ "$DRY_RUN" = "true" ]; then
        warn "DRY-RUN MODE: No actual execution will occur"
    fi
    
    success "Prerequisites check passed"
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

# Validate prd.json
validate_prd() {
    log "Validating prd.json..."
    
    if ! jq empty "$PRD_JSON" 2>/dev/null; then
        error "Invalid JSON in prd.json"
        exit 1
    fi
    
    # Check required fields
    local required_fields=("project" "featureName" "branchName" "description" "quality" "userStories")
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
        
        # Validate path to prevent directory traversal
        local agent_path="$AGENTS_DIR/$agent.md"
        if ! validate_path "$agent_path" "$AGENTS_DIR"; then
            error "Invalid agent path detected (possible security issue): $agent"
            exit 1
        fi
        
        if [ ! -f "$agent_path" ]; then
            error "Agent not found: $agent (expected $agent_path)"
            exit 1
        fi
    done
    
    # Detect dependency cycles
    log "Checking for dependency cycles..."
    if ! detect_dependency_cycles "$PRD_JSON"; then
        error "Dependency cycles detected in prd.json. Please fix dependencies before execution."
        exit 1
    fi
    
    # Validate quality checks
    local quality_count
    quality_count=$(jq '.quality | length' "$PRD_JSON" 2>/dev/null || echo "0")
    if [ "$quality_count" -eq 0 ]; then
        error "No quality checks defined in prd.json.quality (at least one quality check is required)"
        exit 1
    fi
    
    # Validate each quality check command is non-empty
    local quality_checks
    quality_checks=$(jq -r '.quality | to_entries[] | "\(.key)|\(.value)"' "$PRD_JSON")
    local has_empty=false
    while IFS='|' read -r label command; do
        if [ -z "$label" ] || [ -z "$command" ]; then
            error "Quality check has empty label or command"
            has_empty=true
        fi
        if [ -z "$command" ]; then
            error "Quality check '$label' has empty command"
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
    
    # Validate agent name format (should match filename pattern: lowercase, hyphens allowed)
    if [[ ! "$agent_name" =~ ^[a-z0-9-]+$ ]]; then
        error "Invalid agent name format: $agent_name (expected lowercase alphanumeric with hyphens)"
        return 1
    fi
    
    local agent_file="$AGENTS_DIR/$agent_name.md"
    
    # Validate path
    if ! validate_path "$agent_file" "$AGENTS_DIR"; then
        error "Invalid agent path detected: $agent_file"
        return 1
    fi
    
    if [ ! -f "$agent_file" ]; then
        error "Agent file not found: $agent_file"
        return 1
    fi
    
    # Verify agent name matches the file name
    local expected_filename="$agent_name.md"
    local actual_filename
    actual_filename=$(basename "$agent_file")
    
    if [ "$actual_filename" != "$expected_filename" ]; then
        error "Agent name '$agent_name' does not match file name '$actual_filename'"
        error "Expected file: $expected_filename, but found: $actual_filename"
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
    
    # First, try grouped structure: docs/notes/{feature-name}/
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
    
    # If grouped structure didn't have notes, try flat structure: docs/notes/{feature-name}-*.md
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
    local notes_base_dir="$DOCS_DIR/notes"
    local project_lessons_file="$notes_base_dir/project-lessons-insights.json"
    local project_insights_file="$notes_base_dir/project-insights.json"
    local has_lessons=false
    
    # Try project-lessons-insights.json first (explicit lessons file)
    if [ -f "$project_lessons_file" ]; then
        if command -v jq >/dev/null 2>&1; then
            # Filter for insights with lesson: true and category: "projects"
            local lessons
            lessons=$(jq -r '.insights[] | select(.lesson == true) | select(.metadata.category == "projects" or .metadata.category == null)' "$project_lessons_file" 2>/dev/null)
            
            if [ -n "$lessons" ] && [ "$lessons" != "null" ]; then
                echo "## Project-Level Lessons"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/project-lessons-insights.json\`"
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
                    "\n---\n"' "$project_lessons_file" 2>/dev/null || {
                    # Fallback: if jq parsing fails, show raw JSON
                    echo "**Note**: Unable to parse project lessons. Raw content:"
                    echo "\`\`\`json"
                    cat "$project_lessons_file"
                    echo "\`\`\`"
                }
                echo ""
                echo "---"
                echo ""
                has_lessons=true
                log "Reading project-level lessons from ~/docs/{project-name}/notes/project-lessons-insights.json"
            fi
        fi
    fi
    
    # Also try project-insights.json with category: "projects" filter
    if [ "$has_lessons" = "false" ] && [ -f "$project_insights_file" ]; then
        if command -v jq >/dev/null 2>&1; then
            # Check if file has category: "projects" in metadata and has lessons
            local category
            category=$(jq -r '.metadata.category // "unknown"' "$project_insights_file" 2>/dev/null)
            local has_project_lessons
            has_project_lessons=$(jq -r '.insights[] | select(.lesson == true) | .id' "$project_insights_file" 2>/dev/null | head -1)
            
            if [ "$category" = "projects" ] && [ -n "$has_project_lessons" ]; then
                echo "## Project-Level Lessons"
                echo ""
                echo "**Source**: \`~/docs/{project-name}/notes/project-insights.json\` (category: projects)"
                echo ""
                echo "**Purpose**: Lessons learned from past work. Read these to inform your approach and avoid repeating mistakes."
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
                log "Reading project-level lessons from ~/docs/{project-name}/notes/project-insights.json"
            fi
        fi
    fi
    
    # Also search for any insights.json files in notes/ with category: "projects"
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
    echo "     - \`metadata\` (optional): Additional metadata (agent, command, context, commit)"
    echo "   - **Returns**: \`{success: boolean, path: string, error?: string}\`"
    echo "   - **Implementation**: Use \`write_file\` to create note at \`~/docs/{project-name}/notes/{name}.md\` (flat) or \`~/docs/{project-name}/notes/{id}/{type}.md\` (grouped)"
    echo "   - **Frontmatter**: Include YAML frontmatter with \`category\`, \`created\`, \`updated\`, and optional metadata"
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
    echo ""
    echo "### Note Storage Structure"
    echo ""
    echo "**Category is stored in frontmatter/metadata, NOT in directory path.**"
    echo ""
    echo "**Flat Structure (Recommended)**:"
    echo "- \`docs/notes/{id}-{description}-{type}.md\`"
    echo "  - Example: \`docs/notes/US-001-user-login-form-CONTEXT.md\` (category: \"features\" in frontmatter)"
    echo "  - Example: \`docs/notes/memory-leak-2024-01-15-investigating-memory-growth-EVIDENCE.md\` (category: \"investigations\" in frontmatter)"
    echo ""
    echo "**Grouped Structure (Optional, for complex entities)**:"
    echo "- \`docs/notes/{id}-{description}/{type}.md\`"
    echo "  - Example: \`docs/notes/US-001-user-login-form/CONTEXT.md\` (category: \"features\" in frontmatter)"
    echo "  - Example: \`docs/notes/memory-leak-2024-01-15/EVIDENCE.md\` (category: \"investigations\" in frontmatter)"
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
    echo "### When to Use Notes"
    echo ""
    echo "- **At Feature Start**: Create \`CONTEXT.md\` when beginning a new feature (if it doesn't exist)"
    echo "- **During Task Execution**: Update \`TODOS.md\` when task status changes"
    echo "- **When Discoveries Are Made**: Write to \`insights.json\` when patterns or decisions are found, marking as evidence-based when supported by first-hand evidence"
    echo "- **When Gathering Facts**: Write to \`EVIDENCE.md\` when collecting data (investigations)"
    echo ""
    echo "### Implementation Examples"
    echo ""
    echo "**Example: Writing a note**"
    echo "\`\`\`"
    echo "# Use write_file to create note"
    echo "# Path: ~/docs/{project-name}/notes/US-001-user-login-form-CONTEXT.md"
    echo "# Content includes YAML frontmatter with category property:"
    echo "---"
    echo "created: 2024-01-15T10:00:00Z"
    echo "updated: 2024-01-15T10:00:00Z"
    echo "category: features"
    echo "commit: abc123def456"
    echo "---"
    echo ""
    echo "# Feature: User Login Form"
    echo ""
    echo "## Problem Statement"
    echo "Users need to authenticate..."
    echo "\`\`\`"
    echo ""
    echo "**Example: Reading notes by category**"
    echo "\`\`\`"
    echo "# Use list_dir to scan ~/docs/{project-name}/notes/"
    echo "# For each file, use read_file to read frontmatter"
    echo "# Extract category from frontmatter: category: investigations"
    echo "# Filter files where frontmatter.category matches desired category"
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
    story_details=$(jq -r ".userStories[] | select(.id == \"$story_id\") | \"**Story ID**: \(.id)\n**Title**: \(.title)\n**Type**: \(.type)\n**Priority**: \(.priority)\n**Dependencies**: \(.dependencies | join(\", \"))\n\n**Description**:\n\(.description)\n\n**Acceptance Criteria**:\n\(.acceptanceCriteria | .[] | \"- \(.)\")\n\n**Notes**: \(.notes // \"\")" "$PRD_JSON" 2>/dev/null)
    
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

# Add quality checks to prompt
add_quality_checks() {
    local quality_checks
    quality_checks=$(jq -r '.quality | to_entries[] | "- **\(.key)**: `\(.value)`"' "$PRD_JSON" 2>/dev/null)
    
    if [ -z "$quality_checks" ]; then
        warn "No quality checks found in prd.json"
        return 0
    fi
    
    echo "## Quality Checks"
    echo ""
    echo "Before marking this story as complete, you must run all of the following quality check commands. **All commands must pass**:"
    echo ""
    echo "$quality_checks"
    echo ""
    echo "---"
    echo ""
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
    echo "5. **Update Notes** (if applicable):"
    echo "   - Update \`TODOS.md\` in \`~/docs/{project-name}/notes/$FEATURE_NAME/\` when task status changes (category: "features" in frontmatter)"
    echo "   - **Document Execution Attempt**: After execution (success or failure), append to feature \`insights.json\`:"
    echo "     - What approach was tried"
    echo "     - What worked and what didn't"
    echo "     - What errors or issues were encountered (if any)"
    echo "     - What was learned from this attempt"
    echo "     - How the next attempt should differ (if the story didn't complete)"
    echo "     - Whether the insight is evidence-based (\`evidenceBased: true/false\`)"
    echo "     - If evidence-based, document evidence in \`evidence: {}\` object"
    echo "   - Write to feature \`insights.json\` when discoveries are made or decisions are documented"
    echo "   - **Capture Lessons**: After completing work (bug fix or feature completion), document lessons in project-level \`insights.json\` (category: \"projects\"):"
    echo "     - **Always capture lessons from bug fixes**: What was learned from diagnosing/fixing the bug (mark as \`lesson: true\`)"
    echo "     - **Always capture lessons from feature completion**: Implementation learnings, patterns discovered, gotchas (mark as \`lesson: true\`)"
    echo "     - **Capture lessons when >3 iterations needed**: Document complexity and what made it difficult (mark as \`lesson: true\`, include \`iterations\` count)"
    echo "     - Lesson format: Clear description of what was learned, why it matters, how it should influence future work"
    echo "     - Store in project-level insights.json: \`~/docs/{project-name}/notes/project-lessons-insights.json\` or project-level insights.json"
    echo "     - Lessons are automatically read before starting new tasks to inform approach and avoid repeating mistakes"
    echo "     - **Note**: Lesson capture is manual (agents document lessons themselves when completing work)."
    echo "   - Use note tools (write_note, append_note) to maintain persistent memory"
    echo "   - **Note**: If this is the first story in a feature, create \`CONTEXT.md\` with feature goals and scope"
    echo "6. **Update Project Documentation**: Before marking story complete, update project docs if needed:"
    echo "   - **DECISIONS.md** (REQUIRED if architectural/design decisions were made):"
    echo "     - Add entries for any architectural decisions, design choices, or trade-offs made"
    echo "     - Use format: Date, Context, Decision, Consequences"
    echo "     - Document why decisions were made and their implications"
    echo "     - Location: \`docs/DECISIONS.md\`"
    echo "   - **Other project docs** (update as needed):"
    echo "     - Update \`docs/TECH-STACK.md\` if new technologies or dependencies were added"
    echo "     - Update \`docs/ROADMAP.md\` if priorities or plans changed"
    echo "     - Update any other relevant documentation"
    echo "7. **Update Project State**: After successful implementation and documentation updates:"
    echo "   - Set \`passes: true\` for this story in \`docs/feature/$FEATURE_NAME/prd.json\`"
    echo "   - Append progress entry to \`docs/NOTES.md\` using our format (see below)"
    echo "   - Update \`DEV-NOTES.md\` in directories where you edited files (if you discovered reusable patterns)"
    echo "8. **Commit**: Create commit with message: \`feat: $story_id - $story_title\`"
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
build_prompt() {
    local story_id="$1"
    
    # Add all components
    add_agent_definition "$story_id" || return 1
    add_progress_context  # Read FIRST for project-level context
    add_project_context
    add_feature_notes
    add_project_lessons  # Read project-level lessons after feature notes
    add_story_details "$story_id" || return 1
    add_browser_verification "$story_id"
    add_quality_checks
    add_available_tools
    add_execution_instructions "$story_id"
}


# Run quality checks
run_quality_checks() {
    log "Running quality checks..."
    
    local quality_checks
    quality_checks=$(jq -r '.quality | to_entries[] | "\(.key)|\(.value)"' "$PRD_JSON")
    
    local failed=false
    while IFS='|' read -r label command; do
        log "Running check: $label (timeout: ${QUALITY_CHECK_TIMEOUT}s)"
        # Use timeout to prevent hanging commands
        # Use bash -c for safer command execution (still requires trusted prd.json)
        # Note: Commands in prd.json should be trusted - user controls this file
        if ! timeout "$QUALITY_CHECK_TIMEOUT" bash -c "$command" > /tmp/quality-check-$label.log 2>&1; then
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
            
            # Run final quality gate
            log "Running final quality gate..."
            if run_quality_checks; then
                archive_feature
                success "Feature execution complete!"
                return 0
            else
                error "Final quality gate failed"
                return 1
            fi
        fi
        
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
        
        # Build prompt
        local prompt_file
        prompt_file=$(mktemp)
        build_prompt "$story_id" > "$prompt_file"
        
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
        local project_root
        project_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
        if agent -p --force --workspace "$project_root" --model "$MODEL" "$(cat "$prompt_file")" > /tmp/cursor-output.log 2>&1; then
            success "Agent execution completed"
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
        
        rm -f "$prompt_file"
    done
    
    error "Max iterations reached ($MAX_ITERATIONS)"
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
