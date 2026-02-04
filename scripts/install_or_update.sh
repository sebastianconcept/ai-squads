#!/bin/bash

# Install/Update ai-squads (idempotent)
#
# Safe to run multiple times. Works as both initial installation and update.
# - Common files (templates, scripts, rules, skills) → ~/.cursor/
# - Platform-specific definitions for detected CLIs (cursor, claude, gemini, codex)
#
# No prompts. Overwrites existing files. Creates missing directories.
# Run after cloning or pulling new ai-squads changes.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TARGET_DIR="$HOME/.cursor"

# Colors for output (disabled if not a terminal)
if [ -t 1 ]; then
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    RED='\033[0;31m'
    NC='\033[0m' # No Color
else
    GREEN=''
    YELLOW=''
    BLUE=''
    RED=''
    NC=''
fi

log_section() {
    echo -e "${BLUE}▸ $1${NC}"
}

log_success() {
    echo -e "  ${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "  ${RED}✗${NC} $1" >&2
}

log_warning() {
    echo -e "  ${YELLOW}⚠${NC} $1"
}

# Detect platform CLIs
detect_platforms() {
    local platforms=()
    
    if command -v cursor &> /dev/null; then
        platforms+=("cursor")
    fi
    
    if command -v claude &> /dev/null; then
        platforms+=("claude")
    fi
    
    if command -v gemini &> /dev/null; then
        platforms+=("gemini")
    fi
    
    if command -v codex &> /dev/null; then
        platforms+=("codex")
    fi
    
    echo "${platforms[@]}"
}

# Validate platform CLI is actually available and working
validate_platform() {
    local platform="$1"
    local cli_name="$2"
    
    if ! command -v "$cli_name" &> /dev/null; then
        log_error "$platform CLI not found in PATH (expected: $cli_name)"
        return 1
    fi
    
    # Basic validation - check if CLI responds
    if ! "$cli_name" --version &> /dev/null && ! "$cli_name" --help &> /dev/null; then
        log_warning "$platform CLI found but may not be working correctly"
        return 1
    fi
    
    return 0
}

# Update common files to ~/.cursor/
update_common() {
    log_section "Installing common files → $TARGET_DIR"
    
    # Ensure directories exist
    mkdir -p "$TARGET_DIR/commands"
    mkdir -p "$TARGET_DIR/templates"
    mkdir -p "$TARGET_DIR/scripts"
    mkdir -p "$TARGET_DIR/agents"
    mkdir -p "$TARGET_DIR/rules"
    mkdir -p "$TARGET_DIR/skills"
    
    # Commands (source of truth: definitions/commands/)
    if [ -d "$AI_SQUADS_DIR/definitions/commands" ]; then
        cp -r "$AI_SQUADS_DIR/definitions/commands"/*.md "$TARGET_DIR/commands/" 2>/dev/null || true
        log_success "commands"
    fi
    
    # Templates
    if [ -d "$AI_SQUADS_DIR/templates" ]; then
        cp -r "$AI_SQUADS_DIR/templates"/* "$TARGET_DIR/templates/" 2>/dev/null || true
        log_success "templates"
    fi
    
    # Scripts (*.sh files in root scripts/)
    if [ -d "$AI_SQUADS_DIR/scripts" ]; then
        find "$AI_SQUADS_DIR/scripts" -maxdepth 1 -name "*.sh" -exec cp {} "$TARGET_DIR/scripts/" \;
        chmod +x "$TARGET_DIR/scripts"/*.sh 2>/dev/null || true
        log_success "scripts"
    fi
    
    # Agents (source of truth: definitions/agents/)
    if [ -d "$AI_SQUADS_DIR/definitions/agents" ]; then
        cp -r "$AI_SQUADS_DIR/definitions/agents"/*.md "$TARGET_DIR/agents/" 2>/dev/null || true
        log_success "agents"
    fi
    
    # Rules
    if [ -d "$AI_SQUADS_DIR/rules" ]; then
        cp -r "$AI_SQUADS_DIR/rules"/*.md "$TARGET_DIR/rules/" 2>/dev/null || true
        log_success "rules"
    fi
    
    # Skills
    if [ -d "$AI_SQUADS_DIR/skills" ]; then
        cp -r "$AI_SQUADS_DIR/skills"/* "$TARGET_DIR/skills/" 2>/dev/null || true
        log_success "skills"
    fi
}

# Run platform-specific sync script if it exists
sync_platform() {
    local platform="$1"
    local cli_name="$2"
    local sync_script="$AI_SQUADS_DIR/scripts/${platform}-cli/sync-definitions.sh"
    
    # Validate CLI is available
    if ! validate_platform "$platform" "$cli_name"; then
        log_warning "Skipping $platform sync (CLI not available or not working)"
        return 1
    fi
    
    if [ ! -f "$sync_script" ]; then
        log_warning "$platform detected but no sync script yet (scripts/${platform}-cli/sync-definitions.sh)"
        return 1
    fi
    
    log_section "Syncing definitions for $platform"
    chmod +x "$sync_script"
    
    if "$sync_script" 2>&1; then
        log_success "$platform definitions synced"
        return 0
    else
        local exit_code=$?
        log_error "$platform sync failed (exit code: $exit_code)"
        log_warning "You can safely retry installation - idempotent operation"
        return 1
    fi
}

# Main
echo ""
echo "Installing/Updating ai-squads..."
echo ""

# 1. Update common files
update_common

# 2. Detect and sync platforms
echo ""
log_section "Detecting platforms"

platforms=($(detect_platforms))

if [ ${#platforms[@]} -eq 0 ]; then
    log_warning "No CLI platforms detected (cursor, claude, gemini, codex)"
    echo ""
    echo "To use ai-squads, you need at least one AI CLI installed:"
    echo "  - Cursor: https://cursor.sh"
    echo "  - Claude CLI: https://claude.ai/cli"
    echo "  - Gemini CLI: https://ai.google.dev/gemini-api/docs"
    echo "  - Codex CLI: https://developers.openai.com/codex/cli"
    echo ""
    echo "Common files have been installed to $TARGET_DIR"
    echo "Run this script again after installing a CLI to sync platform-specific definitions."
else
    for platform in "${platforms[@]}"; do
        case "$platform" in
            cursor)
                log_success "Cursor CLI detected"
                ;;
            claude)
                log_success "Claude CLI detected"
                ;;
            gemini)
                log_success "Gemini CLI detected"
                ;;
            codex)
                log_success "Codex CLI detected"
                ;;
        esac
    done
    
    echo ""
    
    sync_failures=0
    for platform in "${platforms[@]}"; do
        case "$platform" in
            cursor)
                if ! sync_platform "cursor" "cursor"; then
                    sync_failures=$((sync_failures + 1))
                fi
                ;;
            claude)
                if ! sync_platform "claude" "claude"; then
                    sync_failures=$((sync_failures + 1))
                fi
                ;;
            gemini)
                if ! sync_platform "gemini" "gemini"; then
                    sync_failures=$((sync_failures + 1))
                fi
                ;;
            codex)
                if ! sync_platform "codex" "codex"; then
                    sync_failures=$((sync_failures + 1))
                fi
                ;;
        esac
    done
    
    if [ $sync_failures -gt 0 ]; then
        echo ""
        log_warning "$sync_failures platform sync(s) failed"
        echo "You can safely retry: ./scripts/install_or_update.sh"
    fi
fi

# Done
echo ""
echo -e "${GREEN}✓ Installation/Update complete${NC}"
echo ""
echo "Available commands:"
echo "  - Adopt Project (includes Storybook initialization for frontend projects)"
echo "  - Project Starter (business planning with pitch deck and lean canvas)"
echo "  - Diagnose Issue"
echo "  - Explain System"
echo "  - Catchup (read-only: warm up on uncommitted and branch changes)"
echo "  - Ideate Solution"
echo "  - Plan Feature (includes Storybook initialization for frontend features)"
echo "  - Execute Feature (requires AI CLI)"
echo "  - Plan Game"
echo "  - Review Merge Request"
echo "  - Team Lately"
echo "  - Update Docs (requires AI CLI)"
echo ""
echo "Next: Run '/adopt-project' command from your project root"
echo ""
