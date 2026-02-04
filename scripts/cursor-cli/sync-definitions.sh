#!/bin/bash

# Sync platform-agnostic definitions to Cursor format
#
# Reads from: definitions/agents/*.md, definitions/commands/*.md
# Writes to:  ~/.cursor/agents/*.md, ~/.cursor/commands/*.md
#
# Idempotent: safe to run multiple times.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFINITIONS_DIR="$AI_SQUADS_DIR/definitions"
TARGET_DIR="$HOME/.cursor"

# For Cursor, the format is already compatible (markdown with frontmatter)
# This script copies definitions to ~/.cursor/ with minimal transformation

sync_agents() {
    if [ ! -d "$DEFINITIONS_DIR/agents" ]; then
        return 0
    fi
    mkdir -p "$TARGET_DIR/agents"
    local count=0
    for f in "$DEFINITIONS_DIR/agents"/*.md; do
        [ -f "$f" ] || continue
        cp "$f" "$TARGET_DIR/agents/"
        count=$((count + 1))
    done
    [ "$count" -gt 0 ] && echo "  ✓ agents synced to ~/.cursor/agents/"
}

sync_commands() {
    if [ ! -d "$DEFINITIONS_DIR/commands" ]; then
        return 0
    fi
    mkdir -p "$TARGET_DIR/commands"
    local count=0
    for f in "$DEFINITIONS_DIR/commands"/*.md; do
        [ -f "$f" ] || continue
        cp "$f" "$TARGET_DIR/commands/"
        count=$((count + 1))
    done
    [ "$count" -gt 0 ] && echo "  ✓ commands synced to ~/.cursor/commands/"
}

# Main
if [ ! -d "$DEFINITIONS_DIR" ]; then
    echo "  ○ No definitions/ directory yet (skipping)"
    exit 0
fi

sync_agents
sync_commands
