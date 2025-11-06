#!/bin/bash

# Install ai-squads global commands to ~/.cursor/commands/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
COMMANDS_DIR="$AI_SQUADS_DIR/commands"
TARGET_DIR="$HOME/.cursor/commands"

# Create target directory if it doesn't exist
mkdir -p "$TARGET_DIR"

# Copy command files
echo "Installing ai-squads commands to $TARGET_DIR..."
cp -v "$COMMANDS_DIR"/*.md "$TARGET_DIR/"

# Make scripts executable
chmod +x "$AI_SQUADS_DIR/scripts"/*.sh

echo ""
echo "✓ ai-squads commands installed successfully!"
echo ""
echo "Available commands:"
echo "  - Adopt Project"
echo "  - Plan Feature"
echo "  - Review Merge Request"
echo "  - Invoke Agent"
echo ""
echo "Next: Run 'Adopt Project' command in Cursor"
