#!/bin/bash

# Create game documentation structure
# Creates ~/docs/{project-name}/game/{game_name}/ directory

set -e

# Source common functions
. "$HOME/.cursor/scripts/common.sh"

if [ -z "$1" ]; then
    echo "Usage: $0 <game_name>"
    exit 1
fi

GAME_NAME="$1"
TARGET_DIR="$HOME/.cursor"

if [ ! -d "$TARGET_DIR/templates" ]; then
    echo "Error: ai-squads templates not found in $TARGET_DIR/templates"
    echo "Make sure you've run the global installation: ./scripts/install_or_update.sh"
    exit 1
fi

TEMPLATES_DIR="$TARGET_DIR/templates"
DOCS_DIR="$(get_docs_dir)"
GAME_DIR="$DOCS_DIR/game/$GAME_NAME"

# Validate game name (no special characters except hyphens)
if [[ ! "$GAME_NAME" =~ ^[a-zA-Z0-9-]+$ ]]; then
    echo "Error: Game name can only contain letters, numbers, and hyphens"
    exit 1
fi

# Check if game already exists
if [ -d "$GAME_DIR" ]; then
    echo "Warning: Game directory already exists: $GAME_DIR"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Create game directory
mkdir -p "$GAME_DIR"

# Copy game templates
echo "Creating game documentation for: $GAME_NAME"
cp -v "$TEMPLATES_DIR/game"/*.md "$GAME_DIR/"

echo ""
echo "✓ Game structure created successfully!"
echo ""
echo "Created: $GAME_DIR/"
echo ""
echo "Next steps:"
echo "  1. Use /plan-game command to generate GDD with Eric's guidance"
echo "  2. Customize GDD.md with your game concept"
echo "  3. Use @eric for game design questions and refinement"

