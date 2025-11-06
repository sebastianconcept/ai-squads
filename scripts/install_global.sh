#!/bin/bash

# Install ai-squads to global ~/.cursor/ directory

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TARGET_DIR="$HOME/.cursor"

# Function to check if any files exist in target directory
check_existing_files() {
    local has_files=false
    
    if [ -d "$TARGET_DIR/commands" ] && [ -n "$(ls -A "$TARGET_DIR/commands"/*.mdc 2>/dev/null)" ]; then
        has_files=true
    fi
    
    echo "$has_files"
}

# Check for existing installation
existing=$(check_existing_files)

if [ "$existing" = "true" ]; then
    echo "⚠️  ai-squads files already exist in $TARGET_DIR"
    echo ""
    read -p "Do you want to overwrite existing files? (y/N): " -n 1 -r
    echo ""
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Installation cancelled."
        exit 0
    fi
    
    echo "Overwriting existing files..."
fi

echo "Installing ai-squads commands to $TARGET_DIR..."

# Create target directory if it doesn't exist
mkdir -p "$TARGET_DIR/commands"

# Copy commands
echo "Copying commands..."
cp -v "$AI_SQUADS_DIR/commands"/*.mdc "$TARGET_DIR/commands/"

# Make scripts executable
chmod +x "$AI_SQUADS_DIR/scripts"/*.sh

echo ""
echo "✓ ai-squads commands installed globally to $TARGET_DIR/commands"
echo ""
echo "Available commands:"
echo "  - Adopt Project"
echo "  - Plan Feature"
echo "  - Review Merge Request"
echo ""
echo "Specialist agents:"
echo "  - Rusty (Rust Specialist)"
echo "  - Alan (Smalltalk Specialist)"
echo "  - UIDev (JavaScript/CSS/HTML)"
echo "  - Bob (Jobs to be Done)"
echo "  - Steve (Product & UX)"
echo ""

