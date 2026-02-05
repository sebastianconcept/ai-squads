#!/bin/bash

# Initialize Storybook in a Project
# 
# This script initializes Storybook in a project by:
# 1. Copying Storybook templates from ~/.cursor/templates/storybook/
# 2. Creating storybook/ directory structure in project root
# 3. Optionally installing npm dependencies
# 4. Configuring Storybook for the detected framework
# 
# Usage:
#   ~/.cursor/scripts/init-storybook.sh
# 
# Must be run from project root directory.
# 
# Called automatically during:
# - /adopt-project workflow (if frontend detected)
# - /plan-feature workflow (if frontend feature detected and Storybook not initialized)
# 
# Can also be run manually:
#   cd /path/to/project
#   ~/.cursor/scripts/init-storybook.sh
# 
# Storybook is isolated in storybook/ directory and doesn't interfere with
# existing code. Framework is auto-detected and configured appropriately.

set -e

# Source common functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$SCRIPT_DIR/common.sh"

TARGET_DIR="$HOME/.cursor"

if [ ! -d "$TARGET_DIR/templates/storybook" ]; then
    echo "Error: Storybook templates not found in $TARGET_DIR/templates/storybook"
    echo "Make sure you've run the global installation: ./scripts/install_or_update.sh"
    exit 1
fi

# Get project root (current directory)
PROJECT_ROOT="$(pwd)"

# Check if we're in a git repository
if [ ! -d "$PROJECT_ROOT/.git" ]; then
    echo "Warning: Not in a git repository. Storybook will be initialized anyway."
fi

# Check if Storybook already exists
if [ -d "$PROJECT_ROOT/storybook" ]; then
    echo "⚠️  Storybook already exists in $PROJECT_ROOT/storybook"
    read -p "Do you want to overwrite existing Storybook? (y/N): " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Initialization cancelled."
        exit 0
    fi
    echo "Backing up existing Storybook..."
    mv "$PROJECT_ROOT/storybook" "$PROJECT_ROOT/storybook.backup.$(date +%Y%m%d_%H%M%S)"
fi

echo "Initializing Storybook in $PROJECT_ROOT..."

# Copy Storybook structure
echo "Copying Storybook templates..."
cp -r "$TARGET_DIR/templates/storybook" "$PROJECT_ROOT/storybook"

# Rename package.json.template to package.json
if [ -f "$PROJECT_ROOT/storybook/package.json.template" ]; then
    mv "$PROJECT_ROOT/storybook/package.json.template" "$PROJECT_ROOT/storybook/package.json"
fi

# Note: JavaScript files don't need execute permission, but ensure they're readable
# The scripts will be run with: node scripts/script-name.js

echo ""
echo "✓ Storybook structure created!"
echo ""

# Check if npm/node is available
if ! command -v npm &> /dev/null; then
    echo "⚠️  npm not found. Skipping dependency installation."
    echo "   Run 'cd storybook && npm install' manually when ready."
    exit 0
fi

# Ask if user wants to install dependencies now
read -p "Install Storybook dependencies now? (Y/n): " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "Skipping dependency installation."
    echo "   Run 'cd storybook && npm install' when ready."
else
    echo "Installing Storybook dependencies..."
    cd "$PROJECT_ROOT/storybook"
    npm install
    echo ""
    echo "✓ Dependencies installed!"
fi

echo ""
echo "=========================================="
echo "✓ Storybook initialized successfully!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. Start Storybook: cd storybook && npm run storybook"
echo "  2. Stories will be auto-generated during feature planning"
echo "  3. See ~/docs/{project-name}/STORYBOOK.md for documentation"
echo ""
