#!/bin/bash

# Create project documentation structure
# Creates ~/docs/{project-name}/ directory

set -e

# Source common functions
. "$HOME/.cursor/scripts/common.sh"

TARGET_DIR="$HOME/.cursor"

if [ ! -d "$TARGET_DIR/templates" ]; then
    echo "Error: ai-squads templates not found in $TARGET_DIR/templates"
    echo "Make sure you've run the global installation: ./scripts/install_or_update.sh"
    exit 1
fi

TEMPLATES_DIR="$TARGET_DIR/templates"
DOCS_DIR="$(get_docs_dir)"

# Create directories
mkdir -p "$DOCS_DIR/feature"

# Copy project templates
echo "Creating project documentation..."
cp -v "$TEMPLATES_DIR/project"/*.md "$DOCS_DIR/"

echo ""
echo "✓ Project structure created successfully!"
echo ""
echo "Created:"
echo "  - $DOCS_DIR/"
echo ""
echo "Next steps:"
echo "  1. Customize templates in $DOCS_DIR/"
echo "  2. Configure agent team in $DOCS_DIR/TEAM.md"
echo "  3. Use Cursor commands to plan features and review code"

