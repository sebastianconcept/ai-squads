#!/bin/bash

# Create project documentation structure
# Creates docs/ directory in the project root

set -e

PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
TARGET_DIR="$HOME/.cursor"

if [ ! -d "$TARGET_DIR/templates" ]; then
    echo "Error: ai-squads templates not found in $TARGET_DIR/templates"
    echo "Make sure you've run the global installation: ./scripts/install.sh"
    exit 1
fi

TEMPLATES_DIR="$TARGET_DIR/templates"
DOCS_DIR="$PROJECT_ROOT/docs"

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
echo "  2. Configure agent team in $DOCS_DIR/team.md"
echo "  3. Use Cursor commands to plan features and review code"

