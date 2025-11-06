#!/bin/bash

# Create project documentation structure
# Creates ai-squads-docs/ and .cursor/commands/ in the project root

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

if [ ! -d "$AI_SQUADS_DIR/.cursor" ]; then
    echo "Error: ai-squads directory not found or invalid"
    echo "Make sure you're running this from a project with ai-squads as a submodule"
    exit 1
fi

TEMPLATES_DIR="$AI_SQUADS_DIR/templates"
DOCS_DIR="$PROJECT_ROOT/ai-squads-docs"
COMMANDS_DIR="$PROJECT_ROOT/.cursor/commands"

# Create directories
mkdir -p "$DOCS_DIR/feature"
mkdir -p "$COMMANDS_DIR"

# Copy project templates
echo "Creating project documentation..."
cp -v "$TEMPLATES_DIR/project"/*.md "$DOCS_DIR/"

# Copy command templates
echo "Creating project commands..."
cp -v "$TEMPLATES_DIR/commands"/*.md "$COMMANDS_DIR/"

echo ""
echo "✓ Project structure created successfully!"
echo ""
echo "Created:"
echo "  - $DOCS_DIR/"
echo "  - $COMMANDS_DIR/"
echo ""
echo "Next steps:"
echo "  1. Customize templates in $DOCS_DIR/"
echo "  2. Configure agent team in $DOCS_DIR/team.md"
echo "  3. Use Cursor commands to plan features and review code"

