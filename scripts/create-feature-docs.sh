#!/bin/bash

# Create feature documentation structure
# Creates docs/feature/{feature_name}/ directory

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <feature_name>"
    exit 1
fi

FEATURE_NAME="$1"
PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
TARGET_DIR="$HOME/.cursor"

if [ ! -d "$TARGET_DIR/templates" ]; then
    echo "Error: ai-squads templates not found in $TARGET_DIR/templates"
    echo "Make sure you've run the global installation: ./scripts/install.sh"
    exit 1
fi

TEMPLATES_DIR="$TARGET_DIR/templates"
FEATURE_DIR="$PROJECT_ROOT/docs/feature/$FEATURE_NAME"

# Validate feature name (no special characters except hyphens)
if [[ ! "$FEATURE_NAME" =~ ^[a-zA-Z0-9-]+$ ]]; then
    echo "Error: Feature name can only contain letters, numbers, and hyphens"
    exit 1
fi

# Check if feature already exists
if [ -d "$FEATURE_DIR" ]; then
    echo "Warning: Feature directory already exists: $FEATURE_DIR"
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Create feature directory
mkdir -p "$FEATURE_DIR"

# Copy feature templates
echo "Creating feature documentation for: $FEATURE_NAME"
cp -v "$TEMPLATES_DIR/feature"/*.md "$FEATURE_DIR/"

echo ""
echo "✓ Feature structure created successfully!"
echo ""
echo "Created: $FEATURE_DIR/"
echo ""
echo "Next steps:"
echo "  1. Customize PRD.md, specs.md, and tasks.md"
echo "  2. Use project's agent team to inform planning"

