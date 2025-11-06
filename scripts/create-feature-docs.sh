#!/bin/bash

# Create feature documentation structure
# Creates ai-squads-docs/feature/{feature_name}/ directory

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <feature_name>"
    exit 1
fi

FEATURE_NAME="$1"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

if [ ! -d "$AI_SQUADS_DIR/.cursor" ]; then
    echo "Error: ai-squads directory not found or invalid"
    echo "Make sure you're running this from a project with ai-squads as a submodule"
    exit 1
fi

TEMPLATES_DIR="$AI_SQUADS_DIR/templates"
FEATURE_DIR="$PROJECT_ROOT/ai-squads-docs/feature/$FEATURE_NAME"

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

