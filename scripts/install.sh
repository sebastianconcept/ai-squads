#!/bin/bash

# Install ai-squads globally to ~/.cursor/
# This installs commands, templates, and scripts for use across all projects

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TARGET_DIR="$HOME/.cursor"

# Function to check if any files exist in target directory
check_existing_files() {
    local has_files=false
    
    if [ -d "$TARGET_DIR/commands" ] && [ -n "$(ls -A "$TARGET_DIR/commands"/*.md 2>/dev/null)" ]; then
        has_files=true
    fi
    
    if [ -d "$TARGET_DIR/templates" ] && [ -n "$(ls -A "$TARGET_DIR/templates" 2>/dev/null)" ]; then
        has_files=true
    fi
    
    if [ -d "$TARGET_DIR/rules" ] && [ -n "$(ls -A "$TARGET_DIR/rules"/*.md 2>/dev/null)" ]; then
        has_files=true
    fi
    
    echo "$has_files"
}

echo "=========================================="
echo "Installing ai-squads"
echo "=========================================="
echo ""

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

echo "Installing ai-squads to $TARGET_DIR..."

# Create target directories if they don't exist
mkdir -p "$TARGET_DIR/commands"
mkdir -p "$TARGET_DIR/templates"
mkdir -p "$TARGET_DIR/scripts"
mkdir -p "$TARGET_DIR/rules"
mkdir -p "$TARGET_DIR/skills"

# Copy commands
echo "Copying commands..."
cp -v "$AI_SQUADS_DIR/commands"/*.md "$TARGET_DIR/commands/"

# Copy templates
echo "Copying templates..."
cp -rv "$AI_SQUADS_DIR/templates"/* "$TARGET_DIR/templates/"

# Copy scripts
echo "Copying scripts..."
cp -v "$AI_SQUADS_DIR/scripts"/*.sh "$TARGET_DIR/scripts/"

# Copy rules
echo "Copying rules..."
cp -v "$AI_SQUADS_DIR/rules"/*.md "$TARGET_DIR/rules/"

# Copy skills (if directory exists)
if [ -d "$AI_SQUADS_DIR/skills" ]; then
    echo "Copying skills..."
    cp -rv "$AI_SQUADS_DIR/skills"/* "$TARGET_DIR/skills/"
fi

# Make scripts executable
chmod +x "$TARGET_DIR/scripts"/*.sh

echo ""
echo "=========================================="
echo "✓ Installation complete!"
echo "=========================================="
echo ""
echo "Available commands:"
echo "  - Adopt Project"
echo "  - Diagnose Issue"
echo "  - Explain System"
echo "  - Ideate Solution"
echo "  - Plan Feature"
echo "  - Execute Feature (requires Cursor CLI)"
echo "  - Plan Game"
echo "  - Review Merge Request"
echo "  - Team Lately"
echo ""
echo "Specialist agents:"
echo "  - Rusty (Rust Specialist)"
echo "  - Alan (Smalltalk Specialist)"
echo "  - UIDev (JavaScript/CSS/HTML)"
echo "  - Bob (Jobs to be Done)"
echo "  - Steve (Product & UX)"
echo "  - Rian (Strategic Designer & Growth)"
echo "  - Clovis (Brazilian SaaS Copywriting & Growth)"
echo "  - Gustavo (Financial Advisor & Wealth Management)"
echo "  - Ops (DevOps & Infrastructure)"
echo "  - Eric (Video Game Development)"
echo ""
echo "To verify installation: ./scripts/verify-install.sh"
echo ""
echo "Next: Run '/adopt-project' command in Cursor from any project"
