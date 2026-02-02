#!/bin/bash

# Install ai-squads globally to ~/.cursor/
# 
# This installs commands, templates, scripts, rules, and skills for use across all projects.
# 
# What gets installed:
# - Commands: All workflow commands (plan-feature, adopt-project, etc.)
# - Templates: Documentation templates including Storybook templates
# - Scripts: Helper scripts including execute-feature.sh, init-storybook.sh
# - Agents: Agent definitions (required by execute-feature)
# - Rules: System rules applied globally
# - Skills: Specialized capabilities (browser-verification, etc.)
# 
# After installation, run '/adopt-project' in Cursor to set up a project.
# Storybook will be automatically initialized for frontend projects.

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
mkdir -p "$TARGET_DIR/agents"
mkdir -p "$TARGET_DIR/rules"
mkdir -p "$TARGET_DIR/skills"

# Copy commands
echo "Copying commands..."
cp -v "$AI_SQUADS_DIR/commands"/*.md "$TARGET_DIR/commands/"

# Copy templates
echo "Copying templates..."
cp -rv "$AI_SQUADS_DIR/templates"/* "$TARGET_DIR/templates/"

# Ensure Storybook templates are included (explicit check for clarity)
if [ -d "$AI_SQUADS_DIR/templates/storybook" ]; then
    echo "  ✓ Storybook templates included"
fi

# Copy scripts
echo "Copying scripts..."
cp -v "$AI_SQUADS_DIR/scripts"/*.sh "$TARGET_DIR/scripts/"

# Ensure common.sh is copied (it's a new file)
if [ -f "$AI_SQUADS_DIR/scripts/common.sh" ]; then
    cp -v "$AI_SQUADS_DIR/scripts/common.sh" "$TARGET_DIR/scripts/common.sh"
fi

# Verify init-storybook.sh is included
if [ -f "$AI_SQUADS_DIR/scripts/init-storybook.sh" ]; then
    echo "  ✓ Storybook initialization script included"
fi

# Copy agents (required by execute-feature)
if [ -d "$AI_SQUADS_DIR/agents" ]; then
    echo "Copying agents..."
    cp -v "$AI_SQUADS_DIR/agents"/*.md "$TARGET_DIR/agents/"
    echo "  ✓ Agents included"
fi

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
echo "  - Adopt Project (includes Storybook initialization for frontend projects)"
echo "  - Project Starter (business planning with pitch deck and lean canvas)"
echo "  - Diagnose Issue"
echo "  - Explain System"
echo "  - Catchup (read-only: warm up on uncommitted and branch changes)"
echo "  - Ideate Solution"
echo "  - Plan Feature (includes Storybook initialization for frontend features)"
echo "  - Execute Feature (requires Cursor CLI)"
echo "  - Plan Game"
echo "  - Review Merge Request"
echo "  - Team Lately"
echo "  - Update Docs (requires Cursor CLI)"
echo ""
echo "Storybook Integration:"
echo "  - Auto-initialized for frontend projects during /adopt-project"
echo "  - Auto-initialized when planning frontend features"
echo "  - Manual initialization: ~/.cursor/scripts/init-storybook.sh"
echo ""
echo "Specialist agents:"
echo "  - Ben (Startup Advisor & Business Planning)"
echo "  - Rusty (Rust Specialist)"
echo "  - Alan (Smalltalk Specialist)"
echo "  - UIDev (JavaScript/CSS/HTML)"
echo "  - Bob (Jobs to be Done)"
echo "  - Steve (Product & UX)"
echo "  - Rian (Strategic Designer & Growth)"
echo "  - Clovis (SaaS Copywriting & Growth)"
echo "  - Gustavo (Financial Advisor & Wealth Management)"
echo "  - Ops (DevOps & Infrastructure)"
echo "  - Eric (Video Game Development)"
echo ""
echo "To verify installation: ./scripts/verify-install.sh"
echo ""
echo "Next: Run '/adopt-project' command in Cursor from any project"
