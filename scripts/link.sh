#!/bin/bash

# link.sh - Install AI Squads agents locally in a project
# Usage: ./link.sh [target_repo_path]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [target_repo_path]"
    echo ""
    echo "This script will install AI Squads agents locally in a project."
    echo "It creates a symlink to the .ai-squads directory and links agent files to .cursor/rules/."
    echo ""
    echo "Arguments:"
    echo "  target_repo_path    Path to the target repository where agents will be installed"
    echo "                      (optional - if not provided, uses current directory)"
    echo ""
    echo "Examples:"
    echo "  $0                    # Install in current directory"
    echo "  $0 ../my-project      # Install in specific repository"
    echo "  $0 /path/to/another/repo"
    echo ""
    echo "The script will:"
    echo "  1. Find the ai-squads repository"
    echo "  2. Create a symlink to the .ai-squads directory"
    echo "  3. Create .cursor/rules/ directory and link agent files"
    echo "  4. Make all AI Squads content available (agents, squads, workflows, projects, standards, templates)"
    echo ""
    echo "Benefits:"
    echo "  - Complete access to all AI Squads content"
    echo "  - Preserves directory structure"
    echo "  - Simple and maintainable"
    echo "  - Automatically includes new content"
    echo "  - Modern Cursor .cursor/rules/ integration"
}

# Check if help is requested
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

# Check if target repository path is provided
if [ $# -eq 0 ]; then
    # No target provided, assume we want to install in current directory
    TARGET_REPO="."
    print_status "No target repository specified, using current directory"
else
    TARGET_REPO="$1"
fi

SOURCE_DIR=".ai-squads"

# Find ai-squads directory (local or external)
CURRENT_DIR=$(pwd)
LOCAL_AI_SQUADS="$CURRENT_DIR/$SOURCE_DIR"
EXTERNAL_AI_SQUADS="/Users/seb/projects/ai-squads/$SOURCE_DIR"

if [ -d "$LOCAL_AI_SQUADS" ]; then
    SOURCE_ABSOLUTE_PATH="$LOCAL_AI_SQUADS"
    print_status "Using local ai-squads at: $LOCAL_AI_SQUADS"
elif [ -d "$EXTERNAL_AI_SQUADS" ]; then
    SOURCE_ABSOLUTE_PATH="$EXTERNAL_AI_SQUADS"
    print_status "Using external ai-squads at: $EXTERNAL_AI_SQUADS"
else
    print_error "Could not find ai-squads directory"
    print_error "Please ensure .ai-squads exists in current directory or at /Users/seb/projects/ai-squads/"
    exit 1
fi

print_status "Starting AI Squads installation"
print_status "Source directory: $SOURCE_ABSOLUTE_PATH"
print_status "Target repository: $TARGET_REPO"

# Check if source directory exists
if [ ! -d "$SOURCE_ABSOLUTE_PATH" ]; then
    print_error "Source directory '$SOURCE_ABSOLUTE_PATH' not found"
    print_error "Make sure the ai-squads repository exists at the expected location"
    exit 1
fi

# Check if target repository exists
if [ ! -d "$TARGET_REPO" ]; then
    print_error "Target repository '$TARGET_REPO' does not exist"
    exit 1
fi

# Create symlink to .ai-squads directory
TARGET_AI_SQUADS="$TARGET_REPO/.ai-squads"

# Remove existing symlink or directory if it exists
if [ -L "$TARGET_AI_SQUADS" ]; then
    print_status "Removing existing symlink: $TARGET_AI_SQUADS"
    rm "$TARGET_AI_SQUADS"
elif [ -d "$TARGET_AI_SQUADS" ]; then
    print_warning "Directory $TARGET_AI_SQUADS already exists"
    print_warning "This might be an existing installation. Removing it..."
    rm -rf "$TARGET_AI_SQUADS"
fi

# Create the symlink
print_status "Creating symlink to .ai-squads directory..."
ln -s "$SOURCE_ABSOLUTE_PATH" "$TARGET_AI_SQUADS"
print_success "✅ Symlink created: $TARGET_AI_SQUADS -> $SOURCE_ABSOLUTE_PATH"

# Create .cursor/rules directory structure
CURSOR_DIR="$TARGET_REPO/.cursor"
RULES_DIR="$CURSOR_DIR/rules"

print_status "Creating .cursor/rules directory structure..."
mkdir -p "$RULES_DIR"

# Function to create symlinks for agents from a directory
create_agent_symlinks() {
    local category="$1"
    local agent_dir="$TARGET_AI_SQUADS/$2"
    
    if [ -d "$agent_dir" ]; then
        print_status "Creating symlinks for $category..."
        
        # Find all .mdc and .md files in the agent directory
        for file in "$agent_dir"/*.{mdc,md}; do
            if [ -f "$file" ]; then
                filename=$(basename "$file")
                agent_name="${filename%.*}"  # Remove extension
                
                # Create symlink in .cursor/rules/
                ln -sf "$file" "$RULES_DIR/$filename"
                print_status "  ➕ Linked: $filename"
            fi
        done
    fi
}

# Create symlinks for all categories
create_agent_symlinks "Core Agents" "agents"
create_agent_symlinks "Squads" "squads"
create_agent_symlinks "Workflows" "workflows"

print_success "AI Squads installation completed successfully!"
print_status "Rules directory: $RULES_DIR"
print_status "Symlinked directory: $TARGET_AI_SQUADS"

# Show summary
print_status "Summary of available content:"
echo ""
echo "📁 Available directories:"
if [ -d "$TARGET_AI_SQUADS/agents" ]; then
    agent_count=$(find "$TARGET_AI_SQUADS/agents" -name "*.mdc" -o -name "*.md" | wc -l | tr -d ' ')
    echo "  🤖 Agents: $agent_count files"
fi
if [ -d "$TARGET_AI_SQUADS/squads" ]; then
    squad_count=$(find "$TARGET_AI_SQUADS/squads" -name "*.md" | wc -l | tr -d ' ')
    echo "  👥 Squads: $squad_count files"
fi
if [ -d "$TARGET_AI_SQUADS/workflows" ]; then
    workflow_count=$(find "$TARGET_AI_SQUADS/workflows" -name "*.mdc" | wc -l | tr -d ' ')
    echo "  🔄 Workflows: $workflow_count files"
fi
if [ -d "$TARGET_AI_SQUADS/projects" ]; then
    echo "  📋 Projects: Available"
fi
if [ -d "$TARGET_AI_SQUADS/standards" ]; then
    echo "  📏 Standards: Available"
fi
if [ -d "$TARGET_AI_SQUADS/templates" ]; then
    echo "  📝 Templates: Available"
fi
echo ""
echo "📋 Linked files in .cursor/rules/:"
linked_count=$(ls -la "$RULES_DIR" | grep -E "\.(mdc|md)$" | wc -l | tr -d ' ')
echo "  $linked_count files linked"
echo ""

print_success "✅ AI Squads installation complete!"
print_status "All content is now available via .ai-squads/ symlink"
print_status "Agent files are symlinked in .cursor/rules/ for Cursor recognition"
print_status "This installation provides complete access to all AI Squads content!"
