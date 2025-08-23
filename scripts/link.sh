#!/bin/bash

# link.sh - Install .cursor/rules from .squads-ai into target repository
# Usage: ./link.sh <target_repo_path>

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
    echo "Usage: $0 <target_repo_path>"
    echo ""
    echo "This script will install .cursor/rules from .squads-ai into the target repository."
    echo ""
    echo "Arguments:"
    echo "  target_repo_path    Path to the target repository where .cursor/rules will be installed"
    echo ""
    echo "Examples:"
    echo "  $0 ../my-project"
    echo "  $0 /path/to/another/repo"
    echo ""
    echo "The script will:"
    echo "  1. Verify the target repository exists and is a git repo"
    echo "  2. Create .cursor/rules directory if it doesn't exist"
    echo "  3. Copy all agent and project rules from .squads-ai"
    echo "  4. Create a symlink to the source .squads-ai directory"
    echo "  5. Update .gitignore to exclude the symlinked directory"
}

# Check if help is requested
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

# Check if target repository path is provided
if [ $# -eq 0 ]; then
    print_error "No target repository path provided"
    show_usage
    exit 1
fi

TARGET_REPO="$1"
SOURCE_DIR=".squads-ai"
CURSOR_RULES=".cursor/rules"

# Get the absolute path of the current directory (where .squads-ai is located)
CURRENT_DIR=$(pwd)
SOURCE_ABSOLUTE_PATH="$CURRENT_DIR/$SOURCE_DIR"

print_status "Starting installation of .cursor/rules from .squads-ai"
print_status "Source directory: $SOURCE_ABSOLUTE_PATH"
print_status "Target repository: $TARGET_REPO"

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    print_error "Source directory '$SOURCE_DIR' not found in current directory"
    print_error "Make sure you're running this script from the directory containing .squads-ai"
    exit 1
fi

# Check if target repository exists
if [ ! -d "$TARGET_REPO" ]; then
    print_error "Target repository '$TARGET_REPO' does not exist"
    exit 1
fi

# Check if target is a git repository
if [ ! -d "$TARGET_REPO/.git" ]; then
    print_warning "Target directory '$TARGET_REPO' is not a git repository"
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_status "Installation cancelled"
        exit 0
    fi
fi

# Create .cursor directory in target if it doesn't exist
TARGET_CURSOR_DIR="$TARGET_REPO/.cursor"
if [ ! -d "$TARGET_CURSOR_DIR" ]; then
    print_status "Creating .cursor directory in target repository"
    mkdir -p "$TARGET_CURSOR_DIR"
fi

# Create .cursor/rules directory in target if it doesn't exist
TARGET_RULES_DIR="$TARGET_CURSOR_DIR/rules"
if [ ! -d "$TARGET_RULES_DIR" ]; then
    print_status "Creating .cursor/rules directory in target repository"
    mkdir -p "$TARGET_RULES_DIR"
fi

# Copy all files from .squads-ai to .cursor/rules
print_status "Copying agent and project definitions to target repository"

# Copy all .md files from .squads-ai/agents
if [ -d "$SOURCE_DIR/agents" ]; then
    print_status "Copying agent definitions..."
    cp -r "$SOURCE_DIR/agents"/* "$TARGET_RULES_DIR/"
fi

# Copy all .md files from .squads-ai/squads
if [ -d "$SOURCE_DIR/squads" ]; then
    print_status "Copying squad definitions..."
    cp -r "$SOURCE_DIR/squads"/* "$TARGET_RULES_DIR/"
fi

# Copy all .md files from .squads-ai/projects (if exists and not empty)
if [ -d "$SOURCE_DIR/projects" ] && [ "$(ls -A "$SOURCE_DIR/projects" 2>/dev/null)" ]; then
    print_status "Copying project definitions..."
    cp -r "$SOURCE_DIR/projects"/* "$TARGET_RULES_DIR/"
elif [ -d "$SOURCE_DIR/projects" ]; then
    print_status "Projects directory exists but is empty - skipping"
fi

# Copy other relevant files
if [ -f "$SOURCE_DIR/README.md" ]; then
    print_status "Copying README..."
    cp "$SOURCE_DIR/README.md" "$TARGET_RULES_DIR/"
fi

# Create a symlink to the source .squads-ai directory for easy updates
TARGET_SQUADS_LINK="$TARGET_REPO/.squads-ai"
if [ -L "$TARGET_SQUADS_LINK" ]; then
    print_status "Removing existing symlink..."
    rm "$TARGET_SQUADS_LINK"
fi

print_status "Creating symlink to source .squads-ai directory"
ln -sf "$SOURCE_ABSOLUTE_PATH" "$TARGET_SQUADS_LINK"

# Update .gitignore to exclude the symlinked directory
TARGET_GITIGNORE="$TARGET_REPO/.gitignore"
if [ -f "$TARGET_GITIGNORE" ]; then
    # Check if .squads-ai is already in .gitignore
    if ! grep -q "\.squads-ai" "$TARGET_GITIGNORE"; then
        print_status "Adding .squads-ai to .gitignore"
        echo "" >> "$TARGET_GITIGNORE"
        echo "# Exclude symlinked .squads-ai directory" >> "$TARGET_GITIGNORE"
        echo ".squads-ai" >> "$TARGET_GITIGNORE"
    else
        print_status ".squads-ai already in .gitignore"
    fi
else
    print_status "Creating .gitignore with .squads-ai exclusion"
    echo "# Exclude symlinked .squads-ai directory" > "$TARGET_GITIGNORE"
    echo ".squads-ai" >> "$TARGET_GITIGNORE"
fi

# Create a README in the target .cursor/rules directory
TARGET_README="$TARGET_RULES_DIR/README.md"
cat > "$TARGET_README" << 'EOF'
# Cursor Rules - Installed from .squads-ai

This directory contains cursor rules and agent definitions installed from the `.squads-ai` directory.

## Structure

- **Agent Definitions**: Complete agent specifications in `.squads-ai/agents/`
- **Squad Definitions**: Squad configurations and available agents
- **Project Definitions**: Project-specific instructions and workflows

## Usage

These rules provide:
- Quick reference to agent capabilities
- Source of truth location for complete definitions
- Basic usage information and triggers
- Reference to complete agent definition

## Source

All rules are sourced from: `../.squads-ai/`

## Updates

To update these rules, run the link script again or manually copy from the source directory.

## Integration

These agents are designed to work with Claude's agent system:
- Automatic agent selection based on task context
- Seamless coordination between specialized functions
- Consistent quality standards and best practices
- Cross-platform considerations built-in
EOF

print_success "Installation completed successfully!"
print_status "Target repository now has access to all .squads-ai agents and projects"
print_status "Symlink created at: $TARGET_SQUADS_LINK"
print_status "Rules installed at: $TARGET_RULES_DIR"
print_status ".gitignore updated to exclude symlinked directory"

# Show what was installed
print_status "Installed files:"
ls -la "$TARGET_RULES_DIR" | grep -E "\.(md|txt)$" | while read line; do
    echo "  $line"
done

echo ""
print_status "You can now use these agents in the target repository's cursor rules!"
print_status "To update in the future, simply run this script again."
