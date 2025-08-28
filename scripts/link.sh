#!/bin/bash

# link.sh - Link .cursor/rules from .ai-squads into target repository
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
    echo "This script will link .cursor/rules from .ai-squads into the target repository."
echo "The script is idempotent - it will skip installation if .ai-squads is already present."
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
echo "  2. Check if .ai-squads is already installed (idempotent)"
echo "  3. Create .cursor/rules directory if it doesn't exist"
echo "  4. Create symlinks to all agent, project, and workflow rules from .ai-squads"
echo "  5. Convert .md files to .mdc for Cursor compatibility"
echo "  6. Create a symlink to the source .ai-squads directory"
echo "  7. Update .gitignore to exclude the symlinked directory"
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
SOURCE_DIR=".ai-squads"
CURSOR_RULES=".cursor/rules"

# Get the absolute path of the current directory (where .ai-squads is located)
CURRENT_DIR=$(pwd)
SOURCE_ABSOLUTE_PATH="$CURRENT_DIR/$SOURCE_DIR"

print_status "Starting installation of .cursor/rules from .ai-squads"
print_status "Source directory: $SOURCE_ABSOLUTE_PATH"
print_status "Target repository: $TARGET_REPO"

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    print_error "Source directory '$SOURCE_DIR' not found in current directory"
    print_error "Make sure you're running this script from the directory containing .ai-squads"
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

# Check if .ai-squads is already installed (idempotency check)
TARGET_SQUADS_LINK="$TARGET_REPO/.ai-squads"
if [ -L "$TARGET_SQUADS_LINK" ] || [ -d "$TARGET_SQUADS_LINK" ]; then
    print_status ".ai-squads already installed in target repository"
    print_status "Checking if installation is up to date..."
    
    # Check if it's a symlink to our source
    if [ -L "$TARGET_SQUADS_LINK" ]; then
        LINK_TARGET=$(readlink "$TARGET_SQUADS_LINK")
        if [ "$LINK_TARGET" = "$SOURCE_ABSOLUTE_PATH" ]; then
            print_success ".ai-squads is already properly linked and up to date!"
            print_status "No action needed - installation is current"
            exit 0
        else
            print_warning "Found existing .ai-squads but it links to different location: $LINK_TARGET"
            print_status "This might be from a different installation"
        fi
    else
        print_warning "Found existing .ai-squads directory (not a symlink)"
        print_status "This might be from a different installation"
    fi
    
    read -p "Replace existing installation? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_status "Installation skipped - keeping existing .ai-squads"
        exit 0
    fi
    
    # Remove existing installation
    print_status "Removing existing .ai-squads installation..."
    if [ -L "$TARGET_SQUADS_LINK" ]; then
        rm "$TARGET_SQUADS_LINK"
    else
        rm -rf "$TARGET_SQUADS_LINK"
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

# Check if .cursor/rules already has .ai-squads symlinks
if [ -d "$TARGET_RULES_DIR" ] && [ "$(ls -A "$TARGET_RULES_DIR" 2>/dev/null)" ]; then
    # Check if there are any .mdc files that look like they're from .ai-squads
    AI_SQUADS_FILES=$(find "$TARGET_RULES_DIR" -name "*.mdc" -type l 2>/dev/null | wc -l)
    if [ "$AI_SQUADS_FILES" -gt 0 ]; then
        print_status "Found existing .mdc symlinks in .cursor/rules"
        print_status "Checking if they're from .ai-squads..."
        
        # Sample check - look for a few key files
        SAMPLE_FILES=("director.mdc" "software-engineer.mdc" "elite.mdc")
        EXISTING_COUNT=0
        for file in "${SAMPLE_FILES[@]}"; do
            if [ -L "$TARGET_RULES_DIR/$file" ]; then
                EXISTING_COUNT=$((EXISTING_COUNT + 1))
            fi
        done
        
        if [ "$EXISTING_COUNT" -ge 2 ]; then
            print_status "Found existing .ai-squads symlinks in .cursor/rules"
            print_status "Checking if installation is complete..."
            
            # Check if the README.mdc exists and mentions .ai-squads
            if [ -f "$TARGET_RULES_DIR/README.mdc" ] && grep -q "\.ai-squads" "$TARGET_RULES_DIR/README.mdc"; then
                print_success ".cursor/rules already has .ai-squads installation!"
                print_status "No action needed - rules are already linked"
                exit 0
            fi
        fi
    fi
fi

# Create symlinks from .ai-squads to .cursor/rules for automatic updates
print_status "Creating symlinks to .ai-squads for automatic updates"

# Create symlinks for all .md files from .ai-squads/agents
if [ -d "$SOURCE_DIR/agents" ]; then
    print_status "Creating symlinks for agent definitions..."
    for file in "$SOURCE_DIR/agents"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            ln -sf "$SOURCE_ABSOLUTE_PATH/agents/$filename" "$TARGET_RULES_DIR/$mdc_filename"
        fi
    done
fi

# Create symlinks for all .md files from .ai-squads/squads
if [ -d "$SOURCE_DIR/squads" ]; then
    print_status "Creating symlinks for squad definitions..."
    for file in "$SOURCE_DIR/squads"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            ln -sf "$SOURCE_ABSOLUTE_PATH/squads/$filename" "$TARGET_RULES_DIR/$mdc_filename"
        fi
    done
fi

# Create symlinks for all .md files from .ai-squads/projects (if exists and not empty)
if [ -d "$SOURCE_DIR/projects" ] && [ "$(ls -A "$SOURCE_DIR/projects" 2>/dev/null)" ]; then
    print_status "Creating symlinks for project definitions..."
    for file in "$SOURCE_DIR/projects"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            ln -sf "$SOURCE_ABSOLUTE_PATH/projects/$filename" "$TARGET_RULES_DIR/$mdc_filename"
        fi
    done
elif [ -d "$SOURCE_DIR/projects" ]; then
    print_status "Projects directory exists but is empty - skipping"
fi

# Create symlinks for all workflow files from .ai-squads/workflows (if exists and not empty)
if [ -d "$SOURCE_DIR/workflows" ] && [ "$(ls -A "$SOURCE_DIR/workflows" 2>/dev/null)" ]; then
    print_status "Creating symlinks for workflow definitions..."
    for file in "$SOURCE_DIR/workflows"/*.md*; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            # Convert to .mdc symlink for Cursor compatibility (workflows become rules)
            if [[ "$filename" == *.md ]]; then
                mdc_filename="${filename%.md}.mdc"
            else
                mdc_filename="$filename"
            fi
            ln -sf "$SOURCE_ABSOLUTE_PATH/workflows/$filename" "$TARGET_RULES_DIR/$mdc_filename"
            print_status "  ✓ Linked workflow: $filename -> $mdc_filename"
        fi
    done
elif [ -d "$SOURCE_DIR/workflows" ]; then
    print_status "Workflows directory exists but is empty - skipping"
fi

# Create symlink for README
if [ -f "$SOURCE_DIR/README.md" ]; then
    print_status "Creating symlink for README..."
    ln -sf "$SOURCE_ABSOLUTE_PATH/README.md" "$TARGET_RULES_DIR/README.mdc"
fi

# Create a symlink to the source .ai-squads directory for easy updates
print_status "Creating symlink to source .ai-squads directory"
ln -sf "$SOURCE_ABSOLUTE_PATH" "$TARGET_SQUADS_LINK"

# Update .gitignore to exclude the symlinked directory
TARGET_GITIGNORE="$TARGET_REPO/.gitignore"
if [ -f "$TARGET_GITIGNORE" ]; then
    # Check if .ai-squads is already in .gitignore
    if ! grep -q "\.ai-squads" "$TARGET_GITIGNORE"; then
        print_status "Adding .ai-squads to .gitignore"
        echo "" >> "$TARGET_GITIGNORE"
        echo "# Exclude symlinked .ai-squads directory" >> "$TARGET_GITIGNORE"
        echo ".ai-squads" >> "$TARGET_GITIGNORE"
    else
        print_status ".ai-squads already in .gitignore"
    fi
else
    print_status "Creating .gitignore with .ai-squads exclusion"
    echo "# Exclude symlinked .ai-squads directory" > "$TARGET_GITIGNORE"
    echo ".ai-squads" >> "$TARGET_GITIGNORE"
fi

# Create a README in the target .cursor/rules directory
TARGET_README="$TARGET_RULES_DIR/README.mdc"
cat > "$TARGET_README" << 'EOF'
# Cursor Rules - Linked from .ai-squads

This directory contains cursor rules and agent definitions linked from the `.ai-squads` directory.

## Structure

- **Agent Definitions**: Complete agent specifications in `.ai-squads/agents/`
- **Squad Definitions**: Squad configurations and available agents
- **Project Definitions**: Project-specific instructions and workflows
- **Workflow Definitions**: Process steps and coordination procedures in `.ai-squads/workflows/`

## Usage

These rules provide:
- **Agent Rules**: Quick reference to agent capabilities and behavior
- **Workflow Rules**: Process steps and coordination procedures
- **Project Rules**: Project-specific instructions and context
- **Source of truth location** for complete definitions
- **Basic usage information and triggers**
- **Reference to complete agent definition**

## Source

All rules are symlinked from: `../.ai-squads/`

## Updates

Updates flow automatically! When SquadsAI is updated, all linked repositories get the latest rules.

## Integration

These agents are designed to work with Claude's agent system:
- Automatic agent selection based on task context
- Seamless coordination between specialized functions
- Consistent quality standards and best practices
- Cross-platform considerations built-in

## File Extensions

- **`.mdc` files**: Cursor-compatible rules (symlinked to source `.md` files)
- **Source files**: Original `.md` files in `.ai-squads/` directory
EOF

print_success "Linking completed successfully!"
print_status "Target repository now has access to all .ai-squads agents, projects, and workflows"
print_status "Symlinks created at: $TARGET_RULES_DIR (with .mdc extensions for Cursor)"
print_status "Source symlink created at: $TARGET_SQUADS_LINK"
print_status ".gitignore updated to exclude symlinked directories"

# Show what was linked
print_status "Linked files:"
ls -la "$TARGET_RULES_DIR" | grep -E "\.(mdc|txt)$" | while read line; do
    echo "  $line"
done

echo ""
print_status "You can now use these agents in the target repository's cursor rules!"
print_status "Updates will flow automatically - no need to run the script again!"
print_status "All linked repositories will stay in sync with .ai-squads updates."
