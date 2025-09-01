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
    echo "The script is idempotent and incremental - it will add new agents and update existing ones."
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
    echo "  4. Create/update symlinks to all agent, project, and workflow rules from .ai-squads"
    echo "  5. Convert .md files to .mdc for Cursor compatibility"
    echo "  6. Create/update symlink to the source .ai-squads directory"
    echo "  7. Update .gitignore to exclude the symlinked directory"
    echo ""
    echo "Idempotent behavior:"
    echo "  - Can be run multiple times safely"
    echo "  - Adds new agents without removing existing ones"
    echo "  - Updates existing symlinks to point to current source"
    echo "  - Provides detailed feedback about what was added/updated"
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
    
    # Check if it's a symlink to our source
    if [ -L "$TARGET_SQUADS_LINK" ]; then
        LINK_TARGET=$(readlink "$TARGET_SQUADS_LINK")
        if [ "$LINK_TARGET" = "$SOURCE_ABSOLUTE_PATH" ]; then
            print_status "✓ .ai-squads is properly linked to current source"
        else
            print_warning "Found existing .ai-squads but it links to different location: $LINK_TARGET"
            print_status "This might be from a different installation - will update to current source"
            # Remove the old symlink to replace with correct one
            rm "$TARGET_SQUADS_LINK"
        fi
    else
        print_warning "Found existing .ai-squads directory (not a symlink)"
        print_status "This might be from a different installation - will replace with symlink"
        # Remove the directory to replace with symlink
        rm -rf "$TARGET_SQUADS_LINK"
    fi
else
    print_status "No existing .ai-squads installation found - will create new"
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

# Check if .cursor/rules already has .ai-squads symlinks (informational only)
if [ -d "$TARGET_RULES_DIR" ] && [ "$(ls -A "$TARGET_RULES_DIR" 2>/dev/null)" ]; then
    # Check if there are any .mdc files that look like they're from .ai-squads
    AI_SQUADS_FILES=$(find "$TARGET_RULES_DIR" -name "*.mdc" -type l 2>/dev/null | wc -l)
    if [ "$AI_SQUADS_FILES" -gt 0 ]; then
        print_status "Found existing .mdc symlinks in .cursor/rules - will update incrementally"
    fi
fi

# Create symlinks from .ai-squads to .cursor/rules for automatic updates
print_status "Creating/updating symlinks to .ai-squads for automatic updates"

# Function to create symlink with feedback
create_symlink() {
    local source_file="$1"
    local target_file="$2"
    local description="$3"
    
    if [ -f "$source_file" ]; then
        if [ -L "$target_file" ]; then
            # Check if symlink points to correct source
            local current_target=$(readlink "$target_file")
            local expected_target="$source_file"
            if [ "$current_target" = "$expected_target" ]; then
                print_status "  ✓ $description already correctly linked"
            else
                print_status "  🔄 Updating $description symlink"
                ln -sf "$source_file" "$target_file"
            fi
        else
            print_status "  ➕ Adding new $description symlink"
            ln -sf "$source_file" "$target_file"
        fi
    fi
}

# Create symlinks for all .md files from .ai-squads/agents
if [ -d "$SOURCE_DIR/agents" ]; then
    print_status "Processing agent definitions..."
    for file in "$SOURCE_DIR/agents"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            agent_name="${filename%.md}"
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            create_symlink "$SOURCE_ABSOLUTE_PATH/agents/$filename" "$TARGET_RULES_DIR/$mdc_filename" "agent: $agent_name"
        fi
    done
fi

# Create symlinks for all .md files from .ai-squads/squads
if [ -d "$SOURCE_DIR/squads" ]; then
    print_status "Processing squad definitions..."
    for file in "$SOURCE_DIR/squads"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            squad_name="${filename%.md}"
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            create_symlink "$SOURCE_ABSOLUTE_PATH/squads/$filename" "$TARGET_RULES_DIR/$mdc_filename" "squad: $squad_name"
        fi
    done
fi

# Create symlinks for all .md files from .ai-squads/projects (if exists and not empty)
if [ -d "$SOURCE_DIR/projects" ] && [ "$(ls -A "$SOURCE_DIR/projects" 2>/dev/null)" ]; then
    print_status "Processing project definitions..."
    for file in "$SOURCE_DIR/projects"/*.md; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            project_name="${filename%.md}"
            # Create .mdc symlink for Cursor compatibility
            mdc_filename="${filename%.md}.mdc"
            create_symlink "$SOURCE_ABSOLUTE_PATH/projects/$filename" "$TARGET_RULES_DIR/$mdc_filename" "project: $project_name"
        fi
    done
elif [ -d "$SOURCE_DIR/projects" ]; then
    print_status "Projects directory exists but is empty - skipping"
fi

# Create symlinks for all workflow files from .ai-squads/workflows (if exists and not empty)
if [ -d "$SOURCE_DIR/workflows" ] && [ "$(ls -A "$SOURCE_DIR/workflows" 2>/dev/null)" ]; then
    print_status "Processing workflow definitions..."
    for file in "$SOURCE_DIR/workflows"/*.md*; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            workflow_name="${filename%.*}"
            # Convert to .mdc symlink for Cursor compatibility (workflows become rules)
            if [[ "$filename" == *.md ]]; then
                mdc_filename="${filename%.md}.mdc"
            else
                mdc_filename="$filename"
            fi
            create_symlink "$SOURCE_ABSOLUTE_PATH/workflows/$filename" "$TARGET_RULES_DIR/$mdc_filename" "workflow: $workflow_name"
        fi
    done
elif [ -d "$SOURCE_DIR/workflows" ]; then
    print_status "Workflows directory exists but is empty - skipping"
fi

# Create symlink for README
if [ -f "$SOURCE_DIR/README.md" ]; then
    create_symlink "$SOURCE_ABSOLUTE_PATH/README.md" "$TARGET_RULES_DIR/README.mdc" "README"
fi

# Create a symlink to the source .ai-squads directory for easy updates
print_status "Creating symlink to source .ai-squads directory"
ln -sf "$SOURCE_ABSOLUTE_PATH" "$TARGET_SQUADS_LINK"

# Ensure .ai-squads is excluded from git tracking
TARGET_GITIGNORE="$TARGET_REPO/.gitignore"
print_status "Ensuring .ai-squads is excluded from git tracking..."

# Check if .ai-squads is already in .gitignore
if [ -f "$TARGET_GITIGNORE" ]; then
    if ! grep -q "\.ai-squads" "$TARGET_GITIGNORE"; then
        print_status "Adding .ai-squads to existing .gitignore"
        echo "" >> "$TARGET_GITIGNORE"
        echo "# Exclude symlinked .ai-squads directory" >> "$TARGET_GITIGNORE"
        echo ".ai-squads" >> "$TARGET_GITIGNORE"
        print_success "✓ Added .ai-squads to .gitignore"
    else
        print_status "✓ .ai-squads already in .gitignore"
    fi
else
    print_status "Creating new .gitignore with .ai-squads exclusion"
    echo "# Exclude symlinked .ai-squads directory" > "$TARGET_GITIGNORE"
    echo ".ai-squads" >> "$TARGET_GITIGNORE"
    print_success "✓ Created .gitignore with .ai-squads exclusion"
fi

# Also ensure .cursor/rules is excluded if it contains symlinks
if [ -f "$TARGET_GITIGNORE" ]; then
    if ! grep -q "\.cursor/rules" "$TARGET_GITIGNORE"; then
        print_status "Adding .cursor/rules to .gitignore (contains symlinks)"
        echo "" >> "$TARGET_GITIGNORE"
        echo "# Exclude .cursor/rules directory (contains symlinks)" >> "$TARGET_GITIGNORE"
        echo ".cursor/rules" >> "$TARGET_GITIGNORE"
        print_success "✓ Added .cursor/rules to .gitignore"
    else
        print_status "✓ .cursor/rules already in .gitignore"
    fi
else
    print_status "Adding .cursor/rules to new .gitignore"
    echo "# Exclude .cursor/rules directory (contains symlinks)" >> "$TARGET_GITIGNORE"
    echo ".cursor/rules" >> "$TARGET_GITIGNORE"
    print_success "✓ Added .cursor/rules to .gitignore"
fi

# Check if .ai-squads is already tracked in git (safety check)
if [ -d "$TARGET_REPO/.git" ]; then
    cd "$TARGET_REPO"
    if git ls-files | grep -q "\.ai-squads"; then
        print_warning "⚠️  WARNING: .ai-squads is already tracked in git!"
        print_warning "This could cause issues with your repository."
        print_status "Consider running: git rm -r --cached .ai-squads"
        print_status "Then commit the removal to stop tracking it."
    fi
    if git ls-files | grep -q "\.cursor/rules"; then
        print_warning "⚠️  WARNING: .cursor/rules is already tracked in git!"
        print_warning "This could cause issues with your repository."
        print_status "Consider running: git rm -r --cached .cursor/rules"
        print_status "Then commit the removal to stop tracking it."
    fi
    cd - > /dev/null
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
print_status "Symlinks created/updated at: $TARGET_RULES_DIR (with .mdc extensions for Cursor)"
print_status "Source symlink created/updated at: $TARGET_SQUADS_LINK"

# Show .gitignore status
print_status "Git tracking exclusions:"
if [ -f "$TARGET_GITIGNORE" ]; then
    echo "  📁 .gitignore updated to exclude:"
    grep -E "\.ai-squads|\.cursor/rules" "$TARGET_GITIGNORE" | while read line; do
        echo "    $line"
    done
else
    print_warning "No .gitignore found - please ensure .ai-squads is not tracked"
fi

# Show what was linked
print_status "Linked files:"
ls -la "$TARGET_RULES_DIR" | grep -E "\.(mdc|txt)$" | while read line; do
    echo "  $line"
done

echo ""
print_status "You can now use these agents in the target repository's cursor rules!"
print_status "Updates will flow automatically - no need to run the script again!"
print_status "All linked repositories will stay in sync with .ai-squads updates."
print_status "The script is now idempotent - you can run it repeatedly to add new agents or update existing ones."
