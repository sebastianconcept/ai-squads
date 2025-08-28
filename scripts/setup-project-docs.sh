#!/bin/bash

# setup-project-docs.sh - Set up standardized docs directory structure for all projects
# Usage: ./setup-project-docs.sh [project_name] or ./setup-project-docs.sh --all

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
    echo "Usage: $0 [project_name] or $0 --all"
    echo ""
    echo "This script sets up standardized docs directory structure for projects"
    echo ""
    echo "Options:"
    echo "  project_name    Set up docs for specific project"
    echo "  --all           Set up docs for all existing projects"
    echo ""
    echo "Examples:"
    echo "  $0 stui"
    echo "  $0 --all"
    echo ""
    echo "The script will:"
    echo "  1. Create standardized docs directory structure"
    echo "  2. Copy template documentation files"
    echo "  3. Update project templates to reference docs"
    echo "  4. Ensure all workflows can access documentation"
}

# Function to setup docs for a single project
setup_project_docs() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    local docs_dir="$project_dir/docs"
    
    print_status "Setting up docs for project: $project_name"
    
    # Check if project exists
    if [ ! -d "$project_dir" ]; then
        print_error "Project directory not found: $project_dir"
        return 1
    fi
    
    # Create docs directory if it doesn't exist
    if [ ! -d "$docs_dir" ]; then
        print_status "Creating docs directory: $docs_dir"
        mkdir -p "$docs_dir"
    fi
    
    # Copy template docs structure
    print_status "Copying template docs structure..."
    
    # Copy main docs README
    if [ ! -f "$docs_dir/README.md" ]; then
        cp ".ai-squads/templates/projects/docs/README.md" "$docs_dir/README.md"
        print_success "Created $docs_dir/README.md"
    else
        print_warning "Docs README already exists, skipping"
    fi
    
    # Create subdirectories and copy templates
    local subdirs=("api" "architecture" "deployment" "development" "environment" 
                   "operations" "performance" "training" "lessons-learned" 
                   "best-practices" "troubleshooting" "metrics" "stakeholder" "next-phase")
    
    for subdir in "${subdirs[@]}"; do
        local target_dir="$docs_dir/$subdir"
        local template_dir=".ai-squads/templates/projects/docs/$subdir"
        
        # Create subdirectory
        mkdir -p "$target_dir"
        
        # Copy template files if they exist
        if [ -d "$template_dir" ]; then
            for template_file in "$template_dir"/*.md; do
                if [ -f "$template_file" ]; then
                    local filename=$(basename "$template_file")
                    local target_file="$target_dir/$filename"
                    
                    if [ ! -f "$target_file" ]; then
                        cp "$template_file" "$target_file"
                        print_success "Created $target_file"
                    else
                        print_warning "File already exists, skipping: $target_file"
                    fi
                fi
            done
        fi
        
        # Create placeholder README if subdirectory is empty
        if [ -z "$(ls -A "$target_dir")" ]; then
            cat > "$target_dir/README.md" << EOF
---
description: $subdir Documentation - $project_name
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# $subdir Documentation

> Last Updated: $(date +%Y-%m-%d)
> Project: $project_name

This directory contains $subdir documentation for the $project_name project.

## Contents

- [Document 1](document1.md) - [Description]
- [Document 2](document2.md) - [Description]

## Related Documentation

- [Main Docs](../README.md)
- [Project Overview](../../README.md)
EOF
            print_success "Created placeholder README for $subdir"
        fi
    done
    
    # Create archive directory
    mkdir -p "$docs_dir/archive"
    echo "# Archived Documentation" > "$docs_dir/archive/README.md"
    echo "" >> "$docs_dir/archive/README.md"
    echo "This directory contains archived documentation that is no longer current." >> "$docs_dir/archive/README.md"
    
    print_success "Successfully set up docs for project: $project_name"
}

# Function to setup docs for all projects
setup_all_project_docs() {
    print_status "Setting up docs for all projects..."
    
    # Find all project directories
    local projects_dir=".ai-squads/projects"
    if [ ! -d "$projects_dir" ]; then
        print_error "Projects directory not found: $projects_dir"
        return 1
    fi
    
    local project_count=0
    for project_dir in "$projects_dir"/*/; do
        if [ -d "$project_dir" ]; then
            local project_name=$(basename "$project_dir")
            print_status "Processing project: $project_name"
            setup_project_docs "$project_name"
            ((project_count++))
        fi
    done
    
    if [ $project_count -eq 0 ]; then
        print_warning "No projects found in $projects_dir"
    else
        print_success "Successfully processed $project_count projects"
    fi
}

# Function to update project templates
update_project_templates() {
    print_status "Updating project templates to reference docs..."
    
    # Update main project template
    local project_template=".ai-squads/templates/projects/project.md"
    if [ -f "$project_template" ]; then
        # Check if docs reference already exists
        if ! grep -q "docs/" "$project_template"; then
            print_status "Adding docs reference to project template..."
            # Add docs section after Project Structure
            sed -i.bak '/## Project Structure/a\
\
## Documentation\
\
All project documentation is organized in the `docs/` directory:\
\
- **`docs/README.md`** - Documentation overview and structure\
- **`docs/development/`** - Development setup and guidelines\
- **`docs/architecture/`** - System architecture and design\
- **`docs/api/`** - API documentation and specifications\
- **`docs/deployment/`** - Deployment guides and procedures\
- **`docs/operations/`** - Operations and maintenance\
- **`docs/performance/`** - Performance analysis and optimization\
- **`docs/training/`** - Training materials and onboarding\
- **`docs/lessons-learned/`** - Lessons learned and retrospectives\
- **`docs/best-practices/`** - Best practices and guidelines\
- **`docs/troubleshooting/`** - Common issues and solutions\
- **`docs/metrics/`** - Success metrics and KPIs\
- **`docs/stakeholder/`** - Stakeholder communications\
- **`docs/next-phase/`** - Next phase planning\
\
For detailed documentation, see the [docs/](docs/) directory.\
' "$project_template"
            print_success "Updated project template with docs reference"
        else
            print_warning "Project template already has docs reference"
        fi
    fi
    
    # Update README template
    local readme_template=".ai-squads/templates/projects/README.md"
    if [ -f "$readme_template" ]; then
        # Check if docs reference already exists
        if ! grep -q "docs/" "$readme_template"; then
            print_status "Adding docs reference to README template..."
            # Add docs section after Project Structure
            sed -i.bak '/## Project Structure/a\
\
## Documentation\
\
This project follows a comprehensive documentation structure:\
\
- **`docs/README.md`** - Documentation overview and navigation\
- **`docs/development/setup.md`** - Getting started guide\
- **`docs/architecture/overview.md`** - System architecture\
- **`docs/api/`** - API reference and specifications\
- **`docs/deployment/`** - Deployment and operations guides\
\
For complete documentation, see the [docs/](docs/) directory.\
' "$readme_template"
            print_success "Updated README template with docs reference"
        else
            print_warning "README template already has docs reference"
        fi
    fi
}

# Function to create docs integration script
create_docs_integration() {
    print_status "Creating docs integration script..."
    
    local integration_script="scripts/docs-integration.sh"
    
    cat > "$integration_script" << 'EOF'
#!/bin/bash

# docs-integration.sh - Integration utilities for project documentation
# Usage: ./docs-integration.sh [command] [project_name]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_status() { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1"; }

show_usage() {
    echo "Usage: $0 [command] [project_name]"
    echo ""
    echo "Commands:"
    echo "  list [project]     List all documentation for project"
    echo "  search [project]   Search documentation content"
    echo "  update [project]   Update documentation timestamps"
    echo "  validate [project] Validate documentation completeness"
    echo "  archive [project]  Archive outdated documentation"
    echo ""
    echo "Examples:"
    echo "  $0 list stui"
    echo "  $0 search stui 'deployment'"
    echo "  $0 update stui"
}

list_docs() {
    local project="$1"
    local docs_dir=".ai-squads/projects/$project/docs"
    
    if [ ! -d "$docs_dir" ]; then
        print_error "Docs directory not found: $docs_dir"
        return 1
    fi
    
    echo "Documentation for project: $project"
    echo "=================================="
    find "$docs_dir" -name "*.md" -type f | sort | while read -r file; do
        local rel_path="${file#$docs_dir/}"
        local title=$(head -n 1 "$file" | sed 's/^# //')
        echo "- $rel_path: $title"
    done
}

search_docs() {
    local project="$1"
    local query="$2"
    local docs_dir=".ai-squads/projects/$project/docs"
    
    if [ ! -d "$docs_dir" ]; then
        print_error "Docs directory not found: $docs_dir"
        return 1
    fi
    
    echo "Searching for '$query' in $project documentation..."
    echo "=================================================="
    grep -r -i "$query" "$docs_dir" --include="*.md" | head -20
}

update_docs() {
    local project="$1"
    local docs_dir=".ai-squads/projects/$project/docs"
    
    if [ ! -d "$docs_dir" ]; then
        print_error "Docs directory not found: $docs_dir"
        return 1
    fi
    
    print_status "Updating documentation timestamps for $project..."
    
    # Update last modified dates in markdown files
    find "$docs_dir" -name "*.md" -type f | while read -r file; do
        if grep -q "Last Updated:" "$file"; then
            sed -i.bak "s/Last Updated: .*/Last Updated: $(date +%Y-%m-%d)/" "$file"
            print_success "Updated timestamp in $file"
        fi
    done
    
    print_success "Documentation timestamps updated for $project"
}

validate_docs() {
    local project="$1"
    local docs_dir=".ai-squads/projects/$project/docs"
    
    if [ ! -d "$docs_dir" ]; then
        print_error "Docs directory not found: $docs_dir"
        return 1
    fi
    
    print_status "Validating documentation completeness for $project..."
    
    local required_dirs=("api" "architecture" "deployment" "development" "environment" 
                        "operations" "performance" "training" "lessons-learned" 
                        "best-practices" "troubleshooting" "metrics" "stakeholder" "next-phase")
    
    local missing_dirs=()
    for dir in "${required_dirs[@]}"; do
        if [ ! -d "$docs_dir/$dir" ]; then
            missing_dirs+=("$dir")
        fi
    done
    
    if [ ${#missing_dirs[@]} -eq 0 ]; then
        print_success "All required documentation directories present"
    else
        print_warning "Missing documentation directories: ${missing_dirs[*]}"
    fi
    
    # Check for empty directories
    local empty_dirs=()
    for dir in "$docs_dir"/*/; do
        if [ -d "$dir" ] && [ -z "$(ls -A "$dir")" ]; then
            empty_dirs+=("$(basename "$dir")")
        fi
    done
    
    if [ ${#empty_dirs[@]} -eq 0 ]; then
        print_success "All documentation directories have content"
    else
        print_warning "Empty documentation directories: ${empty_dirs[*]}"
    fi
}

archive_docs() {
    local project="$1"
    local docs_dir=".ai-squads/projects/$project/docs"
    local archive_dir="$docs_dir/archive"
    
    if [ ! -d "$docs_dir" ]; then
        print_error "Docs directory not found: $docs_dir"
        return 1
    fi
    
    print_status "Archiving outdated documentation for $project..."
    
    # Find files older than 6 months
    local cutoff_date=$(date -d '6 months ago' +%Y-%m-%d)
    local archived_count=0
    
    find "$docs_dir" -name "*.md" -type f -not -path "$archive_dir/*" | while read -r file; do
        local file_date=$(grep "Last Updated:" "$file" | sed 's/.*Last Updated: //' | head -1)
        if [ -n "$file_date" ] && [ "$file_date" \< "$cutoff_date" ]; then
            local filename=$(basename "$file")
            local archive_path="$archive_dir/$(date +%Y-%m-%d)-$filename"
            mv "$file" "$archive_path"
            print_success "Archived: $filename -> $archive_path"
            ((archived_count++))
        fi
    done
    
    if [ $archived_count -eq 0 ]; then
        print_status "No outdated documentation found"
    else
        print_success "Archived $archived_count outdated documents"
    fi
}

# Main command handling
case "${1:-}" in
    list)
        if [ -z "${2:-}" ]; then
            print_error "Project name required for list command"
            show_usage
            exit 1
        fi
        list_docs "$2"
        ;;
    search)
        if [ -z "${2:-}" ] || [ -z "${3:-}" ]; then
            print_error "Project name and search query required"
            show_usage
            exit 1
        fi
        search_docs "$2" "$3"
        ;;
    update)
        if [ -z "${2:-}" ]; then
            print_error "Project name required for update command"
            show_usage
            exit 1
        fi
        update_docs "$2"
        ;;
    validate)
        if [ -z "${2:-}" ]; then
            print_error "Project name required for validate command"
            show_usage
            exit 1
        fi
        validate_docs "$2"
        ;;
    archive)
        if [ -z "${2:-}" ]; then
            print_error "Project name required for archive command"
            show_usage
            exit 1
        fi
        archive_docs "$2"
        ;;
    *)
        show_usage
        exit 1
        ;;
esac
EOF
    
    chmod +x "$integration_script"
    print_success "Created docs integration script: $integration_script"
}

# Main script logic
main() {
    print_status "Setting up project documentation system..."
    
    # Check if we're in the right directory
if [ ! -d ".ai-squads" ] || [ ! -d ".ai-squads/templates" ]; then
    print_error "Must be run from the .ai-squads root directory"
    exit 1
fi

# Check if docs templates exist
if [ ! -d ".ai-squads/templates/projects/docs" ]; then
    print_error "Docs templates not found. Please run setup-project-docs.sh first."
    exit 1
fi
    
    case "${1:-}" in
        --help|-h)
            show_usage
            exit 0
            ;;
        --all)
            setup_all_project_docs
            ;;
        "")
            show_usage
            exit 1
            ;;
        *)
            setup_project_docs "$1"
            ;;
    esac
    
    # Update templates and create integration
    update_project_templates
    create_docs_integration
    
    print_success "Project documentation system setup complete!"
    echo ""
    echo "Next steps:"
    echo "1. Review the created documentation structure"
    echo "2. Customize documentation content for your projects"
    echo "3. Use './scripts/docs-integration.sh' for documentation management"
    echo "4. Update your workflows to reference the docs/ directories"
}

# Run main function with all arguments
main "$@"
