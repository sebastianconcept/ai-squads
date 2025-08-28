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
