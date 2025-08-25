#!/bin/bash

# plan-feature.sh - Plan a new feature for an existing project using SquadsAI workflows
# Usage: ./plan-feature.sh <project_name> <feature_name> [options]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
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

print_header() {
    echo -e "${PURPLE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${PURPLE}║                    SquadsAI Feature Planning                 ║${NC}"
    echo -e "${PURPLE}╚══════════════════════════════════════════════════════════════╝${NC}"
}

# Function to show usage
show_usage() {
    echo "Usage: $0 <project_name> <feature_name> [options]"
    echo ""
    echo "This script creates comprehensive feature planning documentation using SquadsAI workflows."
    echo ""
    echo "Arguments:"
    echo "  project_name        Name of the existing project (must exist in .squads-ai/projects/)"
    echo "  feature_name        Name of the feature to plan (will create feature-[FEATURE_NAME]/ directory)"
    echo ""
    echo "Options:"
    echo "  -p, --priority      Feature priority: low, medium, high (default: medium)"
    echo "  -s, --squad         Specific squad to use (default: auto-detect)"
    echo "  -t, --template      Custom template directory (default: templates/projects/)"
    echo "  -v, --verbose       Enable verbose output"
    echo "  -h, --help          Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 stui multi-image-architecture"
    echo "  $0 myapp user-authentication --priority high"
    echo "  $0 backend api-rate-limiting --squad elite"
    echo ""
    echo "The script will:"
    echo "  1. Validate project exists and is properly configured"
    echo "  2. Create feature-[FEATURE_NAME]/ directory structure"
    echo "  3. Generate comprehensive planning documents from templates"
    echo "  4. Set up squad integration and agent assignments"
    echo "  5. Create implementation status tracking"
    echo "  6. Update project status and documentation"
}

# Function to validate project exists
validate_project() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    if [ ! -d "$project_dir" ]; then
        print_error "Project '$project_name' not found in .squads-ai/projects/"
        print_error "Available projects:"
        if [ -d ".squads-ai/projects" ]; then
            ls -1 ".squads-ai/projects/" | sed 's/^/  - /'
        else
            print_error "  No projects directory found"
        fi
        exit 1
    fi
    
    # Check if project has required files
    local required_files=("README.md" "mission.md" "roadmap.md")
    for file in "${required_files[@]}"; do
        if [ ! -f "$project_dir/$file" ]; then
            print_warning "Project missing required file: $file"
        fi
    done
    
    print_success "Project '$project_name' validated"
}

# Function to create feature directory structure
create_feature_structure() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".squads-ai/projects/$project_name/feature-$feature_name"
    
    print_status "Creating feature directory structure..."
    
    # Create feature directory
    mkdir -p "$feature_dir"
    
    # Create subdirectories if needed
    mkdir -p "$feature_dir/docs"
    mkdir -p "$feature_dir/implementation"
    
    print_success "Feature directory structure created: $feature_dir"
}

# Function to copy and customize templates
copy_templates() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".squads-ai/projects/$project_name/feature-$feature_name"
    local template_dir="templates/projects"
    
    print_status "Copying and customizing templates..."
    
    # List of required template files
    local templates=("problem.md" "solution.md" "goal.md" "tasks.md")
    
    for template in "${templates[@]}"; do
        if [ -f "$template_dir/$template" ]; then
            local target_file="$feature_dir/$template"
            cp "$template_dir/$template" "$target_file"
            
            # Customize the template content
            customize_template "$target_file" "$feature_name" "$project_name"
            
            print_status "  ✓ Created $template"
        else
            print_warning "Template not found: $template_dir/$template"
        fi
    done
    
    print_success "All templates copied and customized"
}

# Function to customize template content
customize_template() {
    local file_path="$1"
    local feature_name="$2"
    local project_name="$3"
    
    # Convert feature name to title case
    local feature_title=$(echo "$feature_name" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
    
    # Replace placeholders in the template
    sed -i.bak "s/\[FEATURE_NAME\]/$feature_title/g" "$file_path"
    sed -i.bak "s/\[PROJECT_NAME\]/$project_name/g" "$file_path"
    
    # Remove backup files
    rm -f "${file_path}.bak"
}

# Function to create status tracking files
create_status_files() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".squads-ai/projects/$project_name/feature-$feature_name"
    
    print_status "Creating status tracking files..."
    
    # Create status.md
    cat > "$feature_dir/status.md" << EOF
---
description: Feature Status - $feature_name
type: feature-status
status: planning
priority: $PRIORITY
created: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
---

# Feature Status: $feature_name

## Current Status
- **Phase**: Planning
- **Status**: In Progress
- **Priority**: $PRIORITY
- **Created**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")

## Progress Tracking
- [ ] Problem analysis completed
- [ ] Solution design completed
- [ ] Goal definition completed
- [ ] Task breakdown completed
- [ ] Squad validation completed
- [ ] Ready for implementation

## Next Steps
1. Complete planning documentation
2. Validate with squad agents
3. Begin implementation

## Notes
*This file tracks the current status and progress of the feature planning process.*
EOF

    # Create implementation-status.md
    cat > "$feature_dir/implementation-status.md" << EOF
---
description: Implementation Status - $feature_name
type: implementation-status
status: not-started
---

# Implementation Status: $feature_name

## Overview
This document tracks the implementation progress of the $feature_name feature.

## Implementation Phases
- [ ] **Phase 1**: Planning and Design
- [ ] **Phase 2**: Development
- [ ] **Phase 3**: Testing and Validation
- [ ] **Phase 4**: Deployment

## Current Phase: Planning and Design
- **Status**: In Progress
- **Start Date**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
- **Target Completion**: TBD

## Task Progress
*Tasks will be populated from tasks.md as implementation begins*

## Blockers and Dependencies
*Blockers and dependencies will be documented here*

## Notes
*Implementation notes and observations will be added here*
EOF

    print_success "Status tracking files created"
}

# Function to update project status
update_project_status() {
    local project_name="$1"
    local feature_name="$2"
    local project_status_file=".squads-ai/projects/$project_name/status.md"
    
    print_status "Updating project status..."
    
    if [ -f "$project_status_file" ]; then
        # Add feature to project status
        if ! grep -q "feature-$feature_name" "$project_status_file"; then
            echo "" >> "$project_status_file"
            echo "## Features in Planning" >> "$project_status_file"
            echo "- [ ] **$feature_name** - Currently in planning phase" >> "$project_status_file"
        fi
        print_success "Project status updated"
    else
        print_warning "Project status file not found, skipping update"
    fi
}

# Function to create Cursor rule integration
create_cursor_integration() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".squads-ai/projects/$project_name/feature-$feature_name"
    
    print_status "Creating Cursor rule integration..."
    
    # Create a Cursor rule file for this feature
    cat > "$feature_dir/.cursor-rule.md" << EOF
# Cursor Rule: $feature_name Feature Development

## Overview
This feature is currently in the planning phase. Use the following documents to understand the current state and next steps.

## Key Documents
- **problem.md** - Problem definition and analysis
- **solution.md** - Solution design and approach
- **goal.md** - Success criteria and acceptance criteria
- **tasks.md** - Task breakdown and assignments
- **status.md** - Current status and progress
- **implementation-status.md** - Implementation tracking

## Development Guidelines
1. **Follow SquadsAI Standards**: Use the established coding standards and quality gates
2. **Update Documentation**: Keep all planning documents updated as implementation progresses
3. **Squad Coordination**: Work with assigned squad agents for specialized tasks
4. **Quality Gates**: Ensure all work passes quality standards before proceeding

## Quick Commands
- View problem: \`cat .squads-ai/projects/$project_name/feature-$feature_name/problem.md\`
- View tasks: \`cat .squads-ai/projects/$project_name/feature-$feature_name/tasks.md\`
- Update status: Edit status.md and implementation-status.md files

## Integration
This feature integrates with the SquadsAI system and follows established workflows.
EOF

    print_success "Cursor rule integration created"
}

# Function to display completion summary
show_completion_summary() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".squads-ai/projects/$project_name/feature-$feature_name"
    
    print_header
    echo ""
    print_success "Feature planning completed successfully!"
    echo ""
    echo "📁 Feature Directory: $feature_dir"
    echo "📋 Created Documents:"
    echo "  • problem.md - Problem definition and analysis"
    echo "  • solution.md - Solution design and approach"
    echo "  • goal.md - Success criteria and acceptance criteria"
    echo "  • tasks.md - Task breakdown and assignments"
    echo "  • status.md - Current status and progress"
    echo "  • implementation-status.md - Implementation tracking"
    echo "  • .cursor-rule.md - Cursor integration"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Review and customize the generated documents"
    echo "  2. Validate planning with squad agents"
    echo "  3. Begin implementation following the task breakdown"
    echo "  4. Update status files as work progresses"
    echo ""
    echo "💡 Tips:"
    echo "  • Use 'cat $feature_dir/tasks.md' to view task breakdown"
    echo "  • Edit status.md to track planning progress"
    echo "  • Follow SquadsAI quality gates and standards"
    echo "  • Coordinate with squad agents for specialized work"
    echo ""
    print_success "Your feature is ready for planning and implementation!"
}

# Main execution
main() {
    # Parse command line arguments
    PROJECT_NAME=""
    FEATURE_NAME=""
    PRIORITY="medium"
    SQUAD="auto"
    TEMPLATE_DIR="templates/projects"
    VERBOSE=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -p|--priority)
                PRIORITY="$2"
                shift 2
                ;;
            -s|--squad)
                SQUAD="$2"
                shift 2
                ;;
            -t|--template)
                TEMPLATE_DIR="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                show_usage
                exit 0
                ;;
            -*)
                print_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
            *)
                if [ -z "$PROJECT_NAME" ]; then
                    PROJECT_NAME="$1"
                elif [ -z "$FEATURE_NAME" ]; then
                    FEATURE_NAME="$1"
                else
                    print_error "Too many arguments"
                    show_usage
                    exit 1
                fi
                shift
                ;;
        esac
    done
    
    # Validate required arguments
    if [ -z "$PROJECT_NAME" ] || [ -z "$FEATURE_NAME" ]; then
        print_error "Missing required arguments"
        show_usage
        exit 1
    fi
    
    # Validate priority
    if [[ ! "$PRIORITY" =~ ^(low|medium|high)$ ]]; then
        print_error "Invalid priority: $PRIORITY. Must be low, medium, or high"
        exit 1
    fi
    
    # Show header
    print_header
    echo ""
    print_status "Starting feature planning for: $FEATURE_NAME"
    print_status "Project: $PROJECT_NAME"
    print_status "Priority: $PRIORITY"
    print_status "Squad: $SQUAD"
    echo ""
    
    # Execute planning workflow
    validate_project "$PROJECT_NAME"
    create_feature_structure "$PROJECT_NAME" "$FEATURE_NAME"
    copy_templates "$PROJECT_NAME" "$FEATURE_NAME"
    create_status_files "$PROJECT_NAME" "$FEATURE_NAME"
    update_project_status "$PROJECT_NAME" "$FEATURE_NAME"
    create_cursor_integration "$PROJECT_NAME" "$FEATURE_NAME"
    
    # Show completion summary
    show_completion_summary "$PROJECT_NAME" "$FEATURE_NAME"
}

# Run main function with all arguments
main "$@"
