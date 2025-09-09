#!/bin/bash

# plan-feature.sh - Plan a new feature for an existing project using AI Squads workflows
# 
# This script implements the plan-feature workflow defined in:
# .ai-squads/workflows/planning-workflows.md
# 
# For detailed instructions and Cursor integration, see:
# .ai-squads/workflows/plan-feature.mdc
#
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
    echo -e "${PURPLE}║                    AI Squads Feature Planning                 ║${NC}"
    echo -e "${PURPLE}╚══════════════════════════════════════════════════════════════╝${NC}"
}

# Function to show usage
show_usage() {
    echo "Usage: $0 <project_name> <feature_name> [options]"
    echo ""
    echo "This script creates comprehensive feature planning documentation using AI Squads workflows."
    echo ""
    echo "Arguments:"
    echo "  project_name        Name of the existing project (must exist in .ai-squads/projects/)"
    echo "  feature_name        Name of the feature to plan (will create feature-[FEATURE_NAME]/ directory)"
    echo ""
    echo "Options:"
    echo "  -p, --priority      Feature priority: low, medium, high (default: medium)"
    echo "  -s, --squad         Specific squad to use (default: auto-detect)"
    echo "  -t, --template      Custom template directory (default: .ai-squads/templates/projects/)"
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
    echo "  2. Ensure project has all required planning files (mission.md, roadmap.md, tech-stack.md, decisions.md, tasks.md)"
    echo "  3. Create feature-[FEATURE_NAME]/ directory structure"
    echo "  4. Generate comprehensive planning documents from templates"
    echo "  5. Set up squad integration and agent assignments"
    echo "  6. Create implementation status tracking"
    echo "  7. Update project status and documentation"
}

# Function to validate project exists
validate_project() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    if [ ! -d "$project_dir" ]; then
        print_error "Project '$project_name' not found in .ai-squads/projects/"
        print_error "Available projects:"
        if [ -d ".ai-squads/projects" ]; then
            ls -1 ".ai-squads/projects/" | sed 's/^/  - /'
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

# Function to ensure project has all required planning files
ensure_project_planning_files() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    local template_dir=".ai-squads/templates/projects"
    
    print_status "Ensuring project has all required planning files..."
    
    # List of required project planning files
    local required_files=("mission.md" "roadmap.md" "tech-stack.md" "decisions.md" "tasks.md")
    local missing_files=()
    
    # Check which files are missing
    for file in "${required_files[@]}"; do
        if [ ! -f "$project_dir/$file" ]; then
            missing_files+=("$file")
        fi
    done
    
    # Create missing files from templates
    if [ ${#missing_files[@]} -gt 0 ]; then
        print_status "Creating missing project planning files..."
        
        for file in "${missing_files[@]}"; do
            if [ -f "$template_dir/$file" ]; then
                local target_file="$project_dir/$file"
                cp "$template_dir/$file" "$target_file"
                
                # Customize the template content for the project
                customize_project_template "$target_file" "$project_name"
                
                print_status "  ✓ Created $file"
            else
                print_warning "Template not found: $template_dir/$file"
            fi
        done
        
        print_success "Project planning files created"
    else
        print_status "All required project planning files already exist"
    fi
}

# Function to customize project template content
customize_project_template() {
    local file_path="$1"
    local project_name="$2"
    
    # Convert project name to title case
    local project_title=$(echo "$project_name" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
    
    # Replace placeholders in the template
    sed -i.bak "s/\[PROJECT_NAME\]/$project_title/g" "$file_path"
    sed -i.bak "s/\[project_name\]/$project_name/g" "$file_path"
    
    # Remove backup files
    rm -f "${file_path}.bak"
}

# Function to create feature directory structure
create_feature_structure() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".ai-squads/projects/$project_name/feature-$feature_name"
    
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
    local feature_dir=".ai-squads/projects/$project_name/feature-$feature_name"
    local template_dir=".ai-squads/templates/projects"
    
    print_status "Copying and customizing templates..."
    
    # List of required template files
    local templates=("problem.md" "solution.md" "goal.md" "tasks.md" "jtbd-analysis.md" "story-plan.md" "tech-specs.md")
    
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
    local feature_dir=".ai-squads/projects/$project_name/feature-$feature_name"
    
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
    local project_status_file=".ai-squads/projects/$project_name/status.md"
    
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

# Function to determine appropriate squad based on project type
determine_squad() {
    local project_name="$1"
    local feature_name="$2"
    local project_dir=".ai-squads/projects/$project_name"
    
    # Check if project has tech-stack.md to determine technology
    local tech_stack_file="$project_dir/tech-stack.md"
    local squad="elite"  # Default squad
    
    if [ -f "$tech_stack_file" ]; then
        # Read tech stack to determine appropriate squad
        if grep -qi "rust\|rusty" "$tech_stack_file"; then
            squad="elite"
        elif grep -qi "smalltalk\|pharo" "$tech_stack_file"; then
            squad="elite"
        elif grep -qi "javascript\|typescript\|react\|vue\|angular" "$tech_stack_file"; then
            squad="elite"
        elif grep -qi "python\|django\|flask" "$tech_stack_file"; then
            squad="elite"
        elif grep -qi "java\|spring" "$tech_stack_file"; then
            squad="elite"
        else
            squad="elite"  # Default to elite for unknown tech stacks
        fi
    fi
    
    echo "$squad"
}

# Function to assign agents based on feature type and squad
assign_agents() {
    local feature_name="$2"
    local squad="$3"
    local feature_dir=".ai-squads/projects/$1/feature-$feature_name"
    
    print_status "Assigning squad agents..."
    
    # Create agent assignments file
    cat > "$feature_dir/agent-assignments.md" << EOF
---
description: Agent Assignments - $feature_name
type: agent-assignments
squad: $squad
created: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
---

# Agent Assignments: $feature_name

## Squad: $squad

## Primary Agent Assignments

### @agent:rusty
**Role**: Technical Implementation Lead
**Responsibilities**:
- Backend development and architecture
- Database design and implementation
- API development and integration
- Performance optimization
- Security implementation
- Code quality and standards enforcement

### @agent:uxe
**Role**: User Experience Design Lead
**Responsibilities**:
- User experience design and research
- User journey mapping and optimization
- Interaction design and usability
- Accessibility compliance
- User testing coordination
- JTBD analysis validation

### @agent:uidev
**Role**: Frontend Implementation Lead
**Responsibilities**:
- Frontend development and implementation
- UI component development
- Responsive design implementation
- Cross-browser compatibility
- Performance optimization
- User interface testing

### @agent:scribas
**Role**: Git Workflow and Quality Gate Manager
**Responsibilities**:
- Git workflow enforcement
- Code review coordination
- Quality gate enforcement
- Release management
- Documentation standards
- Branch management

### @agent:team
**Role**: Project Coordination and Quality Assurance
**Responsibilities**:
- Project coordination and communication
- Quality assurance oversight
- Risk management
- Stakeholder communication
- Progress tracking
- Handoff coordination

### @agent:moesta
**Role**: JTBD Expert and Customer Validation
**Responsibilities**:
- Customer job analysis and validation
- Satisfaction gap identification
- Solution alignment validation
- User research coordination
- Business value validation
- Market research

## Specialized Agent Assignments

### @agent:pharo
**Role**: Smalltalk Backend Specialist
**Trigger**: When smalltalk backend decisions needed
**Responsibilities**:
- Smalltalk-specific implementation
- Pharo framework expertise
- Smalltalk best practices
- Backend architecture for Smalltalk

### @agent:alan
**Role**: System Architecture and Technical Leadership
**Trigger**: When complex architectural decisions needed
**Responsibilities**:
- System architecture design
- Technical decision making
- Scalability planning
- Integration architecture

### @agent:godin
**Role**: Marketing and Business Strategy
**Trigger**: When marketing or business strategy input needed
**Responsibilities**:
- Marketing strategy alignment
- Business value proposition
- Market positioning
- Competitive analysis

### @agent:steve
**Role**: Product Strategy and Vision
**Trigger**: When product strategy decisions needed
**Responsibilities**:
- Product strategy alignment
- Feature prioritization
- Product roadmap integration
- Strategic planning

## Workflow Integration

### Planning Phase
1. **@agent:moesta** validates customer jobs and satisfaction gaps
2. **@agent:uxe** designs user experience and journey
3. **@agent:rusty** designs technical architecture
4. **@agent:team** coordinates planning activities

### Implementation Phase
1. **@agent:rusty** leads technical implementation
2. **@agent:uidev** implements frontend components
3. **@agent:uxe** provides UX guidance and validation
4. **@agent:scribas** enforces quality gates and workflow
5. **@agent:team** coordinates implementation activities

### Validation Phase
1. **@agent:moesta** validates customer job satisfaction
2. **@agent:uxe** conducts user testing and validation
3. **@agent:team** coordinates quality assurance
4. **@agent:scribas** manages release process

## Communication Protocols

### Daily Standups
- **@agent:team** coordinates daily standups
- All agents report progress and blockers
- **@agent:moesta** reports on customer validation progress

### Weekly Reviews
- **@agent:team** coordinates weekly reviews
- **@agent:rusty** reports technical progress
- **@agent:uxe** reports UX progress
- **@agent:moesta** reports customer validation progress

### Quality Gates
- **@agent:scribas** enforces all quality gates
- **@agent:moesta** validates JTBD alignment
- **@agent:team** coordinates quality assurance

## Handoff Protocols

### Design to Development
- **@agent:uxe** hands off UX designs to **@agent:uidev**
- **@agent:rusty** provides technical specifications
- **@agent:team** coordinates handoff process

### Development to Testing
- **@agent:rusty** and **@agent:uidev** hand off to **@agent:team**
- **@agent:moesta** provides testing scenarios
- **@agent:scribas** manages testing workflow

### Testing to Deployment
- **@agent:team** hands off to **@agent:scribas**
- **@agent:moesta** validates customer satisfaction
- **@agent:rusty** provides deployment support

## Notes

This feature follows the $squad squad workflow with specialized agent assignments based on feature requirements and technical complexity.
EOF

    print_success "Agent assignments created for squad: $squad"
}

# Function to create agent-specific content in templates
customize_agent_content() {
    local file_path="$1"
    local feature_name="$2"
    local project_name="$3"
    local squad="$4"
    
    # Add agent-specific content based on file type
    case "$(basename "$file_path")" in
        "problem.md")
            # Add agent assignments to problem analysis
            sed -i.bak "s/## Notes/## Agent Assignments\n\n### @agent:moesta\n**Role**: Customer Job Analysis\n**Responsibilities**:\n- Validate customer jobs and satisfaction gaps\n- Analyze user impact and business impact\n- Identify root causes and constraints\n- Coordinate user research and validation\n\n### @agent:uxe\n**Role**: User Experience Analysis\n**Responsibilities**:\n- Analyze user experience impact\n- Identify UX constraints and requirements\n- Design user journey improvements\n- Coordinate user testing and feedback\n\n## Notes/" "$file_path"
            ;;
        "solution.md")
            # Add agent assignments to solution design
            sed -i.bak "s/## Squad Coordination/## Squad Coordination\n\n### @agent:rusty\n**Role**: Technical Architecture Lead\n**Responsibilities**:\n- Design technical architecture and approach\n- Select technology stack and patterns\n- Define security and performance requirements\n- Coordinate technical implementation\n\n### @agent:uxe\n**Role**: User Experience Design Lead\n**Responsibilities**:\n- Design user experience and interface\n- Define user interaction patterns\n- Coordinate user testing and validation\n- Ensure accessibility compliance\n\n### @agent:uidev\n**Role**: Frontend Implementation Lead\n**Responsibilities**:\n- Implement user interface components\n- Ensure responsive design and compatibility\n- Optimize frontend performance\n- Coordinate with UX design\n\n### @agent:scribas\n**Role**: Quality Gate Manager\n**Responsibilities**:\n- Enforce code quality standards\n- Manage git workflow and reviews\n- Coordinate testing and deployment\n- Ensure documentation standards\n\n### @agent:team\n**Role**: Project Coordinator\n**Responsibilities**:\n- Coordinate overall project execution\n- Manage dependencies and handoffs\n- Track progress and quality metrics\n- Facilitate communication between agents\n\n### @agent:moesta\n**Role**: Customer Validation Expert\n**Responsibilities**:\n- Validate solution alignment with customer jobs\n- Coordinate user research and testing\n- Measure customer satisfaction improvement\n- Ensure business value delivery/" "$file_path"
            ;;
        "tasks.md")
            # Add agent assignments to task breakdown
            sed -i.bak "s/## Task Assignment by Squad Agent/## Task Assignment by Squad Agent\n\n### @agent:rusty\n**Responsibilities**: Technical implementation, backend development, systems programming\n\n### @agent:uxe\n**Responsibilities**: User experience design, user research, accessibility\n\n### @agent:uidev\n**Responsibilities**: Frontend implementation, UI development, responsive design\n\n### @agent:scribas\n**Responsibilities**: Git workflow, quality gates, release management\n\n### @agent:team\n**Responsibilities**: Project coordination, quality assurance, handoff management\n\n### @agent:moesta\n**Responsibilities**: JTBD validation, customer research, satisfaction analysis/" "$file_path"
            ;;
        "jtbd-analysis.md")
            # Add agent-specific content for JTBD analysis
            sed -i.bak "s/---\n\*\*Analysis by\*\*: @agent:moesta/---\n\n**Analysis by**: @agent:moesta\n**Squad**: $squad\n**Feature**: $feature_name\n**Project**: $project_name/" "$file_path"
            ;;
    esac
    
    # Remove backup files
    rm -f "${file_path}.bak"
}

# Function to create Cursor rule integration
create_cursor_integration() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".ai-squads/projects/$project_name/feature-$feature_name"
    
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
- **jtbd-analysis.md** - Customer jobs and satisfaction analysis
- **story-plan.md** - Narrative strategy and storytelling elements
- **tech-specs.md** - Technical specifications and requirements
- **agent-assignments.md** - Squad agent assignments and responsibilities
- **status.md** - Current status and progress
- **implementation-status.md** - Implementation tracking

## Development Guidelines
1. **Follow AI Squads Standards**: Use the established coding standards and quality gates
2. **JTBD Validation**: Ensure solutions address real customer jobs and satisfaction gaps
3. **Storytelling Integration**: Ensure narrative elements enhance user experience and engagement
4. **Update Documentation**: Keep all planning documents updated as implementation progresses
5. **Squad Coordination**: Work with assigned squad agents for specialized tasks
6. **Quality Gates**: Ensure all work passes quality standards before proceeding

## Agent Integration
This feature is assigned to the $squad squad with specific agent responsibilities:
- **@agent:rusty**: Technical implementation and architecture
- **@agent:uxe**: User experience design and research
- **@agent:uidev**: Frontend implementation and UI development
- **@agent:scribas**: Git workflow and quality gate enforcement
- **@agent:team**: Project coordination and quality assurance
- **@agent:moesta**: JTBD validation and customer research

## Quick Commands
- View problem: \`cat .ai-squads/projects/$project_name/feature-$feature_name/problem.md\`
- View JTBD analysis: \`cat .ai-squads/projects/$project_name/feature-$feature_name/jtbd-analysis.md\`
- View story plan: \`cat .ai-squads/projects/$project_name/feature-$feature_name/story-plan.md\`
- View tasks: \`cat .ai-squads/projects/$project_name/feature-$feature_name/tasks.md\`
- View agent assignments: \`cat .ai-squads/projects/$project_name/feature-$feature_name/agent-assignments.md\`
- Update status: Edit status.md and implementation-status.md files

## Integration
This feature integrates with the AI Squads system and follows established workflows with JTBD methodology and storytelling integration.
EOF

    print_success "Cursor rule integration created"
}

# Function to display completion summary
show_completion_summary() {
    local project_name="$1"
    local feature_name="$2"
    local feature_dir=".ai-squads/projects/$project_name/feature-$feature_name"
    
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
    echo "  • jtbd-analysis.md - Customer jobs and satisfaction analysis"
    echo "  • story-plan.md - Narrative strategy and storytelling elements"
    echo "  • tech-specs.md - Technical specifications and requirements"
    echo "  • agent-assignments.md - Squad agent assignments and responsibilities"
    echo "  • status.md - Current status and progress"
    echo "  • implementation-status.md - Implementation tracking"
    echo "  • .cursor-rule.md - Cursor integration"
    echo ""
    echo "🏗️  Project Planning Files:"
    echo "  • Ensured all required project planning files exist"
    echo "  • mission.md, roadmap.md, tech-stack.md, decisions.md, tasks.md"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Review and customize the generated documents"
    echo "  2. Validate with squad agents (@agent:moesta for JTBD validation)"
    echo "  3. Review agent assignments and responsibilities"
    echo "  4. Begin implementation following the task breakdown"
    echo "  5. Update status files as work progresses"
    echo ""
    echo "💡 Tips:"
    echo "  • Use 'cat $feature_dir/tasks.md' to view task breakdown"
    echo "  • Use 'cat $feature_dir/agent-assignments.md' to view agent responsibilities"
    echo "  • Edit status.md to track planning progress"
    echo "  • Follow AI Squads quality gates and standards"
    echo "  • Coordinate with squad agents for specialized work"
    echo "  • @agent:moesta will validate JTBD alignment"
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
    TEMPLATE_DIR=".ai-squads/templates/projects"
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
    
    # Determine squad and assign agents
    local detected_squad=$(determine_squad "$PROJECT_NAME" "$FEATURE_NAME")
    if [ "$SQUAD" != "auto" ]; then
        detected_squad="$SQUAD"
    fi
    
    # Execute planning workflow
    validate_project "$PROJECT_NAME"
    ensure_project_planning_files "$PROJECT_NAME"
    create_feature_structure "$PROJECT_NAME" "$FEATURE_NAME"
    copy_templates "$PROJECT_NAME" "$FEATURE_NAME"
    assign_agents "$PROJECT_NAME" "$FEATURE_NAME" "$detected_squad"
    create_status_files "$PROJECT_NAME" "$FEATURE_NAME"
    update_project_status "$PROJECT_NAME" "$FEATURE_NAME"
    create_cursor_integration "$PROJECT_NAME" "$FEATURE_NAME"
    
    # Customize templates with agent-specific content
    local feature_dir=".ai-squads/projects/$PROJECT_NAME/feature-$FEATURE_NAME"
    for template_file in "$feature_dir"/*.md; do
        if [ -f "$template_file" ]; then
            customize_agent_content "$template_file" "$FEATURE_NAME" "$PROJECT_NAME" "$detected_squad"
        fi
    done
    
    # Show completion summary
    show_completion_summary "$PROJECT_NAME" "$FEATURE_NAME"
}

# Run main function with all arguments
main "$@"
