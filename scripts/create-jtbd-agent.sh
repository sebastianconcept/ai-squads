#!/bin/bash

# create-jtbd-agent.sh - Set up JTBD Expert Agent for AI Squads projects
# 
# This script helps set up the JTBD Expert Agent in new or existing projects
# to ensure proper integration with the Jobs To Be Done methodology.

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
    echo -e "${PURPLE}║                AI Squads JTBD Agent Setup                    ║${NC}"
    echo -e "${PURPLE}╚══════════════════════════════════════════════════════════════╝${NC}"
}

# Function to show usage
show_usage() {
    echo "Usage: $0 <project_name> [options]"
    echo ""
    echo "This script sets up the JTBD Expert Agent for AI Squads projects."
    echo ""
    echo "Arguments:"
    echo "  project_name        Name of the project to set up JTBD agent for"
    echo ""
    echo "Options:"
    echo "  -f, --force         Force setup even if agent already exists"
    echo "  -v, --verbose       Enable verbose output"
    echo "  -h, --help          Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 myproject"
    echo "  $0 backend-api --force"
    echo ""
    echo "The script will:"
    echo "  1. Validate project exists and is properly configured"
    echo "  2. Set up JTBD agent integration"
    echo "  3. Create JTBD analysis templates"
    echo "  4. Update project configuration for JTBD methodology"
    echo "  5. Ensure proper agent activation in workflows"
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
    
    print_success "Project '$project_name' validated"
}

# Function to check if JTBD agent is already set up
check_jtbd_setup() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    if [ -f "$project_dir/jtbd-agent-setup.md" ]; then
        if [ "$FORCE" = true ]; then
            print_warning "JTBD agent already set up, but --force specified. Reinstalling..."
        else
            print_error "JTBD agent already set up for project '$project_name'"
            print_error "Use --force to reinstall"
            exit 1
        fi
    fi
}

# Function to create JTBD agent setup documentation
create_jtbd_setup() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Creating JTBD agent setup documentation..."
    
    cat > "$project_dir/jtbd-agent-setup.md" << EOF
# JTBD Expert Agent Setup

## Overview

This project has been configured with the JTBD Expert Agent (@agent:moesta) to ensure all features and solutions are validated against customer jobs and satisfaction needs.

## Agent Integration

### Agent Details
- **Name**: @agent:moesta
- **Specialization**: Customer jobs analysis and solution validation
- **Squad**: Elite Squad
- **Activation**: Automatic in planning workflows

### Capabilities
- Customer jobs identification and mapping
- Job satisfaction gap analysis
- Solution alignment validation
- Unintended consequence identification
- Customer research methodology design

## Workflow Integration

### Feature Planning
The JTBD agent automatically participates in feature planning to:
1. Analyze customer jobs and validate feature alignment
2. Map job progress and identify satisfaction gaps
3. Validate solutions against JTBD principles
4. Ensure solutions don't create new jobs or friction

### Project Planning
The JTBD agent participates in project planning to:
1. Establish JTBD foundation for project scope
2. Validate project alignment with customer jobs
3. Create job satisfaction measurement frameworks

### Quality Gates
All features must pass JTBD validation:
- Customer jobs clearly identified
- Solution addresses real satisfaction gaps
- Unintended consequences identified and mitigated
- Job satisfaction metrics established

## Documents Created

### JTBD Analysis Template
- **jtbd-analysis.md**: Customer jobs and satisfaction analysis template
- **customer-research.md**: JTBD-focused research methodology template

### Integration Files
- **jtbd-agent-setup.md**: This setup documentation
- Updated project configuration for JTBD methodology

## Usage

### Activating JTBD Agent
The agent is automatically activated in planning workflows:
- Feature planning: @agent:moesta analyzes customer jobs
- Solution design: @agent:moesta validates solution alignment
- Project scope: @agent:moesta ensures customer focus

### Manual Activation
To manually activate the JTBD agent:
\`\`\`
@agent:moesta
\`\`\`

### JTBD Analysis
Use the jtbd-analysis.md template for comprehensive customer jobs analysis:
1. Identify customer jobs and context
2. Map job progress and satisfaction gaps
3. Validate solution alignment
4. Identify unintended consequences
5. Establish success metrics

## Success Metrics

### Job Understanding
- Clear articulation of customer jobs
- Accurate identification of satisfaction gaps
- Validated understanding of job context

### Solution Alignment
- Solutions address real job satisfaction gaps
- Minimal unintended consequences
- Measurable job satisfaction improvement

### Strategic Impact
- Project scope aligned with customer jobs
- Feature prioritization based on job impact
- Customer research yields actionable insights

## Next Steps

1. **Review Templates**: Familiarize yourself with JTBD analysis templates
2. **Plan Features**: Use plan-feature.sh to create JTBD-validated features
3. **Conduct Research**: Use customer-research.md for JTBD-focused research
4. **Validate Solutions**: Ensure all solutions pass JTBD validation
5. **Measure Success**: Track job satisfaction metrics and improvement

## Support

For questions about JTBD methodology or agent integration:
- Review JTBD agent documentation in .ai-squads/agents/jtbd-expert.md
- Check Elite Squad configuration in .ai-squads/squads/elite.md
- Use @agent:moesta for specific JTBD guidance

---

**Setup by**: AI Squads JTBD Agent Setup Script  
**Date**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")  
**Status**: Active
EOF

    print_success "JTBD agent setup documentation created"
}

# Function to update project configuration
update_project_config() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Updating project configuration for JTBD methodology..."
    
    # Update project README if it exists
    if [ -f "$project_dir/README.md" ]; then
        if ! grep -q "JTBD" "$project_dir/README.md"; then
            echo "" >> "$project_dir/README.md"
            echo "## JTBD Integration" >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "This project uses Jobs To Be Done methodology to ensure all features address real customer needs. The @agent:moesta validates feature alignment with customer jobs and satisfaction gaps." >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "### JTBD Documents" >> "$project_dir/README.md"
            echo "- **jtbd-analysis.md**: Customer jobs and satisfaction analysis" >> "$project_dir/README.md"
            echo "- **customer-research.md**: JTBD-focused research methodology" >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "### Quality Gates" >> "$project_dir/README.md"
            echo "All features must pass JTBD validation before implementation." >> "$project_dir/README.md"
        fi
        print_success "Project README updated with JTBD integration"
    fi
    
    # Create JTBD directory structure
    mkdir -p "$project_dir/jtbd"
    mkdir -p "$project_dir/jtbd/templates"
    mkdir -p "$project_dir/jtbd/research"
    
    # Copy JTBD templates to project
    if [ -f ".ai-squads/templates/projects/jtbd-analysis.md" ]; then
        cp ".ai-squads/templates/projects/jtbd-analysis.md" "$project_dir/jtbd/templates/"
        print_status "  ✓ Copied jtbd-analysis.md template"
    fi
    
    if [ -f ".ai-squads/templates/projects/customer-research.md" ]; then
        cp ".ai-squads/templates/projects/customer-research.md" "$project_dir/jtbd/templates/"
        print_status "  ✓ Copied customer-research.md template"
    fi
    
    print_success "Project JTBD directory structure created"
}

# Function to create JTBD workflow integration
create_workflow_integration() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Creating JTBD workflow integration..."
    
    cat > "$project_dir/jtbd/workflow-integration.md" << EOF
# JTBD Workflow Integration

## Overview

This document outlines how the JTBD Expert Agent integrates with project workflows to ensure customer-focused development.

## Workflow Integration Points

### 1. Feature Planning Workflow
**Trigger**: Feature planning initiation
**JTBD Agent Role**: Lead customer jobs analysis

**Workflow**:
1. @agent:moesta analyzes customer jobs and context
2. @agent:moesta identifies satisfaction gaps
3. @agent:moesta validates feature alignment with jobs
4. @agent:moesta creates job satisfaction metrics
5. Feature proceeds only after JTBD validation

### 2. Solution Design Workflow
**Trigger**: Solution architecture planning
**JTBD Agent Role**: Solution validation and optimization

**Workflow**:
1. @agent:moesta reviews proposed solution
2. @agent:moesta maps solution to job satisfaction gaps
3. @agent:moesta identifies unintended consequences
4. @agent:moesta recommends job-focused optimizations
5. Solution proceeds only after JTBD validation

### 3. Project Scope Workflow
**Trigger**: Project scope definition or changes
**JTBD Agent Role**: Scope validation and customer alignment

**Workflow**:
1. @agent:moesta analyzes project mission through JTBD lens
2. @agent:moesta identifies primary customer jobs
3. @agent:moesta validates scope against job satisfaction needs
4. @agent:moesta recommends scope adjustments if needed
5. Project scope proceeds only after JTBD validation

## Quality Gates

### JTBD Validation Requirements
- [ ] Customer jobs clearly identified and articulated
- [ ] Job satisfaction gaps identified and prioritized
- [ ] Solution directly addresses identified gaps
- [ ] Unintended consequences identified and mitigated
- [ ] Job satisfaction metrics established
- [ ] Customer research plan created (if needed)

### Validation Process
1. **Initial Review**: JTBD agent reviews planning documents
2. **Gap Analysis**: Identifies missing JTBD analysis or validation
3. **Recommendations**: Provides specific improvement suggestions
4. **Final Validation**: Confirms all JTBD requirements met
5. **Approval**: Feature/solution approved for implementation

## Agent Activation Commands

### Manual Activation
\`\`\`
@agent:moesta
\`\`\`

### Specific JTBD Tasks
\`\`\`
@agent:moesta analyze customer jobs for [FEATURE_NAME]
@agent:moesta validate solution alignment for [FEATURE_NAME]
@agent:moesta create job satisfaction metrics for [FEATURE_NAME]
\`\`\`

## Success Metrics

### Planning Quality
- All features include comprehensive JTBD analysis
- Solutions validated against customer jobs
- Unintended consequences identified early
- Job satisfaction metrics established

### Implementation Quality
- Solutions directly address customer jobs
- Minimal unintended consequences
- Measurable job satisfaction improvement
- Customer validation of job satisfaction

### Strategic Impact
- Project scope aligned with customer needs
- Feature prioritization based on job impact
- Customer research yields actionable insights
- Product strategy informed by job understanding

---

**Integration by**: AI Squads JTBD Agent Setup Script  
**Date**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")  
**Status**: Active
EOF

    print_success "JTBD workflow integration created"
}

# Function to display completion summary
show_completion_summary() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_header
    echo ""
    print_success "JTBD Expert Agent setup completed successfully!"
    echo ""
    echo "📁 Project Directory: $project_dir"
    echo "🤖 JTBD Agent: @agent:moesta"
    echo "📋 Created Documents:"
    echo "  • jtbd-agent-setup.md - JTBD agent setup documentation"
    echo "  • jtbd/workflow-integration.md - Workflow integration guide"
    echo "  • jtbd/templates/ - JTBD analysis templates"
    echo "  • jtbd/research/ - Research methodology directory"
    echo ""
    echo "🔄 Workflow Integration:"
    echo "  • JTBD agent automatically activated in planning workflows"
    echo "  • Feature planning includes customer jobs analysis"
    echo "  • Solution design validated against JTBD principles"
    echo "  • Quality gates include JTBD validation"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Review JTBD agent setup documentation"
    echo "  2. Use plan-feature.sh to create JTBD-validated features"
    echo "  3. Activate @agent:moesta for specific guidance"
    echo "  4. Conduct customer research using JTBD methodology"
    echo "  5. Ensure all solutions pass JTBD validation"
    echo ""
    echo "💡 Tips:"
    echo "  • Use 'cat $project_dir/jtbd-agent-setup.md' to view setup details"
    echo "  • Activate JTBD agent with '@agent:moesta'"
    echo "  • Follow JTBD methodology in all feature planning"
    echo "  • Measure success through job satisfaction metrics"
    echo ""
    print_success "Your project is now ready for JTBD-focused development!"
}

# Main execution
main() {
    # Parse command line arguments
    PROJECT_NAME=""
    FORCE=false
    VERBOSE=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -f|--force)
                FORCE=true
                shift
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
    if [ -z "$PROJECT_NAME" ]; then
        print_error "Missing required argument: project_name"
        show_usage
        exit 1
    fi
    
    # Show header
    print_header
    echo ""
    print_status "Setting up JTBD Expert Agent for project: $PROJECT_NAME"
    echo ""
    
    # Execute setup workflow
    validate_project "$PROJECT_NAME"
    check_jtbd_setup "$PROJECT_NAME"
    create_jtbd_setup "$PROJECT_NAME"
    update_project_config "$PROJECT_NAME"
    create_workflow_integration "$PROJECT_NAME"
    
    # Show completion summary
    show_completion_summary "$PROJECT_NAME"
}

# Run main function with all arguments
main "$@"
