#!/bin/bash

# create-pharo-agent.sh - Set up Pharo Development Agent for AI Squads projects
# 
# This script helps set up the Pharo Development Agent in new or existing projects
# to ensure proper integration with modern Pharo development workflows.

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
    echo -e "${PURPLE}║                AI Squads Pharo Agent Setup                    ║${NC}"
    echo -e "${PURPLE}╚══════════════════════════════════════════════════════════════╝${NC}"
}

# Function to show usage
show_usage() {
    echo "Usage: $0 <project_name> [options]"
    echo ""
    echo "This script sets up the Pharo Development Agent for AI Squads projects."
    echo ""
    echo "Arguments:"
    echo "  project_name        Name of the project to set up Pharo agent for"
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
    echo "  2. Set up Pharo agent integration"
    echo "  3. Create modern development workflow templates"
    echo "  4. Update project configuration for Pharo development"
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

# Function to check if Pharo agent is already set up
check_pharo_setup() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    if [ -f "$project_dir/pharo-agent-setup.md" ]; then
        if [ "$FORCE" = true ]; then
            print_warning "Pharo agent already set up, but --force specified. Reinstalling..."
        else
            print_error "Pharo agent already set up for project '$project_name'"
            print_error "Use --force to reinstall"
            exit 1
        fi
    fi
}

# Function to create Pharo agent setup documentation
create_pharo_setup() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Creating Pharo agent setup documentation..."
    
    cat > "$project_dir/pharo-agent-setup.md" << EOF
# Pharo Development Agent Setup

## Overview

This project has been configured with the Pharo Development Agent (@agent:pharo) to ensure modern Smalltalk development practices with source-first workflows and comprehensive automation.

## Agent Integration

### Agent Details
- **Name**: @agent:pharo
- **Specialization**: Modern Pharo development with source-first workflows
- **Squad**: Elite Squad
- **Activation**: Automatic in development workflows

### Capabilities
- Source-first development practices
- Automated testing and CI/CD integration
- Documentation generation from source
- Performance monitoring and optimization
- Modern package management with Metacello
- Git-based version control workflows

## Workflow Integration

### Development Workflow
The Pharo agent automatically participates in development to:
1. Guide source-first development practices
2. Implement automated testing frameworks
3. Generate documentation from source code
4. Set up CI/CD pipelines
5. Optimize performance and monitoring

### Project Setup
The Pharo agent participates in project setup to:
1. Create modern project structure
2. Set up automated build systems
3. Configure testing frameworks
4. Establish documentation generation
5. Integrate with version control

### Quality Gates
All development must pass modern quality gates:
- Source-first development practices
- Comprehensive test coverage (minimum 80%)
- Auto-generated documentation
- Successful CI/CD pipeline
- Performance benchmarks met

## Documents Created

### Development Templates
- **modern-workflow.md**: Modern Pharo development workflow guide
- **testing-framework.md**: Comprehensive testing setup template
- **ci-cd-setup.md**: CI/CD pipeline configuration template

### Integration Files
- **pharo-agent-setup.md**: This setup documentation
- Updated project configuration for modern Pharo development

## Usage

### Activating Pharo Agent
The agent is automatically activated in development workflows:
- Project setup: @agent:pharo creates modern project structure
- Development: @agent:pharo guides source-first practices
- Testing: @agent:pharo implements comprehensive testing
- Documentation: @agent:pharo generates docs from source
- CI/CD: @agent:pharo sets up automated pipelines

### Manual Activation
To manually activate the Pharo agent:
\`\`\`
@agent:pharo
\`\`\`

### Modern Development
Use the modern-workflow.md template for comprehensive Pharo development:
1. Set up source-first project structure
2. Implement automated testing
3. Generate documentation from source
4. Configure CI/CD pipelines
5. Establish performance monitoring

## Success Metrics

### Development Quality
- Source-first development practices implemented
- Comprehensive test coverage achieved
- Documentation auto-generated from source
- CI/CD pipeline operational

### Performance
- Build times optimized
- Test execution automated
- Documentation generation streamlined
- Deployment automated

### Team Productivity
- Modern development practices adopted
- Automated workflows reduce manual tasks
- Quality gates ensure consistent standards
- Knowledge sharing through documentation

## Next Steps

1. **Review Templates**: Familiarize yourself with modern development templates
2. **Set Up Project**: Use modern-workflow.md to create project structure
3. **Implement Testing**: Use testing-framework.md for comprehensive testing
4. **Configure CI/CD**: Use ci-cd-setup.md for automated pipelines
5. **Generate Docs**: Set up documentation generation from source

## Support

For questions about modern Pharo development or agent integration:
- Review Pharo agent documentation in .ai-squads/agents/pharo.mdc
- Check Elite Squad configuration in .ai-squads/squads/elite.mdc
- Use @agent:pharo for specific modern development guidance

---

**Setup by**: AI Squads Pharo Agent Setup Script  
**Date**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")  
**Status**: Active
EOF

    print_success "Pharo agent setup documentation created"
}

# Function to update project configuration
update_project_config() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Updating project configuration for modern Pharo development..."
    
    # Update project README if it exists
    if [ -f "$project_dir/README.md" ]; then
        if ! grep -q "Pharo" "$project_dir/README.md"; then
            echo "" >> "$project_dir/README.md"
            echo "## Pharo Development Integration" >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "This project uses modern Pharo development practices with source-first workflows and comprehensive automation. The @agent:pharo guides development practices and ensures quality standards." >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "### Development Documents" >> "$project_dir/README.md"
            echo "- **modern-workflow.md**: Modern Pharo development workflow" >> "$project_dir/README.md"
            echo "- **testing-framework.md**: Comprehensive testing setup" >> "$project_dir/README.md"
            echo "- **ci-cd-setup.md**: CI/CD pipeline configuration" >> "$project_dir/README.md"
            echo "" >> "$project_dir/README.md"
            echo "### Quality Gates" >> "$project_dir/README.md"
            echo "All development must pass modern quality gates including source-first practices, comprehensive testing, and automated documentation." >> "$project_dir/README.md"
        fi
        print_success "Project README updated with Pharo development integration"
    fi
    
    # Create Pharo directory structure
    mkdir -p "$project_dir/pharo"
    mkdir -p "$project_dir/pharo/templates"
    mkdir -p "$project_dir/pharo/workflows"
    
    # Copy Pharo templates to project
    if [ -f ".ai-squads/templates/projects/modern-workflow.md" ]; then
        cp ".ai-squads/templates/projects/modern-workflow.md" "$project_dir/pharo/templates/"
        print_status "  ✓ Copied modern-workflow.md template"
    fi
    
    if [ -f ".ai-squads/templates/projects/testing-framework.md" ]; then
        cp ".ai-squads/templates/projects/testing-framework.md" "$project_dir/pharo/templates/"
        print_status "  ✓ Copied testing-framework.md template"
    fi
    
    if [ -f ".ai-squads/templates/projects/ci-cd-setup.md" ]; then
        cp ".ai-squads/templates/projects/ci-cd-setup.md" "$project_dir/pharo/templates/"
        print_status "  ✓ Copied ci-cd-setup.md template"
    fi
    
    print_success "Project Pharo directory structure created"
}

# Function to create Pharo workflow integration
create_workflow_integration() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_status "Creating Pharo workflow integration..."
    
    cat > "$project_dir/pharo/workflow-integration.md" << EOF
# Pharo Development Workflow Integration

## Overview

This document outlines how the Pharo Development Agent integrates with project workflows to ensure modern Smalltalk development practices.

## Workflow Integration Points

### 1. Project Setup Workflow
**Trigger**: Project initialization
**Pharo Agent Role**: Lead modern project structure creation

**Workflow**:
1. @agent:pharo creates modern project structure
2. @agent:pharo sets up source-first development practices
3. @agent:pharo configures automated testing framework
4. @agent:pharo establishes documentation generation
5. @agent:pharo sets up CI/CD pipeline configuration

### 2. Development Workflow
**Trigger**: Feature development initiation
**Pharo Agent Role**: Guide source-first development practices

**Workflow**:
1. @agent:pharo guides source-first development
2. @agent:pharo implements automated testing
3. @agent:pharo generates documentation from source
4. @agent:pharo optimizes performance
5. @agent:pharo ensures quality gates compliance

### 3. Testing Workflow
**Trigger**: Test execution or test-driven development
**Pharo Agent Role**: Comprehensive testing implementation

**Workflow**:
1. @agent:pharo implements unit testing
2. @agent:pharo sets up integration testing
3. @agent:pharo configures performance testing
4. @agent:pharo generates coverage reports
5. @agent:pharo integrates with CI/CD pipeline

### 4. Documentation Workflow
**Trigger**: Documentation generation or updates
**Pharo Agent Role**: Automated documentation from source

**Workflow**:
1. @agent:pharo generates API documentation
2. @agent:pharo creates user guides
3. @agent:pharo documents architecture
4. @agent:pharo maintains documentation quality
5. @agent:pharo integrates with development workflow

### 5. CI/CD Workflow
**Trigger**: Continuous integration or deployment
**Pharo Agent Role**: Automated pipeline implementation

**Workflow**:
1. @agent:pharo sets up automated builds
2. @agent:pharo configures automated testing
3. @agent:pharo implements automated deployment
4. @agent:pharo monitors performance
5. @agent:pharo ensures quality gates

## Quality Gates

### Modern Development Requirements
- [ ] Source-first development practices implemented
- [ ] Comprehensive test coverage (minimum 80%)
- [ ] Documentation auto-generated from source
- [ ] CI/CD pipeline operational
- [ ] Performance benchmarks established
- [ ] Security testing integrated

### Validation Process
1. **Initial Review**: Pharo agent reviews project setup
2. **Development Check**: Validates source-first practices
3. **Testing Verification**: Confirms comprehensive testing
4. **Documentation Review**: Ensures auto-generation works
5. **CI/CD Validation**: Verifies pipeline functionality
6. **Performance Check**: Confirms benchmarks met
7. **Final Approval**: Project approved for modern development

## Agent Activation Commands

### Manual Activation
\`\`\`
@agent:pharo
\`\`\`

### Specific Pharo Tasks
\`\`\`
@agent:pharo setup modern project structure for [PROJECT_NAME]
@agent:pharo implement testing framework for [PROJECT_NAME]
@agent:pharo configure CI/CD pipeline for [PROJECT_NAME]
@agent:pharo generate documentation for [PROJECT_NAME]
\`\`\`

## Success Metrics

### Development Quality
- Source-first development practices implemented
- Comprehensive test coverage achieved
- Documentation auto-generated from source
- CI/CD pipeline operational
- Performance benchmarks met

### Team Productivity
- Modern development practices adopted
- Automated workflows reduce manual tasks
- Quality gates ensure consistent standards
- Knowledge sharing through documentation
- Reduced time to deployment

### Strategic Impact
- Project structure optimized for modern development
- Development practices aligned with industry standards
- Quality assurance automated and consistent
- Team productivity improved through automation
- Knowledge transfer facilitated through documentation

---

**Integration by**: AI Squads Pharo Agent Setup Script  
**Date**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")  
**Status**: Active
EOF

    print_success "Pharo workflow integration created"
}

# Function to display completion summary
show_completion_summary() {
    local project_name="$1"
    local project_dir=".ai-squads/projects/$project_name"
    
    print_header
    echo ""
    print_success "Pharo Development Agent setup completed successfully!"
    echo ""
    echo "📁 Project Directory: $project_dir"
    echo "🤖 Pharo Agent: @agent:pharo"
    echo "📋 Created Documents:"
    echo "  • pharo-agent-setup.md - Pharo agent setup documentation"
    echo "  • pharo/workflow-integration.md - Workflow integration guide"
    echo "  • pharo/templates/ - Modern development templates"
    echo "  • pharo/workflows/ - Development workflow directory"
    echo ""
    echo "🔄 Workflow Integration:"
    echo "  • Pharo agent automatically activated in development workflows"
    echo "  • Project setup includes modern development practices"
    echo "  • Development guided by source-first principles"
    echo "  • Quality gates include comprehensive testing and documentation"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Review Pharo agent setup documentation"
    echo "  2. Use modern-workflow.md to set up project structure"
    echo "  3. Activate @agent:pharo for specific guidance"
    echo "  4. Implement testing framework using templates"
    echo "  5. Set up CI/CD pipeline for automated development"
    echo ""
    echo "💡 Tips:"
    echo "  • Use 'cat $project_dir/pharo-agent-setup.md' to view setup details"
    echo "  • Activate Pharo agent with '@agent:pharo'"
    echo "  • Follow modern development practices in all development"
    echo "  • Measure success through automated quality gates"
    echo ""
    print_success "Your project is now ready for modern Pharo development!"
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
    print_status "Setting up Pharo Development Agent for project: $PROJECT_NAME"
    echo ""
    
    # Execute setup workflow
    validate_project "$PROJECT_NAME"
    check_pharo_setup "$PROJECT_NAME"
    create_pharo_setup "$PROJECT_NAME"
    update_project_config "$PROJECT_NAME"
    create_workflow_integration "$PROJECT_NAME"
    
    # Show completion summary
    show_completion_summary "$PROJECT_NAME"
}

# Run main function with all arguments
main "$@"
