#!/bin/bash

# create-writer-agent.sh - Set up Writer Agent for SquadsAI projects
# 
# This script sets up the Writer Agent for a given project, including:
# - Writer agent setup documentation
# - Story planning templates and directory structure
# - Workflow integration documentation
# - Updated project configuration for storytelling methodology

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script configuration
SCRIPT_NAME="create-writer-agent.sh"
VERSION="1.0.0"

# Default values
PROJECT_NAME=""
FORCE=false
VERBOSE=false

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

# Function to show help
show_help() {
    cat << EOF
$SCRIPT_NAME v$VERSION - Set up Writer Agent for SquadsAI projects

USAGE:
    $0 <project_name> [options]

ARGUMENTS:
    project_name    Name of the project to set up Writer Agent for

OPTIONS:
    -f, --force     Force setup even if agent already exists
    -v, --verbose   Enable verbose output
    -h, --help      Show this help message

EXAMPLES:
    # Basic usage
    $0 myproject

    # Force reinstall
    $0 backend-api --force

DESCRIPTION:
    This script sets up the Writer Agent for a SquadsAI project, including:
    - Writer agent setup documentation
    - Story planning templates and directory structure
    - Workflow integration documentation
    - Updated project configuration for storytelling methodology

    The Writer Agent specializes in:
    - Storytelling and narrative craft
    - Character development and psychology
    - Story arc design and structure
    - Content strategy and engagement
    - Literary techniques and quality standards

EOF
}

# Function to validate project name
validate_project() {
    local project_name="$1"
    
    if [[ -z "$project_name" ]]; then
        print_error "Project name is required"
        exit 1
    fi
    
    if [[ ! "$project_name" =~ ^[a-zA-Z0-9_-]+$ ]]; then
        print_error "Project name must contain only letters, numbers, hyphens, and underscores"
        exit 1
    fi
}

# Function to check if writer agent is already set up
check_writer_setup() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    if [[ -d "$project_dir" ]] && [[ -f "$project_dir/writer-setup.md" ]]; then
        if [[ "$FORCE" == true ]]; then
            print_warning "Writer agent already exists for project '$project_name'. Force reinstalling..."
            return 0
        else
            print_error "Writer agent already exists for project '$project_name'"
            print_error "Use --force to reinstall"
            exit 1
        fi
    fi
    
    return 0
}

# Function to create writer agent setup
create_writer_setup() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    print_status "Creating writer agent setup for project '$project_name'..."
    
    # Create project directory if it doesn't exist
    mkdir -p "$project_dir"
    
    # Create writer setup documentation
    cat > "$project_dir/writer-setup.md" << EOF
# Writer Agent Setup - $project_name

## Overview
This document describes the Writer Agent setup for the $project_name project, including storytelling integration, narrative strategy, and content quality standards.

## Writer Agent Integration

### **Agent Details**
- **Agent**: @agent:writer
- **Specialization**: Storytelling, character development, and narrative craft
- **Location**: \`.squads-ai/agents/writer.md\`
- **Squad**: Elite Squad (9 agents total)

### **Capabilities**
- **Storytelling Expertise**: Narrative structure, plot development, pacing
- **Character Development**: Multi-dimensional characters with clear motivations
- **Archetype Application**: Expert use of literary archetypes and patterns
- **Conflict Design**: Meaningful conflicts that drive story progression
- **Theme Integration**: Weaving themes and meaning throughout narratives

### **Literary Techniques**
- **Point of View**: Strategic narrative perspective choice
- **Dialogue Crafting**: Natural, character-driven conversations
- **Setting Development**: Rich, immersive world-building
- **Symbolism**: Subtle use of symbols and motifs
- **Foreshadowing**: Strategic placement of hints and clues

## Story Planning Integration

### **Required Documents**
- **story-plan.md**: Narrative strategy and storytelling elements
- **Character Development**: User personas and character arcs
- **Story Structure**: Narrative arc and key story beats
- **Content Strategy**: Tone, voice, and engagement techniques

### **Workflow Integration**
- **Planning Phase**: Story strategy and character development
- **Design Phase**: Narrative content creation and refinement
- **Implementation Phase**: Story integration and testing
- **Review Phase**: Story effectiveness and audience response

## Quality Standards

### **Story Validation**
- **Clarity**: Stories are easy to understand
- **Engagement**: Content captures and maintains attention
- **Authenticity**: Stories feel genuine and relatable
- **Purpose**: Stories serve user goals and project objectives

### **Character Consistency**
- **Voice**: Characters maintain consistent personalities
- **Behavior**: Actions align with established motivations
- **Growth**: Character development is meaningful and logical
- **Relationships**: Interactions are authentic and purposeful

### **Narrative Flow**
- **Pacing**: Story progression is neither too fast nor too slow
- **Transitions**: Story elements connect smoothly
- **Closure**: Stories provide satisfying resolution
- **Continuity**: Stories fit with overall product narrative

## Implementation Guidelines

### **Story Planning Process**
1. **Story Context**: Define project background and narrative goals
2. **Character Development**: Create user personas and character arcs
3. **Story Structure**: Design narrative arc and key story beats
4. **Content Strategy**: Define tone, voice, and engagement techniques
5. **Quality Validation**: Ensure stories meet quality standards

### **Content Creation**
- **User Interface Text**: Labels, buttons, messages, help content
- **Onboarding Flow**: Engaging user introduction experiences
- **Error Messages**: Helpful and empathetic error handling
- **Success Feedback**: Celebrating user achievements
- **Help and Documentation**: Supporting users through challenges

### **Collaboration Points**
- **UX Expert**: User experience storytelling collaboration
- **Product Planner**: Product narrative alignment
- **Software Engineer**: Technical constraint integration
- **UI Implementor**: User interface narrative elements

## Success Metrics

### **Engagement Metrics**
- **Time on Task**: User engagement duration
- **Completion Rate**: Story journey completion
- **Return Visits**: User return frequency
- **Sharing**: Story experience sharing

### **Quality Metrics**
- **User Satisfaction**: Experience rating scores
- **Understanding**: Message comprehension
- **Emotional Response**: Intended emotion achievement
- **Action Taken**: Desired action completion

## Next Steps

1. **Review Story Planning**: Ensure story-plan.md is created for features
2. **Content Creation**: Begin creating narrative content
3. **Quality Validation**: Validate stories against quality standards
4. **User Testing**: Test story effectiveness with users
5. **Iteration**: Refine stories based on feedback

## References

- **Writer Agent**: \`.squads-ai/agents/writer.md\` - Complete agent specification
- **Elite Squad**: \`.squads-ai/squads/elite.md\` - Squad with writer agent
- **Story Template**: \`templates/projects/story-plan.md\` - Story planning template
- **Feature Planning**: \`scripts/plan-feature.sh\` - Enhanced with story planning

---

*This setup ensures that the $project_name project benefits from expert storytelling, creating engaging, meaningful experiences that resonate with users and enhance project success.*
EOF

    print_success "Writer agent setup documentation created"
}

# Function to update project configuration
update_project_config() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    print_status "Updating project configuration for storytelling integration..."
    
    # Create or update project configuration
    if [[ ! -f "$project_dir/project-config.md" ]]; then
        cat > "$project_dir/project-config.md" << EOF
# Project Configuration - $project_name

## Overview
Configuration and settings for the $project_name project, including agent integration and methodology.

## Agent Integration

### **Active Agents**
- **@agent:director**: Project coordination and strategic oversight
- **@agent:jtbd-expert**: Customer jobs analysis and solution validation
- **@agent:writer**: Storytelling, character development, and narrative craft
- **@agent:software-engineer**: Full-stack development and systems programming
- **@agent:ux-expert**: User experience research and design
- **@agent:ui-implementor**: Frontend implementation across platforms
- **@agent:product-planner**: Strategic product planning and development
- **@agent:git-workflow**: Version control and workflow management
- **@agent:collaboration**: Team coordination and quality assurance

## Methodology Integration

### **Jobs To Be Done (JTBD)**
- **Integration**: Full JTBD methodology integration
- **Validation**: Required for all feature planning
- **Quality Gates**: JTBD validation at every phase

### **Storytelling Integration**
- **Integration**: Full storytelling methodology integration
- **Validation**: Required for all feature planning
- **Quality Gates**: Story validation at every phase

## Quality Standards

### **Planning Quality**
- **Completeness**: All required documents created
- **Clarity**: Clear problem and solution definitions
- **JTBD Validation**: Customer jobs identified and solution alignment validated
- **Story Validation**: Narrative strategy and character development planned
- **Feasibility**: Technical and resource feasibility confirmed
- **Alignment**: Alignment with project and squad goals

### **Execution Quality**
- **Standards Compliance**: Follow squad coding and quality standards
- **Testing**: Comprehensive testing coverage
- **Documentation**: Updated documentation and code comments
- **Performance**: Meet performance and quality requirements
- **Story Quality**: Maintain narrative quality and character consistency

### **Review Quality**
- **Goal Achievement**: All defined goals met
- **Quality Standards**: All quality standards satisfied
- **Documentation**: Complete and accurate documentation
- **Knowledge Transfer**: Learnings shared with the team
- **Story Effectiveness**: Stories achieve intended engagement and impact

## Workflow Integration

### **Feature Planning**
- **Required Documents**: problem.md, solution.md, goal.md, tasks.md, jtbd-analysis.md, story-plan.md
- **Quality Gates**: JTBD validation, story validation, technical feasibility
- **Agent Coordination**: Director, JTBD Expert, Writer, and other relevant agents

### **Implementation**
- **Quality Gates**: Code standards, testing, documentation, story quality
- **Agent Coordination**: Software Engineer, UI Implementor, Writer for content
- **Validation**: Technical quality, user experience, narrative effectiveness

### **Review**
- **Quality Gates**: Goal achievement, quality standards, story effectiveness
- **Agent Coordination**: All relevant agents for comprehensive review
- **Documentation**: Complete project documentation and story records

---

*This configuration ensures that the $project_name project follows SquadsAI standards with full JTBD and storytelling methodology integration.*
EOF
        print_success "Project configuration created"
    else
        print_status "Project configuration already exists, updating for storytelling integration..."
        
        # Add storytelling integration to existing config
        if ! grep -q "storytelling" "$project_dir/project-config.md"; then
            # Add storytelling section after JTBD section
            sed -i.bak '/### \*\*Jobs To Be Done (JTBD)\*\*/a\
\
### **Storytelling Integration**\
- **Integration**: Full storytelling methodology integration\
- **Validation**: Required for all feature planning\
- **Quality Gates**: Story validation at every phase' "$project_dir/project-config.md"
            
            # Add story validation to quality standards
            sed -i.bak '/JTBD Validation/a\
- **Story Validation**: Narrative strategy and character development planned' "$project_dir/project-config.md"
            
            # Add story quality to execution quality
            sed -i.bak '/Performance/a\
- **Story Quality**: Maintain narrative quality and character consistency' "$project_dir/project-config.md"
            
            # Add story effectiveness to review quality
            sed -i.bak '/Knowledge Transfer/a\
- **Story Effectiveness**: Stories achieve intended engagement and impact' "$project_dir/project-config.md"
            
            # Add story-plan.md to required documents
            sed -i.bak '/jtbd-analysis.md/a\
- **story-plan.md**: Narrative strategy and storytelling elements' "$project_dir/project-config.md"
            
            # Remove backup files
            rm -f "$project_dir/project-config.md.bak"
            
            print_success "Project configuration updated for storytelling integration"
        else
            print_status "Project configuration already includes storytelling integration"
        fi
    fi
}

# Function to create workflow integration documentation
create_workflow_integration() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    print_status "Creating workflow integration documentation..."
    
    cat > "$project_dir/storytelling-workflow.md" << EOF
# Storytelling Workflow Integration - $project_name

## Overview
This document describes how storytelling methodology is integrated into the project workflow, ensuring that all features benefit from expert narrative craft and character development.

## Workflow Integration Points

### **Feature Planning Workflow**
1. **Problem Analysis**: Define the problem and context
2. **Solution Design**: Design technical solution approach
3. **JTBD Analysis**: Validate customer jobs and satisfaction gaps
4. **Story Planning**: Create narrative strategy and character development
5. **Goal Definition**: Define success criteria and acceptance criteria
6. **Task Breakdown**: Create comprehensive task breakdown
7. **Squad Validation**: Validate with all relevant squad agents

### **Story Planning Process**
1. **Story Context**: Define project background and narrative goals
2. **Character Development**: Create user personas and character arcs
3. **Story Structure**: Design narrative arc and key story beats
4. **Content Strategy**: Define tone, voice, and engagement techniques
5. **Quality Validation**: Ensure stories meet quality standards

### **Implementation Workflow**
1. **Technical Implementation**: Implement technical solution
2. **Story Integration**: Integrate narrative elements and content
3. **Quality Gates**: Validate against all quality standards
4. **Documentation Updates**: Update all relevant documentation
5. **Story Testing**: Test narrative effectiveness with users

### **Review Workflow**
1. **Goal Validation**: Validate against defined goals
2. **Quality Review**: Verify all quality standards
3. **Story Effectiveness**: Validate narrative engagement and impact
4. **Documentation Review**: Update final documentation
5. **Knowledge Transfer**: Share learnings with the team

## Quality Gates

### **Planning Quality Gates**
- **Completeness**: All required documents created including story-plan.md
- **Clarity**: Clear problem, solution, and story definitions
- **JTBD Validation**: Customer jobs identified and solution alignment validated
- **Story Validation**: Narrative strategy and character development planned
- **Feasibility**: Technical and resource feasibility confirmed
- **Alignment**: Alignment with project and squad goals

### **Execution Quality Gates**
- **Standards Compliance**: Follow squad coding and quality standards
- **Testing**: Comprehensive testing coverage
- **Documentation**: Updated documentation and code comments
- **Performance**: Meet performance and quality requirements
- **Story Quality**: Maintain narrative quality and character consistency

### **Review Quality Gates**
- **Goal Achievement**: All defined goals met
- **Quality Standards**: All quality standards satisfied
- **Documentation**: Complete and accurate documentation
- **Knowledge Transfer**: Learnings shared with the team
- **Story Effectiveness**: Stories achieve intended engagement and impact

## Agent Coordination

### **Writer Agent Role**
- **Story Strategy**: Lead narrative strategy and character development
- **Content Creation**: Create engaging narrative content
- **Quality Validation**: Ensure story quality and consistency
- **Collaboration**: Work with all other agents for story integration

### **Agent Collaboration**
- **Director Agent**: Coordinate overall project including story strategy
- **JTBD Expert**: Ensure stories address real customer needs
- **UX Expert**: Collaborate on user experience storytelling
- **Product Planner**: Align stories with product vision
- **Software Engineer**: Ensure stories work within technical constraints
- **UI Implementor**: Integrate narrative elements into interfaces

## Content Types

### **User Experience Stories**
- **User Journey Narratives**: Compelling user journey descriptions
- **Character-Driven Personas**: Engaging user persona development
- **Onboarding Experiences**: Engaging user introduction flows
- **Error Handling**: Helpful and empathetic error messages
- **Success Feedback**: Celebrating user achievements

### **Product Narratives**
- **Product Evolution Stories**: Product development narratives
- **Feature Demonstrations**: Engaging feature presentations
- **Brand Stories**: Authentic brand narratives
- **Case Studies**: Compelling value demonstration

### **Technical Content**
- **Concept Explanations**: Making complex concepts accessible
- **Tutorial Content**: Engaging learning materials
- **Process Descriptions**: Memorable technical process explanations
- **Release Notes**: Compelling feature announcements

## Success Metrics

### **Story Quality Metrics**
- **Engagement**: Stories capture and maintain attention
- **Emotional Impact**: Narratives evoke appropriate emotional responses
- **Memorability**: Stories are memorable and shareable
- **Authenticity**: Characters and situations feel genuine

### **Project Integration Metrics**
- **Alignment**: Stories support and enhance project objectives
- **Consistency**: Narrative elements align with project constraints
- **Collaboration**: Stories work well with other project components
- **Timeliness**: Narrative content delivered when needed

### **Audience Response Metrics**
- **Understanding**: Target audience comprehends story messages
- **Connection**: Stories resonate with audience experiences
- **Action**: Stories inspire desired audience responses
- **Feedback**: Positive audience reactions and engagement

## Best Practices

### **Story Development**
- **User-Centered**: Focus on user needs and experiences
- **Authentic**: Create genuine, relatable narratives
- **Engaging**: Maintain user interest and attention
- **Purposeful**: Ensure stories serve project objectives

### **Content Creation**
- **Consistent Voice**: Maintain consistent tone and personality
- **Clear Messaging**: Ensure messages are easy to understand
- **Emotional Connection**: Create emotional resonance with users
- **Action-Oriented**: Inspire desired user actions

### **Quality Assurance**
- **Regular Review**: Regularly review and refine stories
- **User Testing**: Test stories with target users
- **Feedback Integration**: Incorporate user feedback
- **Continuous Improvement**: Continuously improve narrative quality

## Troubleshooting

### **Common Issues**
- **Story Misalignment**: Stories don't align with project goals
- **Character Inconsistency**: Characters behave inconsistently
- **Engagement Issues**: Stories don't engage users effectively
- **Quality Problems**: Stories don't meet quality standards

### **Solutions**
- **Review Story Context**: Ensure story aligns with project context
- **Validate Character Development**: Ensure consistent character behavior
- **Test User Engagement**: Test stories with target users
- **Apply Quality Standards**: Ensure all quality standards are met

---

*This workflow integration ensures that storytelling methodology is seamlessly integrated into all project phases, creating engaging, meaningful experiences that enhance project success.*
EOF

    print_success "Workflow integration documentation created"
}

# Function to show completion summary
show_completion_summary() {
    local project_name="$1"
    local project_dir=".squads-ai/projects/$project_name"
    
    print_header
    echo ""
    print_success "Writer Agent setup completed successfully!"
    echo ""
    echo "📁 Project Directory: $project_dir"
    echo "📋 Created Files:"
    echo "  • writer-setup.md - Writer agent setup documentation"
    echo "  • project-config.md - Updated project configuration"
    echo "  • storytelling-workflow.md - Workflow integration guide"
    echo ""
    echo "🤖 Writer Agent Integration:"
    echo "  • Agent: @agent:writer added to Elite Squad"
    echo "  • Specialization: Storytelling, character development, narrative craft"
    echo "  • Capabilities: Literary techniques, genre mastery, character psychology"
    echo "  • Workflow: Integrated into all planning and execution phases"
    echo ""
    echo "📚 Story Planning Integration:"
    echo "  • story-plan.md template added to feature planning"
    echo "  • Required for all feature planning workflows"
    echo "  • Quality gates include story validation"
    echo "  • Agent coordination for narrative elements"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Review the created documentation"
    echo "  2. Use story-plan.md in feature planning"
    echo "  3. Coordinate with @agent:writer for narrative elements"
    echo "  4. Validate stories against quality standards"
    echo ""
    echo "💡 Tips:"
    echo "  • Use 'cat $project_dir/story-plan.md' to view story planning template"
    echo "  • Coordinate with @agent:writer for storytelling expertise"
    echo "  • Ensure all features include narrative strategy"
    echo "  • Validate story quality in quality gates"
    echo ""
    print_success "Your project now has full Writer Agent integration!"
}

# Function to print header
print_header() {
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║                    SquadsAI Writer Agent Setup               ║"
    echo "║                        v$VERSION                              ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
}

# Main execution
main() {
    # Parse command line arguments
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
                show_help
                exit 0
                ;;
            -*)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
            *)
                if [[ -z "$PROJECT_NAME" ]]; then
                    PROJECT_NAME="$1"
                else
                    print_error "Multiple project names specified"
                    exit 1
                fi
                shift
                ;;
        esac
    done
    
    # Validate arguments
    if [[ -z "$PROJECT_NAME" ]]; then
        print_error "Project name is required"
        show_help
        exit 1
    fi
    
    # Validate project name
    validate_project "$PROJECT_NAME"
    
    # Check if writer agent is already set up
    check_writer_setup "$PROJECT_NAME"
    
    # Create writer agent setup
    create_writer_setup "$PROJECT_NAME"
    
    # Update project configuration
    update_project_config "$PROJECT_NAME"
    
    # Create workflow integration documentation
    create_workflow_integration "$PROJECT_NAME"
    
    # Show completion summary
    show_completion_summary "$PROJECT_NAME"
}

# Run main function with all arguments
main "$@"
