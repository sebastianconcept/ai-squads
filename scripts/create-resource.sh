#!/bin/bash

# create-resource.sh - Create a new squad or agent using templates
# Usage: ./create-resource.sh squad <squad_name>
# Usage: ./create-resource.sh agent <agent_name>

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
    echo "Usage: $0 <resource_type> <name>"
    echo ""
    echo "This script will create a new squad or agent using templates from ai-squads/templates/"
    echo ""
    echo "Resource Types:"
      echo "  squad <squad_name>    Create a new squad using ai-squads/templates/squad-template.md"
echo "  agent <squad_name>    Create a new agent using ai-squads/templates/agent-template.md"
    echo ""
    echo "Examples:"
    echo "  $0 squad web-dev"
    echo "  $0 squad mobile-apps"
    echo "  $0 agent data-analyst"
    echo "  $0 agent security-expert"
    echo ""
    echo "The script will:"
    echo "  1. Validate the resource type and name"
    echo "  2. Create the new resource file from template"
    echo "  3. Update the resource file with the correct name"
    echo "  4. Create a reference file in .cursor/rules/"
    echo "  5. Update the README to include the new resource"
}

# Function to normalize resource name
normalize_resource_name() {
    local name="$1"
    # Convert to lowercase and replace spaces/underscores with hyphens
    echo "$name" | tr '[:upper:]' '[:lower:]' | sed 's/[[:space:]_]/-/g' | sed 's/-+/-/g' | sed 's/^-//;s/-$//'
}

# Function to create resource display name
create_display_name() {
    local name="$1"
    # Convert hyphens to spaces and capitalize first letter of each word
    echo "$name" | sed 's/-/ /g' | sed 's/\b\w/\U&/g'
}

# Function to create squad
create_squad() {
    local squad_name="$1"
    local normalized_name=$(normalize_resource_name "$squad_name")
    local display_name=$(create_display_name "$normalized_name")
    local squad_file="$SQUADS_DIR/$normalized_name.md"
    
    print_status "Creating new squad: $display_name"
    print_status "Normalized name: $normalized_name"
    print_status "Squad file: $squad_file"
    
    # Check if squad already exists
    if [ -f "$squad_file" ]; then
        print_warning "Squad file already exists: $squad_file"
        read -p "Overwrite existing squad? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_status "Squad creation cancelled"
            return 1
        fi
        print_status "Overwriting existing squad file"
    fi
    
    # Copy template to new squad file
    print_status "Creating squad from template..."
    cp "$SQUAD_TEMPLATE" "$squad_file"
    
    # Update the squad file with the correct name
    print_status "Updating squad file with correct name..."
    sed -i.bak "s/\[SQUAD_NAME\]/$display_name/g" "$squad_file"
    sed -i.bak "s/\[One sentence description of the squad's purpose and focus area\]/Specialized squad for [PURPOSE]. Focuses on [FOCUS_AREA]./g" "$squad_file"
    
    # Remove backup file
    rm "${squad_file}.bak"
    
    print_success "Squad file created: $squad_file"
    
    # Create reference file in .cursor/rules
    local cursor_squad_file="$CURSOR_RULES_DIR/${normalized_name}-squad.md"
    print_status "Creating reference file in .cursor/rules..."
    
    # Create the reference file
    cat > "$cursor_squad_file" << EOF
---
description: $display_name Squad
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# $display_name Squad

## Source of Truth
This squad is defined in \`ai-squads/squads/$normalized_name.md\`

## Quick Reference
- **Focus**: [PURPOSE_AND_FOCUS]
- **Specialization**: [SPECIALIZATION]
- **Members**: [NUMBER] specialized agents including Director and core team
- **Technology**: [TECHNOLOGY_STACK]

## Usage
- **Activation**: Activated by Director Agent for [PROJECT_TYPE] projects
- **Workflow**: [WORKFLOW_DESCRIPTION]
- **Outputs**: [EXPECTED_OUTPUTS]

## Squad Reference
For complete squad definition and capabilities, see: \`@~/ai-squads/squads/$normalized_name.md\`
EOF
    
    print_success "Reference file created: $cursor_squad_file"
    
    # Update the README to include the new squad
    update_readme "$normalized_name-squad.md" "claude-mapping.md"
    
    return 0
}

# Function to create agent
create_agent() {
    local agent_name="$1"
    local normalized_name=$(normalize_resource_name "$agent_name")
    local display_name=$(create_display_name "$normalized_name")
    local agent_file="$AGENTS_DIR/$normalized_name.md"
    
    print_status "Creating new agent: $display_name"
    print_status "Normalized name: $normalized_name"
    print_status "Agent file: $agent_file"
    
    # Check if agent already exists
    if [ -f "$agent_file" ]; then
        print_warning "Agent file already exists: $agent_file"
        read -p "Overwrite existing agent? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_status "Agent creation cancelled"
            return 1
        fi
        print_status "Overwriting existing agent file"
    fi
    
    # Copy template to new agent file
    print_status "Creating agent from template..."
    cp "$AGENT_TEMPLATE" "$agent_file"
    
    # Update the agent file with the correct name
    print_status "Updating agent file with correct name..."
    sed -i.bak "s/\[AGENT_NAME\]/$display_name/g" "$agent_file"
    sed -i.bak "s/\[BRIEF_DESCRIPTION\]/[Brief description of what this agent does]/g" "$agent_file"
    
    # Remove backup file
    rm "${agent_file}.bak"
    
    print_success "Agent file created: $agent_file"
    
    # Create reference file in .cursor/rules
    local cursor_agent_file="$CURSOR_RULES_DIR/${normalized_name}-agent.md"
    print_status "Creating reference file in .cursor/rules..."
    
    # Create the reference file
    cat > "$cursor_agent_file" << EOF
---
description: $display_name Agent - [Brief description]
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# $display_name Agent

## Source of Truth
This agent is defined in \`ai-squads/agents/$normalized_name.md\`

## Quick Reference
- **Primary Role**: [Primary role and responsibility]
- **Core Capabilities**: [Key capabilities and specializations]
- **Key Workflows**: [Main workflows and processes]
- **Integration**: Works with [other agents this agent collaborates with]

## Usage
- **Activation**: Activated by Director Agent when [activation criteria]
- **Triggers**: [What triggers this agent's activation]
- **Outputs**: [What this agent produces or delivers]

## Agent Reference
For complete agent definition and capabilities, see: \`@~/ai-squads/agents/$normalized_name.md\`
EOF
    
    print_success "Reference file created: $cursor_agent_file"
    
    # Update the README to include the new agent
    update_readme "$normalized_name-agent.md" "claude-mapping.md"
    
    return 0
}

# Function to update README
update_readme() {
    local resource_file="$1"
    local before_file="$2"
    local readme_file="$CURSOR_RULES_DIR/README.md"
    
    if [ -f "$readme_file" ]; then
        print_status "Updating README to include new resource..."
        
        # Add the new resource to the structure section
        if ! grep -q "$resource_file" "$readme_file"; then
            # Find the line before the specified file and insert the new resource
            sed -i.bak "/$before_file - /i\\
- \`$resource_file\` - References \`ai-squads/[type]/$resource_file\`" "$readme_file"
            
            # Remove backup file
            rm "${readme_file}.bak"
            print_success "README updated with new resource reference"
        else
            print_status "README already contains reference to new resource"
        fi
    else
        print_warning "README file not found, skipping update"
    fi
}

# Check if help is requested
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
    show_usage
    exit 0
fi

# Check if resource type and name are provided
if [ $# -lt 2 ]; then
    print_error "Resource type and name are required"
    show_usage
    exit 1
fi

RESOURCE_TYPE="$1"
RESOURCE_NAME="$2"

# Validate resource type
if [[ "$RESOURCE_TYPE" != "squad" && "$RESOURCE_TYPE" != "agent" ]]; then
    print_error "Invalid resource type: $RESOURCE_TYPE"
    print_error "Valid types are: squad, agent"
    show_usage
    exit 1
fi

# Set up paths
SQUAD_TEMPLATE="ai-squads/templates/squad-template.md"
AGENT_TEMPLATE="ai-squads/templates/agent-template.md"
SQUADS_DIR="ai-squads/squads"
AGENTS_DIR="ai-squads/agents"
CURSOR_RULES_DIR=".cursor/rules"

# Check if templates exist
if [ "$RESOURCE_TYPE" == "squad" ] && [ ! -f "$SQUAD_TEMPLATE" ]; then
    print_error "Squad template not found: $SQUAD_TEMPLATE"
    print_error "Make sure you're running this script from the project root directory"
    exit 1
fi

if [ "$RESOURCE_TYPE" == "agent" ] && [ ! -f "$AGENT_TEMPLATE" ]; then
    print_error "Agent template not found: $AGENT_TEMPLATE"
    print_error "Make sure you're running this script from the project root directory"
    exit 1
fi

# Create necessary directories
if [ "$RESOURCE_TYPE" == "squad" ]; then
    if [ ! -d "$SQUADS_DIR" ]; then
        print_status "Creating squads directory: $SQUADS_DIR"
        mkdir -p "$SQUADS_DIR"
    fi
    create_squad "$RESOURCE_NAME"
elif [ "$RESOURCE_TYPE" == "agent" ]; then
    if [ ! -d "$AGENTS_DIR" ]; then
        print_status "Creating agents directory: $AGENTS_DIR"
        mkdir -p "$AGENTS_DIR"
    fi
    create_agent "$RESOURCE_NAME"
fi

# Create cursor rules directory if it doesn't exist
if [ ! -d "$CURSOR_RULES_DIR" ]; then
    print_status "Creating cursor rules directory: $CURSOR_RULES_DIR"
    mkdir -p "$CURSOR_RULES_DIR"
fi

# Show what was created
if [ "$RESOURCE_TYPE" == "squad" ]; then
    local normalized_name=$(normalize_resource_name "$RESOURCE_NAME")
    print_success "Squad creation completed successfully!"
    echo ""
    print_status "Created files:"
    echo "  📁 Squad definition: ai-squads/squads/$normalized_name.md"
    echo "  📁 Cursor reference: .cursor/rules/$normalized_name-squad.md"
    echo ""
    print_status "Next steps:"
    echo "  1. Edit ai-squads/squads/$normalized_name.md to customize squad details"
    echo "  2. Update the reference file with specific information"
    echo "  3. Add squad members and team preferences"
    echo "  4. Test the squad with the Director Agent"
elif [ "$RESOURCE_TYPE" == "agent" ]; then
    local normalized_name=$(normalize_resource_name "$RESOURCE_NAME")
    print_success "Agent creation completed successfully!"
    echo ""
    print_status "Created files:"
    echo "  📁 Agent definition: ai-squads/agents/$normalized_name.md"
    echo "  📁 Cursor reference: .cursor/rules/$normalized_name-agent.md"
    echo ""
    print_status "Next steps:"
    echo "  1. Edit ai-squads/agents/$normalized_name.md to customize agent details"
    echo "  2. Update the reference file with specific information"
    echo "  3. Define core capabilities and workflows"
    echo "  4. Test the agent with the Director Agent"
fi

echo ""
print_status "You can now use this $RESOURCE_TYPE in your SquadAI system!"
