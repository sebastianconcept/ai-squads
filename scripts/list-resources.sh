#!/bin/bash

# list-resources.sh - List all available squads and agents
# Usage: ./list-resources.sh

set -e

# Colors for output
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_header() {
    echo -e "${BLUE}$1${NC}"
}

print_success() {
    echo -e "${GREEN}$1${NC}"
}

print_warning() {
    echo -e "${YELLOW}$1${NC}"
}

echo "🏗️  SquadAI Resource Inventory"
echo "=============================="
echo ""

# List Squads
print_header "📁 Squads (.squads-ai/squads/)"
if [ -d ".squads-ai/squads" ]; then
    for squad in .squads-ai/squads/*.md; do
        if [ -f "$squad" ]; then
            squad_name=$(basename "$squad" .md)
            if [ "$squad_name" != "template" ]; then
                print_success "  ✅ $squad_name"
            fi
        fi
    done
else
    print_warning "  ⚠️  No squads directory found"
fi

echo ""

# List Agents
print_header "🤖 Agents (.squads-ai/agents/)"
if [ -d ".squads-ai/agents" ]; then
    for agent in .squads-ai/agents/*.md; do
        if [ -f "$agent" ]; then
            agent_name=$(basename "$agent" .md)
            if [ "$agent_name" != "template" ]; then
                print_success "  ✅ $agent_name"
            fi
        fi
    done
else
    print_warning "  ⚠️  No agents directory found"
fi

echo ""

# List Templates
print_header "📋 Templates (.squads-ai/templates/)"
if [ -d ".squads-ai/templates" ]; then
    for template in .squads-ai/templates/*.md; do
        if [ -f "$template" ]; then
            template_name=$(basename "$template" .md)
            print_success "  📄 $template_name"
        fi
    done
    
    # List project templates subdirectory
    if [ -d ".squads-ai/templates/projects" ]; then
        print_header "  📁 Project Templates (.squads-ai/templates/projects/)"
        for template in .squads-ai/templates/projects/*.md; do
            if [ -f "$template" ]; then
                template_name=$(basename "$template" .md)
                print_success "    📄 $template_name"
            fi
        done
    fi
else
    print_warning "  ⚠️  No templates directory found"
fi

echo ""

# List Cursor Rules
print_header "📚 Cursor Rules (.cursor/rules/)"
if [ -d ".cursor/rules" ]; then
    for rule in .cursor/rules/*.md; do
        if [ -f "$rule" ]; then
            rule_name=$(basename "$rule" .md)
            print_success "  🔗 $rule_name"
        fi
    done
else
    print_warning "  ⚠️  No cursor rules directory found"
fi

echo ""
echo "🚀 Quick Actions:"
echo "  Create new squad:   ./scripts/create-resource.sh squad <name>"
echo "  Create new agent:   ./scripts/create-resource.sh agent <name>"
echo "  List resources:     ./scripts/list-resources.sh"
echo "  Link to repo:       ./scripts/link.sh <target_repo>"
