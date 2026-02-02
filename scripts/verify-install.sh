#!/bin/bash

# Verify ai-squads installation
# Checks that all required files are in place

set -e

TARGET_DIR="$HOME/.cursor"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "Verifying ai-squads installation"
echo "=========================================="
echo ""

errors=0
warnings=0

# Check function
check_exists() {
    local path="$1"
    local description="$2"
    local required="$3"
    
    if [ -e "$path" ]; then
        echo "✓ $description"
        return 0
    else
        if [ "$required" = "required" ]; then
            echo "✗ $description (MISSING)"
            ((errors++))
        else
            echo "⚠ $description (optional, not found)"
            ((warnings++))
        fi
        return 1
    fi
}

echo "Checking global installation at $TARGET_DIR..."
echo ""

# Check directories
echo "## Directories"
check_exists "$TARGET_DIR/commands" "commands/" required
check_exists "$TARGET_DIR/templates" "templates/" required
check_exists "$TARGET_DIR/scripts" "scripts/" required
check_exists "$TARGET_DIR/agents" "agents/" required
check_exists "$TARGET_DIR/rules" "rules/" required
check_exists "$TARGET_DIR/skills" "skills/" required
echo ""

# Check commands
echo "## Commands"
check_exists "$TARGET_DIR/commands/adopt-project.md" "adopt-project.md" required
check_exists "$TARGET_DIR/commands/project-starter.md" "project-starter.md" required
check_exists "$TARGET_DIR/commands/diagnose-issue.md" "diagnose-issue.md" required
check_exists "$TARGET_DIR/commands/explain-system.md" "explain-system.md" required
check_exists "$TARGET_DIR/commands/ideate-solution.md" "ideate-solution.md" required
check_exists "$TARGET_DIR/commands/plan-feature.md" "plan-feature.md" required
check_exists "$TARGET_DIR/commands/review-merge-request.md" "review-merge-request.md" required
check_exists "$TARGET_DIR/commands/team-lately.md" "team-lately.md" required
echo ""

# Check agent commands
echo "## Agent Commands"
check_exists "$TARGET_DIR/commands/ben.md" "ben.md (Startup Advisor)" required
check_exists "$TARGET_DIR/commands/rusty.md" "rusty.md (Rust)" required
check_exists "$TARGET_DIR/commands/alan.md" "alan.md (Smalltalk)" required
check_exists "$TARGET_DIR/commands/uidev.md" "uidev.md (JavaScript)" required
check_exists "$TARGET_DIR/commands/bob.md" "bob.md (JTBD)" required
check_exists "$TARGET_DIR/commands/steve.md" "steve.md (UI/UX)" required
check_exists "$TARGET_DIR/commands/rian.md" "rian.md (Strategic)" required
check_exists "$TARGET_DIR/commands/clovis.md" "clovis.md (Brazilian)" required
check_exists "$TARGET_DIR/commands/gustavo.md" "gustavo.md (Finance)" required
check_exists "$TARGET_DIR/commands/ops.md" "ops.md (DevOps)" required
echo ""

# Check templates
echo "## Templates"
check_exists "$TARGET_DIR/templates/project" "templates/project/" required
check_exists "$TARGET_DIR/templates/feature" "templates/feature/" required
check_exists "$TARGET_DIR/templates/commands" "templates/commands/" required
check_exists "$TARGET_DIR/templates/team-lately" "templates/team-lately/" required
check_exists "$TARGET_DIR/templates/lean-canvas.md" "lean-canvas.md" required
check_exists "$TARGET_DIR/templates/pitch-deck.md" "pitch-deck.md" required
check_exists "$TARGET_DIR/templates/ideation" "templates/ideation/" optional
check_exists "$TARGET_DIR/templates/diagnosis" "templates/diagnosis/" optional
echo ""

# Check scripts
echo "## Scripts"
check_exists "$TARGET_DIR/scripts/common.sh" "common.sh" required
check_exists "$TARGET_DIR/scripts/execute-feature.sh" "execute-feature.sh" required
check_exists "$TARGET_DIR/scripts/create-project-docs.sh" "create-project-docs.sh" required
check_exists "$TARGET_DIR/scripts/create-feature-docs.sh" "create-feature-docs.sh" required
check_exists "$TARGET_DIR/scripts/init-storybook.sh" "init-storybook.sh" required
check_exists "$TARGET_DIR/scripts/verify-install.sh" "verify-install.sh" optional
echo ""

# Check agents (required by execute-feature when run from any project)
echo "## Agents (for execute-feature)"
check_exists "$TARGET_DIR/agents/rust-specialist.md" "rust-specialist.md" required
check_exists "$TARGET_DIR/agents/smalltalk-specialist.md" "smalltalk-specialist.md" required
check_exists "$TARGET_DIR/agents/javascript-specialist.md" "javascript-specialist.md" required
check_exists "$TARGET_DIR/agents/jobs-to-be-done.md" "jobs-to-be-done.md" required
check_exists "$TARGET_DIR/agents/ui-ux.md" "ui-ux.md" required
check_exists "$TARGET_DIR/agents/ui-developer.md" "ui-developer.md" required
check_exists "$TARGET_DIR/agents/strategic-designer.md" "strategic-designer.md" required
check_exists "$TARGET_DIR/agents/copywriter.md" "copywriter.md" required
check_exists "$TARGET_DIR/agents/financial-advisor.md" "financial-advisor.md" required
check_exists "$TARGET_DIR/agents/devops-specialist.md" "devops-specialist.md" required
check_exists "$TARGET_DIR/agents/startup-advisor.md" "startup-advisor.md" required
check_exists "$TARGET_DIR/agents/video-game-specialist.md" "video-game-specialist.md" required
echo ""

# Check rules
echo "## Rules"
check_exists "$TARGET_DIR/rules/system.md" "system.md" required
echo ""

# Check source repository (for agents and standards)
echo "Checking source repository at $AI_SQUADS_DIR..."
echo ""

echo "## Agents (in source repo)"
check_exists "$AI_SQUADS_DIR/agents/rust-specialist.md" "rust-specialist.md" required
check_exists "$AI_SQUADS_DIR/agents/smalltalk-specialist.md" "smalltalk-specialist.md" required
check_exists "$AI_SQUADS_DIR/agents/javascript-specialist.md" "javascript-specialist.md" required
check_exists "$AI_SQUADS_DIR/agents/jobs-to-be-done.md" "jobs-to-be-done.md" required
check_exists "$AI_SQUADS_DIR/agents/ui-ux.md" "ui-ux.md" required
check_exists "$AI_SQUADS_DIR/agents/ui-developer.md" "ui-developer.md" required
check_exists "$AI_SQUADS_DIR/agents/strategic-designer.md" "strategic-designer.md" required
check_exists "$AI_SQUADS_DIR/agents/copywriter.md" "copywriter.md" required
check_exists "$AI_SQUADS_DIR/agents/financial-advisor.md" "financial-advisor.md" required
check_exists "$AI_SQUADS_DIR/agents/devops-specialist.md" "devops-specialist.md" required
check_exists "$AI_SQUADS_DIR/agents/startup-advisor.md" "startup-advisor.md" required
check_exists "$AI_SQUADS_DIR/agents/video-game-specialist.md" "video-game-specialist.md" required
echo ""

echo "## Standards (in source repo)"
check_exists "$AI_SQUADS_DIR/standards/code/rust-style.md" "rust-style.md" required
check_exists "$AI_SQUADS_DIR/standards/code/smalltalk-style.md" "smalltalk-style.md" required
check_exists "$AI_SQUADS_DIR/standards/code/javascript-style.md" "javascript-style.md" required
check_exists "$AI_SQUADS_DIR/standards/code/htmx-style.md" "htmx-style.md" required
echo ""

# Summary
echo "=========================================="
if [ $errors -eq 0 ] && [ $warnings -eq 0 ]; then
    echo "✓ All checks passed!"
    echo "=========================================="
    exit 0
elif [ $errors -eq 0 ]; then
    echo "⚠ Installation complete with $warnings warning(s)"
    echo "=========================================="
    exit 0
else
    echo "✗ Installation has $errors error(s) and $warnings warning(s)"
    echo ""
    echo "Run ./scripts/install.sh to fix missing files"
    echo "=========================================="
    exit 1
fi

