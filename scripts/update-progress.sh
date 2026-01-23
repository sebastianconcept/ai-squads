#!/bin/bash

# update-progress.sh
# Generates or updates ~/docs/{project-name}/PROGRESS.md with project-level progress digest
# Can be called by agents, users, or execution loop

set -euo pipefail

# Source common functions
. "$HOME/.cursor/scripts/common.sh"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[update-progress]${NC} $1" >&2
}

error() {
    echo -e "${RED}[update-progress] ERROR:${NC} $1" >&2
}

warn() {
    echo -e "${YELLOW}[update-progress] WARN:${NC} $1" >&2
}

DOCS_DIR="$(get_docs_dir)"
PROGRESS_FILE="$DOCS_DIR/PROGRESS.md"

# Get current timestamp
CURRENT_TIME=$(date -u +"%Y-%m-%d %H:%M UTC")

# Check if this is a business planning project
# Business planning projects have notes/business-planning/ directory or business-planning category notes
BUSINESS_PLANNING_MODE=false
if [ -d "$DOCS_DIR/notes/business-planning" ] || \
   find "$DOCS_DIR/notes" -name "*.md" -type f 2>/dev/null | grep -q "business-planning" || \
   grep -r "category.*business-planning" "$DOCS_DIR/notes" 2>/dev/null | grep -q .; then
    BUSINESS_PLANNING_MODE=true
fi

# Initialize PROGRESS.md if it doesn't exist
if [ ! -f "$PROGRESS_FILE" ]; then
    log "Creating initial PROGRESS.md..."
    
    # Try to get project info from README.md
    CORE_VALUE=""
    if [ -f "$DOCS_DIR/README.md" ]; then
        # Extract first meaningful line (skip title, get first paragraph)
        CORE_VALUE=$(grep -v "^#" "$DOCS_DIR/README.md" | grep -v "^$" | head -1 | sed 's/^[[:space:]]*//' | cut -c1-100)
    fi
    
    # Check if business planning mode should be used
    # Also check if LEAN-CANVAS.md or PITCH-DECK.md exists (indicators of business planning project)
    if [ "$BUSINESS_PLANNING_MODE" = true ] || [ -f "$DOCS_DIR/LEAN-CANVAS.md" ] || [ -f "$DOCS_DIR/PITCH-DECK.md" ]; then
        # Business planning mode
        cat > "$PROGRESS_FILE" <<EOF
# Project Progress

**Last updated:** $CURRENT_TIME
**Generated from:** Business planning conversation, notes, business documents

## Project Reference

See: \`$DOCS_DIR/README.md\`
**Core value:** ${CORE_VALUE:-[Business concept]}
**Current focus:** Business planning

## Current Position

**Active Feature:** Business Planning
**Current Story:** Completing business planning conversation
**Status:** Business planning in progress
**Last activity:** $CURRENT_TIME — Business planning started

**Business Planning Progress:**
- Lean Canvas: ${LEAN_CANVAS_STATUS:-Not started}
- Pitch Deck: ${PITCH_DECK_STATUS:-Not started}
- Business model components discussed: 0/9
- Information gaps: *To be tracked by startup advisor*

## Business Planning Context

### Lean Canvas Components
- [ ] Problem
- [ ] Solution
- [ ] Key Metrics
- [ ] Unique Value Proposition
- [ ] Unfair Advantage
- [ ] Channels
- [ ] Customer Segments
- [ ] Cost Structure
- [ ] Revenue Streams

### Information Gaps
*Startup advisor will track gaps in notes/business-planning/TODOS.md*

### Recent Insights
*Key insights will be documented in notes/business-planning/insights.json*

## Session Continuity

**Last session:** $CURRENT_TIME
**Stopped at:** Business planning started
**Resume context:**
- Status: Business planning in progress
- Next: Begin conversation with startup advisor

## Next Actions

**Immediate:**
- [ ] Begin business planning conversation with startup advisor
- [ ] Discuss business idea and problem statement

**Upcoming:**
- [ ] Complete lean canvas components
- [ ] Generate LEAN-CANVAS.md (when information complete)
- [ ] Generate PITCH-DECK.md (when information complete)
- [ ] Begin roadmap execution (when business planning complete)
EOF
    else
        # Standard feature execution mode
        cat > "$PROGRESS_FILE" <<EOF
# Project Progress

**Last updated:** $CURRENT_TIME
**Generated from:** prd.json files, feature notes, DECISIONS.md, git history

## Project Reference

See: \`$DOCS_DIR/README.md\`
**Core value:** ${CORE_VALUE:-[Project description]}
**Current focus:** No active features

## Current Position

**Active Feature:** None
**Current Story:** None
**Status:** No active features
**Last activity:** $CURRENT_TIME — Project initialized

**Overall Progress:** [░░░░░░░░░░] 0%
- Features planned: 0
- Features complete: 0
- Stories total: 0
- Stories complete: 0

## Performance Metrics

### Velocity
- **Stories completed:** 0
- **Average duration:** N/A
- **Total execution time:** 0 hours
- **Average iterations per story:** N/A
- **Success rate:** N/A

### By Feature
| Feature | Stories | Complete | Avg Duration | Avg Iterations | Status |
|---------|---------|----------|--------------|----------------|--------|
| *No features yet* | | | | | |

### By Story Type
| Type | Stories | Avg Duration | Avg Iterations |
|------|---------|--------------|----------------|
| *No stories yet* | | | | |

### By Agent
| Agent | Stories | Avg Duration | Avg Iterations |
|-------|---------|--------------|----------------|
| *No stories yet* | | | | |

### Recent Trend
- **Last 5 stories:** N/A
- **Trend:** N/A
- **Quality check pass rate:** N/A

### Gap Closure
- **Gap closure stories:** 0
- **Gap closure rate:** 0%
- **Most common gap type:** N/A

*Updated after each story completion*

## Accumulated Context

### Recent Decisions
*No decisions yet. See \`$DOCS_DIR/DECISIONS.md\` when available.*

### Active Blockers
*No active blockers*

### Pending Todos
- **Total pending:** 0 todos across 0 features

### Active Investigations
*No active investigations*

## Quality Metrics

### Quality Check Performance
- **Typecheck pass rate:** N/A
- **Lint pass rate:** N/A
- **Test pass rate:** N/A
- **Format pass rate:** N/A

### Common Quality Issues
*No quality issues tracked yet*

## Session Continuity

**Last session:** $CURRENT_TIME
**Stopped at:** Project initialization
**Resume context:**
- Status: No active features

## Next Actions

**Immediate:**
- [ ] Plan first feature

**Upcoming:**
- [ ] *No upcoming actions*
EOF
    fi
    log "Created initial PROGRESS.md"
    exit 0
fi

# Update existing PROGRESS.md
log "Updating PROGRESS.md..."

# This is a placeholder - full implementation would:
# 1. Scan $DOCS_DIR/feature/*/prd.json files
# 2. Read feature notes from $DOCS_DIR/notes/features/
# 3. Read investigation notes from $DOCS_DIR/notes/ (filter by category: "investigations" in frontmatter)
# 4. Read recent decisions from $DOCS_DIR/DECISIONS.md
# 5. Query git history for timestamps and commits
# 6. Calculate metrics
# 7. Generate updated PROGRESS.md

# For now, just update the timestamp
if command -v sed >/dev/null 2>&1; then
    sed -i.bak "s/\*\*Last updated:\*\*.*/\*\*Last updated:\*\* $CURRENT_TIME/" "$PROGRESS_FILE"
    rm -f "${PROGRESS_FILE}.bak"
    log "Updated timestamp in PROGRESS.md"
else
    warn "sed not available, cannot update timestamp automatically"
fi

log "PROGRESS.md update complete (basic update - full generation coming in future)"
log "Note: Full PROGRESS.md generation requires parsing prd.json files, notes, and git history"
log "This will be implemented as the execution loop is developed"
