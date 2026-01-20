#!/bin/bash

# update-progress.sh
# Generates or updates docs/progress.md with project-level progress digest
# Can be called by agents, users, or execution loop

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

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

# Check if we're in a project root (has docs/ directory)
if [ ! -d "docs" ]; then
    error "Not in a project root (docs/ directory not found)"
    exit 1
fi

PROGRESS_FILE="docs/progress.md"
PROJECT_ROOT="$(pwd)"

# Get current timestamp
CURRENT_TIME=$(date -u +"%Y-%m-%d %H:%M UTC")

# Initialize progress.md if it doesn't exist
if [ ! -f "$PROGRESS_FILE" ]; then
    log "Creating initial progress.md..."
    
    # Try to get project info from README.md
    CORE_VALUE=""
    if [ -f "docs/README.md" ]; then
        # Extract first meaningful line (skip title, get first paragraph)
        CORE_VALUE=$(grep -v "^#" docs/README.md | grep -v "^$" | head -1 | sed 's/^[[:space:]]*//' | cut -c1-100)
    fi
    
    cat > "$PROGRESS_FILE" <<EOF
# Project Progress

**Last updated:** $CURRENT_TIME
**Generated from:** prd.json files, feature notes, DECISIONS.md, git history

## Project Reference

See: \`docs/README.md\`
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
*No decisions yet. See \`docs/DECISIONS.md\` when available.*

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
    log "Created initial progress.md"
    exit 0
fi

# Update existing progress.md
log "Updating progress.md..."

# This is a placeholder - full implementation would:
# 1. Scan docs/feature/*/prd.json files
# 2. Read feature notes from docs/notes/features/
# 3. Read investigation notes from docs/notes/ (filter by category: "investigations" in frontmatter)
# 4. Read recent decisions from docs/DECISIONS.md
# 5. Query git history for timestamps and commits
# 6. Calculate metrics
# 7. Generate updated progress.md

# For now, just update the timestamp
if command -v sed >/dev/null 2>&1; then
    sed -i.bak "s/\*\*Last updated:\*\*.*/\*\*Last updated:\*\* $CURRENT_TIME/" "$PROGRESS_FILE"
    rm -f "${PROGRESS_FILE}.bak"
    log "Updated timestamp in progress.md"
else
    warn "sed not available, cannot update timestamp automatically"
fi

log "Progress.md update complete (basic update - full generation coming in future)"
log "Note: Full progress.md generation requires parsing prd.json files, notes, and git history"
log "This will be implemented as the execution loop is developed"
