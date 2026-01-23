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
log "Updating PROGRESS.md with full metrics calculation..."

# Check dependencies
if ! command -v jq >/dev/null 2>&1; then
    error "jq is required for PROGRESS.md generation. Please install jq."
    exit 1
fi

# Helper function to extract category from markdown frontmatter
extract_category_from_frontmatter() {
    local file="$1"
    if [ ! -f "$file" ]; then
        return 1
    fi
    awk '
        /^---$/ { in_frontmatter = !in_frontmatter; next }
        in_frontmatter && /^category:/ {
            gsub(/^category:[[:space:]]*/, "")
            gsub(/^["'\'']|["'\'']$/, "")
            gsub(/[[:space:]]*$/, "")
            print
            exit
        }
    ' "$file" 2>/dev/null
}

# Initialize metrics
TOTAL_STORIES=0
COMPLETE_STORIES=0
TOTAL_FEATURES=0
COMPLETE_FEATURES=0
TOTAL_ITERATIONS=0
GAP_CLOSURE_STORIES=0
FEATURE_METRICS=()
STORY_TYPE_METRICS=()
AGENT_METRICS=()
RECENT_STORIES=()
ACTIVE_FEATURE=""
CURRENT_STORY=""
LAST_ACTIVITY=""

# Scan all prd.json files
FEATURES_DIR="$DOCS_DIR/feature"
if [ -d "$FEATURES_DIR" ]; then
    while IFS= read -r prd_file; do
        if [ ! -f "$prd_file" ]; then
            continue
        fi
        
        local feature_name
        feature_name=$(basename "$(dirname "$prd_file")")
        TOTAL_FEATURES=$((TOTAL_FEATURES + 1))
        
        # Check if feature is complete (all stories pass)
        local feature_complete
        feature_complete=$(jq -r 'if (.userStories | length > 0) and (.userStories | all(.passes == true)) then "true" else "false" end' "$prd_file" 2>/dev/null)
        if [ "$feature_complete" = "true" ]; then
            COMPLETE_FEATURES=$((COMPLETE_FEATURES + 1))
        fi
        
        # Process stories
        local stories_json
        stories_json=$(jq -c '.userStories[]?' "$prd_file" 2>/dev/null)
        
        while IFS= read -r story; do
            if [ -z "$story" ]; then
                continue
            fi
            
            TOTAL_STORIES=$((TOTAL_STORIES + 1))
            
            local story_id story_type story_agent story_passes story_iterations story_gap_closure
            story_id=$(echo "$story" | jq -r '.id // "unknown"')
            story_type=$(echo "$story" | jq -r '.type // "unknown"')
            story_agent=$(echo "$story" | jq -r '.agent // "unknown"')
            story_passes=$(echo "$story" | jq -r '.passes // false')
            story_iterations=$(echo "$story" | jq -r '.iterations // 0')
            story_gap_closure=$(echo "$story" | jq -r '.gapClosure // false')
            
            if [ "$story_passes" = "true" ]; then
                COMPLETE_STORIES=$((COMPLETE_STORIES + 1))
            fi
            
            TOTAL_ITERATIONS=$((TOTAL_ITERATIONS + story_iterations))
            
            if [ "$story_gap_closure" = "true" ]; then
                GAP_CLOSURE_STORIES=$((GAP_CLOSURE_STORIES + 1))
            fi
            
            # Track by feature
            FEATURE_METRICS+=("$feature_name|$story_passes|$story_iterations")
            
            # Track by story type
            STORY_TYPE_METRICS+=("$story_type|$story_passes|$story_iterations")
            
            # Track by agent
            AGENT_METRICS+=("$story_agent|$story_passes|$story_iterations")
            
            # Track recent stories (last 5)
            if [ "$story_passes" = "true" ]; then
                RECENT_STORIES+=("$story_id|$feature_name|$story_type")
                if [ ${#RECENT_STORIES[@]} -gt 5 ]; then
                    RECENT_STORIES=("${RECENT_STORIES[@]:1}")
                fi
            fi
            
            # Find active feature (first incomplete story)
            if [ "$story_passes" = "false" ] && [ -z "$ACTIVE_FEATURE" ]; then
                ACTIVE_FEATURE="$feature_name"
                CURRENT_STORY="$story_id"
                LAST_ACTIVITY="Story $story_id in progress"
            fi
        done <<< "$stories_json"
    done < <(find "$FEATURES_DIR" -name "prd.json" -type f 2>/dev/null | sort)
fi

# Calculate overall progress
OVERALL_PROGRESS=0
if [ $TOTAL_STORIES -gt 0 ]; then
    OVERALL_PROGRESS=$((COMPLETE_STORIES * 100 / TOTAL_STORIES))
fi

# Helper function for division (works without bc)
calculate_percentage() {
    local numerator=$1
    local denominator=$2
    if [ $denominator -eq 0 ]; then
        echo "N/A"
        return
    fi
    # Use awk for floating point division if bc not available
    if command -v bc >/dev/null 2>&1; then
        echo "scale=1; $numerator * 100 / $denominator" | bc
    else
        # Fallback: integer division with awk
        awk "BEGIN {printf \"%.1f\", ($numerator * 100) / $denominator}"
    fi
}

calculate_average() {
    local total=$1
    local count=$2
    if [ $count -eq 0 ]; then
        echo "N/A"
        return
    fi
    if command -v bc >/dev/null 2>&1; then
        echo "scale=1; $total / $count" | bc
    else
        awk "BEGIN {printf \"%.1f\", $total / $count}"
    fi
}

# Calculate averages
AVG_ITERATIONS="N/A"
if [ $COMPLETE_STORIES -gt 0 ]; then
    AVG_ITERATIONS=$(calculate_average $TOTAL_ITERATIONS $COMPLETE_STORIES)
fi

SUCCESS_RATE="N/A"
if [ $TOTAL_STORIES -gt 0 ]; then
    SUCCESS_RATE=$(calculate_percentage $COMPLETE_STORIES $TOTAL_STORIES)
fi

GAP_CLOSURE_RATE="0%"
if [ $TOTAL_STORIES -gt 0 ]; then
    GAP_CLOSURE_RATE=$(calculate_percentage $GAP_CLOSURE_STORIES $TOTAL_STORIES)
    GAP_CLOSURE_RATE="${GAP_CLOSURE_RATE}%"
fi

# Aggregate feature metrics
declare -A FEATURE_STATS
for metric in "${FEATURE_METRICS[@]}"; do
    IFS='|' read -r feature passes iterations <<< "$metric"
    if [ -z "${FEATURE_STATS[$feature]}" ]; then
        FEATURE_STATS[$feature]="0|0|0|0"  # stories|complete|total_iterations|avg_iterations
    fi
    IFS='|' read -r s c ti ai <<< "${FEATURE_STATS[$feature]}"
    s=$((s + 1))
    if [ "$passes" = "true" ]; then
        c=$((c + 1))
    fi
    ti=$((ti + iterations))
    FEATURE_STATS[$feature]="$s|$c|$ti|$ai"
done

# Calculate averages for features
for feature in "${!FEATURE_STATS[@]}"; do
    IFS='|' read -r s c ti ai <<< "${FEATURE_STATS[$feature]}"
    avg_iter=$(calculate_average $ti $c)
    FEATURE_STATS[$feature]="$s|$c|$ti|$avg_iter"
done

# Aggregate story type metrics
declare -A TYPE_STATS
for metric in "${STORY_TYPE_METRICS[@]}"; do
    IFS='|' read -r type passes iterations <<< "$metric"
    if [ -z "${TYPE_STATS[$type]}" ]; then
        TYPE_STATS[$type]="0|0|0"  # stories|total_iterations|avg_iterations
    fi
    IFS='|' read -r s ti ai <<< "${TYPE_STATS[$type]}"
    s=$((s + 1))
    if [ "$passes" = "true" ]; then
        ti=$((ti + iterations))
    fi
    TYPE_STATS[$type]="$s|$ti|$ai"
done

# Calculate averages for story types
for type in "${!TYPE_STATS[@]}"; do
    IFS='|' read -r s ti ai <<< "${TYPE_STATS[$type]}"
    avg_iter=$(calculate_average $ti $s)
    TYPE_STATS[$type]="$s|$ti|$avg_iter"
done

# Aggregate agent metrics
declare -A AGENT_STATS
for metric in "${AGENT_METRICS[@]}"; do
    IFS='|' read -r agent passes iterations <<< "$metric"
    if [ -z "${AGENT_STATS[$agent]}" ]; then
        AGENT_STATS[$agent]="0|0|0"  # stories|total_iterations|avg_iterations
    fi
    IFS='|' read -r s ti ai <<< "${AGENT_STATS[$agent]}"
    s=$((s + 1))
    if [ "$passes" = "true" ]; then
        ti=$((ti + iterations))
    fi
    AGENT_STATS[$agent]="$s|$ti|$ai"
done

# Calculate averages for agents
for agent in "${!AGENT_STATS[@]}"; do
    IFS='|' read -r s ti ai <<< "${AGENT_STATS[$agent]}"
    avg_iter=$(calculate_average $ti $s)
    AGENT_STATS[$agent]="$s|$ti|$avg_iter"
done

# Read recent decisions (last 5)
RECENT_DECISIONS=()
if [ -f "$DOCS_DIR/DECISIONS.md" ]; then
    # Extract last 5 decision entries (simple pattern: lines starting with ## or ###)
    RECENT_DECISIONS=($(grep -n "^##" "$DOCS_DIR/DECISIONS.md" | tail -5 | cut -d: -f1))
fi

# Read active blockers from feature notes
ACTIVE_BLOCKERS=()
NOTES_DIR="$DOCS_DIR/notes"
if [ -d "$NOTES_DIR" ]; then
    # Find all TODOS.md files in feature notes
    while IFS= read -r todos_file; do
        if [ -f "$todos_file" ]; then
            # Check if file has category: features in frontmatter
            local category
            category=$(extract_category_from_frontmatter "$todos_file")
            if [ "$category" = "features" ] || [ -z "$category" ]; then
                # Look for "Blocked" or "## Blocked" sections
                if grep -q -i "blocked\|blocker" "$todos_file"; then
                    local feature_name
                    feature_name=$(basename "$(dirname "$todos_file")")
                    ACTIVE_BLOCKERS+=("$feature_name")
                fi
            fi
        fi
    done < <(find "$NOTES_DIR" -name "TODOS.md" -type f 2>/dev/null)
fi

# Count pending todos
PENDING_TODOS=0
FEATURES_WITH_TODOS=0
if [ -d "$NOTES_DIR" ]; then
    while IFS= read -r todos_file; do
        if [ -f "$todos_file" ]; then
            local category
            category=$(extract_category_from_frontmatter "$todos_file")
            if [ "$category" = "features" ] || [ -z "$category" ]; then
                local todo_count
                todo_count=$(grep -c "^- \[ \]" "$todos_file" 2>/dev/null || echo "0")
                if [ "$todo_count" -gt 0 ]; then
                    PENDING_TODOS=$((PENDING_TODOS + todo_count))
                    FEATURES_WITH_TODOS=$((FEATURES_WITH_TODOS + 1))
                fi
            fi
        fi
    done < <(find "$NOTES_DIR" -name "TODOS.md" -type f 2>/dev/null)
fi

# Find active investigations
ACTIVE_INVESTIGATIONS=()
if [ -d "$NOTES_DIR" ]; then
    while IFS= read -r note_file; do
        if [ -f "$note_file" ]; then
            local category
            category=$(extract_category_from_frontmatter "$note_file")
            if [ "$category" = "investigations" ]; then
                local investigation_name
                investigation_name=$(basename "$note_file" .md)
                ACTIVE_INVESTIGATIONS+=("$investigation_name")
            fi
        fi
    done < <(find "$NOTES_DIR" -name "*.md" -type f 2>/dev/null | head -10)
fi

# Determine status
if [ -z "$ACTIVE_FEATURE" ]; then
    if [ $COMPLETE_FEATURES -eq $TOTAL_FEATURES ] && [ $TOTAL_FEATURES -gt 0 ]; then
        STATUS="All features complete"
    else
        STATUS="No active features"
    fi
else
    STATUS="Feature in progress: $ACTIVE_FEATURE"
fi

# Generate progress bar
PROGRESS_BAR=""
for i in {1..10}; do
    if [ $i -le $((OVERALL_PROGRESS / 10)) ]; then
        PROGRESS_BAR="${PROGRESS_BAR}█"
    else
        PROGRESS_BAR="${PROGRESS_BAR}░"
    fi
done

# Generate PROGRESS.md
{
    echo "# Project Progress"
    echo ""
    echo "**Last updated:** $CURRENT_TIME"
    echo "**Generated from:** prd.json files, feature notes, DECISIONS.md, git history"
    echo ""
    echo "## Project Reference"
    echo ""
    echo "See: \`$DOCS_DIR/README.md\`"
    echo "**Core value:** ${CORE_VALUE:-[Project description]}"
    echo "**Current focus:** ${ACTIVE_FEATURE:-No active features}"
    echo ""
    echo "## Current Position"
    echo ""
    echo "**Active Feature:** ${ACTIVE_FEATURE:-None}"
    echo "**Current Story:** ${CURRENT_STORY:-None}"
    echo "**Status:** $STATUS"
    echo "**Last activity:** $CURRENT_TIME — ${LAST_ACTIVITY:-No recent activity}"
    echo ""
    echo "**Overall Progress:** [$PROGRESS_BAR] $OVERALL_PROGRESS%"
    echo "- Features planned: $TOTAL_FEATURES"
    echo "- Features complete: $COMPLETE_FEATURES"
    echo "- Stories total: $TOTAL_STORIES"
    echo "- Stories complete: $COMPLETE_STORIES"
    echo ""
    echo "## Performance Metrics"
    echo ""
    echo "### Velocity"
    echo "- **Stories completed:** $COMPLETE_STORIES"
    echo "- **Average duration:** N/A (duration tracking not yet implemented)"
    echo "- **Total execution time:** N/A"
    echo "- **Average iterations per story:** $AVG_ITERATIONS"
    echo "- **Success rate:** ${SUCCESS_RATE}%"
    echo ""
    echo "### By Feature"
    echo "| Feature | Stories | Complete | Avg Iterations | Status |"
    echo "|---------|---------|----------|----------------|--------|"
    if [ ${#FEATURE_STATS[@]} -eq 0 ]; then
        echo "| *No features yet* | | | | |"
    else
        for feature in "${!FEATURE_STATS[@]}"; do
            IFS='|' read -r s c ti ai <<< "${FEATURE_STATS[$feature]}"
            local feature_status
            if [ $c -eq $s ] && [ $s -gt 0 ]; then
                feature_status="Complete"
            elif [ $c -gt 0 ]; then
                feature_status="In Progress"
            else
                feature_status="Not Started"
            fi
            echo "| $feature | $s | $c | $ai | $feature_status |"
        done | sort
    fi
    echo ""
    echo "### By Story Type"
    echo "| Type | Stories | Avg Iterations |"
    echo "|------|---------|----------------|"
    if [ ${#TYPE_STATS[@]} -eq 0 ]; then
        echo "| *No stories yet* | | |"
    else
        for type in "${!TYPE_STATS[@]}"; do
            IFS='|' read -r s ti ai <<< "${TYPE_STATS[$type]}"
            echo "| $type | $s | $ai |"
        done | sort
    fi
    echo ""
    echo "### By Agent"
    echo "| Agent | Stories | Avg Iterations |"
    echo "|-------|---------|----------------|"
    if [ ${#AGENT_STATS[@]} -eq 0 ]; then
        echo "| *No stories yet* | | |"
    else
        for agent in "${!AGENT_STATS[@]}"; do
            IFS='|' read -r s ti ai <<< "${AGENT_STATS[$agent]}"
            echo "| $agent | $s | $ai |"
        done | sort
    fi
    echo ""
    echo "### Recent Trend"
    echo "- **Last 5 stories:** ${#RECENT_STORIES[@]} completed"
    echo "- **Trend:** ${SUCCESS_RATE}% success rate"
    echo "- **Quality check pass rate:** N/A (tracking not yet implemented)"
    echo ""
    echo "### Gap Closure"
    echo "- **Gap closure stories:** $GAP_CLOSURE_STORIES"
    echo "- **Gap closure rate:** ${GAP_CLOSURE_RATE}%"
    echo "- **Most common gap type:** N/A (tracking not yet implemented)"
    echo ""
    echo "*Updated after each story completion*"
    echo ""
    echo "## Accumulated Context"
    echo ""
    echo "### Recent Decisions"
    if [ ${#RECENT_DECISIONS[@]} -eq 0 ]; then
        echo "*No decisions yet. See \`$DOCS_DIR/DECISIONS.md\` when available.*"
    else
        echo "See \`$DOCS_DIR/DECISIONS.md\` for recent decisions (last 5 entries)."
    fi
    echo ""
    echo "### Active Blockers"
    if [ ${#ACTIVE_BLOCKERS[@]} -eq 0 ]; then
        echo "*No active blockers*"
    else
        for blocker in "${ACTIVE_BLOCKERS[@]}"; do
            echo "- $blocker"
        done
    fi
    echo ""
    echo "### Pending Todos"
    echo "- **Total pending:** $PENDING_TODOS todos across $FEATURES_WITH_TODOS features"
    echo ""
    echo "### Active Investigations"
    if [ ${#ACTIVE_INVESTIGATIONS[@]} -eq 0 ]; then
        echo "*No active investigations*"
    else
        for investigation in "${ACTIVE_INVESTIGATIONS[@]}"; do
            echo "- $investigation"
        done
    fi
    echo ""
    echo "## Quality Metrics"
    echo ""
    echo "### Quality Check Performance"
    echo "- **Typecheck pass rate:** N/A (tracking not yet implemented)"
    echo "- **Lint pass rate:** N/A (tracking not yet implemented)"
    echo "- **Test pass rate:** N/A (tracking not yet implemented)"
    echo "- **Format pass rate:** N/A (tracking not yet implemented)"
    echo ""
    echo "### Common Quality Issues"
    echo "*No quality issues tracked yet*"
    echo ""
    echo "## Session Continuity"
    echo ""
    echo "**Last session:** $CURRENT_TIME"
    echo "**Stopped at:** ${STATUS:-Project initialization}"
    echo "**Resume context:**"
    echo "- Status: $STATUS"
    if [ -n "$ACTIVE_FEATURE" ]; then
        echo "- Active feature: $ACTIVE_FEATURE"
        echo "- Current story: $CURRENT_STORY"
    fi
    echo ""
    echo "## Next Actions"
    echo ""
    echo "**Immediate:**"
    if [ -n "$ACTIVE_FEATURE" ]; then
        echo "- [ ] Continue with story $CURRENT_STORY in feature $ACTIVE_FEATURE"
    else
        echo "- [ ] Plan next feature"
    fi
    echo ""
    echo "**Upcoming:**"
    if [ $TOTAL_FEATURES -eq 0 ]; then
        echo "- [ ] Plan first feature"
    else
        echo "- [ ] Continue feature execution"
    fi
} > "$PROGRESS_FILE"

log "PROGRESS.md updated with full metrics calculation"
