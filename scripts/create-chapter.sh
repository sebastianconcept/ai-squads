#!/bin/bash

# Chapter Generation Script for AI Self-Notes System
# Creates chapters when triggers are met and maintains chapter organization

set -euo pipefail

# Source common functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/common.sh"

# Get current timestamp in ISO 8601 format
get_timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Get current git commit hash (if in repository)
get_commit_hash() {
    if git rev-parse --git-dir > /dev/null 2>&1; then
        git rev-parse --short HEAD 2>/dev/null || echo ""
    else
        echo ""
    fi
}

# Count notes in entity directory
count_notes_in_entity() {
    local entity_dir="$1"
    local count=0
    
    if [ ! -d "$entity_dir" ]; then
        echo 0
        return
    fi
    
    # Count all note files (context.md, evidence.md, todos.md, insights.json)
    for file in "$entity_dir"/*.md "$entity_dir"/*.json; do
        if [ -f "$file" ] && [[ "$(basename "$file")" != "index.md" ]]; then
            count=$((count + 1))
        fi
    done
    
    echo "$count"
}

# Count append operations in a note file (timestamp separators)
count_append_operations() {
    local note_file="$1"
    
    if [ ! -f "$note_file" ]; then
        echo 0
        return
    fi
    
    # Count lines matching timestamp separator pattern: "## YYYY-MM-DDTHH:MM:SSZ"
    grep -c "^## [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}Z" "$note_file" 2>/dev/null || echo 0
}

# Count lines in note file (excluding frontmatter)
count_lines_in_note() {
    local note_file="$1"
    
    if [ ! -f "$note_file" ]; then
        echo 0
        return
    fi
    
    # Skip frontmatter (lines between --- markers) and count content lines
    awk '
        /^---$/ { in_frontmatter = !in_frontmatter; next }
        !in_frontmatter && NF > 0 { count++ }
        END { print count+0 }
    ' "$note_file"
}

# Count insights in insights.json
count_insights() {
    local insights_file="$1"
    
    if [ ! -f "$insights_file" ]; then
        echo 0
        return
    fi
    
    if ! command -v jq >/dev/null 2>&1; then
        echo 0
        return
    fi
    
    jq -r '.insights | length' "$insights_file" 2>/dev/null || echo 0
}

# Get next chapter number
get_next_chapter_number() {
    local chapters_dir="$1"
    
    if [ ! -d "$chapters_dir" ]; then
        echo 1
        return
    fi
    
    # Find highest chapter number
    local max_num=0
    for file in "$chapters_dir"/[0-9][0-9]-*.md; do
        if [ -f "$file" ]; then
            local num
            num=$(basename "$file" | cut -d'-' -f1 | sed 's/^0*//')
            if [ -z "$num" ]; then
                num=0
            fi
            if [ "$num" -gt "$max_num" ]; then
                max_num=$num
            fi
        fi
    done
    
    echo $((max_num + 1))
}

# Generate chapter name from trigger type
generate_chapter_name() {
    local trigger_type="$1"
    local chapter_num="$2"
    
    case "$trigger_type" in
        "note-count")
            if [ "$chapter_num" -eq 1 ]; then
                echo "initial-phase"
            else
                echo "continued-work"
            fi
            ;;
        "append-operations")
            echo "progress-update"
            ;;
        "line-count")
            echo "detailed-analysis"
            ;;
        "insights-count")
            echo "insights-summary"
            ;;
        "feature-completion")
            echo "feature-completion"
            ;;
        "bug-fix")
            echo "bug-resolution"
            ;;
        "business-goal")
            echo "goal-achievement"
            ;;
        "significant-lesson")
            echo "lesson-discovery"
            ;;
        "hypothesis-resolution")
            echo "hypothesis-resolution"
            ;;
        *)
            echo "chapter-${chapter_num}"
            ;;
    esac
}

# Create chapter file
create_chapter_file() {
    local entity_dir="$1"
    local chapter_num="$2"
    local chapter_name="$3"
    local trigger_type="$4"
    local category="$5"
    local commit_hash="$6"
    
    local chapters_dir="$entity_dir/chapters"
    mkdir -p "$chapters_dir"
    
    local chapter_file="$chapters_dir/$(printf "%02d" "$chapter_num")-${chapter_name}.md"
    local timestamp
    timestamp=$(get_timestamp)
    
    # Create chapter file with frontmatter
    cat > "$chapter_file" <<EOF
---
created: ${timestamp}
updated: ${timestamp}
category: ${category}
chapter: ${chapter_num}
title: ${chapter_name}
trigger: ${trigger_type}
commit: ${commit_hash}
---

# Chapter ${chapter_num}: $(echo "$chapter_name" | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g')

## Summary

[This chapter will be populated by the agent when creating the chapter. It should include a 2-3 paragraph summary of key findings, timeline, and conclusions.]

## Timeline

- **${timestamp}**: Chapter created (trigger: ${trigger_type})

## Key Findings

[To be populated by agent]

## Important Evidence

[To be populated by agent]

## Hypotheses Tested

[To be populated by agent - for investigations]

## Decisions Made

[To be populated by agent]

## Lessons Learned

[To be populated by agent]

## Next Steps

[To be populated by agent]

## References

- Original notes: See parent directory for full notes
- Related chapters: See [Chapter Index](./index.md)
EOF
    
    echo "$chapter_file"
}

# Update chapter index
update_chapter_index() {
    local entity_dir="$1"
    local category="$2"
    local chapters_dir="$entity_dir/chapters"
    
    if [ ! -d "$chapters_dir" ]; then
        return 0
    fi
    
    local index_file="$chapters_dir/index.md"
    local timestamp
    timestamp=$(get_timestamp)
    
    # Collect all chapters
    local chapters=()
    for file in "$chapters_dir"/[0-9][0-9]-*.md; do
        if [ -f "$file" ] && [ "$(basename "$file")" != "index.md" ]; then
            chapters+=("$file")
        fi
    done
    
    # Sort by chapter number
    IFS=$'\n' sorted_chapters=($(printf '%s\n' "${chapters[@]}" | sort))
    
    # Create index
    cat > "$index_file" <<EOF
---
created: ${timestamp}
updated: ${timestamp}
category: ${category}
autoGenerated: true
---

# Chapter Index

## Chapters

EOF
    
    # Add each chapter to index
    local chapter_num=1
    for chapter_file in "${sorted_chapters[@]}"; do
        local chapter_basename
        chapter_basename=$(basename "$chapter_file" .md)
        local chapter_num_str
        chapter_num_str=$(echo "$chapter_basename" | cut -d'-' -f1 | sed 's/^0*//')
        local chapter_title
        chapter_title=$(echo "$chapter_basename" | cut -d'-' -f2- | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g')
        
        # Extract trigger from chapter file
        local trigger
        trigger=$(grep "^trigger:" "$chapter_file" 2>/dev/null | cut -d':' -f2 | xargs || echo "unknown")
        
        # Extract created date
        local created
        created=$(grep "^created:" "$chapter_file" 2>/dev/null | cut -d':' -f2- | xargs || echo "")
        
        cat >> "$index_file" <<EOF
${chapter_num_str}. **[${chapter_title}](./${chapter_basename}.md)**${created:+ (${created})}
   - Trigger: ${trigger}
   - Summary: [To be populated]

EOF
        chapter_num=$((chapter_num + 1))
    done
    
    cat >> "$index_file" <<EOF

## Overall Summary

[Brief summary of entire investigation/feature across all chapters - to be populated by agent]
EOF
}

# Update context.md with chapter TOC
update_context_with_chapters() {
    local entity_dir="$1"
    local context_file="$entity_dir/context.md"
    local chapters_dir="$entity_dir/chapters"
    
    if [ ! -f "$context_file" ] || [ ! -d "$chapters_dir" ]; then
        return 0
    fi
    
    # Check if chapters section already exists
    if grep -q "^## Chapters" "$context_file"; then
        # Update existing chapters section
        local temp_file
        temp_file=$(mktemp)
        
        # Copy everything before "## Chapters" section
        sed '/^## Chapters$/,$d' "$context_file" > "$temp_file"
        
        # Add updated chapters section
        cat >> "$temp_file" <<EOF

## Chapters

This ${entity_dir##*/} is organized into chapters for better navigation:

EOF
        
        # Add chapter links
        local chapter_num=1
        for file in "$chapters_dir"/[0-9][0-9]-*.md; do
            if [ -f "$file" ] && [ "$(basename "$file")" != "index.md" ]; then
                local chapter_basename
                chapter_basename=$(basename "$file" .md)
                local chapter_title
                chapter_title=$(echo "$chapter_basename" | cut -d'-' -f2- | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g')
                
                cat >> "$temp_file" <<EOF
${chapter_num}. **[${chapter_title}](./chapters/${chapter_basename}.md)** - [Brief description]

EOF
                chapter_num=$((chapter_num + 1))
            fi
        done
        
        cat >> "$temp_file" <<EOF

See [Chapter Index](./chapters/index.md) for detailed summaries.

EOF
        
        # Copy everything after chapters section (if any)
        if grep -q "^## " "$context_file" && ! grep -q "^## Chapters$" "$context_file"; then
            # There are other sections after chapters, preserve them
            sed -n '/^## Chapters$/,$p' "$context_file" | sed '1d' | sed '/^## /,$d' >> "$temp_file"
        fi
        
        mv "$temp_file" "$context_file"
    else
        # Add chapters section before any existing "## " sections (but after frontmatter)
        local temp_file
        temp_file=$(mktemp)
        
        # Copy frontmatter
        awk '/^---$/{p=1} p==1{print} /^---$/{p=2; next} p==2 && /^## /{exit} p==2' "$context_file" > "$temp_file"
        
        # Add chapters section
        cat >> "$temp_file" <<EOF

## Chapters

This ${entity_dir##*/} is organized into chapters for better navigation:

EOF
        
        # Add chapter links
        local chapter_num=1
        for file in "$chapters_dir"/[0-9][0-9]-*.md; do
            if [ -f "$file" ] && [ "$(basename "$file")" != "index.md" ]; then
                local chapter_basename
                chapter_basename=$(basename "$file" .md)
                local chapter_title
                chapter_title=$(echo "$chapter_basename" | cut -d'-' -f2- | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g')
                
                cat >> "$temp_file" <<EOF
${chapter_num}. **[${chapter_title}](./chapters/${chapter_basename}.md)** - [Brief description]

EOF
                chapter_num=$((chapter_num + 1))
            fi
        done
        
        cat >> "$temp_file" <<EOF

See [Chapter Index](./chapters/index.md) for detailed summaries.

EOF
        
        # Copy rest of file
        awk '/^---$/{p=1} p==1{next} /^---$/{p=2; next} p==2' "$context_file" >> "$temp_file"
        
        mv "$temp_file" "$context_file"
    fi
}

# Check quantitative triggers
check_quantitative_triggers() {
    local entity_dir="$1"
    local category="$2"
    
    # Check note count trigger (20 notes)
    local note_count
    note_count=$(count_notes_in_entity "$entity_dir")
    if [ "$note_count" -ge 20 ]; then
        echo "note-count:$note_count"
        return 0
    fi
    
    # Check append operations trigger (10 appends in any note)
    for note_file in "$entity_dir"/*.md; do
        if [ -f "$note_file" ] && [[ "$(basename "$note_file")" != "index.md" ]]; then
            local append_count
            append_count=$(count_append_operations "$note_file")
            if [ "$append_count" -ge 10 ]; then
                echo "append-operations:$(basename "$note_file"):$append_count"
                return 0
            fi
        fi
    done
    
    # Check line count trigger (500 lines in any note)
    for note_file in "$entity_dir"/*.md; do
        if [ -f "$note_file" ] && [[ "$(basename "$note_file")" != "index.md" ]]; then
            local line_count
            line_count=$(count_lines_in_note "$note_file")
            if [ "$line_count" -ge 500 ]; then
                echo "line-count:$(basename "$note_file"):$line_count"
                return 0
            fi
        fi
    done
    
    # Check insights count trigger (15 insights)
    local insights_file="$entity_dir/insights.json"
    if [ -f "$insights_file" ]; then
        local insights_count
        insights_count=$(count_insights "$insights_file")
        if [ "$insights_count" -ge 15 ]; then
            echo "insights-count:$insights_count"
            return 0
        fi
    fi
    
    return 1
}

# Main function: Create chapter if triggers are met
create_chapter_if_needed() {
    local entity_dir="$1"
    local category="$2"
    local trigger_type="${3:-auto}"
    
    local chapters_dir="$entity_dir/chapters"
    local commit_hash
    commit_hash=$(get_commit_hash)
    
    # Get next chapter number
    local chapter_num
    chapter_num=$(get_next_chapter_number "$chapters_dir")
    
    # Generate chapter name
    local chapter_name
    chapter_name=$(generate_chapter_name "$trigger_type" "$chapter_num")
    
    # Create chapter file
    local chapter_file
    chapter_file=$(create_chapter_file "$entity_dir" "$chapter_num" "$chapter_name" "$trigger_type" "$category" "$commit_hash")
    
    # Update chapter index
    update_chapter_index "$entity_dir" "$category"
    
    # Update context.md with chapter TOC
    update_context_with_chapters "$entity_dir"
    
    echo "$chapter_file"
}

# Main entry point
main() {
    local entity_dir="$1"
    local category="${2:-features}"
    local trigger_type="${3:-auto}"
    
    if [ "$trigger_type" = "auto" ]; then
        # Check quantitative triggers
        local trigger_result
        if trigger_result=$(check_quantitative_triggers "$entity_dir" "$category"); then
            local detected_trigger
            detected_trigger=$(echo "$trigger_result" | cut -d':' -f1)
            create_chapter_if_needed "$entity_dir" "$category" "$detected_trigger"
        fi
    else
        # Manual chapter creation
        create_chapter_if_needed "$entity_dir" "$category" "$trigger_type"
    fi
}

# Run main if script is executed directly
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    if [ $# -lt 1 ]; then
        echo "Usage: $0 <entity_dir> [category] [trigger_type]"
        echo "  entity_dir: Directory containing notes (e.g., ~/docs/project/notes/feature-name)"
        echo "  category: Category (features, investigations, projects) - default: features"
        echo "  trigger_type: auto, note-count, append-operations, line-count, insights-count, feature-completion, bug-fix, business-goal, significant-lesson, hypothesis-resolution"
        exit 1
    fi
    
    main "$@"
fi
