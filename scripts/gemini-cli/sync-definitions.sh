#!/bin/bash

# Sync platform-agnostic definitions to Gemini CLI skills
#
# Reads from: definitions/commands/*.md
# Writes to:  ~/.gemini/skills/ai-squads/<command-name>/SKILL.md
#
# Gemini discovers user skills from ~/.gemini/skills/ (Agent Skills standard:
# SKILL.md with name + description + body). Each ai-squads command is exposed
# as a skill so Gemini can activate it when relevant (or list via /skills).
# Idempotent: safe to run multiple times.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFINITIONS_DIR="$AI_SQUADS_DIR/definitions"
COMMANDS_DIR="$DEFINITIONS_DIR/commands"
SKILLS_ROOT="${HOME}/.gemini/skills"
AI_SQUADS_SKILLS_DIR="$SKILLS_ROOT/ai-squads"

if [ ! -d "$DEFINITIONS_DIR" ]; then
    echo "  ○ No definitions/ directory yet (skipping)"
    exit 0
fi

if [ ! -d "$COMMANDS_DIR" ]; then
    echo "  ○ No definitions/commands/ directory yet (skipping)"
    exit 0
fi

mkdir -p "$SKILLS_ROOT"

# Sanitize description for YAML: single line, ≤500 chars, escape \ and " for double-quoted value.
sanitize_description() {
    echo "$1" | tr '\n' ' ' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//' | head -c 500 | sed 's/\\/\\\\/g; s/"/\\"/g'
}

write_skill() {
    local name="$1"
    local desc="$2"
    local body="$3"
    local dir="$AI_SQUADS_SKILLS_DIR/$name"
    mkdir -p "$dir"
    local desc_yml
    desc_yml="\"$(sanitize_description "$desc")\""
    {
        echo "---"
        echo "name: $name"
        echo "description: $desc_yml"
        echo "---"
        echo ""
        echo "$body"
    } > "$dir/SKILL.md"
}

# Each command .md must have YAML frontmatter with "name:". Description = first # heading in body, or fallback.
count=0
skipped=0
for f in "$COMMANDS_DIR"/*.md; do
    [ -f "$f" ] || continue
    name=$(awk '/^---$/{c++} c==1 && /^name:/{gsub(/^name:[[:space:]]*/,""); sub(/[[:space:]]*$/,""); print; exit}' "$f")
    if [ -z "$name" ]; then
        echo "  ○ Skipped (no frontmatter name): $(basename "$f")" >&2
        skipped=$((skipped + 1))
        continue
    fi
    body=$(awk 'BEGIN{skip=1} /^---$/{n++; if(n==2) skip=0; next} !skip' "$f")
    desc=$(echo "$body" | awk '/^# /{gsub(/^# /,""); sub(/[[:space:]]*$/,""); print; exit}' | head -c 500)
    [ -n "$desc" ] || desc="Use when the user wants to run the $name workflow (ai-squads command)."
    write_skill "$name" "$desc" "$body"
    count=$((count + 1))
done

# Remove skill dirs that no longer exist in definitions (idempotent: empty definitions → clear all).
if [ -d "$AI_SQUADS_SKILLS_DIR" ]; then
    for dir in "$AI_SQUADS_SKILLS_DIR"/*/; do
        [ -d "$dir" ] || continue
        base=$(basename "$dir")
        [ -f "$COMMANDS_DIR/${base}.md" ] || rm -rf "$dir"
    done
fi
[ "$skipped" -gt 0 ] && echo "  ○ $skipped file(s) skipped (missing frontmatter name)" >&2

echo "  ✓ $count commands synced as Gemini skills to $AI_SQUADS_SKILLS_DIR/"
echo "  ○ Invoke in Gemini with /skills or let the model activate when relevant"
