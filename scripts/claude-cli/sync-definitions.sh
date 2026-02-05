#!/bin/bash

# Sync platform-agnostic definitions to Claude CLI (Claude Code) format
#
# Reads from: definitions/commands/*.md
# Writes to:  ~/.claude/commands/<command-name>.md
#
# Claude Code discovers personal commands from ~/.claude/commands/ (markdown;
# filename = slash command, e.g. plan-feature.md → /plan-feature).
# Idempotent: safe to run multiple times.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_SQUADS_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFINITIONS_DIR="$AI_SQUADS_DIR/definitions"
COMMANDS_DIR="$DEFINITIONS_DIR/commands"
TARGET_DIR="${HOME}/.claude/commands"

if [ ! -d "$DEFINITIONS_DIR" ]; then
    echo "  ○ No definitions/ directory yet (skipping)"
    exit 0
fi

if [ ! -d "$COMMANDS_DIR" ]; then
    echo "  ○ No definitions/commands/ directory yet (skipping)"
    exit 0
fi

mkdir -p "$TARGET_DIR"

# Sanitize description for YAML: single line, ≤500 chars, escape \ and " for double-quoted value.
sanitize_description() {
    echo "$1" | tr '\n' ' ' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//' | head -c 500 | sed 's/\\/\\\\/g; s/"/\\"/g'
}

# Write one command .md for Claude: frontmatter with description, then body.
write_command() {
    local name="$1"
    local desc="$2"
    local body="$3"
    local dest="$TARGET_DIR/${name}.md"
    local desc_yml
    desc_yml="\"$(sanitize_description "$desc")\""
    {
        echo "---"
        echo "description: $desc_yml"
        echo "---"
        echo ""
        echo "$body"
    } > "$dest"
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
    [ -n "$desc" ] || desc="ai-squads $name workflow"
    write_command "$name" "$desc" "$body"
    count=$((count + 1))
done

# Remove command files that no longer exist in definitions (idempotent: empty definitions → clear all).
for dest in "$TARGET_DIR"/*.md; do
    [ -f "$dest" ] || continue
    base=$(basename "$dest" .md)
    [ -f "$COMMANDS_DIR/${base}.md" ] || rm -f "$dest"
done
[ "$skipped" -gt 0 ] && echo "  ○ $skipped file(s) skipped (missing frontmatter name)" >&2

echo "  ✓ $count commands synced to $TARGET_DIR/"
echo "  ○ Invoke in Claude Code with /<name> (e.g. /plan-feature)"
