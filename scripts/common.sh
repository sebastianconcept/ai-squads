#!/bin/bash

# Common helper functions for ai-squads scripts
# This file is sourced by other scripts to provide shared functionality

# Get the documentation directory for the current project
# Returns: $HOME/docs/{project-name}/ where project-name is derived from git repository
# Requires: git repository (errors if not found)
# Creates: The docs directory structure if it doesn't exist (idempotent)
get_docs_dir() {
    local project_root
    local project_name
    local docs_dir
    
    # Require git repository - no fallback
    if ! project_root="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "Error: Not in a git repository. ai-squads requires a git repository." >&2
        exit 1
    fi
    
    # Derive project name from repository directory
    project_name="$(basename "$project_root")"
    
    # Validate HOME is set
    if [ -z "$HOME" ]; then
        echo "Error: HOME environment variable is not set." >&2
        exit 1
    fi
    
    # Build docs directory path using $HOME (not ~) to avoid literal tilde directory
    docs_dir="$HOME/docs/$project_name"
    
    # Create the docs directory structure if it doesn't exist (idempotent)
    mkdir -p "$docs_dir"
    
    # Return docs directory path
    echo "$docs_dir"
}

# Detect if project has frontend code
# Returns: "true" if frontend detected, "false" otherwise
# Checks: frontend/, src/, app/, public/, or package.json with frontend dependencies
has_frontend() {
    local project_root
    local docs_dir
    local tech_stack_path
    
    # Get project root
    if ! project_root="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        # If not in git repo, use current directory
        project_root="$(pwd)"
    fi
    
    # Check for common frontend directories
    if [ -d "$project_root/frontend" ] || \
       [ -d "$project_root/src" ] || \
       [ -d "$project_root/app" ] || \
       [ -d "$project_root/public" ] || \
       [ -d "$project_root/www" ] || \
       [ -d "$project_root/web" ]; then
        echo "true"
        return 0
    fi
    
    # Check root package.json for frontend dependencies
    if [ -f "$project_root/package.json" ]; then
        if grep -qE '"react"|"vue"|"svelte"|"angular"|"@angular"|"alpinejs"|"htmx"' "$project_root/package.json" 2>/dev/null; then
            echo "true"
            return 0
        fi
    fi
    
    # Check TECH-STACK.md for frontend technologies
    docs_dir="$(get_docs_dir 2>/dev/null || echo '')"
    if [ -n "$docs_dir" ] && [ -f "$docs_dir/TECH-STACK.md" ]; then
        if grep -qiE "frontend|react|vue|svelte|angular|javascript|html|css" "$docs_dir/TECH-STACK.md" 2>/dev/null; then
            echo "true"
            return 0
        fi
    fi
    
    echo "false"
}
