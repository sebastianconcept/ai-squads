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
