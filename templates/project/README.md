# Project Documentation

This directory contains planning and documentation for this project, managed by ai-squads.

**Location**: This documentation is stored in `~/docs/{project-name}/` where `{project-name}` is automatically derived from the git repository name.

## Contents

- `MISSION.md` - Project mission and goals
- `ROADMAP.md` - Project roadmap and priorities
- `TECH-STACK.md` - Technology stack details
- `DECISIONS.md` - Architectural and design decisions
- `TEAM.md` - AI agent team configuration
- `PREFERENCES.md` - Interaction language and preferences (agents respond in this language)
- `PROGRESS.md` - Project progress tracking
- `NOTES.md` - Project notes (append-only)
- `feature/` - Feature planning documents

## Usage

This documentation is maintained through ai-squads commands:
- `Plan Feature` - Create new feature plans
- `Review Merge Request` - Review code changes
- `Invoke Agent` - Get help from specialist agents

## Structure

Features are planned in `feature/{feature_name}/` directories, each containing:
- `PRD.md` - Product requirements document
- `SPECS.md` - Technical specifications
- `prd.json` - Machine-readable execution format (replaces deprecated tasks.md)

