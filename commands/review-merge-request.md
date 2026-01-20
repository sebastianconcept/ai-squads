---
name: review-merge-request
alwaysApply: false
---

# Review Merge Request Workflow

This workflow guides the review of merge requests using the project's agent team.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. Project must have `~/docs/{project-name}/TEAM.md` with agent team

## Steps

### 1. Identify Changes
Determine what to review:
- Current git diff (uncommitted changes)
- Selected files in editor
- Branch comparison (if specified)

### 2. Load Project Context
- Read `~/docs/{project-name}/TEAM.md` to get agent team
- Read `~/docs/{project-name}/TECH-STACK.md` for technical context
- Identify relevant style guides based on file types

### 3. Analyze File Types
Categorize changed files:
- Rust files → Rust Specialist + rust-style.md
- Smalltalk files → Smalltalk Specialist + smalltalk-style.md
- JavaScript files → JavaScript Specialist + javascript-style.md
- HTML/htmx files → UI Developer + htmx-style.md
- UI/UX changes → UI/UX Agent
- Feature additions → Jobs to be Done Agent

### 4. Invoke Relevant Agents
For each category of changes:
- Load appropriate agent rules from global ai-squads installation
- Load corresponding style guide from global ai-squads installation
- Provide context: file paths, changes, project tech stack
- Generate review feedback

### 5. Generate Review Feedback

For each agent invoked, provide:
- **Code Quality**: Does it follow best practices?
- **Style Compliance**: Does it match style guide?
- **Architecture**: Does it fit project structure?
- **Potential Issues**: Bugs, performance, security
- **Suggestions**: Improvements, alternatives

### 6. Aggregate Feedback
- Combine feedback from all relevant agents
- Prioritize critical issues
- Group by category (bugs, style, architecture, etc.)
- Provide actionable recommendations

### 7. Present Review
Format review as:
- Summary of changes
- Critical issues (if any)
- Style and quality feedback
- Suggestions for improvement
- Positive feedback (what's good)

## Output

A comprehensive review covering:
- Code quality and style
- Architecture and design
- Potential bugs or issues
- Performance considerations
- Security concerns
- Suggestions for improvement

## Notes

- Only invoke agents relevant to changed files
- Reference specific style guides for code standards
- Provide constructive, actionable feedback
- Balance criticism with positive reinforcement
- Consider project's tech stack and constraints
