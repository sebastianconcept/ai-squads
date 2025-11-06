---
name: plan-feature
alwaysApply: false
---

# Plan Feature Workflow

This workflow guides the planning of a new feature for a project.

## Prerequisites

1. Project must have been adopted (have `ai-squads-docs/` directory)
2. Project must have `ai-squads-docs/team.md` with agent team

## Steps

### 1. Get Feature Name
- Prompt user for feature name
- Validate name (no special characters, spaces become hyphens)
- Check if feature already exists

### 2. Create Feature Structure
- Run `ai-squads/scripts/create-feature-docs.sh {feature_name}` from project root
- This creates `ai-squads-docs/feature/{feature_name}/` directory
- Copies PRD.md, specs.md, tasks.md templates

### 3. Load Project Context
- Read `ai-squads-docs/team.md` to get agent team
- Read `ai-squads-docs/tech-stack.md` for technical context
- Read `ai-squads-docs/mission.md` for project alignment

### 4. Gather Feature Information
Ask questions based on project's agent team:

**From Jobs to be Done Agent:**
- What job is this feature helping users accomplish?
- What progress are users trying to make?
- What's the current struggle or pain point?

**From UI/UX Agent:**
- Who are the users of this feature?
- What's the primary user flow?
- What are the key interactions?

**From Technical Agents (Rust/JavaScript/etc.):**
- What are the technical requirements?
- What are the performance considerations?
- What are the integration points?

### 5. Generate Planning Documents

**PRD.md (Product Requirements Document):**
- Feature overview
- User stories/jobs to be done
- Success metrics
- Out of scope items

**specs.md (Technical Specifications):**
- Technical approach
- Architecture decisions
- API design (if applicable)
- Data models (if applicable)
- Integration points

**tasks.md (Implementation Tasks):**
- Breakdown of work items
- Dependencies
- Estimated effort
- Implementation order

### 6. Customize with Agent Insights
- Use Rust Specialist for backend tasks
- Use JavaScript Specialist for frontend tasks
- Use UI Developer for implementation details
- Use UI/UX Agent for interaction design
- Use Jobs to be Done for user value

### 7. Review and Refine
- Present generated documents
- Allow user to refine
- Update documents based on feedback

## Output

After completion, the feature should have:
- `ai-squads-docs/feature/{feature_name}/PRD.md`
- `ai-squads-docs/feature/{feature_name}/specs.md`
- `ai-squads-docs/feature/{feature_name}/tasks.md`

## Notes

- Reference project's tech stack and team
- Keep documents brief and actionable
- Use project's agent team to inform planning
- Ensure alignment with project mission
