---
name: plan-feature
alwaysApply: false
---

# Plan Feature Workflow

This workflow guides the planning of a new feature for a project.

Important: Do NOT start implementing. Just create the planning files.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. Project must have `~/docs/{project-name}/TEAM.md` with agent team

## Steps

### 1. Get Feature Name
- Prompt user for feature name
- Validate name (no special characters, spaces become hyphens)
- Check if feature already exists
- Ask 3-5 essential clarifying questions (with lettered options) if necessary

### Format Questions Like This:

```
1. What problem does this feature solve?
   A. Users are confused about how to use the system
   B. Current workflow is too slow or inefficient
   C. Missing functionality that users are requesting
   D. Other: [please specify]

2. What is the expected user interaction?
   A. One-time setup or configuration
   B. Regular daily/weekly use
   C. Occasional use when needed
   D. Automated background process

3. What is the implementation priority?
   A. MVP - core functionality only
   B. Complete feature with all edge cases
   C. Prototype to validate concept
   D. Enhancement to existing feature
```

This lets users respond with "1A, 2C, 3B" for quick iteration.



### 2. Create Feature Structure
- Run `~/.cursor/scripts/create-feature-docs.sh {feature_name}` from project root
- This creates `~/docs/{project-name}/feature/{feature_name}/` directory
- Copies PRD.md and SPECS.md templates
- Note: `tasks.md` is deprecated - `prd.json` replaces it (generated in step 6)

### 3. Load Project Context
- Read `~/docs/{project-name}/TEAM.md` to get agent team
- Read `~/docs/{project-name}/TECH-STACK.md` for technical context
- Read `~/docs/{project-name}/MISSION.md` for project alignment
- Read `~/docs/{project-name}/PRODUCT-DISCOVERY.md` (if exists) for product strategy context:
  - Use product vision to align feature
  - Use feature prioritization framework to evaluate feature
  - Use product roadmap to understand phase
  - Use jobs to be done to understand user value

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

### 5. Determine if Research Phase Needed

**Analyze feature complexity/uncertainty/risk:**

Check for:
- **Complexity indicators:**
  - New technology/language/framework not in current stack
  - External service/API integration
  - Complex algorithms or data structures
  - Architectural changes (new services, databases, etc.)
- **Uncertainty indicators:**
  - Knowledge gaps identified
  - Ambiguous or incomplete requirements
  - Multiple implementation approaches exist
  - Performance or scalability concerns
- **Risk indicators:**
  - Touches critical system components
  - Security implications
  - Migration or data transformation
  - High failure impact

**User flags:**
- `--research`: Force research phase (even if trivial)
- `--skip-research`: Skip research phase (even if non-trivial)

**Decision:**
- If non-trivial OR `--research` flag → Proceed to Research Phase (Step 6)
- If trivial AND no `--research` flag → Skip to Planning Documents (Step 7)

### 6. Research Phase (if triggered)

**Process:**
1. Determine research scope based on:
   - Feature description
   - Assigned specialist agent
   - Project tech stack (`~/docs/{project-name}/TECH-STACK.md`)
   - Project context (`~/docs/{project-name}/MISSION.md`, `~/docs/{project-name}/QUALITY.md`)

2. Specialist agent performs methodical research:
   - Investigate technical blockers
   - Research available techniques/libraries
   - Research best practices
   - Research integration challenges
   - Research performance considerations

3. Generate `RESEARCH.md`:
   - Research findings per area
   - Comparative analysis
   - Recommendations with rationale
   - Sources and evidence

**Output:** `~/docs/{project-name}/feature/{feature_name}/RESEARCH.md`

**Integration:** Research findings will inform planning documents (PRD, specs, prd.json)

**See:** `~/docs/{project-name}/feature/autonomous-execution/RESEARCH-PHASE.md` for detailed specification

### 7. Generate Planning Documents

**PRD.md (Product Requirements Document):**
- Feature overview
- User stories/jobs to be done (see format below)
- Success metrics
- Out of scope items
- **If RESEARCH.md exists:** Incorporate research findings into user stories and technical constraints

**User Stories Format:**

Each story needs:
- **ID:** `US-001`, `US-002`, etc.
- **Title:** Short descriptive name
- **Description:** "As a [user], I want [feature] so that [benefit]"
- **Acceptance Criteria:** Verifiable checklist of what "done" means

Each story should be small enough to implement in one focused session.

**Format:**
```markdown
### US-001: [Title]
**Description:** As a [user], I want [feature] so that [benefit].

**Acceptance Criteria:**
- [ ] Specific verifiable criterion
- [ ] Another criterion
- [ ] Typecheck/lint passes (quality checks from ~/docs/{project-name}/QUALITY.md)
- [ ] **[UI stories only]** Verify in browser using browser-verification skill
```

**Important:** 
- Acceptance criteria must be verifiable, not vague. "Works correctly" is bad. "Button shows confirmation dialog before deleting" is good.
- **For any story with UI changes:** Always include "Verify in browser using browser-verification skill" as acceptance criteria. This ensures visual verification of frontend work.
- Stories will be converted to `prd.json` format with type detection (frontend/backend/etc.) and agent assignment.

**Functional Requirements:**

Numbered list of specific functionalities:
- "FR-1: The system must allow users to..."
- "FR-2: When a user clicks X, the system must..."

Be explicit and unambiguous. Functional requirements complement user stories by providing detailed system behavior specifications.

**SPECS.md (Technical Specifications):**
- Technical approach
- Architecture decisions
- API design (if applicable)
- Data models (if applicable)
- Integration points
- **If RESEARCH.md exists:** Incorporate research recommendations into technical approach, document integration challenges and performance considerations

**prd.json (Machine-readable execution format - replaces tasks.md):**
- User stories with actionable properties (id, title, description, type, dependencies, agent, acceptanceCriteria, priority)
- Each story has `passes: false` initially (tracks completion status)
- Quality check commands from `~/docs/{project-name}/QUALITY.md` (project-level, copied to `prd.json.quality`)
- Story types: `backend`, `frontend`, `integration`, `config`, `infrastructure`, `fullstack`, `library`
- Agent assignments based on story type and tech stack
- Dependencies between stories for proper execution order
- **If RESEARCH.md exists:** Research blockers inform dependencies, research techniques inform story notes, research recommendations inform agent guidance
- **Note**: `tasks.md` is deprecated. `prd.json` replaces it entirely.


### 6. Customize with Agent Insights
- Use Rust Specialist for backend tasks
- Use JavaScript Specialist for frontend tasks
- Use UI Developer for implementation details
- Use UI/UX Agent for interaction design
- Use Jobs to be Done for user value
- Use Strategic Designer for growth and conversion strategy
- Use Clovis for customer-facing copy and Brazilian market messaging

### 7. Review and Refine
- Present generated documents
- Allow user to refine
- Update documents based on feedback

## Output

After completion, the feature should have:
- `~/docs/{project-name}/feature/{feature_name}/PRD.md`
- `~/docs/{project-name}/feature/{feature_name}/SPECS.md`
- `~/docs/{project-name}/feature/{feature_name}/prd.json` (required for autonomous execution)
- `~/docs/{project-name}/feature/{feature_name}/RESEARCH.md` (if research phase was triggered)

**Note**: `tasks.md` is deprecated. `prd.json` replaces it entirely because it's machine-readable and tracks execution status via `userStories[].passes`.

## Notes

- Reference project's tech stack and team
- Keep documents brief and actionable
- Use project's agent team to inform planning
- Ensure alignment with project mission


## Checklist
Before saving the PRD:

[ ] Asked clarifying questions with lettered options
[ ] Incorporated user's answers
[ ] User stories are small and specific
[ ] Functional requirements are numbered and unambiguous
[ ] Non-goals section defines clear boundaries
[ ] Saved to ~/docs/{project-name}/feature/{feature-name}/PRD.md
[ ] Saved to ~/docs/{project-name}/feature/{feature-name}/prd.json
