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
- **Cursor**: Run `~/.cursor/scripts/create-feature-docs.sh {feature_name}` from project root
- **Claude/Gemini**: Run `<ai-squads>/scripts/create-feature-docs.sh {feature_name}` from project root (where `<ai-squads>` is your local clone path)
- This creates `~/docs/{project-name}/feature/{feature_name}/` directory
- Copies PRD.md, SPECS.md, OPEN-QUESTIONS.md, and VERIFY.md templates (VERIFY.md is populated in step 9 when the plan is ready to implement)
- Note: `tasks.md` is deprecated - `prd.json` replaces it (generated in step 7)
- OPEN-QUESTIONS.md will be populated in step 8 after planning documents are complete

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

### 7.5. Initialize Storybook (if feature has frontend/fullstack stories and Storybook not yet initialized)

**Timing**: Before UX workflow, check if Storybook is needed and initialized.

**Detection**: 
1. Check if feature has `frontend` or `fullstack` stories in `prd.json`
2. Check if `storybook/` directory exists in project root
3. If frontend/fullstack stories exist AND Storybook not initialized:
   - Ask user: "This feature has frontend components. Would you like to initialize Storybook for component documentation? (Stories will be auto-generated from UX specifications)"
   - If yes: 
     - **Cursor**: Run `~/.cursor/scripts/storybook-init.sh` from project root
     - **Claude/Gemini**: Run `<ai-squads>/scripts/storybook-init.sh` from project root
     - This creates `storybook/` directory and installs dependencies
     - Storybook will be used in Step 5 (Storybook Story Generation)
   - If no: Continue without Storybook (can be initialized later, stories won't be generated)

**Note**: Storybook initialization is optional but recommended for frontend features. It can be done later if skipped. If Storybook is not initialized, the story generation step will be skipped automatically.

### 7.6. UX Workflow (if feature has frontend/fullstack stories)

**Timing**: This step occurs after Step 7 (Generate Planning Documents) completes and the initial `prd.json` file has been created and saved.

**Detection**: Check the saved `prd.json` file for any stories with `type: "frontend"` or `type: "fullstack"`. If found, trigger UX workflow.

**Decision: Full UX Workflow vs. Quick UX Mode**

Ask user or determine automatically based on:
- **Full UX Workflow**: Complex features (> 3 screens, > 5 components), new user-facing features, high business impact, epic features
- **Quick UX Mode**: Simple features (single component, simple interaction), internal tools, low-risk changes, iterative improvements

**If Full UX Workflow:**

1. **Multi-Agent UX Clarification** (Steve coordinates):
   - **Steve (Senior UX Architect)**: Coordinates session, asks UX & technical questions
   - **Bob (JTBD)**: Asks about struggling moments, job statements, forces of progress, competing solutions
   - **Rian (Strategic Designer)**: Asks about growth metrics, decision points, conversion funnel, ethical design
   - **Ben (Startup Advisor)**: Asks about business hypothesis, MVP definition, growth impact
   - **Design System Question**: "Do you want to use an existing design system?" (Material Design, Tailwind UI, shadcn/ui, Chakra UI, Ant Design, or custom)
   - **Technical Constraints Questions**:
     - Browser support requirements (which browsers? which versions?)
     - Performance targets (Lighthouse scores, Core Web Vitals)
     - Accessibility requirements (WCAG level: A, AA, AAA?)
     - Technical stack constraints (what frameworks/libraries are available?)
   - **Epic Detection**: If feature is tagged as `epic: true` in prd.json, suggest persona research
   - **Output**: Refined PRD with clarified requirements, design system preference, technical constraints

2. **6-Pass UX Methodology** (Steve leads, with Bob, Rian, Ben input):
   - **Pass 1: Mental Model** (Bob & Steve): Struggling moments, job statements, first-time vs. returning users, context of use, desired outcomes
   - **Pass 2: Information Architecture** (Steve & Rian): Concept organization, visibility decisions
   - **Pass 3: Affordances** (Steve, with Rian & Bob input): Interaction signals, micro-animations, copy/microcopy, mobile-first considerations, ethical design checklist
   - **Pass 4: Cognitive Load** (Rian & Steve): Friction points, decision architecture, ethical bias application
   - **Pass 5: State Design** (Steve, with Rian input): Empty, loading, success, error states with clear guidance
   - **Pass 6: Flow Integrity** (Rian & Steve): Usability heuristics checklist, end-to-end journey, animation flow
   - **Conflict Resolution**: Steve → Rian+Steve → Rian (final authority)
   - **Output**: `ux-specs.json` (structured JSON) and `UX-SPECS.md` (human-readable markdown)

3. **Layout Design Phase** (Steve, with Rian input):
   - Visual hierarchy (primary, secondary, tertiary focal points)
   - Grid systems and breakpoints
   - Typography hierarchy with specific sizes/weights
   - Spacing rhythm and component composition
   - Color usage guidelines
   - Responsive layout behavior
   - **Output**: Enhanced `ux-specs.json` with `layout` section

4. **Validation Phase**:
   - Pre-translation validation: Check UX-SPECS completeness, all passes complete, struggling moment defined, business hypothesis validated, technical constraints documented, usability heuristics checked, ethical design verified
   - **See**: `templates/feature/UX-SPECS-VALIDATION.md` for complete validation checklist
   - If validation fails, list gaps and ask user to address

5. **Storybook Story Generation** (After UX-SPECS validation, before translation - only if Storybook is initialized):
   
   **Prerequisites**: Storybook must be initialized in project (`storybook/` directory exists)
   
   **If Storybook not initialized**: Skip this step, continue to translation (Storybook is optional)
   
   **If Storybook initialized**:
   - Determine component package context (frontend, mobile, etc.) from feature location
   - **Detect framework** for the package (React, Vue, Svelte, HTML, etc.) using framework detection
   - **Check if Storybook applies**: Skip Storybook generation if native/game engine detected
   - Extract component specifications from ux-specs.json
   - Generate Storybook story files in appropriate location with framework-appropriate format:
     - Shared components: `storybook/stories/components/`
     - Package-specific: `storybook/stories/packages/{package-name}/components/`
     - File extension matches framework (.jsx for React, .vue for Vue, .svelte for Svelte, .js for HTML)
   - Create stories for all component states (from Pass 5: State Design) in framework-appropriate format
   - Include design system token references (from layout specs)
   - Add accessibility documentation (from technical constraints)
   - Generate correct import paths based on package location and framework (relative paths, adapts to project structure)
   - **For native/game engine**: Document alternative approach (skip Storybook, use platform-specific tools)
   - **Error handling**: If story generation fails, log error and continue workflow (don't block feature planning)

6. **UX-SPECS to prd.json Story Translation**:
   - Parse `ux-specs.json` (validate against schema)
   - **See**: `templates/feature/ux-specs-to-prd-translation.md` for detailed translation guide
   - Extract atomic units from passes and layout sections
   - Map dependencies (Foundation → Layout → Components → Interactions → States → Polish)
   - Generate stories with:
     - Job statements in descriptions
     - Layout context (grid system, spacing, typography, visual hierarchy)
     - Accessibility requirements in acceptance criteria
     - Performance requirements in acceptance criteria
     - Browser verification requirements in acceptance criteria
     - Code standards references
     - **Storybook path** in story metadata (link to generated Storybook story)
   - Merge UX-derived stories into existing `prd.json` (append, don't replace)
   - **Post-translation validation**: Ensure all UX-SPECS sections represented, all acceptance criteria verifiable

**If Quick UX Mode:**

1. **Basic Clarification** (Steve coordinates, 5-10 minutes):
   - Essential questions from Bob, Rian, Ben
   - Design system question
   - Basic technical constraints

2. **3-Pass Methodology**:
   - **Pass 1: Mental Model** (Bob & Steve): Struggling moment, job statement, desired outcome
   - **Pass 3: Affordances** (Steve): Interaction signals, basic accessibility
   - **Pass 6: Flow Integrity** (Rian & Steve): Usability heuristics, end-to-end check

3. **Basic Story Generation**: Translate to prd.json stories (Foundation → Components → States)

**Output Files:**
- `ux-specs.json`: Structured JSON format (validated against schema)
- `UX-SPECS.md`: Human-readable markdown version
- Updated `prd.json`: Merged with UX-derived stories

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
- Use Clovis for customer-facing copy and market messaging

### 7. Review and Refine
- Present generated documents
- Allow user to refine
- Update documents based on feedback

### 8. Generate Open Questions Document

After planning documents are complete, identify and document any open questions that need to be resolved before or during implementation.

**Process:**
1. Review PRD.md, SPECS.md, and prd.json for ambiguities, decisions needed, or knowledge gaps
2. Identify questions that could block implementation or cause rework
3. Organize questions by category (e.g., Schema and DB, Architecture, Events, Dependencies)
4. For each question, document:
   - Question ID (OQ-X.Y format)
   - Clear question text
   - Impact on implementation
   - Suggested owner (agent or role)
   - Which user stories are blocked by this question

**Format:**
- Use the OPEN-QUESTIONS.md template
- Questions start as unresolved (no "Resolved:" prefix)
- When a question is resolved through discussion, mark it as "Resolved:" and add a decision section below the table
- Include a summary table at the end showing resolved vs. unresolved questions

**Common question categories:**
- Schema and database design (keys, nullability, migrations)
- Architecture decisions (crate layout, service boundaries, patterns)
- Integration points (APIs, events, external services)
- Configuration and deployment (env vars, secrets, infrastructure)
- Testing strategy (unit vs integration, test harnesses)
- Dependencies and versions (library choices, compatibility)
- Cross-cutting concerns (error handling, logging, observability)

**Output:** `~/docs/{project-name}/feature/{feature_name}/OPEN-QUESTIONS.md`

**Note:** This document is a living artifact. As questions are resolved through implementation or discussion, update the document to mark them resolved and document the decision. This helps maintain clarity for implementers and prevents rework.

### 9. Produce verification (evidence gathering)

**When:** After the plan is ready to implement — i.e. when open questions are resolved (or the user accepts remaining ones) and the feature docs (PRD, SPECS, prd.json, OPEN-QUESTIONS) are complete. Verification is always a form of **evidence gathering**: logs, observable steps, and environments so we can confirm the feature works (or diagnose when it doesn’t).

**Process:**

1. **Ensure VERIFY.md exists** for the feature. It is copied from the template when the feature structure is created (`create-feature-docs.sh`). If the feature was created before the template existed:
   - **Cursor**: Copy from `~/.cursor/templates/feature/VERIFY.md`
   - **Claude/Gemini**: Copy from `<ai-squads>/templates/feature/VERIFY.md` (where `<ai-squads>` is your local clone path)
   - **In-repo**: Or use `templates/feature/VERIFY.md` if available
   - Into: `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` (or `docs/feature/{feature_name}/VERIFY.md`)
2. **Populate VERIFY.md** so it’s clear how to produce verification (evidence) for this feature:
   - **Happy path:** Derive ordered, observable steps from the acceptance criteria in prd.json (and PRD.md). Each step should be something a human can do and observe (e.g. “Open /login and submit valid credentials; expect redirect to /dashboard”).
   - **Logs and evidence:** From SPECS.md and TECH-STACK.md, document where backend and frontend logs are (local: e.g. `docker compose logs -f api`, process stdout; remote: use TECH-STACK **Environments** for hostname/SSH and the log command). Document what to look for (status codes, request_id, error variants). For Rust backends, note that logs are usually stdout; for remote, note `ssh … 'docker compose logs -f <service>'`.
   - **Environment:** Point to project TECH-STACK **Environments** for local/staging/production hostnames and SSH; or repeat the relevant URLs and log commands in VERIFY.md for this feature.
3. **Frame verification as evidence gathering:** In the summary for the user, state that VERIFY.md defines how we’ll gather evidence to confirm the feature works (happy path + logs, local or remote). Commands like `verify-feature` and `diagnose-issue` use this to suggest exact log-fetch commands and interpret what the user pastes.

**Output:** `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` (or in-repo `docs/feature/{feature_name}/VERIFY.md`) — populated with happy path, log locations, and environment references so verification is clear before implementation.

**Optional project doc:** If the project doesn’t yet have `~/docs/{project-name}/EVIDENCE-GATHERING.md`, suggest copying from `~/.cursor/templates/project/EVIDENCE-GATHERING.md` so the project has a single place that explains how evidence gathering works (TECH-STACK Environments + feature VERIFY.md). See `rules/system.md` → Evidence and Environments.

## Output

After completion, the feature should have:
- `~/docs/{project-name}/feature/{feature_name}/PRD.md`
- `~/docs/{project-name}/feature/{feature_name}/SPECS.md`
- `~/docs/{project-name}/feature/{feature_name}/prd.json` (required for autonomous execution)
- `~/docs/{project-name}/feature/{feature_name}/OPEN-QUESTIONS.md` (questions to resolve before/during implementation)
- `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` (how to verify the feature — happy path, log locations, environments; verification = evidence gathering)
- `~/docs/{project-name}/feature/{feature_name}/RESEARCH.md` (if research phase was triggered)
- `~/docs/{project-name}/feature/{feature_name}/ux-specs.json` (if feature has frontend/fullstack stories)
- `~/docs/{project-name}/feature/{feature_name}/UX-SPECS.md` (if feature has frontend/fullstack stories)

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
[ ] Generated OPEN-QUESTIONS.md with unresolved questions organized by category
[ ] Documented impact and suggested owner for each question
[ ] Included summary table of resolved vs. unresolved questions
[ ] When plan is ready to implement (open questions resolved or accepted): produced VERIFY.md with happy path (from acceptance criteria), log locations (from SPECS/TECH-STACK), and environment references (from TECH-STACK Environments)
[ ] Verification framed as evidence gathering (logs, observable steps, environments) so implementers and verify-feature/diagnose-issue know how to gather first-hand evidence

**If feature has frontend/fullstack stories:**

[ ] Detected frontend/fullstack stories in prd.json
[ ] Determined workflow mode (Full UX vs. Quick UX)
[ ] Completed multi-agent UX clarification (if Full UX)
[ ] Completed 6-pass methodology (or 3-pass for Quick UX)
[ ] Completed Layout Design Phase (if Full UX)
[ ] Validated UX-SPECS completeness
[ ] Generated Storybook stories from ux-specs.json (framework-aware, skipped if native/game engine)
[ ] Translated UX-SPECS to prd.json stories
[ ] Merged UX-derived stories into prd.json (with storybookPath metadata)
[ ] Saved ux-specs.json (validated against schema)
[ ] Saved UX-SPECS.md (human-readable version)
[ ] Saved VERIFY.md (happy path, log locations, environments — verification = evidence gathering)
