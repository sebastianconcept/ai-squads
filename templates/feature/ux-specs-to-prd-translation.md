# UX-SPECS to prd.json Story Translation Guide

This guide explains how to translate `ux-specs.json` (structured JSON format) into granular user stories in `prd.json` format.

## Schema Location

The `ux-specs-schema.json` file is located at:
- **Primary location**: `~/docs/ai-squads/feature/ux-workflow-improvement/ux-specs-schema.json`
- **Feature directory**: Copied to feature directory by `create-feature-docs.sh` (if available)

When validating, check feature directory first, then primary location.

## Translation Process

### 1. Pre-Translation Validation

Before translating, validate `ux-specs.json`:
- [ ] File exists and is valid JSON
- [ ] Validates against `ux-specs-schema.json`
- [ ] All required sections present:
  - [ ] `passes.mentalModel` (with strugglingMoment, jobStatement, desiredOutcome)
  - [ ] `passes.informationArchitecture`
  - [ ] `passes.affordances`
  - [ ] `passes.cognitiveLoad`
  - [ ] `passes.stateDesign`
  - [ ] `passes.flowIntegrity`
  - [ ] `layout` section (if Full UX workflow)
- [ ] Struggling moment defined
- [ ] Job statement defined
- [ ] Business hypothesis validated (if present)
- [ ] Technical constraints documented
- [ ] Usability heuristics checked (Pass 6)

### 2. Extract Atomic Units

Break down UX-SPECS into discrete, implementable features:

**From Foundation (Design System):**
- Design system setup (if specified: Material, Tailwind UI, shadcn/ui, etc.)
- Design tokens (colors, typography, spacing, shadows)
- Base styles, shared utilities
- Animation utilities (transition presets, easing functions)
- Grid system setup (from layout specs)
- Spacing scale utilities (from layout specs)
- Typography scale (from layout specs)

**From Layout Shell:**
- Page structure with grid system and container widths
- Navigation with responsive behavior
- Main containers with layout patterns
- Visual hierarchy implementation
- Typography hierarchy setup
- Spacing rhythm implementation

**From Components (from Pass 2: Information Architecture, Pass 3: Affordances):**
- Reusable building blocks (buttons, cards, inputs) with basic interaction states
- Component composition specifications
- Spacing between components
- Layout templates for common screens
- Responsive behavior per component

**From Interactions (from Pass 3: Affordances, Pass 4: Cognitive Load):**
- Dynamic behaviors (drag-drop, form validation, transitions, micro-animations)
- Decision architecture implementation
- Choice architecture implementation

**From States (from Pass 5: State Design):**
- Empty states for each component
- Loading states for each component
- Error states for each component
- Success states for each component
- State transition animations

**From Polish (from Pass 6: Flow Integrity):**
- Advanced animations
- Responsive refinements
- Edge cases
- Accessibility (reduced motion support)
- Usability heuristic implementations

### 3. Map Dependencies

Identify what must be built first (build-order sequence):

1. **Foundation Stories** (no dependencies):
   - Design system setup
   - Design tokens
   - Base styles
   - Animation utilities
   - Grid system
   - Spacing scale
   - Typography scale

2. **Layout Shell Stories** (depends on Foundation):
   - Page structure
   - Navigation
   - Main containers
   - Visual hierarchy
   - Typography hierarchy
   - Spacing rhythm

3. **Core Component Stories** (depends on Foundation, Layout Shell):
   - Reusable building blocks
   - Component composition
   - Layout templates

4. **Interaction Stories** (depends on Components):
   - Dynamic behaviors
   - Decision architecture
   - Choice architecture

5. **State Stories** (depends on Components):
   - Empty, loading, error, success states
   - State transition animations

6. **Polish Stories** (depends on all above):
   - Advanced animations
   - Responsive refinements
   - Edge cases
   - Accessibility enhancements

### 4. Generate Stories

For each atomic unit, create a user story with:

**Story Structure:**
```json
{
  "id": "US-XXX",
  "title": "Descriptive name (e.g., 'Implement Primary Button Component')",
  "description": "As a [user], I want [feature] so that [benefit]. When [situation], I want to [action], so I can [outcome]",
  "type": "frontend",
  "priority": "high|medium|low",
  "agent": "ui-developer",
  "dependencies": ["US-YYY", "US-ZZZ"],
  "acceptanceCriteria": [
    "Specific verifiable criterion",
    "Another criterion"
  ],
  "passes": false,
  "iterations": 0,
  "notes": "",
  "epic": false
}
```

**Description Format:**
- Include job statement from `passes.mentalModel.jobStatement`
- Reference the job the story is helping users accomplish

**Acceptance Criteria Must Include:**

1. **Functional Requirements**:
   - Specific behavior (e.g., "Button shows hover state with 200ms ease-out transition")
   - Interaction details (e.g., "Form validation shows inline error messages")

2. **Layout Requirements** (from layout specs):
   - Grid system reference (e.g., "Uses 12-column grid system")
   - Spacing scale reference (e.g., "Uses spacing-4 (16px) between elements")
   - Typography hierarchy reference (e.g., "Uses H2 (24px/32px, font-weight 600)")
   - Visual hierarchy reference (e.g., "Primary button is visually prominent")
   - Component composition reference (e.g., "Button group uses 8px spacing between buttons")
   - Responsive requirements (e.g., "Stacks vertically on mobile (< 768px)")

3. **Accessibility Requirements**:
   - "Button is keyboard accessible (Tab key, Enter/Space to activate)"
   - "Screen reader announces button purpose (ARIA label)"
   - "Color contrast meets WCAG AA (4.5:1 for text)"
   - "Visible focus indicator on keyboard focus"
   - "Respects prefers-reduced-motion (no animation if disabled)"

4. **Performance Requirements**:
   - "Animation runs at 60fps"
   - "Image optimization applied (WebP format, lazy loading)"
   - "Bundle size impact: < 5KB"

5. **Browser Verification**:
   - "Feature works in Chrome, Firefox, Safari (latest 2 versions)"
   - "Feature is verified in browser before marking complete"

6. **Code Standards**:
   - "Follows javascript-style.md standards"
   - "Follows htmx-style.md standards"
   - "Uses semantic HTML"

**Layout Context** (can be in description or notes):
- Grid system: "12-column"
- Spacing: "spacing-4 (16px) from adjacent elements"
- Typography: "body text (16px/24px)"
- Visual hierarchy: "primary focal point"
- Responsive: "full width on mobile, auto width on desktop"

### 5. Story Examples

**Foundation Story:**
```json
{
  "id": "US-010",
  "title": "Set up shadcn/ui design system and design tokens",
  "description": "As a developer, I want design system tokens configured so that all components use consistent colors, typography, and spacing. When implementing UI components, I want to use design tokens, so I can maintain visual consistency.",
  "type": "frontend",
  "priority": "high",
  "agent": "ui-developer",
  "dependencies": [],
  "acceptanceCriteria": [
    "shadcn/ui components installed and configured",
    "Design tokens (colors, typography, spacing) defined in CSS variables/Tailwind config",
    "Base component utilities created",
    "Animation utilities configured with reduced motion support",
    "Accessibility utilities (focus styles, ARIA helpers) implemented",
    "Follows javascript-style.md standards"
  ],
  "passes": false,
  "iterations": 0,
  "notes": "",
  "epic": false
}
```

**Component Story:**
```json
{
  "id": "US-025",
  "title": "Implement Primary Button Component",
  "description": "As a user, I want a primary button so that I can perform the main action. When I need to submit a form, I want to click a prominent button, so I can complete my task confidently.",
  "type": "frontend",
  "priority": "high",
  "agent": "ui-developer",
  "dependencies": ["US-010"],
  "acceptanceCriteria": [
    "Button uses spacing-4 (16px) from adjacent elements",
    "Button follows typography hierarchy: body text size 16px/24px",
    "Button is visually prominent (primary color, larger size)",
    "Button shows hover state with 200ms ease-out transition",
    "Button shows active/pressed state",
    "Button is keyboard accessible (Tab key, Enter/Space to activate)",
    "Screen reader announces button purpose (ARIA label)",
    "Color contrast meets WCAG AA (4.5:1 for text)",
    "Visible focus indicator on keyboard focus",
    "Respects prefers-reduced-motion (no animation if disabled)",
    "Animation runs at 60fps",
    "Button stacks vertically on mobile (< 768px)",
    "Button layout horizontally on desktop (>= 768px)",
    "Feature works in Chrome, Firefox, Safari (latest 2 versions)",
    "Feature is verified in browser before marking complete",
    "Follows javascript-style.md standards",
    "Uses semantic HTML"
  ],
  "passes": false,
  "iterations": 0,
  "notes": "Layout context: Uses 12-column grid system, spacing-4 (16px), primary focal point",
  "epic": false
}
```

**State Story:**
```json
{
  "id": "US-045",
  "title": "Implement Form Error States",
  "description": "As a user, I want clear error messages when form validation fails so that I can fix my mistakes. When I submit an invalid form, I want to see helpful error messages, so I can correct my input quickly.",
  "type": "frontend",
  "priority": "medium",
  "agent": "ui-developer",
  "dependencies": ["US-030"],
  "acceptanceCriteria": [
    "Error messages display inline below form fields",
    "Error messages are clear and helpful, not technical",
    "Error messages suggest solutions (e.g., 'Password must be at least 8 characters')",
    "Error states are visually distinct but not alarming (red border, error icon)",
    "Error recovery paths are obvious (fix input, error clears)",
    "Error state uses subtle shake animation (0.3s) on error",
    "Respects prefers-reduced-motion (no animation if disabled)",
    "Screen reader announces errors (ARIA live region)",
    "Color contrast meets WCAG AA (4.5:1 for error text)",
    "Follows javascript-style.md standards"
  ],
  "passes": false,
  "iterations": 0,
  "notes": "",
  "epic": false
}
```

### 4.5. Generate Storybook Stories

For each component identified in UX-SPECS:
1. Determine component package (frontend, mobile, etc.) from feature context
2. **Detect framework** for the package using framework detection script (`storybook/scripts/detect-frameworks.js`)
3. **Check applicability**: If native/game engine, skip Storybook and document alternative
4. Create Storybook story file in appropriate location with framework-appropriate extension:
   - Shared components: `storybook/stories/components/{ComponentName}.stories.{js|jsx|vue|svelte}`
   - Package-specific: `storybook/stories/packages/{package-name}/components/{ComponentName}.stories.{js|jsx|vue|svelte}`
5. Generate stories for all states (empty, loading, error, success) in framework-appropriate format:
   - React: JSX format
   - Vue: Vue component format
   - Svelte: Svelte component format
   - HTML: HTML string format
6. Include design system token references
7. Add accessibility documentation
8. Generate correct import paths based on framework (e.g., `../../frontend/components/Button` for HTML, `../../frontend/components/Button.jsx` for React)
9. Link to component story in prd.json via `storybookPath` metadata

**Story Metadata in prd.json**:
```json
{
  "id": "US-025",
  "title": "Implement Primary Button Component",
  "storybookPath": "packages/frontend/Button",
  "package": "frontend",
  ...
}
```

**Error Handling**:
- If framework detection fails: Default to HTML, log warning, continue workflow
- If story generation fails: Log error, continue workflow (don't block feature planning)
- If component import fails: Generate story with placeholder, document issue in comments
- If native/game engine detected: Skip Storybook, document alternative approach

### 6. Merge into prd.json

**Important**: UX-derived stories are **merged** into existing `prd.json`, not replacing initial planning stories.

**Merge Strategy:**
1. Read existing `prd.json`
2. Generate UX-derived stories (starting from next available US-XXX ID)
3. Append UX-derived stories to `userStories` array
4. Maintain existing story IDs and structure
5. Add `storybookPath` metadata to component stories (if Storybook story was generated)
6. Update `prd.json.quality` if needed (should already be set)

**Story ID Continuation:**
- If existing stories go up to US-020, UX-derived stories start at US-021
- Ensure no ID conflicts

### 7. Post-Translation Validation

After translation, validate:
- [ ] All UX-SPECS sections represented in stories
- [ ] All acceptance criteria are verifiable (not vague)
- [ ] All technical constraints considered
- [ ] All accessibility requirements met
- [ ] Dependencies are correct (Foundation → Layout → Components → Interactions → States → Polish)
- [ ] Story structure matches prd.json schema
- [ ] Job statements included in story descriptions
- [ ] Layout context included in stories
- [ ] Browser verification requirements included
- [ ] Storybook stories generated (if applicable, web frameworks only)
- [ ] storybookPath metadata added to component stories (if Storybook story exists)

### 8. Integration with Execution Loop

UX-derived stories integrate seamlessly with existing execution loop:
- Stories have `passes: false` initially
- Stories can be retried (`iterations` counter tracks attempts)
- Learnings accumulate in `notes` field
- Stories only marked `passes: true` when all acceptance criteria met
- Quality gates apply (typecheck, lint, format, test)
- Browser verification required for frontend stories
- `ux-specs.json` and `UX-SPECS.md` remain as reference documents

## Quick UX Mode Translation

For Quick UX Mode (3-pass methodology), translation is simplified:

1. **Foundation Stories**: Same as Full UX
2. **Component Stories**: Basic components only (no complex interactions)
3. **State Stories**: Essential states only (empty, error)

Skip:
- Layout Design Phase (use basic responsive behavior)
- Complex interactions
- Advanced animations
- Polish stories

## Notes

- `ux-specs.json` and `UX-SPECS.md` remain as "design blueprint" reference
- Implementation happens through trackable, iterable stories
- Stories can reference UX-SPECS in notes for context
- uidev agent reads UX-SPECS during implementation for layout context
