# UX-SPECS Validation Checklist

This checklist ensures `ux-specs.json` quality and completeness before translation to prd.json stories.

## Pre-Translation Validation

### Schema Validation
- [ ] `ux-specs.json` exists and is valid JSON
- [ ] Validates against `ux-specs-schema.json`
- [ ] All required fields present:
  - [ ] `version`
  - [ ] `feature`
  - [ ] `passes.mentalModel`
  - [ ] `passes.informationArchitecture`
  - [ ] `passes.affordances`
  - [ ] `passes.cognitiveLoad`
  - [ ] `passes.stateDesign`
  - [ ] `passes.flowIntegrity`
  - [ ] `layout` (if Full UX workflow)

### Pass 1: Mental Model Completeness
- [ ] `strugglingMoment.trigger` defined
- [ ] `strugglingMoment.circumstances` defined
- [ ] `strugglingMoment.when` defined
- [ ] `jobStatement` defined (format: "When [situation], I want to [motivation], so I can [outcome]")
- [ ] `desiredOutcome` defined
- [ ] `forcesOfProgress` defined (push, pull, anxiety, habit)
- [ ] `competingSolutions` identified
- [ ] `contextOfUse` defined (when, where, emotional state, distractions)
- [ ] `userTypes` defined (firstTime, returning)
- [ ] `functionalJobs`, `emotionalJobs`, `socialJobs` identified

### Pass 2: Information Architecture Completeness
- [ ] `concepts` enumerated (all user-visible concepts)
- [ ] `concepts` have visibility levels (primary, secondary, hidden)
- [ ] `groups` defined (logical groupings)
- [ ] `groups` have rationale

### Pass 3: Affordances Completeness
- [ ] `actions` mapped to visual/interaction signals
- [ ] `copyGuidelines` defined (button labels, error messages, tooltips, form labels)
- [ ] `mobileFirst` considerations defined (touch targets, navigation patterns, form design)
- [ ] `interactionDesign` defined for animations (hover, active, feedback, reducedMotion)
- [ ] `ethicalDesign` checklist complete:
  - [ ] Animations reduce cognitive load
  - [ ] Animations respect prefers-reduced-motion
  - [ ] No dark patterns
  - [ ] User autonomy preserved
  - [ ] Transparency maintained

### Pass 4: Cognitive Load Completeness
- [ ] `frictionPoints` identified (choice overload, uncertainty, waiting)
- [ ] `frictionPoints` have simplification strategies
- [ ] `defaults` defined with rationale
- [ ] `decisionArchitecture` defined:
  - [ ] Decision points identified
  - [ ] Cognitive biases analyzed
  - [ ] Choice architecture designed

### Pass 5: State Design Completeness
- [ ] `elements` defined with states
- [ ] Each element has states: empty, loading, success, error (as applicable)
- [ ] Each state defines:
  - [ ] `userSees` (what user sees)
  - [ ] `userUnderstands` (what user understands)
  - [ ] `userCanDo` (what actions user can take)
- [ ] `transitionAnimation` defined for state changes

### Pass 6: Flow Integrity Completeness
- [ ] `risks` identified with mitigation strategies
- [ ] `visibilityDecisions` defined (mustBeVisible, canBeImplied)
- [ ] `constraints` defined (hard rules for visual phase)
- [ ] `animationFlow` defined (guides user, rationale)
- [ ] `usabilityHeuristics` checklist complete:
  - [ ] Visibility of system status
  - [ ] Match between system and real world
  - [ ] User control and freedom
  - [ ] Consistency and standards
  - [ ] Error prevention
  - [ ] Recognition rather than recall
  - [ ] Flexibility and efficiency of use
  - [ ] Aesthetic and minimalist design
  - [ ] Help users recognize, diagnose, and recover from errors
  - [ ] Help and documentation

### Layout Design Completeness (Full UX Only)
- [ ] `visualHierarchy.focalPoints` defined (primary, secondary, tertiary per screen)
- [ ] `visualHierarchy.typography` defined (hierarchy with sizes, weights, line-heights)
- [ ] `visualHierarchy.colorUsage` defined (primary, secondary, semantic, neutral)
- [ ] `visualHierarchy.spacingRhythm` defined (scale, vertical, horizontal, rules)
- [ ] `gridSystem` defined (type, breakpoints, container widths)
- [ ] `layoutPatterns` defined (single-column, two-column, sidebar, etc.)
- [ ] `componentComposition` defined (how components work together)

### Business Hypothesis (if present)
- [ ] `businessHypothesis.statement` defined
- [ ] `businessHypothesis.successCriteria` defined
- [ ] `businessHypothesis.validationMethod` defined
- [ ] `businessHypothesis.assumption` defined
- [ ] `businessHypothesis.mvpDefinition` defined
- [ ] `businessHypothesis.growthImpact` defined (activation, retention, conversion)

### Technical Constraints
- [ ] `technicalConstraints.browserSupport` defined
- [ ] `technicalConstraints.performanceTargets` defined
- [ ] `technicalConstraints.accessibilityLevel` defined
- [ ] `technicalConstraints.techStack` defined

### Design System
- [ ] `designSystem.type` defined (existing or custom)
- [ ] `designSystem.name` defined (if existing)
- [ ] `designSystem.reference` defined (if custom)

## Post-Translation Validation

After translating to prd.json stories, validate:

### Story Completeness
- [ ] All UX-SPECS sections represented in stories
- [ ] Foundation stories generated (design system, tokens, grid, spacing, typography)
- [ ] Layout shell stories generated (if Full UX)
- [ ] Component stories generated
- [ ] Interaction stories generated
- [ ] State stories generated
- [ ] Polish stories generated

### Story Quality
- [ ] All acceptance criteria are verifiable (not vague)
- [ ] Job statements included in story descriptions
- [ ] Layout context included in stories (grid, spacing, typography, visual hierarchy)
- [ ] Accessibility requirements included in acceptance criteria
- [ ] Performance requirements included in acceptance criteria
- [ ] Browser verification requirements included in acceptance criteria
- [ ] Code standards references included

### Dependencies
- [ ] Dependencies are correct (Foundation → Layout → Components → Interactions → States → Polish)
- [ ] No circular dependencies
- [ ] All dependencies exist in prd.json

### Integration
- [ ] Stories merged into existing prd.json (not replacing)
- [ ] Story IDs are sequential and don't conflict
- [ ] Story structure matches prd.json schema
- [ ] Stories have `passes: false` initially
- [ ] Stories have `iterations: 0` initially
- [ ] Stories have empty `notes` initially

## Validation Failure Handling

If validation fails:
1. List all gaps/missing items
2. Ask user to address gaps
3. Re-run validation after fixes
4. Only proceed to translation when validation passes

## Quick UX Mode Validation

For Quick UX Mode (3-pass), validate:
- [ ] Pass 1 (Mental Model) complete
- [ ] Pass 3 (Affordances) complete
- [ ] Pass 6 (Flow Integrity) complete
- [ ] Basic story generation (Foundation → Components → States)

Skip validation for:
- Pass 2 (Information Architecture)
- Pass 4 (Cognitive Load) - basic only
- Pass 5 (State Design) - essential states only
- Layout Design Phase
