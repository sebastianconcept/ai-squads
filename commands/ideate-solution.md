---
name: ideate-solution
alwaysApply: false
---

# Ideate Solution Command

This command generates multiple solution approaches to a problem, ranging from minimal changes to comprehensive implementations, helping you explore the solution space before committing to an approach.

## Prerequisites

1. Project must have been adopted (have `docs/` directory)
2. Problem must be solvable within the scope of the adopted project(s)

## When to Use

Invoke this command when you need to:
- Explore different ways to solve a problem before choosing an approach
- Understand the trade-offs between minimal and comprehensive solutions
- Get a spectrum of options from quick-fix to elegant redesign
- Prepare for a well-informed feature planning session
- Compare implementation effort vs. long-term maintainability
- Decide whether to patch, refactor, or rebuild

## How It Works

### 1. Problem Definition

Accept the problem from the user in one of these forms:

**Verbal Description:**
- User describes the problem in their own words
- Ask clarifying questions to understand context

**Documentation Reference:**
- User points to an issue, bug report, or feature request
- User shows error logs or system behavior
- User references existing docs describing the problem

**Scope Clarification:**
If the problem seems to span multiple projects or could benefit from a wider scope:
- Ask if the solution should be contained to the current project
- Offer to expand scope if multiple adopted projects could be involved
- Clarify boundaries for the solution space

### 2. Load Project Context

Read and understand from:
- `docs/README.md` - Project overview
- `docs/mission.md` - Project purpose and goals
- `docs/tech-stack.md` - Technologies and constraints
- `docs/DECISIONS.md` - Existing architectural decisions
- `docs/team.md` - Available agent expertise
- Relevant source code areas affected by the problem

### 3. Generate Solution Approaches

Create **at least 2 approaches**, always ordered from minimal to comprehensive:

**Approach 1 - Minimal (Quick Win):**
- Smallest possible change to address the problem
- Lowest risk, fastest to implement
- May involve trade-offs or technical debt
- Ideal for: urgent fixes, testing hypotheses, time constraints

**Approach 2 - Balanced (Pragmatic):**
- Thoughtful solution without over-engineering
- Good balance of effort vs. long-term value
- Addresses root cause while being practical
- Ideal for: most real-world scenarios

**Approach 3+ - Comprehensive (Elegant):**
- Full redesign or refactoring if warranted
- Optimal architecture for the problem domain
- Higher upfront cost, but best long-term maintainability
- Ideal for: critical systems, foundational changes, scaling concerns

### 4. Approach Structure

For each approach, use the template from `templates/ideation/approach.md`.

Key sections:
- **Philosophy**: One-line mindset summary
- **Summary**: 2-3 sentences on what this approach does
- **Changes Required**: Specific files, modules, APIs affected
- **Pros/Cons**: Clear trade-offs
- **Performance Impact**: Runtime costs (async blocking, DB locks, external calls, memory, CPU, behavior under load)
- **Effort Estimate**: Time, complexity, and risk levels
- **Technical Considerations**: Architecture, dependencies, testing
- **When to Choose This**: Specific scenarios where this fits

### 5. Comparison Matrix

After presenting approaches, provide a quick comparison using `templates/ideation/comparison-matrix.md`.

The matrix compares: Time to Ship, Risk Level, Technical Debt, Long-term Value, Scope of Change, and Performance Under Load across all approaches.

### 6. Interactive Follow-up

After presenting approaches, always offer:

**Option A - Generate Another Approach:**
- Create additional approaches if user wants more options
- Can be more minimal, more comprehensive, or explore a different direction
- Useful when none of the approaches feel quite right

**Option B - Dive Deeper:**
- Explore one approach in detail
- Discuss implementation specifics
- Analyze implications and ripple effects
- Work through edge cases and potential issues
- Map out dependencies and integration points

**Option C - Prepare for Planning:**
- Generate an optimized prompt for the `plan-feature` command
- Includes all relevant context for the chosen approach
- Structures the problem and solution for feature planning
- Sets up for efficient PRD, specs, and task generation

## Response Patterns

### Initial Problem Gathering

```markdown
## Problem Understanding

I'd like to help you explore solutions. Let me understand the problem:

**What I understood:**
[Restate the problem to confirm understanding]

**Clarifying Questions:**
1. [Question about scope or constraints]
2. [Question about priorities]
3. [Question about affected areas]

**Scope Check:**
This seems contained to [project name]. Should I consider solutions involving other adopted projects, or keep the scope focused here?
```

### Solution Presentation

```markdown
## Solution Approaches for: [Problem Summary]

I've identified [N] approaches, ordered from minimal changes to comprehensive redesign.

---

[Approach 1]

---

[Approach 2]

---

[Approach N]

---

## Quick Comparison

[Comparison Matrix]

---

## What's Next?

I can help you:

**A. Generate Another Approach**
   Want to explore a different direction? I can create additional options.

**B. Dive Deeper into an Approach**
   Pick an approach number and I'll detail the implementation, implications, and edge cases.

**C. Prepare for Feature Planning**
   Choose an approach and I'll generate an optimized prompt for `plan-feature` with all the context needed for efficient planning.

Which would you like?
```

### Plan-Feature Prompt Generation

When user selects Option C, generate a handoff prompt using `templates/ideation/plan-feature-prompt.md`.

Key sections to populate:
- **Feature Name**: kebab-case identifier
- **Context**: Problem background and rationale
- **Chosen Approach**: Name and summary
- **Key Requirements**: Derived from the selected approach
- **Technical Direction**: Architecture, patterns, integration points
- **Constraints**: Time, technical, and scope boundaries
- **Out of Scope**: Explicit exclusions
- **Success Criteria**: How to verify the problem is solved

## Example Usage

```
@ideate-solution users are complaining about slow search results

@ideate-solution here's the bug report from GitHub issue #42 [link]

@ideate-solution we need to add multi-tenancy - what are our options?

@ideate-solution the authentication flow is getting complex, how can we simplify it?

@ideate-solution show me approaches to migrate from REST to GraphQL

@ideate-solution our database is hitting performance limits under load

@ideate-solution I want to add dark mode - what's the minimal vs proper way?
```

## Quality Checklist

Before presenting approaches, verify:

- [ ] Problem is clearly understood and restated
- [ ] Scope is clarified (single project vs. multiple)
- [ ] At least 2 approaches provided
- [ ] Approaches ordered from minimal to comprehensive
- [ ] Each approach has clear pros/cons
- [ ] Effort estimates are realistic
- [ ] Technical considerations are specific to the codebase
- [ ] Comparison matrix is provided
- [ ] All three follow-up options are offered
- [ ] Approaches align with project's tech stack and decisions

## Pro Tips

1. **Don't Skip the Minimal**: Even if it seems hacky, showing the quick fix helps calibrate expectations
2. **Be Honest About Trade-offs**: Every approach has downsides - surface them clearly
3. **Reference Existing Patterns**: Connect approaches to patterns already in the codebase
4. **Consider the Team**: Factor in team expertise when estimating complexity
5. **Think About Migration**: For comprehensive approaches, consider the transition path
6. **Connect to Mission**: Best solutions align with project goals, not just technical elegance
7. **Acknowledge Uncertainty**: If you're unsure about estimates, say so and explain why
8. **Build on Decisions**: Reference `DECISIONS.md` - don't propose approaches that contradict established decisions without calling it out
9. **Think Under Load**: A correct solution can still starve the system - flag async blocking, lock contention, N+1 queries, and backpressure gaps

## Templates

This command uses templates from `templates/ideation/`:

| Template | Purpose |
|----------|---------|
| `approach.md` | Structure for each solution approach |
| `comparison-matrix.md` | Quick comparison table across approaches |
| `plan-feature-prompt.md` | Handoff prompt for `plan-feature` command |

These templates ensure consistent output structure and are easier to maintain separately from command logic.

## Related Commands

- `adopt-project` - Required before using this command
- `explain-system` - Understand the system before ideating solutions
- `plan-feature` - Next step after choosing an approach
- `review-merge-request` - Use to review the implemented solution

