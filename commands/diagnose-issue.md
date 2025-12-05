---
name: diagnose-issue
alwaysApply: false
---

# Diagnose Issue Command

This command helps investigate problems by analyzing evidence, generating hypotheses, and guiding the user toward a clear root cause and problem definition.

## Prerequisites

1. Project must have been adopted (have `docs/` directory)
2. User has a problem to investigate (even if vague)

## When to Use

Invoke this command when you need to:
- Investigate an issue with unclear cause
- Analyze logs, errors, or unexpected behavior
- Generate hypotheses about what's going wrong
- Narrow down a problem through systematic investigation
- Formulate a clear problem definition before solving
- Debug production issues or intermittent failures

## How It Works

### 1. Evidence Gathering

Accept input from the user in any combination:

**Problem Description:**
- User describes symptoms in their own words
- Observed vs. expected behavior
- When it started, frequency, patterns

**Hard Evidence:**
- Error messages or stack traces
- Log excerpts (application, system, database)
- Metrics or monitoring data
- Screenshots or recordings
- Network traces or API responses
- Database query results

**Soft Evidence:**
- User reports or complaints
- Timing correlations ("started after deployment X")
- Environmental factors ("only happens in production")
- Workarounds that users have found

### 2. Load Project Context

Read and understand from:
- `docs/README.md` - Project overview
- `docs/tech-stack.md` - Technologies and architecture
- `docs/DECISIONS.md` - Design decisions that may be relevant
- Relevant source code in suspected areas

### 3. Evidence Analysis

For each piece of evidence:
- **Parse and interpret** - What does this actually tell us?
- **Identify anomalies** - What stands out as unusual?
- **Note timestamps** - Establish timeline of events
- **Correlate** - How do different pieces connect?

### 4. Hypothesis Generation

Generate hypotheses ordered by likelihood:

**For each hypothesis:**
- State the proposed cause clearly
- Explain what evidence supports it
- Explain what evidence contradicts it
- Rate confidence level (Low/Medium/High)
- Identify what would confirm or refute it

**Hypothesis Categories:**
- **Code bugs** - Logic errors, race conditions, edge cases
- **Data issues** - Corrupted data, schema mismatches, encoding
- **Infrastructure** - Resources, networking, dependencies
- **Configuration** - Environment vars, feature flags, settings
- **External systems** - Third-party APIs, databases, services
- **Concurrency** - Deadlocks, starvation, ordering issues
- **Resource exhaustion** - Memory, connections, file handles

### 5. Investigation Guidance

When cause is NOT yet determined, provide:

**Immediate Checks:**
- Quick things to verify right now
- Commands to run, logs to check
- Questions to ask users or ops

**Evidence to Gather:**
- What specific data would narrow the hypotheses
- How to capture it (logging, tracing, reproduction steps)
- What to look for in each data source

**Experiments to Run:**
- Controlled tests to confirm/refute hypotheses
- Reproduction scenarios
- Isolation strategies

### 6. Root Cause Determination

When sufficient evidence points to a cause:

**Confidence Assessment:**
- What evidence confirms this cause
- Are there any remaining doubts
- Could there be multiple contributing factors

**Problem Definition:**
Generate a clear, actionable problem statement:
- What is broken (specific component/behavior)
- Why it's broken (root cause)
- Under what conditions (triggers/environment)
- What impact it has (user-facing, system-level)

### 7. Next Steps

**If cause is found with high confidence:**
- Present clear problem definition
- Offer to invoke `ideate-solution` with the diagnosis
- Generate optimized prompt for solution ideation

**If cause is uncertain:**
- Summarize remaining hypotheses
- Prioritize next investigation steps
- Offer to continue diagnosis with new evidence

## Response Patterns

### Initial Evidence Gathering

```markdown
## Issue Investigation

I'll help you diagnose this issue. Let me understand what we're working with:

**What I understood:**
[Restate the problem/symptoms]

**Evidence received:**
- [List each piece of evidence provided]

**Initial questions:**
1. [Question to clarify symptoms]
2. [Question about timing/frequency]
3. [Question about environment/context]

**Can you also provide:**
- [Specific evidence that would help]
```

### Hypothesis Presentation

```markdown
## Diagnostic Hypotheses

Based on the evidence, here are the most likely causes:

---

### Hypothesis 1: [Name] ⭐ High Confidence

**Proposed cause:** [Clear statement]

**Supporting evidence:**
- [Evidence point 1]
- [Evidence point 2]

**Contradicting evidence:**
- [None / Evidence that doesn't fit]

**To confirm:** [What would prove this]
**To refute:** [What would disprove this]

---

### Hypothesis 2: [Name] 🔶 Medium Confidence

[Same structure]

---

## Investigation Plan

**Immediate checks:**
1. [Quick verification 1]
2. [Quick verification 2]

**Evidence to gather:**
1. [Data source 1] - Looking for [specific pattern]
2. [Data source 2] - Looking for [specific pattern]

**Experiments:**
1. [Controlled test to isolate variable]

Which hypothesis seems most likely based on your experience? Or should we gather more evidence?
```

### Root Cause Found

```markdown
## Diagnosis Complete ✓

### Root Cause Identified

**Problem:** [Clear problem statement]

**Cause:** [Root cause explanation]

**Evidence trail:**
1. [Evidence] → [What it showed]
2. [Evidence] → [What it confirmed]

**Confidence:** High / Medium

**Contributing factors:**
- [Factor 1 if applicable]
- [Factor 2 if applicable]

---

## Problem Definition

> **[Component/Feature]** is **[failing/misbehaving]** because **[root cause]**, 
> triggered when **[conditions]**, resulting in **[impact]**.

---

## Next Steps

I can help you:

**A. Ideate Solutions**
   I'll generate a prompt for `@ideate-solution` with this diagnosis, 
   so you can explore fix approaches from minimal to comprehensive.

**B. Investigate Further**
   If you want more certainty before proceeding, I can suggest 
   additional validation steps.

**C. Document the Issue**
   Generate a bug report or incident documentation with this analysis.

Which would you like?
```

### Ideate-Solution Handoff

When user selects Option A, generate:

```markdown
## Ideate-Solution Prompt

Use this with `@ideate-solution` for targeted solution exploration:

---

**Problem:** [One-line summary]

**Diagnosis:**
[Root cause explanation from investigation]

**Evidence:**
- [Key evidence points]

**Constraints:**
- [Technical constraints discovered]
- [Time/urgency constraints]

**Impact:**
- [User impact]
- [System impact]

**Suspected code areas:**
- [File/module 1]
- [File/module 2]

---

Copy this context when running `@ideate-solution`.
```

## Evidence Analysis Patterns

### Log Analysis

When analyzing logs, look for:
- **Errors and warnings** - Obvious failures
- **Timing patterns** - Delays, timeouts, ordering
- **Request correlation** - Trace IDs, session IDs
- **Resource indicators** - Memory, connections, queues
- **State transitions** - Unexpected states or missing transitions

### Error Analysis

When analyzing errors:
- **Error type** - What category of failure
- **Stack trace** - Where in code it originated
- **Context** - What was happening when it failed
- **Frequency** - One-off vs. recurring
- **Blast radius** - Single user vs. system-wide

### Performance Issues

When investigating slowness:
- **Where** - Which component is slow
- **When** - Under what conditions
- **How much** - Quantify the degradation
- **Correlation** - With load, time, data size

## Example Usage

```
@diagnose-issue the API is returning 500 errors intermittently

@diagnose-issue users report slow page loads - here are the logs [paste]

@diagnose-issue login fails for some users but not others

@diagnose-issue here's a stack trace from production [paste]

@diagnose-issue the background job stops processing after a few hours

@diagnose-issue data is appearing duplicated in the database
```

## Quality Checklist

Before presenting diagnosis, verify:

- [ ] All provided evidence has been analyzed
- [ ] Hypotheses are ordered by likelihood
- [ ] Each hypothesis has supporting/contradicting evidence noted
- [ ] Confidence levels are justified
- [ ] Investigation steps are specific and actionable
- [ ] Problem definition (if reached) is clear and testable
- [ ] Next steps are offered based on diagnosis state
- [ ] Technical context from project docs is considered

## Pro Tips

1. **Don't jump to conclusions** - Gather evidence before committing to a hypothesis
2. **Correlation ≠ Causation** - "Started after deploy X" doesn't mean X caused it
3. **Check the obvious first** - Configuration, permissions, and connectivity before complex theories
4. **Reproduce before fixing** - A reproducible issue is half-solved
5. **Consider multiple causes** - Issues can have contributing factors, not just one root cause
6. **Trust the evidence** - When evidence contradicts intuition, investigate the evidence
7. **Document as you go** - The investigation trail is valuable for future issues
8. **Time-bound investigations** - Set checkpoints to avoid rabbit holes

## Templates

This command uses templates from `templates/diagnosis/`:

| Template | Purpose |
|----------|---------|
| `hypothesis.md` | Structure for each hypothesis |
| `problem-definition.md` | Clear problem statement format |
| `ideate-prompt.md` | Handoff to `ideate-solution` |

## Related Commands

- `explain-system` - Understand the system before diagnosing
- `ideate-solution` - Next step after root cause is found
- `plan-feature` - For implementing the fix
- `review-merge-request` - Review the fix implementation

