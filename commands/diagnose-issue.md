---
name: diagnose-issue
alwaysApply: false
---

# Diagnose Issue Command

This command helps investigate problems by analyzing evidence, generating hypotheses, and guiding the user toward a clear root cause and problem definition.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. User has a problem to investigate (even if vague)

## When to Use

Invoke this command when you need to:
- Investigate an issue with unclear cause
- Analyze logs, errors, or unexpected behavior
- Generate hypotheses about what's going wrong
- Narrow down a problem through systematic investigation
- Formulate a clear problem definition before solving
- Debug production issues or intermittent failures
- **Investigate cross-service:** When the cause may lie in a dependency service (e.g. gRPC/HTTP upstream always returns None when it should be conditional; correlation across services via trace-id)

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

**First-hand evidence: fetching logs (local or remote)**

- **Logs are first-hand evidence** (strongest in the evidence hierarchy; see `rules/system.md`). Prefer suggesting concrete commands the user can run to capture logs, then interpret what they paste.
- **Where to get logs:** Use project **TECH-STACK.md** (especially an **Environments** section with hostnames and SSH) and, if the issue relates to a feature, **VERIFY.md** for that feature. Those docs document where backend and frontend logs live and how to fetch them.
- **Local:** e.g. `docker compose logs -f api`, or stdout of the process running locally (`cargo run`, `npm run dev`). For Rust backends, logs are usually stdout; no default log file.
- **Remote (staging or production):** e.g. `ssh user@staging.example.com 'cd /opt/app && docker compose logs -f api'` — use the hostname and SSH command from TECH-STACK **Environments** (or VERIFY.md) for the relevant environment. Specify which environment (staging vs production) when suggesting remote log fetch.
- **What to ask for:** After suggesting a command, ask the user to run it and paste the relevant excerpt (with timestamps around the incident). Treat pasted logs as first-hand evidence and document them in EVIDENCE.md with source and timestamp.

**Cross-service investigation and correlation (trace-id)**

- **Cause may be in a dependency service.** The failing service might be correct; the bug or misbehavior may be in an upstream or downstream service (e.g. a dependency always returns `None` or a wrong value when it should return conditionally and correctly). gRPC or HTTP calls propagate the bad behavior; investigating only one service can miss the root cause.
- **Service graph:** Use project context (SPECS, DECISIONS, code) to identify which services call which (dependency graph). Include dependency services in hypotheses and in evidence gathering: fetch logs from the calling service and from the dependency service(s) for the same request.
- **Correlation via trace-id:** gRPC and HTTP calls often propagate a common **trace-id** (or `trace_id`, `request_id`, `span_id`, or similar) in headers or metadata. Backend logs typically emit this in log entries. Use it to **correlate** log entries across services:
  1. Obtain a **trace-id** from one service’s logs (e.g. the entry where the error or symptom appears).
  2. Fetch logs from the other service(s) for the same time window and filter/grep by that trace-id (e.g. `grep trace_id=abc123` or project-specific log field).
  3. Order entries by timestamp to reconstruct the **request flow** and identify where the failure or wrong behavior occurred (e.g. dependency returned None; caller then failed).
- **Evidence sequence for cross-service:** In the evidence sequence, include steps like: (1) Get trace-id from service A logs; (2) Fetch service B logs, grep by trace-id, paste excerpt; (3) Map request flow and identify which service first produced the wrong outcome. Use TECH-STACK Environments and VERIFY.md for log locations per service (local or remote).

**Soft Evidence:**
- User reports or complaints
- Timing correlations ("started after deployment X")
- Environmental factors ("only happens in production")
- Workarounds that users have found

### 2. Load Project Context

Read and understand from:
- `~/docs/{project-name}/README.md` - Project overview
- `~/docs/{project-name}/TECH-STACK.md` - Technologies and architecture; if it has an **Environments** section, use it for hostnames and SSH per environment (local, staging, production) so you can suggest concrete log-fetch commands (local or remote)
- `~/docs/{project-name}/DECISIONS.md` - Design decisions that may be relevant
- Relevant source code in suspected areas
- **If the issue relates to a feature:** `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` (or in-repo `docs/feature/{feature_name}/VERIFY.md`) — log locations, happy path, and environment-specific commands for that feature

### 3. Evidence Analysis

For each piece of evidence:
- **Parse and interpret** - What does this actually tell us?
- **Identify anomalies** - What stands out as unusual?
- **Note timestamps** - Establish timeline of events
- **Correlate** - How do different pieces connect?
- **Correlate across services** - If multiple services are involved, use a common **trace-id** (or request_id, span_id) to link log entries across services. Same trace-id = same request flow; order by timestamp to see call direction and where the failure or wrong behavior first appeared.

### 4. Hypothesis Generation

Generate hypotheses ordered by likelihood using a scientific, evidence-based approach:

**For each hypothesis:**
- State the proposed cause clearly
- Explain what evidence supports it
- Explain what evidence contradicts it
- Rate confidence level (Low/Medium/High)
- Identify what would confirm or refute it
- Design small experiments to test the hypothesis

**Hypothesis Generation Principles:**
- **Generate Multiple Hypotheses**: Don't fixate on the first explanation - consider alternatives
- **Design Small Experiments**: Create minimal, controlled tests to validate or refute each hypothesis
- **Reduce Ambiguity**: Use experiments to narrow down possibilities and unlock progress
- **Evidence-Based**: Base hypotheses on observed evidence, not assumptions
- **Document in Notes**: Use investigation notes (`~/docs/{project-name}/notes/{issue-id}/`) to track hypotheses, evidence, and experiments (category: "investigations" stored in frontmatter)

**Hypothesis Categories:**
- **Code bugs** - Logic errors, race conditions, edge cases
- **Data issues** - Corrupted data, schema mismatches, encoding
- **Infrastructure** - Resources, networking, dependencies
- **Configuration** - Environment vars, feature flags, settings
- **External systems** - Third-party APIs, databases, services
- **Dependency / cross-service** - Bug or misbehavior in an upstream or downstream service (e.g. dependency always returns None or wrong value when it should be conditional; gRPC/HTTP propagates the bad behavior; correlate via trace-id across services)
- **Concurrency** - Deadlocks, starvation, ordering issues
- **Resource exhaustion** - Memory, connections, file handles

**Using Investigation Notes:**
- Create notes in `~/docs/{project-name}/notes/{issue-id}/` when starting investigation (category: "investigations" stored in frontmatter/metadata, not in directory path)
- Document hypotheses in `insights.json` with confidence levels, evidence-based tracking, and supporting/contradicting evidence
- Document evidence in `EVIDENCE.md` as it's gathered (logs, metrics, test results, experimental data) with timestamps and sources
- Update `TODOS.md` with experiments to run and checks to perform
- Reference evidence in hypotheses (supporting or contradicting)
- Discard hypotheses that are contradicted by evidence to focus on viable paths
- Mark insights as `evidenceBased: true` when supported by first-hand evidence (logs, test results, metrics, experimental data)
- Include `evidence: {}` object in insights.json when evidence-based

**Note Creation Workflow:**

1. **When Starting Investigation:**
   - Generate an issue ID (e.g., `memory-leak-2024-01-15` or `api-timeout-2024-01-15`)
   - Create note directory: `~/docs/{project-name}/notes/{issue-id}/` (or use flat structure: `~/docs/{project-name}/notes/{issue-id}-*.md`)
   - Create initial `CONTEXT.md` with investigation scope, goals, and success criteria (category: "investigations" in frontmatter)
   - Create initial `EVIDENCE.md` to document evidence as it's gathered (category: "investigations" in frontmatter)
   - Create initial `TODOS.md` to track experiments and checks (category: "investigations" in frontmatter)
   - Create initial `insights.json` to document hypotheses and findings (category: "investigations" in metadata)

2. **During Investigation:**
   - **Document Evidence Immediately**: When gathering logs, metrics, test results, or experimental data, append to `EVIDENCE.md` with:
     - What was observed
     - When (ISO 8601 timestamp)
     - Source (logs, metrics, test results, etc.)
     - Controlled inputs (if applicable)
     - Observed outputs (if applicable)
     - Commit hash (to scope validity to codebase version)
   - **Document Hypotheses**: When generating hypotheses, add to `insights.json` with:
     - Hypothesis title and description
     - Confidence level (Low/Medium/High)
     - Supporting evidence (references to EVIDENCE.md entries)
     - Contradicting evidence (if any)
     - Type: "hypothesis"
     - `evidenceBased: true` if supported by first-hand evidence
     - `evidence: {}` object documenting the evidence
- **Track Experiments**: Update `TODOS.md` with:
  - Experiments to run (with expected outcomes)
  - Checks to perform
  - Status (pending, in-progress, completed, blocked)
- **Document reproduction/evidence sequences**: When you define an ordered sequence of evidence-gathering steps to reproduce the issue or to confirm/refute a hypothesis, document it in `TODOS.md` (as an ordered checklist) or in `REPRODUCTION.md` / `EVIDENCE-SEQUENCE.md` in the same notes directory. When the user completes a step and pastes evidence, update `EVIDENCE.md`, mark the step done, and suggest the next step.
  - **Update Insights**: When experiments confirm or refute hypotheses:
     - Update hypothesis status in `insights.json` (verified, discarded)
     - Add execution-attempt insights documenting what was tried
     - Include commit hash that validates the reasoning

3. **When Resuming Investigation:**
   - Read existing notes from `~/docs/{project-name}/notes/{issue-id}/` (or matching flat files)
   - Review `CONTEXT.md` to understand investigation scope
   - Review `EVIDENCE.md` to see what evidence has been gathered
   - Review `TODOS.md` to see what experiments are pending
   - Review `REPRODUCTION.md` or `EVIDENCE-SEQUENCE.md` if present — ordered evidence sequence; continue from the next pending step
   - Review `insights.json` to see previous hypotheses and findings (prioritize evidence-based insights)
   - Continue investigation from where it left off

**Note File Format Examples:**

See `~/docs/{project-name}/feature/self-notes/specs.md` for complete format specifications. Key points:
- Markdown notes (CONTEXT.md, EVIDENCE.md, TODOS.md) use YAML frontmatter with `category: investigations`
- insights.json uses JSON format with `metadata.category: "investigations"`
- All notes include timestamps, commit hash (if in git repo), and optional metadata (agent, command, context)

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
- **Where to get logs:** Use project TECH-STACK **Environments** and, if relevant, feature **VERIFY.md** to suggest exact commands for fetching backend/frontend logs (local: e.g. `docker compose logs`, process stdout; remote: `ssh … 'docker compose logs -f <service>'` for the chosen staging or production host). Suggest the user run the command and paste the excerpt; treat as first-hand evidence.

**Reproduction and evidence sequences**

- **Reproducing an issue** (or confirming/refuting a hypothesis) is a **sequence of evidence-gathering steps**. Define an ordered list: "To reproduce this issue" or "To confirm hypothesis H, collect evidence in this order: 1) … 2) … 3) …"
- Each step should be concrete: **where** to get evidence (logs, metrics, UI, network), **how** (exact command or action), and **what to look for** (pattern, status, error variant). Use TECH-STACK Environments and VERIFY.md for log locations and commands.
- **Document the sequence** in investigation notes: in `TODOS.md` (as an ordered checklist) or in a dedicated `REPRODUCTION.md` (or `EVIDENCE-SEQUENCE.md`) in `~/docs/{project-name}/notes/{issue-id}/`. See `templates/diagnosis/evidence-sequence.md` for structure. When the user completes a step and pastes evidence, append to `EVIDENCE.md`, mark the step done, and suggest the next step.
- **Per hypothesis:** When presenting hypotheses, optionally attach to each an **evidence sequence to confirm/refute** — ordered steps that, when followed, produce first-hand evidence that confirms or refutes that hypothesis. Following the sequence reduces ambiguity and unlocks progress.

**Experiments to Run:**
- Controlled tests to confirm/refute hypotheses
- Reproduction scenarios (when framed as an evidence sequence, ordered steps to collect evidence)
- Isolation strategies
- **Design Principle**: Small, focused experiments that test one hypothesis at a time
- **Document**: Record experimental inputs and observed outputs in `EVIDENCE.md`
- **Purpose**: Reduce ambiguity and unlock progress by confirming or discarding hypotheses
- **When appropriate:** Structure as an **evidence sequence** — ordered steps to collect evidence; document in TODOS.md or REPRODUCTION.md so the user can follow step-by-step

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

**Presenting evidence: narrative and diagram**

- **Narrative backed by evidence:** Present the diagnosis as a short **story** of what happened, with every claim tied to evidence. E.g. "Request entered service A at T1 (log entry X, trace-id T). Service A called service B via gRPC (trace-id T propagated). Service B returned None at T2 (log entry Y, trace-id T). Service A then failed at T3 because it did not handle None (log entry Z, trace-id T)." Reference specific log excerpts, trace-ids, and timestamps so the user can verify.
- **Diagram:** Provide a **request-flow diagram** so the user can see the path of the request and where the failure or wrong behavior occurred. Use **Mermaid** (sequence or flowchart) or ASCII. Include: services involved, direction of calls, propagation of trace-id, and the point where the failure or incorrect behavior appears (e.g. "B returns None", "A 500"). For cross-service issues, the diagram is especially useful to show which service is the root cause (e.g. dependency B) vs which service surfaces the symptom (caller A).
- **When to use:** Always offer narrative + diagram when the investigation spans multiple services or when correlation (e.g. trace-id) was used. For single-service issues, a short narrative with evidence references is still valuable; add a diagram if it clarifies flow or failure point.

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

**Evidence sequence (to confirm/refute):**  
*Ordered steps to collect evidence — follow in order; paste what you find after each step. Document in TODOS.md or REPRODUCTION.md.*

To reproduce the issue / To confirm Hypothesis 1:
1. [Step 1 — e.g. "Fetch backend logs from staging around time T: `ssh … 'docker compose logs --since … api'`; paste excerpt"]
2. [Step 2 — e.g. "Look for request_id X in logs; note status and any error variant"]
3. [Step 3 — e.g. "Check frontend network tab for the same request; paste response"]

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

### Evidence narrative

*Story of what happened, with each claim backed by evidence (log excerpt, trace-id, timestamp). For cross-service: request entered A (evidence X), A called B with trace-id T (evidence Y), B returned [wrong value] (evidence Z), A then failed (evidence W).*

[1–2 short paragraphs, with explicit references to log entries and trace-id where applicable.]

---

### Request flow (diagram)

*Use Mermaid or ASCII. Show services, call direction, trace-id propagation, and where the failure or wrong behavior occurred. For cross-service: mark which service is root cause vs which surfaces the symptom.*

Example (Mermaid sequence diagram):

    sequenceDiagram
      participant User
      participant A as Service A
      participant B as Service B (dependency)
      User->>A: request (trace-id T)
      A->>B: gRPC call (trace-id T)
      B-->>A: None / wrong value
      A-->>User: 500
      Note over B: Root cause: always returns None

*(Render in a mermaid code block or use ASCII equivalent.)*

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
- **Request correlation** - **Trace IDs, request_id, span_id** (propagated across gRPC/HTTP); use to correlate log entries **across services** (same trace-id = same request flow; order by timestamp to see call direction and where failure or wrong value first appeared)
- **Resource indicators** - Memory, connections, queues
- **State transitions** - Unexpected states or missing transitions

### Cross-service correlation

- **Trace-id (or request_id, span_id):** Identify the field name used in your stack (e.g. `trace_id`, `request_id`, `X-Request-Id`). Extract it from one service’s log entry, then grep/filter other services’ logs by that value for the same time window.
- **Request flow:** Order correlated entries by timestamp to get: which service received the request first, which called which, where the wrong return value or error first appeared (often the **root cause** is in the dependency that returned the wrong value; the **symptom** appears in the caller).
- **Hypothesis:** If a dependency always returns None (or a wrong value) when it should return conditionally, the caller may fail or misbehave; the fix may be in the dependency service, not the caller.

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

@diagnose-issue API returns 500 when calling the user-service — might be in our service or in the dependency; we have trace_id in gRPC and in logs
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
- [ ] **Cross-service:** If multiple services are involved, considered dependency service as possible root cause; used trace-id (or request_id/span_id) to correlate log entries across services
- [ ] **Evidence presentation:** Diagnosis includes a **narrative** backed by evidence (references to log entries, trace-id, timestamps) and a **diagram** (request flow, services, point of failure) when useful—especially for cross-service issues

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
| `evidence-sequence.md` | Ordered evidence-gathering steps to reproduce an issue or confirm/refute a hypothesis (REPRODUCTION.md / EVIDENCE-SEQUENCE.md in notes) |

## Related Commands

- `explain-system` - Understand the system before diagnosing
- `verify-feature` - Align on happy path and log locations; useful when the issue relates to a feature and you need first-hand log evidence (local or remote)
- `ideate-solution` - Next step after root cause is found
- `plan-feature` - For implementing the fix
- `review-merge-request` - Review the fix implementation

