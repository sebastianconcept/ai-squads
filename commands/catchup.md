---
name: catchup
alwaysApply: false
---

# Catchup Command

**Read-only.** This command warms up the current agent with the changes currently visible in the project. It does not modify any files, branches, or state—it only prepares the agent to discuss or explain what it has perceived.

Use it at the start of a session or after switching context to get a shared understanding of local changes and their intent before planning, reviewing, or debugging.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. Current directory must be a git repository
3. Git must be installed and accessible

## When to Use

Invoke this command when you need to:
- Align the agent with your current uncommitted and branch-level work before a discussion
- Quickly summarize what’s changed since master/main and what it might mean
- Use branch name and diffs to infer what you’re trying to do next
- Get an interpretation of your changes and their implications before asking questions or giving instructions

## How It Works

### 1. Determine Base Branch

Resolve the integration branch for comparison:
- Prefer `master` if it exists, otherwise `main`
- If neither exists, use the default branch from `git symbolic-ref refs/remotes/origin/HEAD` or ask the user
- Record current branch name for intent hints

### 2. Gather Change Scope (Order Matters)

**First — Uncommitted changes (staged and unstaged):**

```bash
# Working tree and index vs HEAD
git status
git diff HEAD
```

- Treat this as the “right now” snapshot: what the user is actively editing
- Include both staged and unstaged changes in the analysis

**Second — Branch vs base (main/master):**

```bash
# Commits and file changes on current branch compared to base
git log {base}..HEAD --oneline
git diff {base}..HEAD
```

- Use this to define the full scope of the branch: everything that’s “in progress” compared to the integration branch
- Combine with uncommitted changes to form the full catchup scope

### 3. Optional Project Context

If available, skim for extra context (do not block if missing):
- `~/docs/{project-name}/README.md` or `MISSION.md` — project goals
- `~/docs/{project-name}/TECH-STACK.md` — technologies and conventions; if it has an **Environments** section (local, staging, production), note when changes touch deployment or config and how they might affect staging/production
- `~/docs/{project-name}/ROADMAP.md` or `DECISIONS.md` — if relevant to interpreting the change
- `~/docs/{project-name}/feature/{branch-name}/` — if present to add contex for interpreting the change

Use this to ground “intent” and “implications” in project goals and tech choices.

### 4. Produce the Catchup Output

Structure the reply as follows.

#### What are we up to?

- In a few short paragraphs, describe what the agent infers these changes are trying to achieve.
- Use branch name, file paths, docs and the substance of the diff (e.g. new APIs, refactors, tests, config) to justify the interpretation.
- Call out uncertainty: “This might be X; the branch name suggests Y.”

#### Implications

- Product/design: e.g. new flows, changed assumptions, or follow-ups implied by the change.
- Risk or follow-up: e.g. incomplete work, possible breaking changes, or suggested next steps.
- Technical: e.g. new dependencies, behavior changes, areas that might need tests or docs.

#### Offer to discuss

- End with a clear, short offer to go deeper, e.g.:
  - “We can walk through any part of these changes, refine the intent, or plan next steps—what do you want to focus on?”

## Output Format

Use this structure in your response:

```markdown
## Catchup

#### What are we up to?
[2–4 short paragraphs: your interpretation of what these changes are aiming to achieve and why]

### Implications
- **Product / design:** [bullets, if relevant]
- **Risks / follow-ups:** [bullets, if relevant]
- **Technical:** [bullets]

### Next
[One sentence offering to discuss, explain, or plan next steps.]
```

## Example Usage

```
@catchup
```

```
@catchup I’m about to refactor the auth layer—use this to get up to speed
```

```
@catchup we’re on feature/user-settings and we want to add a new setting
```

## Quality Checklist

Before finalizing, ensure:

- [ ] Uncommitted changes were inspected first, then branch vs base
- [ ] Base branch is correct (main/master or user-specified)
- [ ] “Changes detected” clearly separates uncommitted vs branch scope
- [ ] “Intent” is justified from paths, commits, and branch name
- [ ] “Implications” are scoped to what the changes suggest, not generic
- [ ] The reply ends with an explicit offer to discuss or go deeper
- [ ] No files, branches, or git state were modified (read-only)

## Related Commands

- `explain-system` — narrative explanation of the system; use after catchup when you need architecture or design context
- `review-merge-request` — structured review of changes; use when the goal is critique and suggestions rather than “get up to speed”
- `adopt-project` — ensures the project has the `~/docs/{project-name}/` structure catchup can optionally use
