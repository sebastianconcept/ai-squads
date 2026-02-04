---
name: verify-feature
alwaysApply: false
---

# Verify Feature Command

This command prepares an agent to help you verify that a feature or fix is working as expected. It catches the agent up on your branch and changes, loads the feature’s happy path and log context, and gets ready for you to test manually or to interpret logs after you exercise a flow.

**Read-only for git/context.** The command does not modify files; it only gathers context and prepares the agent for verification.

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. Current directory must be a git repository
3. (Optional but recommended) Feature has been planned — `prd.json` exists for the feature so the agent can derive the happy path

## When to Use

Invoke this command when you want to:

- Verify a feature or fix after implementation (by hand or after `/execute-feature`)
- Walk through the happy path with the agent’s guidance (locally or against a remote environment, e.g. a droplet reached via SSH)
- Have the agent interpret backend/frontend logs after you exercise a flow (logs may be local or on a remote host)
- Align the agent with branch, changes, and the feature in progress before verification

## How It Works

### 1. Catch Up (Branch and Changes)

Do the same scope gathering as the **Catchup** command so the agent knows where things stand:

**Determine base branch**

- Prefer `master`, else `main`, else default from `origin/HEAD` or ask the user
- Record current branch name (used later to infer feature)

**Gather change scope (order matters)**

**First — Uncommitted changes (staged and unstaged):**

```bash
git status
git diff HEAD
```

- Treat as the “right now” snapshot

**Second — Branch vs base:**

```bash
git log {base}..HEAD --oneline
git diff {base}..HEAD
```

- Full scope of the branch compared to integration branch

**Synthesize**

- In a few short paragraphs, describe what these changes are trying to achieve (branch name, paths, diffs, commit messages)
- Call out uncertainty: e.g. “Branch suggests feature X; changes look like Y.”
- Note implications: product/design, risks/follow-ups, technical (as in catchup)

### 2. Resolve the Feature

Identify which feature (or fix) we’re verifying:

- **If user passed a feature name** (e.g. `@verify-feature login-form`): use that.
- **Else**: try to infer from the current branch:
  - Common patterns: `feature/{name}`, `fix/{name}`, `feat/{name}` → use `{name}` as feature name.
  - If branch is `main`/`master` or doesn’t match a pattern, either ask the user which feature to verify or leave feature “unknown” and continue with catch-up + project context only.
- **Resolve feature docs** using the same convention as execute-feature:
  - **In-repo (preferred)**: `docs/feature/{feature_name}/prd.json`
  - **Home docs**: `~/docs/{project-name}/feature/{feature_name}/prd.json`
- If no `prd.json` exists for the resolved name, say so and continue without feature-specific happy path or story list; you can still use project context and catch-up.

### 3. Load Feature Context (Happy Path and Flows)

When feature docs exist, load:

- **PRD** — `docs/feature/{feature_name}/PRD.md` or `~/docs/{project-name}/feature/{feature_name}/PRD.md`
- **prd.json** — user stories, acceptance criteria, dependencies, types (backend/frontend/fullstack)
- **SPECS** — `docs/feature/{feature_name}/SPECS.md` or `~/docs/{project-name}/feature/{feature_name}/SPECS.md`
- **Optional**: `UX-SPECS.md` / `ux-specs.json` if present (helps for front-end flows)

**Derive the happy path**

- From `prd.json`: for each user story (respecting dependency order), list:
  - Story id and title
  - Acceptance criteria as **verification steps** (what “working” looks like)
- If the PRD or SPECS has an explicit “Verification” or “Happy path” section, use that to order or refine steps.
- Produce a short, ordered list: “To verify this feature, you would: 1) … 2) …” so the user (or agent) can walk through it.

**Optional: feature-level verification doc**

- If `docs/feature/{feature_name}/VERIFY.md` or `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` exists, read it.
- It may define: happy path steps, log locations, expected log patterns, and environment (local/staging). Use it to enrich the agent’s awareness.

### 4. Be Aware of Logs (Backend and Frontend)

The agent should know where logs for this feature’s flows will appear:

**From project and feature docs**

- **TECH-STACK.md** — runtime (Node, Rust, etc.), servers, frontend framework; optional **Environments** section with hostnames and SSH per environment (local, staging, production). Use to infer:
  - Backend: where app logs go (stdout, file, log aggregation)
  - Frontend: browser console, network tab, optional logging service
  - Staging/production: which hostnames and SSH commands to use for remote verification
- **SPECS.md** (and VERIFY.md if present) — APIs, services, and flows involved; mention any documented log locations or correlation ids.
- **PRD / user stories** — which flows touch backend vs frontend so the agent knows which logs matter for which step.

**From code (lightweight inference)**

- If helpful and quick: infer backend log location (e.g. server entrypoint, middleware, or config that sets log destination).
- For frontend: “Check browser console and network tab for the pages/APIs involved in this feature.”

**Rust backends (when TECH-STACK or code indicates Rust)**

- Logs usually go to **stdout** (e.g. `tracing` + `tracing-subscriber`, or `log` + `env_logger`). No default log file unless configured.
- **Where to look:** `docker compose logs -f <service>` or terminal running `cargo run` / `cargo watch -x run`; no extra “log file” unless the app or deployment writes one.
- **What to look for:** Structured fields (e.g. `request_id`, `span`, `method`, `path`, `status`), HTTP status in access logs, and **typed error variants** in error logs (from `thiserror`), not raw panics.
- **Correlation:** If the backend logs a `request_id` or trace id, mention it so the user can correlate frontend requests with backend lines.
- **cargo-watch:** If the project uses cargo-watch (e.g. in Docker), avoid suggesting manual `cargo run` for “see logs”; point to container/process stdout instead.

**Remote verification (staging or production, e.g. droplet via SSH)**

- The backend (or full stack) may run **remotely** — **staging** or **production** — e.g. a Docker setup on a DigitalOcean droplet or other host reachable via SSH.
- **Hostnames per project:** Each project can document hostnames (and SSH targets) for each environment in one place, e.g. `~/docs/{project-name}/TECH-STACK.md` in an **Environments** section, or a dedicated project doc. Document at least: **Local** (optional), **Staging** (URL + SSH if used), **Production** (URL + SSH if used). The agent should read that to know which hostname/SSH to use for “verify on staging” vs “verify on production.”
- **Where to look for logs:** On the remote host, e.g. `ssh user@staging.example.com 'docker compose logs -f api'`. VERIFY.md (feature-level) or TECH-STACK (project-level) may document the SSH target and service names per environment.
- **Happy path:** Verification can be run against the remote API or UI (staging or production URL). Use project-documented hostnames for the chosen environment.
- **Summarize for the agent:** When remote: state **which environment** (staging vs production) and “Backend logs: `ssh … 'docker compose logs -f <service>'` per project TECH-STACK/Environments; happy path: hit <staging or production URL>.”

**Summarize for the agent**

- **Backend**: e.g. “Backend logs: stdout of process X” or “Log file Y” or “Check TECH-STACK / SPECS for log location.” For Rust: “Backend logs: stdout (docker logs / cargo run); look for request_id, status, and error variants.” If remote (staging or production): “Backend logs: see TECH-STACK Environments for hostname/SSH; e.g. `ssh user@<staging-or-prod-host> 'docker compose logs -f <service>'`; happy path: use project-documented URL for that environment.”
- **Frontend**: e.g. “Frontend: browser console and network tab for [relevant routes/APIs].”
- If the user says they exercised a flow, the agent will use this to suggest where to look and how to interpret what they paste.

### 5. Produce the Verify-Feature Summary

Structure the reply so the agent (and user) can use it for verification and log-checking.

**Suggested output structure:**

```markdown
## Verify Feature

### What we’re on
- **Branch:** {current_branch}
- **Feature (inferred or given):** {feature_name} {or "unknown — specify which feature to verify"}
- **What’s changed (catch-up):** [2–4 short paragraphs: branch + uncommitted changes, intent, implications]

### Happy path (how to confirm it works)
[Ordered list of verification steps derived from acceptance criteria / PRD / VERIFY.md]

### Where to look for logs
- **Backend:** [brief line per relevant service/log location]
- **Frontend:** [browser console, network tab, relevant routes/APIs]

### How we can verify
- **Option A — You run the happy path:** I can guide you step-by-step; you tell me what you see (or paste UI/errors).
- **Option B — You exercised a flow:** Tell me what you did (e.g. “I logged in and opened the dashboard”). I’ll tell you what log lines or responses to look for and help interpret what you paste.

What do you want to do first?
```

Keep the “What we’re on” and “Happy path” sections concise so the user can skim and the agent can reuse them in follow-up.

### 6. Ready for User-Driven Verification

After the summary, the agent should:

- Be ready to **guide the user step-by-step** through the happy path (e.g. “Click X, then Y; you should see Z”).
- Be ready to **interpret evidence** when the user says they exercised a flow:
  - Ask for or use: backend log excerpts, frontend console/network output, screenshots, or error messages.
  - Map that back to the happy path and acceptance criteria: did the flow succeed? If not, which step failed and what do the logs say?
- Prefer **evidence-based** interpretation (see `rules/system.md`): use logs and user-provided output to confirm or correct the picture.

Do not modify git state or project files; only read and reason.

## Output Format

Use the structure in **§ 5** (Verify-Feature Summary). Optionally add:

- **Implications** (from catch-up): product/design, risks/follow-ups, technical — if relevant to verification.
- **Next:** One sentence offering to guide the happy path or interpret logs after the user exercises a flow.

## Example Usage

```
@verify-feature
```

```
@verify-feature login-form
```

```
@verify-feature I'm on feature/payment-flow, want to verify the checkout path and check backend logs
```

## Quality Checklist

Before finalizing, ensure:

- [ ] Uncommitted changes were inspected first, then branch vs base (same as catchup)
- [ ] Base branch is correct (main/master or user-specified)
- [ ] Feature name is resolved (from argument or branch) or clearly stated as unknown
- [ ] If feature docs exist: happy path is derived from acceptance criteria (and VERIFY.md if present)
- [ ] Log locations (backend/frontend) are stated briefly from TECH-STACK / SPECS / VERIFY / code
- [ ] Reply ends with an explicit offer: guide happy path or interpret logs after user exercises a flow
- [ ] No files, branches, or git state were modified (read-only)

## Related Commands

- **catchup** — Same branch/change context, no feature happy path or log focus; use when you only need “what’s going on.”
- **execute-feature** — Implements the feature; run verify-feature after (or instead of) to confirm behavior.
- **diagnose-issue** — When verification fails or something is broken, use for hypothesis-driven diagnosis.
- **check-health** — System-level health checks; verify-feature is feature-flow and log verification.
