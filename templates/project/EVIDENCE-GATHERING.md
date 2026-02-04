# Evidence Gathering

Verification is always a form of **evidence gathering**. To confirm that something works (or to diagnose when it doesn’t), we need first-hand evidence: logs, observable steps, and a clear picture of where the system runs (local, staging, production).

This doc ties together where and how this project gathers evidence. See also the system rule **Evidence and Environments** (in `rules/system.md`).

## Project-level: where the system runs

**TECH-STACK.md** — If it has an **Environments** section, that’s the single place for:

- **Local** — URLs or commands (e.g. `docker compose up`, `cargo run`)
- **Staging** — Hostname, API/app URL, SSH command, and how to fetch logs (e.g. `ssh user@staging.example.com 'docker compose logs -f api'`)
- **Production** — Same for production

Use those hostnames and SSH commands whenever you need to fetch logs or run checks remotely (e.g. for `verify-feature`, `diagnose-issue`, `check-health`).

## Feature-level: how to verify each feature

Each feature can have **VERIFY.md** in its feature directory:

- `~/docs/{project-name}/feature/{feature_name}/VERIFY.md` (home docs)
- or `docs/feature/{feature_name}/VERIFY.md` (in-repo)

VERIFY.md documents:

- **Happy path** — Ordered, observable steps to confirm the feature works (derived from acceptance criteria)
- **Logs and evidence** — Where backend and frontend logs are, and what to look for (status codes, request_id, error variants)
- **Environment** — Local / staging / production URLs or SSH; can point to TECH-STACK Environments or repeat for the feature

When you verify a feature or investigate an issue that touches a feature, use that feature’s VERIFY.md for log locations and happy path. When you plan a feature, produce VERIFY.md (from the template) so verification is clear before implementation.

## Summary

| Need | Use |
|------|-----|
| Where does the system run? (hostnames, SSH) | TECH-STACK.md **Environments** |
| How do we verify this feature? (happy path, logs) | Feature **VERIFY.md** |
| First-hand evidence (logs, steps) | Prefer logs and observable steps; use the above for exact commands (local or remote) |
