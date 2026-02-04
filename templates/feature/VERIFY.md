# Feature Verification

Use this file to document how to verify this feature works and where to look for logs. It is optional; the agent can still derive the happy path from `prd.json` and acceptance criteria.

## Happy path (manual verification)

Ordered steps a human can follow to confirm the feature works:

1. [Step 1 — e.g. "Open /login and enter valid credentials"]
2. [Step 2 — e.g. "Submit form; expect redirect to /dashboard"]
3. [Step 3 — e.g. "On dashboard, confirm welcome message shows user name"]

Add or remove steps to match your feature. Keep steps concrete and observable.

## Logs and evidence

### Backend

- **Where:** [e.g. "stdout of `npm run dev`" or "logs/app.log" or "docker logs api"]
- **What to look for:** [e.g. "POST /auth/login 200" or "log line containing request_id and 'auth success'"]

**Rust backend (if applicable):**

- **Where:** stdout of the Rust process — e.g. `docker compose logs -f api` or the terminal running `cargo run` / `cargo watch -x run`. No log file unless the app or deployment explicitly writes one.
- **What to look for:** Structured log fields (`request_id`, `method`, `path`, `status`), HTTP status in access/request logs, and **error variants** from your `thiserror` types (not panics). Use `request_id` to correlate with frontend or API client requests.

### Frontend

- **Where:** [e.g. "Browser DevTools → Console and Network tab"]
- **What to look for:** [e.g. "No errors in console; GET /me returns 200 and user object"]

### Optional: correlation

- [e.g. "Backend logs request_id; frontend can show it in UI for debugging"]

## Environment

Hostnames and SSH targets for each environment are **documented per project** (e.g. in `~/docs/{project-name}/TECH-STACK.md` under **Environments**). This section can repeat or override them for this feature only.

- **Local:** [any local-only steps or URLs, e.g. http://localhost:8080]
- **Staging:** [e.g. "API: https://api-staging.example.com (see project TECH-STACK for SSH and log command)" or paste staging URL and `ssh … 'docker compose logs -f api'` here]
- **Production:** [e.g. "API: https://api.example.com (see project TECH-STACK for SSH and log command)" or paste production URL and SSH/log command here]

If staging or production run on a remote host (e.g. DigitalOcean droplet via SSH), the project’s TECH-STACK **Environments** table should list, per environment:
- API / app URL (hostname)
- SSH command (e.g. `ssh deploy@staging.example.com`)
- Logs: e.g. `ssh deploy@staging.example.com 'cd /opt/app && docker compose logs -f api'`
- Any VPN, keys, or env vars required
