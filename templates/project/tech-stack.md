# Tech Stack

## Backend
- [Technology]: [Version/Purpose]
- [Technology]: [Version/Purpose]

## Frontend
- [Technology]: [Version/Purpose]
- [Technology]: [Version/Purpose]
- For SvelteKit projects, list e.g. SvelteKit, SkeletonUI, Tailwind so the UI Developer agent and Storybook use the correct stack.

## Database
- [Database]: [Version/Purpose]

## Infrastructure
- [Tool/Service]: [Purpose]

## Environments (optional)

Document hostnames and SSH targets per environment so verify-feature and agents know where to reach each. Use one place per project (e.g. this file or a dedicated envs doc).

| Environment | API / app URL | SSH (if applicable) | Notes |
|-------------|---------------|--------------------|-------|
| **Local**   | e.g. http://localhost:8080 | — | `docker compose up` or `cargo run` |
| **Staging** | e.g. https://api-staging.example.com | e.g. `ssh deploy@staging.example.com` | Droplet or VM; logs: `ssh … 'docker compose logs -f api'` |
| **Production** | e.g. https://api.example.com | e.g. `ssh deploy@api.example.com` | Same pattern; document only what the team needs |

- **Staging / Production:** When the app runs on a remote host (e.g. DigitalOcean droplet), document the hostname and SSH command so verification and log checks can be run remotely.
- **Per project:** Each project maintains its own hostnames here (or in `~/docs/{project-name}/`); feature-level VERIFY.md can reference “staging” / “production” and point to this section.

## Development Tools
- [Tool]: [Purpose]

## Key Dependencies
- [Dependency]: [Why it's used]

