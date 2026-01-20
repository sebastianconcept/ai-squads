# Dev Notes

> **Location determines scope**: 
> - Root level (`DEV-NOTES.md`): Global project notes, important for entire repository
> - Feature directory (`~/docs/{project-name}/feature/{name}/DEV-NOTES.md`): Feature-specific notes
> - Subdirectory (`DEV-NOTES.md`): Directory-level learnings

## Purpose

Dev notes capture project-specific learnings, patterns, gotchas, and important context that agents should be aware of when working on this project (or feature, or directory).

**Directory-level DEV-NOTES.md**: When working in a directory, check for `DEV-NOTES.md` in that directory or parent directories. Add reusable patterns, gotchas, or module-specific conventions that would help future work in that directory.

## What to Include

**Important**: Before adding to DEV-NOTES.md, check if the knowledge is already in:
- Your agent's style guide (e.g., `rust-style.md`, `javascript-style.md`)
- Your agent's rules and capabilities
- General coding standards and best practices

**Only add NEW learnings** that are NOT already covered in coding standards:

- **Project-specific patterns**: How things are done in this codebase (not general best practices)
- **Gotchas**: Common mistakes or things to watch out for (especially from bugfixes)
- **Bugfix learnings**: Valuable insights from diagnosing and fixing bugs
- **Architecture decisions**: Why certain approaches were chosen for this project
- **Dependencies**: Important dependencies or their quirks specific to this project
- **Configuration**: Important config values or environment variables
- **Testing patterns**: How tests are structured in this project (project-specific)
- **Deployment notes**: Important deployment considerations
- **Integration notes**: How this project integrates with other systems

## Format

Use clear headings and bullet points. Keep it concise but informative.

## Example

```markdown
# Dev Notes

## Database Patterns

- Always use transactions for multi-step operations
- Connection pool size is set to 20 in production
- Use `db.query()` for read operations, `db.execute()` for writes

## API Patterns

- All API routes use `/api/v1/` prefix
- Authentication is handled via Bearer tokens in Authorization header
- Rate limiting: 100 requests per minute per IP

## Gotchas

- The `User` model has a soft-delete, check `deleted_at` before queries
- Redis cache TTL is 5 minutes for user sessions
- File uploads are limited to 10MB

## Testing

- Use `test/fixtures/` for test data
- Run `npm run test:integration` for full test suite
- Mock external APIs in `test/mocks/`

## Deployment

- Staging: `staging.example.com`
- Production: `app.example.com`
- Database migrations run automatically on deploy
```

## Notes

- Dev notes are automatically included in agent prompts during feature execution
- **Check coding standards first**: Only add knowledge that's NOT already in style guides or agent rules
- **Bugfix learnings are especially valuable**: Document learnings from diagnosing and fixing bugs
- Agents should be selective: Only append genuinely NEW, reusable knowledge
- Keep dev notes up to date as the project evolves
- Remove outdated information to keep context relevant
