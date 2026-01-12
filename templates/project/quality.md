# Quality Checks

This file defines the quality check commands that must pass before code is committed. These checks are used across all features in this project.

## Typecheck

```bash
npm run typecheck
```

## Lint

```bash
npm run lint
```

## Format

```bash
npm run format:check
```

## Test

```bash
npm test
```

---

## Examples for Different Tech Stacks

### Rust
```markdown
## Style
```bash
cargo clippy
```

## Unit
```bash
cargo test
```
```

### JavaScript/TypeScript
```markdown
## Typecheck
```bash
npm run typecheck
```

## Lint
```bash
npm run lint
```

## Format
```bash
npm run format:check
```

## Test
```bash
npm test
```
```

### Python
```markdown
## Lint
```bash
ruff check
```

## Format
```bash
ruff format --check
```

## Test
```bash
pytest
```
```
