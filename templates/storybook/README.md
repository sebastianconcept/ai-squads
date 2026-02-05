# Storybook Setup

This directory contains Storybook templates that will be copied to your project when you initialize Storybook.

## What Gets Copied

When you run `~/.cursor/scripts/storybook-init.sh` from your project root, it copies:

- `.storybook/` - Storybook configuration files
- `scripts/` - Framework detection and story generation scripts
- `package.json` - Storybook dependencies
- `stories/` - Directory structure for component stories

## Usage

After initialization:

1. **Install dependencies** (if not done during initialization):
   ```bash
   cd storybook
   npm install
   ```

2. **Start Storybook**:
   ```bash
   cd storybook
   npm run storybook
   ```

3. **Generate stories** (automatically during feature planning, or manually):
   ```bash
   cd storybook
   npm run generate-stories ~/docs/{project-name}/feature/{feature-name} frontend
   ```

## Framework Detection

Storybook automatically detects your framework:
- Checks `TECH-STACK.md` for explicit framework
- Checks `package.json` for framework dependencies
- Checks file extensions (.jsx, .vue, .svelte)
- Defaults to HTML if no framework detected

## Documentation

See `~/docs/{project-name}/STORYBOOK.md` for complete documentation.
