# Storybook Component Library

This document describes the Storybook setup and usage for this project.

## Overview

Storybook is a tool for building and documenting UI components in isolation. It's integrated into the UX workflow to automatically generate component stories from `ux-specs.json` during feature planning.

**Location**: Storybook is isolated in `storybook/` directory at project root to preserve separation from product code.

## Quick Start

1. **Install dependencies** (if not already installed):
   ```bash
   cd storybook
   npm install
   ```

2. **Start Storybook**:
   ```bash
   cd storybook
   npm run storybook
   ```

3. **View components**: Open http://localhost:6006 in your browser

## Monorepo Structure

Storybook works with flexible monorepo structures:

```
project-root/
  ├── frontend/              # Frontend application code
  ├── backend1/              # Backend service 1 (not used by Storybook)
  ├── mobile/                # Mobile application code (if applicable)
  └── storybook/             # Storybook (isolated, shared tool)
      ├── .storybook/        # Storybook configuration
      ├── stories/           # Component stories
      │   ├── components/    # Shared components
      │   └── packages/     # Package-specific components
      │       ├── frontend/
      │       └── mobile/
      └── package.json       # Storybook dependencies (isolated)
```

**Key Principles**:
- **Isolation**: Storybook lives in its own directory, separate from product code
- **Flexible Structure**: Works with any monorepo structure (no `packages/` wrapper required)
- **Backend Agnostic**: Storybook only integrates with frontend/mobile (doesn't need backend access)
- **Framework-Aware**: Automatically detects framework (React, Vue, Svelte, HTML/Alpine.js) and generates appropriate stories

## Framework Detection

Storybook automatically detects the framework used in each package:

1. **Checks TECH-STACK.md** for explicit framework specification
2. **Checks package.json** for framework dependencies (React, Vue, Svelte, etc.)
3. **Checks file extensions** (.jsx for React, .vue for Vue, .svelte for Svelte)
4. **App-at-root**: When `frontend/` doesn't exist or has no package.json, detection runs on project root (e.g. SvelteKit apps at repo root)
5. **Defaults to HTML** if no framework detected

**Supported Frameworks**:
- HTML/Alpine.js (default)
- React
- Vue
- Svelte
- Angular (easy to add)

**Native/Game Engine Handling**: Storybook is automatically skipped for native iOS/Android apps or game engines (Godot, Unity, etc.). Alternative documentation approaches are used instead.

## Component Organization

### Shared Components

Components used across multiple packages are stored in:
```
storybook/stories/components/
```

### Package-Specific Components

Components specific to a package are stored in:
```
storybook/stories/packages/{package-name}/components/
```

## Automated Story Generation

Stories are automatically generated from `ux-specs.json` during the UX workflow:

1. **During Feature Planning**: After UX-SPECS validation, stories are generated automatically
2. **Framework Detection**: Framework is detected per package
3. **Component Extraction**: Components are extracted from:
   - `passes.affordances.actions` (component interactions)
   - `passes.stateDesign.elements` (component states)
   - `layout.componentComposition` (component relationships)
4. **Story Generation**: Stories are generated in framework-appropriate format:
   - React: JSX format
   - Vue: Vue component format
   - Svelte: Svelte component format
   - HTML: HTML string format

**Manual Regeneration**:
```bash
cd storybook
npm run generate-stories <feature-path> [package-name]
```

**Note**: Generated stories are marked with "DO NOT EDIT MANUALLY - Auto-generated". If you need to customize a story, mark it with "MANUAL EDIT" to prevent regeneration from overwriting your changes.

## Story Structure

Each component story includes:

- **All component states**: Empty, loading, error, success (from Pass 5: State Design)
- **Design system tokens**: Color, spacing, typography references
- **Accessibility documentation**: WCAG level and requirements
- **Framework-appropriate format**: Matches detected framework

## Design System Integration

Design tokens are automatically extracted from `ux-specs.json` layout specs:

- **Colors**: From `layout.visualHierarchy.colorUsage`
- **Spacing**: From `layout.spacingRhythm`
- **Typography**: From `layout.typographyHierarchy`
- **Grid System**: From `layout.gridSystem`

## Accessibility Testing

The `@storybook/addon-a11y` addon is configured to automatically test accessibility:

- **WCAG Compliance**: Tests against WCAG AA standards (configurable)
- **Keyboard Navigation**: Verifies keyboard accessibility
- **Screen Reader**: Checks ARIA labels and roles
- **Color Contrast**: Validates color contrast ratios

Accessibility level is extracted from `technicalConstraints.accessibilityLevel` in `ux-specs.json`.

## Alpine.js Integration

For HTML/Alpine.js projects, Alpine.js is automatically initialized in Storybook:

- **Auto-initialization**: Alpine.js starts automatically for each story
- **Cleanup**: Alpine instances are cleaned up between stories
- **Component Support**: Alpine.js components work seamlessly in stories

## htmx Handling

htmx interactions are documented visually in Storybook:

- **Visual States Only**: Stories show visual component states (static HTML)
- **htmx Attributes Documented**: Include htmx attributes in code examples for reference
- **Separate Testing**: Test actual htmx interactions in the application, not in Storybook

**Note**: Storybook doesn't run a server, so htmx interactions won't work. Use Storybook for visual documentation, test htmx in the application.

## Adding New Components

### Automatic (Recommended)

Stories are automatically generated during feature planning. No manual work required.

### Manual (Rare)

If you need to manually create or customize a story:

1. **Create story file** in appropriate location:
   - Shared: `storybook/stories/components/{ComponentName}.stories.{js|jsx|vue|svelte}`
   - Package-specific: `storybook/stories/packages/{package-name}/components/{ComponentName}.stories.{js|jsx|vue|svelte}`

2. **Use framework-appropriate format** (see story templates in plan)

3. **Mark with "MANUAL EDIT"** to prevent regeneration from overwriting

## Troubleshooting

### Storybook Won't Start

- **Check dependencies**: Run `npm install` in `storybook/` directory
- **Check framework detection**: Run `npm run detect-frameworks` to see detected frameworks
- **Check configuration**: Verify `storybook/.storybook/main.js` is correct

### Stories Don't Load

- **Check import paths**: Verify component import paths are correct
- **Check component exists**: Ensure component file exists in package
- **Check framework compatibility**: Verify story format matches detected framework

### Framework Detection Wrong

- **Manually specify**: Add framework to `TECH-STACK.md`:
  ```
  Framework: react
  ```
- **Check package.json**: Verify framework dependencies are listed

### Story Generation Fails

- **Check logs**: Review error messages in console
- **Check ux-specs.json**: Verify file is valid JSON and follows schema
- **Continue workflow**: Story generation failure doesn't block feature planning (workflow continues)

### Component Import Fails

- **Fix import path**: Update import path in generated story
- **Component doesn't exist yet**: Story will work once component is implemented (placeholder is fine)

## Error Handling & Fallbacks

Storybook integration includes comprehensive error handling:

- **Framework detection failure**: Defaults to HTML, logs warning, continues workflow
- **Story generation failure**: Logs error, continues workflow (doesn't block feature planning)
- **Component import failure**: Generates story with placeholder, documents issue in comments
- **Storybook setup failure**: Skips Storybook generation, documents components in markdown instead
- **Native/game engine**: Automatically skips Storybook, documents alternative approach

**Key Principle**: Storybook is helpful but not blocking. Workflow continues even if Storybook has issues.

## Migration for Existing Projects

To add Storybook to an existing project:

1. **Framework detection works automatically** for existing projects
2. **Story generation can be run retroactively** for existing features:
   ```bash
   cd storybook
   npm run generate-stories ~/docs/{project-name}/feature/{feature-name} frontend
   ```
3. **Generate incrementally**: Start with new features, add existing features over time

## Advanced Features (Post-MVP, Optional)

These features can be added if needed:

- **Visual regression testing**: Chromatic, Percy integration
- **Component usage analytics**: Track component usage across projects
- **Design system token visualization**: Visualize design tokens in Storybook
- **Component versioning**: Version components and track changes

## Related Documentation

- **UX Workflow**: See `definitions/commands/plan-feature.md` for UX workflow integration
- **Story Generation**: See `storybook/scripts/generate-stories.js` for generation logic
- **Framework Detection**: See `storybook/scripts/detect-frameworks.js` for detection logic
- **Agent Profiles**: See `definitions/agents/ui-developer.md` and `definitions/agents/ui-ux.md` for Storybook integration

## Support

For issues or questions:
1. Check troubleshooting section above
2. Review error logs
3. Check framework detection output: `npm run detect-frameworks`
4. Verify Storybook configuration: `storybook/.storybook/main.js`
