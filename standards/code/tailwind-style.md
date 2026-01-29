# Tailwind CSS Code Style Standards

Apply this standard when the project uses Tailwind CSS. See official [Tailwind CSS](https://tailwindcss.com/docs) documentation for details.

## General Principles

- **Utility-first**: Prefer Tailwind utility classes over custom CSS. Use custom CSS or `@apply` only when utilities would be repeated many times or when design tokens require it.
- **Readability**: Keep class lists readable. Use consistent order; break long lists across lines when it helps.
- **Design tokens**: When the project has a design system (e.g. SkeletonUI, custom theme), prefer its tokens/variables in `tailwind.config` over arbitrary values where possible.

## Class Order

Use a consistent order so classes are easy to scan and merge:

1. Layout (display, position, flex/grid)
2. Sizing (width, height, min/max)
3. Spacing (margin, padding)
4. Typography (font, text, leading)
5. Visual (background, border, shadow)
6. Effects (opacity, transition)
7. Responsive/screen variants last (e.g. `md:`, `lg:`)

Example: `flex w-full max-w-md p-4 text-sm font-medium bg-white border rounded shadow-md hover:shadow-lg md:p-6`.

## When to Use @apply

- Use `@apply` sparingly: only for repeated patterns (e.g. a button variant used in many places) or when component-scoped style blocks need to reuse utilities.
- Prefer utility classes in markup for one-off styling. Avoid large `@apply` blocks that duplicate what could be inline utilities.
- Keep `@apply` in component-scoped style blocks (e.g. `<style>` in Svelte/Vue, CSS modules) so styles stay local.

## Responsive and Variants

- **Mobile-first**: Write base (unprefixed) styles for the smallest screen; add `sm:`, `md:`, `lg:` etc. for larger breakpoints.
- Use the project’s breakpoints (default or from `tailwind.config`); avoid arbitrary values for breakpoints unless the design requires it.
- Prefer standard variant order: responsive (`sm:`, `md:`), state (`hover:`, `focus:`, `disabled:`), dark (`dark:`).

## Custom Config

- Put project-specific theme values (colors, fonts, spacing) in `tailwind.config.js` (or equivalent) so they are reused and named consistently.
- Extend the default theme rather than replacing it when you only need a few overrides.

## With Design Systems

When the project uses a component library with its own theme (e.g. SkeletonUI):

- Prefer the library’s design tokens and Tailwind plugin/theme integration over ad-hoc colors and spacing.
- Use Tailwind for layout, spacing, and overrides; use the library’s components and tokens for buttons, inputs, and other UI primitives.
