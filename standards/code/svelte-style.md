# Svelte and SvelteKit Code Style Standards

Apply this standard when the project uses SvelteKit (and optionally SkeletonUI, Tailwind). See official [SvelteKit](https://kit.svelte.dev/) and [Skeleton UI](https://skeleton.dev/) documentation for details.

## Svelte

- **Component structure**: Single-file components (`.svelte`); keep script, markup, and style in clear order
- **Reactivity**: Use `$:` for derived state; use stores (`writable`, `readable`, `derived`) for shared or cross-component state
- **Slots**: Use named and default slots for composition; keep slot APIs minimal
- **Events**: Use `createEventDispatcher` or callback props for parent communication; prefer explicit event names

## SvelteKit

- **Routing**: Use file-based routing; `+page.svelte` for pages, `+layout.svelte` for layouts
- **Load**: Put data loading in `+page.server.ts` or `+layout.server.ts`; use `load` for server-side data; avoid `document`/`window` in `load`
- **Form actions**: Use form actions in `+page.server.ts` for mutations; use progressive enhancement where possible
- **$app modules**: Use `$app/navigation`, `$app/stores`, `$app/environment` as needed; respect `browser` when branching client-only logic

## SSR (Server-Side Rendering)

- **Server load**: No browser globals (`document`, `window`, `localStorage`) in server `load` or in code that runs during SSR
- **Client-only logic**: Use `onMount` or guard with `browser` from `$app/environment` for code that must run only in the browser
- **Server-only data**: Do not serialize or pass server-only secrets or internals to the client; only expose data that is safe for the client

## SkeletonUI (when the project uses it)

- Use SkeletonUI components and theming as the default UI layer
- Use Tailwind for layout, spacing, and overrides; prefer SkeletonUI tokens/variables where they exist, then Tailwind utilities

## Tailwind

- See `tailwind-style.md` for Tailwind conventions. In SvelteKit + SkeletonUI projects: prefer SkeletonUI design tokens/variables, then Tailwind classes; keep class lists readable; use `@apply` sparingly.

## Separation of Concerns and Scaling

- **Thin pages/layouts**: Keep `+page.svelte` and `+layout.svelte` thin; delegate UI to components in `$lib` or route-local components
- **Server logic**: Put server-only logic in `+page.server.ts` / `+layout.server.ts` and form actions; do not mix server logic into `.svelte` files
- **Layout hierarchy**: Use layout hierarchy for shared UI and shared data; nest layouts where structure repeats
- **Co-locate route files**: Keep `+page.svelte`, `+page.server.ts`, and route-specific components together
- **Shared code**: Use `$lib` (or equivalent) for shared, cross-route code (components, utilities, types) so features grow without tangling
