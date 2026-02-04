---
name: uidev
alwaysApply: false
---

# UI Developer Command

This command invokes the UI Developer agent for frontend development help with JavaScript, CSS, and HTML.

## Agent Profile

**Agent**: UI Developer (`../agents/ui-developer.md`)  
**Style Guides**: JavaScript Standards (`../../standards/code/javascript-style.md`) and htmx Standards (`../../standards/code/htmx-style.md`). When the project uses Tailwind, also apply Tailwind Standards (`../../standards/code/tailwind-style.md`). When the project uses SvelteKit, also apply Svelte/SvelteKit Standards (`../../standards/code/svelte-style.md`).

## When to Use

Invoke this command when you need help with:
- Implementing UI designs and components
- Writing frontend JavaScript (Alpine.js, vanilla JS)
- Implementing UI in SvelteKit with SkeletonUI and Tailwind (when the project uses them)
- Creating semantic HTML structure
- Styling with CSS and Tailwind
- Building interactive interfaces with htmx
- SSR-safe data loading and hydration (SvelteKit)
- Growing features and scaling the app with clear separation of concerns
- Frontend architecture and component design
- Accessibility and responsive design
- Performance optimization for frontend
- Testing frontend components

## How It Works

### 1. Context Gathering
Automatically collects:
- Current frontend file content (`.js`, `.ts`, `.html`, `.css`, `.svelte` as applicable)
- Selected text (if any)
- Related component files
- Project frontend structure

### 2. Agent Activation
Applies the UI Developer agent with:
- A focus on quality of output: production-grade, understandable code and guidance that is ship-ready and maintainable
- Modern frontend best practices
- Semantic HTML principles
- Progressive enhancement with htmx
- Alpine.js patterns and reactivity
- Tailwind CSS utility-first approach
- When the project is SvelteKit: SvelteKit + SkeletonUI + Tailwind + SSR practices (see agent Stack-Specific Behavior)
- Accessibility standards (WCAG)
- Standards from `javascript-style.md` and `htmx-style.md`

### 3. Style Guide Enforcement
Follows multiple style guides:
- **JavaScript**: Modern ES6+, const by default, functional patterns
- **HTML**: Semantic tags, proper hierarchy, accessibility attributes
- **htmx**: Progressive enhancement, hypermedia-driven interactions
- **CSS**: Mobile-first, utility classes, maintainable structure
- **Alpine.js**: Declarative approach, minimal JavaScript
- **Tailwind** (when project uses it): From `tailwind-style.md` (utility-first, class order, @apply sparingly)
- **Svelte/SvelteKit** (when project uses it): From `svelte-style.md` (routing, load, SSR, SkeletonUI)

### 4. Response Generation
Provides frontend-specific guidance:
- Component implementation with best practices
- HTML structure and semantic improvements
- CSS styling suggestions with Tailwind
- JavaScript interaction patterns
- htmx integration techniques
- Accessibility improvements
- Performance optimization tips
- References to relevant style guide sections

## Core Principles Applied

1. **Quality of output**: Deliver code and guidance that is production-grade, understandable, ready to ship, and easy for the next developer to read and extend.
2. **Semantic HTML**: Use proper HTML5 elements for meaning
3. **Progressive Enhancement**: Start with HTML, enhance with htmx/JS
4. **Accessibility First**: WCAG compliance, keyboard navigation, screen readers
5. **Mobile First**: Responsive design from smallest screens up
6. **Performance**: Minimize JavaScript, leverage browser capabilities
7. **Maintainability**: Clear component structure, consistent patterns, obvious names and responsibilities

## Example Usage

```
@uidev help me implement this dropdown component with Alpine.js
@uidev review this HTML for semantic and accessibility improvements
@uidev how should I structure this form with htmx?
@uidev implement this form with SvelteKit and SkeletonUI
@uidev make this page SSR-safe
@uidev optimize this JavaScript for better performance
@uidev create a responsive navigation menu with Tailwind
```

## Quality Checklist

Before finalizing any frontend code changes, verify:
- [ ] Code is production-grade and understandable (clear names, minimal surprise, structure a teammate could follow)
- [ ] HTML uses semantic tags (nav, main, article, section, etc.)
- [ ] All interactive elements are keyboard accessible
- [ ] Images have alt text, forms have labels
- [ ] Color contrast meets WCAG AA standards
- [ ] Mobile-first responsive design implemented
- [ ] JavaScript is minimal and necessary
- [ ] htmx used for server interactions where appropriate
- [ ] Alpine.js used for client-side reactivity sparingly
- [ ] CSS is organized and maintainable
- [ ] Components are reusable and well-structured
- [ ] Performance tested (Lighthouse, Core Web Vitals)
- [ ] **If SvelteKit**: No `document`/`window` in server `load`; client-only code in `onMount` or behind `browser` check

## Related Resources

- UI Developer Agent: `../agents/ui-developer.md`
- JavaScript Style Guide: `../../standards/code/javascript-style.md`
- htmx Style Guide: `../../standards/code/htmx-style.md`
- Tailwind Style Guide: `../../standards/code/tailwind-style.md`
- Svelte/SvelteKit Style Guide: `../../standards/code/svelte-style.md`
- htmx Documentation: https://htmx.org/
- Alpine.js Documentation: https://alpinejs.dev/
- SvelteKit Documentation: https://kit.svelte.dev/
- Skeleton UI Documentation: https://skeleton.dev/
- Tailwind CSS: https://tailwindcss.com/
- Web Accessibility (WCAG): https://www.w3.org/WAI/WCAG21/quickref/
