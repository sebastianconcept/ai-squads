# HTML Style Guide

## Context

HTML-specific code style rules for SquadsAI projects using semantic HTML with modern CSS frameworks. This guide aligns with our comprehensive CSS tech stack standards documented in `../tech-stacks/css-styling.md`.

## Structure Rules
- Use 2 spaces for indentation
- Place nested elements on new lines with proper indentation
- Content between tags should be on its own line when multi-line
- Use semantic HTML elements for proper document structure
- Ensure proper heading hierarchy (h1 → h2 → h3)

## Attribute Formatting
- Place each HTML attribute on its own line
- Align attributes vertically
- Keep the closing `>` on the same line as the last attribute
- Use double quotes for attribute values consistently
- Order attributes logically: id, class, data-*, aria-*, event handlers

## Semantic HTML
- Use `<header>`, `<nav>`, `<main>`, `<section>`, `<article>`, `<aside>`, `<footer>`
- Use `<button>` for interactive elements, not `<div>`
- Use `<nav>` for navigation menus
- Use `<main>` for primary content area
- Use `<section>` for thematic grouping of content

## Accessibility
- Include `alt` attributes for all images
- Use `aria-label` for elements without visible text
- Ensure proper focus management with `tabindex`
- Use semantic elements for screen readers
- Implement skip links for keyboard navigation

## CSS Class Integration
- Follow our multi-line Tailwind CSS formatting standards
- Use consistent class naming conventions
- Group related classes logically
- Implement responsive design with proper breakpoints

## Example HTML Structure

```html
<div class="container">
  <header class="flex flex-col space-y-2
                 md:flex-row md:space-y-0 md:space-x-4">
    <h1 class="text-primary dark:text-primary-300">
      Page Title
    </h1>
    <nav class="flex flex-col space-y-2
                md:flex-row md:space-y-0 md:space-x-4"
         role="navigation"
         aria-label="Main navigation">
      <a href="/"
         class="btn-ghost"
         aria-current="page">
        Home
      </a>
      <a href="/about"
         class="btn-ghost">
        About
      </a>
    </nav>
  </header>
  
  <main class="flex-1">
    <section class="prose dark:prose-invert">
      <h2 class="text-2xl font-bold mb-4">
        Section Title
      </h2>
      <p class="text-gray-700 dark:text-gray-300">
        Content goes here with proper semantic structure.
      </p>
    </section>
  </main>
</div>
```

## Form Elements
- Use proper `<form>` elements with `action` and `method`
- Include `<label>` elements for all form controls
- Use appropriate input types (`email`, `tel`, `url`, etc.)
- Implement proper validation attributes
- Use `<fieldset>` and `<legend>` for grouped form controls

## Meta Tags
- Include proper `<meta charset="utf-8">`
- Use viewport meta tag for responsive design
- Include description and keywords meta tags
- Use Open Graph meta tags for social sharing
- Implement proper title tags for each page

## Reference

For comprehensive CSS tech stack standards, styling approaches, and responsive design patterns, see `../tech-stacks/css-styling.md`.
