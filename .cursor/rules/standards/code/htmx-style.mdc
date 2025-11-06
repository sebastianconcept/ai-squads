# htmx Code Style Standards

## General Principles

- Prefer progressive enhancement
- Keep HTML semantic and accessible
- Use htmx attributes for dynamic behavior
- Maintain server-side rendering when possible

## HTML Structure

- Use semantic HTML elements
- Keep markup clean and readable
- Structure forms logically
- Use proper form attributes (name, id, etc.)

## htmx Attributes

- Use `hx-get`, `hx-post`, `hx-put`, `hx-delete` appropriately
- Specify `hx-target` explicitly when not swapping self
- Use `hx-swap` to control how content is inserted
- Use `hx-trigger` for custom event handling

## Common Patterns

- Use `hx-boost="true"` for progressive enhancement of links
- Use `hx-swap-oob` for out-of-band swaps
- Use `hx-sync` for request synchronization
- Use `hx-vals` for dynamic values

## Form Handling

- Use `hx-post` or `hx-put` for form submissions
- Handle form validation on server
- Use `hx-target` to show validation errors
- Return appropriate HTTP status codes

## Loading States

- Use `hx-indicator` for loading indicators
- Show loading states clearly
- Handle errors gracefully
- Provide user feedback

## Events

- Use htmx events (`htmx:beforeRequest`, `htmx:afterSwap`, etc.)
- Handle errors with `htmx:responseError`
- Use custom events when needed
- Keep event handlers minimal

## Server Integration

- Return HTML fragments, not full pages
- Use appropriate HTTP status codes
- Handle redirects properly
- Return partial content for swaps

## Accessibility

- Maintain keyboard navigation
- Ensure screen reader compatibility
- Use ARIA attributes when needed
- Test with assistive technologies

## Performance

- Minimize payload sizes
- Use `hx-swap="none"` when no swap needed
- Consider `hx-push-url` for URL updates
- Cache appropriately

## Best Practices

- Keep htmx attributes readable
- Document complex interactions
- Test across browsers
- Degrade gracefully without JavaScript
