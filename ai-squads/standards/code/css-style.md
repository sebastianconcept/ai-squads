# CSS Style Guide

## Context

CSS-specific code style rules for SquadsAI projects using modern CSS and styling solutions. This guide aligns with our comprehensive tech stack standards documented in `../tech-stacks/css-styling.md`.

## Primary Approach: Tailwind CSS

We always use the latest version of TailwindCSS for all CSS projects.

### Multi-line CSS classes in markup

- We use a unique multi-line formatting style when writing Tailwind CSS classes in HTML markup and ERB tags, where the classes for each responsive size are written on their own dedicated line.
- The top-most line should be the smallest size (no responsive prefix). Each line below it should be the next responsive size up.
- Each line of CSS classes should be aligned vertically.
- focus and hover classes should be on their own additional dedicated lines.
- We implement one additional responsive breakpoint size called 'xs' which represents 400px.
- If there are any custom CSS classes being used, those should be included at the start of the first line.

**Example of multi-line Tailwind CSS classes:**

```html
<div class="custom-cta bg-gray-50 dark:bg-gray-900 p-4 rounded cursor-pointer w-full
            hover:bg-gray-100 dark:hover:bg-gray-800
            xs:p-6
            sm:p-8 sm:font-medium
            md:p-10 md:text-lg
            lg:p-12 lg:text-xl lg:font-semibold lg:2-3/5
            xl:p-14 xl:text-2xl
            2xl:p-16 2xl:text-3xl 2xl:font-bold 2xl:w-3/4">
  I'm a call-to-action!
</div>
```

## Alternative Approaches

When Tailwind CSS isn't suitable for specific use cases, follow these alternative approaches:

### CSS Modules
- Use for component-scoped styling in React/Vue applications
- Follow the naming convention: `ComponentName.module.css`
- Use descriptive class names without BEM complexity
- Leverage composition for shared styles

### BEM Methodology
- Use for complex component hierarchies when CSS Modules aren't available
- Follow Block__Element--Modifier pattern
- Example: `.card__title--featured`, `.button--primary`

### CSS Custom Properties
- Use for theming and dynamic values
- Follow kebab-case naming: `--color-primary`, `--spacing-4`
- Define in `:root` or component scope as appropriate

### Utility-First with Custom Utilities
- Extend Tailwind with project-specific utilities
- Use consistent naming: `u-text-gradient`, `u-shadow-custom`
- Document all custom utilities in project documentation

## Code Organization

### File Structure
- **Base**: `_base.css` - Reset, typography, variables
- **Components**: `_components.css` - Reusable component styles
- **Utilities**: `_utilities.css` - Custom utility classes
- **Layouts**: `_layouts.css` - Page and section layouts
- **Themes**: `_themes.css` - Light/dark theme variations

### Import Order
```css
/* 1. Base styles */
@import 'base.css';

/* 2. Component styles */
@import 'components.css';

/* 3. Layout styles */
@import 'layouts.css';

/* 4. Utility styles */
@import 'utilities.css';

/* 5. Theme styles */
@import 'themes.css';
```

## Responsive Design

### Mobile-First Approach
- Start with mobile styles as the foundation
- Use progressive enhancement for larger screens
- Ensure touch targets are at least 44px minimum

### Breakpoint Strategy
- **xs**: 400px (custom breakpoint)
- **sm**: 640px (Tailwind default)
- **md**: 768px (Tailwind default)
- **lg**: 1024px (Tailwind default)
- **xl**: 1280px (Tailwind default)
- **2xl**: 1536px (Tailwind default)

## Performance Considerations

### Critical CSS
- Extract and inline critical CSS for above-the-fold content
- Use tools like Critical or PurgeCSS for optimization
- Implement lazy loading for non-critical styles

### Build Optimization
- Use PostCSS with autoprefixer for vendor prefix management
- Implement CSS minification in production builds
- Use PurgeCSS to remove unused CSS rules

## Accessibility Standards

### Color and Contrast
- Maintain minimum 4.5:1 contrast ratio for normal text
- Support high contrast mode preferences
- Ensure text remains readable at 200% zoom

### Focus Management
- Provide clear focus indicators for keyboard navigation
- Implement proper focus management for dynamic content
- Use skip links for keyboard navigation

## Integration with Frameworks

### React
- Use styled-components or CSS Modules with Tailwind
- Leverage Tailwind's utility classes in JSX
- Use CSS-in-JS for dynamic styling when needed

### Vue
- Use scoped styles with Tailwind utilities
- Implement CSS Modules for component isolation
- Use Vue's class binding with Tailwind classes

### Angular
- Use component styles with Tailwind integration
- Implement CSS Modules for scoped styling
- Use Angular's class binding with utility classes

### Svelte
- Use component styles with CSS custom properties
- Integrate Tailwind utilities in component markup
- Leverage Svelte's reactive styling capabilities

## Testing and Quality Assurance

### Visual Regression Testing
- Implement visual regression testing for UI components
- Use tools like Percy or Chromatic for automated testing
- Test across different browsers and screen sizes

### Accessibility Testing
- Test with screen readers and keyboard navigation
- Use automated accessibility testing tools
- Validate color contrast and focus indicators

### Performance Testing
- Monitor CSS bundle size and loading performance
- Use Lighthouse for performance auditing
- Profile CSS rendering performance in browser dev tools

## Reference

For comprehensive CSS tech stack standards, build tools, and advanced patterns, see `../tech-stacks/css-styling.md`.
