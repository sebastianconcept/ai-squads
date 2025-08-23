# CSS/Styling Tech-Stack Standards

## Context

Global preferences on libraries and development standards for SquadsAI Elite squad projects using modern CSS and styling solutions for responsive, accessible, and performant web applications.

## Technology Stack Preferences

### For CSS/Styling

- **Preprocessors**: Sass/SCSS for advanced styling capabilities
- **CSS-in-JS**: Styled-components, Emotion for component-based styling
- **Frameworks**: Tailwind CSS for utility-first, Bootstrap for rapid prototyping
- **Methodologies**: BEM, CSS Modules, CSS Custom Properties
- **Build Tools**: PostCSS with autoprefixer, CSS minification
- **Responsive Design**: Mobile-first approach, CSS Grid and Flexbox
- **Performance**: Critical CSS extraction, lazy loading of non-critical styles
- **Accessibility**: High contrast ratios, focus management, screen reader support

## Code Style and Standards

### CSS Architecture
- **Component-Based**: Organize CSS by components and features
- **Utility-First**: Use utility classes for common patterns
- **Custom Properties**: Leverage CSS custom properties for theming
- **Scoped Styles**: Use CSS Modules or scoped styling to prevent conflicts
- **Consistent Naming**: Follow established naming conventions

### CSS Methodologies

#### BEM (Block Element Modifier)
- **Block**: Standalone entity (e.g., `.card`)
- **Element**: Parts of a block (e.g., `.card__title`)
- **Modifier**: Different states or versions (e.g., `.card--featured`)

#### CSS Modules
- **Local Scope**: Styles are automatically scoped to components
- **Composition**: Use composition for shared styles
- **Naming**: Use descriptive class names without BEM complexity

#### Utility-First (Tailwind CSS)
- **Atomic Classes**: Use single-purpose utility classes
- **Responsive Variants**: Leverage responsive prefixes (sm:, md:, lg:)
- **Custom Utilities**: Extend with project-specific utilities
- **Component Extraction**: Extract common patterns into components

### Naming Conventions
- **Classes**: kebab-case (e.g., `user-profile`, `button-primary`)
- **Custom Properties**: kebab-case with descriptive names (e.g., `--color-primary`)
- **Breakpoints**: Use semantic names (e.g., `--breakpoint-tablet`)
- **Spacing Scale**: Use consistent spacing scale (e.g., `--spacing-4`)

## Project Structure Standards

### Directory Layout
```
styles/
├── base/                   # Base styles and resets
│   ├── _reset.scss        # CSS reset/normalize
│   ├── _typography.scss   # Typography rules
│   └── _variables.scss    # Global variables
├── components/             # Component-specific styles
│   ├── _buttons.scss      # Button styles
│   ├── _forms.scss        # Form styles
│   └── _navigation.scss   # Navigation styles
├── layouts/                # Layout and grid styles
│   ├── _header.scss       # Header layout
│   ├── _footer.scss       # Footer layout
│   └── _grid.scss         # Grid system
├── utilities/              # Utility classes
│   ├── _spacing.scss      # Spacing utilities
│   ├── _colors.scss       # Color utilities
│   └── _typography.scss   # Typography utilities
├── themes/                 # Theme variations
│   ├── _light.scss        # Light theme
│   └── _dark.scss         # Dark theme
└── main.scss              # Main entry point
```

### File Organization
- **Modular Structure**: Separate concerns into logical files
- **Import Order**: Import base styles first, then components, then utilities
- **Naming**: Use descriptive file names with underscore prefix for partials
- **Index Files**: Create index files for easy importing

## Responsive Design Standards

### Mobile-First Approach
- **Base Styles**: Start with mobile styles as the foundation
- **Progressive Enhancement**: Add complexity for larger screens
- **Breakpoint Strategy**: Use consistent breakpoint system
- **Touch Targets**: Ensure adequate touch target sizes (44px minimum)

### Breakpoint System
```scss
// Mobile-first breakpoints
$breakpoints: (
  'small': 576px,
  'medium': 768px,
  'large': 992px,
  'xlarge': 1200px,
  'xxlarge': 1400px
);
```

### CSS Grid and Flexbox
- **Layout Grid**: Use CSS Grid for page-level layouts
- **Component Layout**: Use Flexbox for component-level layouts
- **Responsive Grid**: Implement responsive grid systems
- **Grid Areas**: Use named grid areas for complex layouts

## Performance Standards

### CSS Optimization
- **Critical CSS**: Extract and inline critical CSS for above-the-fold content
- **Lazy Loading**: Load non-critical CSS asynchronously
- **Minification**: Compress CSS in production builds
- **Tree Shaking**: Remove unused CSS rules
- **Caching**: Implement proper CSS caching strategies

### Build Tools
- **PostCSS**: Use PostCSS for autoprefixing and optimization
- **CSS Modules**: Enable CSS Modules for scoped styling
- **PurgeCSS**: Remove unused CSS in production builds
- **Critical**: Extract critical CSS for performance optimization

## Accessibility Standards

### Visual Accessibility
- **Color Contrast**: Maintain minimum 4.5:1 contrast ratio for normal text
- **Focus Indicators**: Provide clear focus indicators for keyboard navigation
- **Text Scaling**: Ensure text remains readable at 200% zoom
- **High Contrast**: Support high contrast mode preferences

### Screen Reader Support
- **Semantic HTML**: Use semantic HTML elements for proper structure
- **ARIA Labels**: Provide ARIA labels for complex components
- **Skip Links**: Implement skip links for keyboard navigation
- **Focus Management**: Manage focus for dynamic content

## Component Design

### Button Components
- **States**: Design for default, hover, active, and disabled states
- **Variants**: Create primary, secondary, and tertiary button styles
- **Sizes**: Provide consistent sizing options (small, medium, large)
- **Accessibility**: Ensure proper focus and keyboard support

### Form Components
- **Input Styles**: Consistent styling for form inputs
- **Validation States**: Visual feedback for validation states
- **Error Messages**: Clear error message styling and positioning
- **Focus States**: Obvious focus indicators for form elements

### Navigation Components
- **Responsive Menu**: Mobile-friendly navigation patterns
- **Breadcrumbs**: Clear breadcrumb navigation styling
- **Pagination**: Accessible pagination component styles
- **Tab Navigation**: Tab-based navigation with proper ARIA support

## Theme System

### CSS Custom Properties
- **Color Palette**: Define consistent color variables
- **Spacing Scale**: Use consistent spacing variables
- **Typography Scale**: Define typography scale variables
- **Breakpoint Variables**: Store breakpoint values as variables

### Theme Switching
- **Light/Dark Themes**: Implement theme switching capability
- **CSS Variables**: Use CSS custom properties for theme values
- **Media Queries**: Support system preference for theme selection
- **Smooth Transitions**: Provide smooth transitions between themes

## Testing and Quality Assurance

### Cross-Browser Testing
- **Browser Support**: Test across major browsers and versions
- **Progressive Enhancement**: Ensure graceful degradation
- **Vendor Prefixes**: Use autoprefixer for vendor prefix management
- **Fallbacks**: Provide fallbacks for modern CSS features
- **Browser DevTools**: Use browser developer tools for debugging
- **Visual Regression**: Implement visual regression testing
- **Accessibility Testing**: Test with screen readers and keyboard navigation

### Performance Testing
- **CSS Size**: Monitor CSS bundle size
- **Render Performance**: Test CSS rendering performance
- **Critical Path**: Optimize critical rendering path
- **Lighthouse**: Use Lighthouse for performance auditing
- **CSS Profiling**: Use browser CSS profiling tools
- **Bundle Analysis**: Analyze CSS bundle composition
- **Loading Performance**: Measure CSS loading and parsing time

## Integration Patterns

### With JavaScript Frameworks
- **React**: Use styled-components or CSS Modules
- **Vue**: Use scoped styles or CSS Modules
- **Angular**: Use component styles or CSS Modules
- **Svelte**: Use component styles with CSS custom properties

### With Build Tools
- **Webpack**: Configure CSS loaders and optimization
- **Vite**: Use built-in CSS support and optimization
- **Parcel**: Leverage automatic CSS processing
- **Rollup**: Configure CSS plugins for bundling
