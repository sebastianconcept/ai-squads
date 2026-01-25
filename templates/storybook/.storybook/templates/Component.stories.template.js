/**
 * Story Template for Manual Override
 * 
 * Use this template when you need to manually create or customize a Storybook story.
 * This is rare - most stories are auto-generated from ux-specs.json during feature planning.
 * 
 * When to use:
 * - Component needs custom story that can't be generated from ux-specs.json
 * - Complex interactions that require manual setup
 * - Testing edge cases not covered by auto-generated stories
 * 
 * Instructions:
 * 1. Copy this file to: storybook/stories/components/{ComponentName}.stories.{js|jsx|vue|svelte}
 * 2. Customize the story content
 * 3. Add "MANUAL EDIT" comment at the top to prevent regeneration from overwriting
 * 
 * Framework: HTML (adjust for React, Vue, Svelte as needed)
 * 
 * Note: Auto-generated stories are marked "DO NOT EDIT MANUALLY" and will be
 * regenerated from ux-specs.json. Manual stories with "MANUAL EDIT" are preserved.
 */

// Framework: HTML (adjust for React, Vue, Svelte as needed)

export default {
  title: 'Components/ComponentName', // Adjust path as needed
  parameters: {
    designSystem: {
      tokens: {
        color: 'primary',      // Adjust based on design system
        spacing: 'spacing-4',   // Adjust based on design system
        typography: 'body'      // Adjust based on design system
      }
    },
    accessibility: {
      level: 'AA', // Adjust based on requirements
    }
  }
};

// Default state
export const Default = {
  render: () => `<div class="component-name">Default State</div>`,
};

// Empty state
export const Empty = {
  render: () => `<div class="component-name empty">Empty State</div>`,
};

// Loading state
export const Loading = {
  render: () => `<div class="component-name loading">Loading...</div>`,
};

// Error state
export const Error = {
  render: () => `<div class="component-name error">Error State</div>`,
};

// Success state
export const Success = {
  render: () => `<div class="component-name success">Success State</div>`,
};

// Add more states as needed
