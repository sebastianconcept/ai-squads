// Framework-specific decorators
const decorators = [];

// Initialize Alpine.js decorator if HTML framework detected
// This runs asynchronously, but decorators array is populated before Storybook uses it
(async () => {
  try {
    const detection = await import('../scripts/detect-frameworks.js');
    const frameworks = detection.frameworks || { frontend: 'html', mobile: 'html' };
    
    // Only add Alpine.js decorator if HTML framework detected
    if (frameworks.frontend === 'html' || frameworks.mobile === 'html') {
      try {
        const AlpineModule = await import('alpinejs');
        const Alpine = AlpineModule.default;
        
        decorators.push((story) => {
          // Clean up any existing Alpine instances
          if (window.Alpine && window.Alpine.stop) {
            window.Alpine.stop();
          }
          
          // Initialize Alpine.js fresh for each story
          window.Alpine = Alpine;
          Alpine.start();
          
          // Return story with Alpine initialized
          const storyElement = story();
          
          // Ensure Alpine processes the story element
          if (storyElement && typeof storyElement === 'object' && storyElement.innerHTML) {
            Alpine.initTree(storyElement);
          }
          
          return storyElement;
        });
      } catch (e) {
        console.warn('Alpine.js not available, skipping Alpine.js decorator:', e.message);
      }
    }
  } catch (e) {
    console.warn('Framework detection failed in preview, continuing without Alpine.js:', e.message);
  }
})();

// React/Vue/Svelte don't need Alpine.js decorator
export { decorators };

// Global parameters (framework-agnostic)
export const parameters = {
  actions: { argTypesRegex: "^on[A-Z].*" },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
  // Accessibility addon configuration
  a11y: {
    config: {
      rules: [
        {
          id: 'color-contrast',
          enabled: true,
        },
        {
          id: 'keyboard-accessibility',
          enabled: true,
        },
      ],
    },
  },
};
