import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Try to import framework detection, fallback if it fails
let frameworks = { frontend: 'html', mobile: 'html' };
let skipStorybook = { frontend: false, mobile: false };

// Synchronous import for framework detection (if possible)
try {
  // Use dynamic import in async context if needed
  const detectionModule = await import('../scripts/detect-frameworks.js');
  frameworks = detectionModule.frameworks || frameworks;
  skipStorybook = detectionModule.skipStorybook || skipStorybook;
} catch (e) {
  // Silently fallback to defaults
  frameworks = { frontend: 'html', mobile: 'html' };
  skipStorybook = { frontend: false, mobile: false };
}

// Determine primary framework (or support multiple)
const primaryFramework = frameworks.frontend || 'html';
const frameworkMap = {
  'react': '@storybook/react',
  'vue': '@storybook/vue',
  'svelte': '@storybook/svelte',
  'angular': '@storybook/angular',
  'html': '@storybook/html',
};

const frameworkName = frameworkMap[primaryFramework] || '@storybook/html';

export default {
  stories: ['../stories/**/*.stories.@(js|jsx|ts|tsx|vue|svelte)'],
  addons: [
    '@storybook/addon-essentials',
    '@storybook/addon-a11y',
    '@storybook/addon-docs',
  ],
  framework: {
    name: frameworkName,
    options: {}
  },
  // Configure to resolve imports from monorepo packages
  // Adapts to project structure (no packages/ wrapper assumed)
  async viteFinal(config) {
    // Vite configuration for Storybook 7.x
    config.resolve = config.resolve || {};
    config.resolve.alias = {
      ...config.resolve.alias,
      // Support flexible monorepo structures
      '@frontend': path.resolve(projectRoot, 'frontend'),
      '@mobile': path.resolve(projectRoot, 'mobile'),
      // Can add more aliases based on detected project structure
    };
    return config;
  },
  // Webpack fallback for older Storybook versions or if Vite isn't available
  async webpackFinal(config) {
    const projectRoot = path.resolve(__dirname, '../..');
    config.resolve = config.resolve || {};
    config.resolve.alias = {
      ...config.resolve.alias,
      // Support flexible monorepo structures
      '@frontend': path.resolve(projectRoot, 'frontend'),
      '@mobile': path.resolve(projectRoot, 'mobile'),
      // Can add more aliases based on detected project structure
    };
    return config;
  },
};
