/**
 * Storybook Story Generation Script
 * 
 * Automatically generates Storybook component stories from ux-specs.json during
 * the UX workflow. Stories are framework-aware and generated in the appropriate
 * format for the detected framework (React, Vue, Svelte, HTML).
 * 
 * Process:
 * 1. Reads ux-specs.json from feature directory
 * 2. Detects framework for the package (frontend, mobile, etc.)
 * 3. Extracts components from ux-specs.json (affordances, stateDesign, layout)
 * 4. Generates framework-appropriate story files with all component states
 * 5. Includes design tokens, accessibility requirements, and import paths
 * 
 * Usage: 
 *   node scripts/generate-stories.js <feature-path> [package-name]
 * 
 * Example:
 *   node scripts/generate-stories.js ~/docs/my-project/feature/user-auth frontend
 * 
 * Error Handling:
 * - If story generation fails, logs error and continues (doesn't block workflow)
 * - If native/game engine detected, skips Storybook generation
 * - If component doesn't exist yet, generates story with placeholder
 * 
 * Story Organization:
 * - Shared components: storybook/stories/components/
 * - Package-specific: storybook/stories/packages/{package-name}/components/
 * 
 * Files are marked "DO NOT EDIT MANUALLY - Auto-generated" and are regenerated
 * from ux-specs.json. Manual edits can be preserved by marking with "MANUAL EDIT".
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { frameworks, skipStorybook } from './detect-frameworks.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../../');
async function generateStories() {
  const args = process.argv.slice(2);
  const featurePath = args[0] || null;
  const packageName = args[1] || 'frontend';

  try {
    // Find ux-specs.json
    let uxSpecsPath;
    if (featurePath) {
      uxSpecsPath = path.resolve(featurePath, 'ux-specs.json');
    } else {
      // Try common locations
      const homeDir = process.env.HOME || process.env.USERPROFILE || '';
      const projectName = path.basename(projectRoot);
      const commonPaths = [
        path.join(homeDir, 'docs', projectName, 'feature', '*', 'ux-specs.json'),
        path.join(projectRoot, 'docs', 'feature', '*', 'ux-specs.json'),
      ];
      
      // For now, require explicit path
      if (!featurePath) {
        console.error('Error: feature-path is required');
        console.error('Usage: node scripts/generate-stories.js <feature-path> [package-name]');
        process.exit(1);
      }
    }

    if (!fs.existsSync(uxSpecsPath)) {
      console.error(`Error: ux-specs.json not found at ${uxSpecsPath}`);
      process.exit(1);
    }

    // Read and parse ux-specs.json
    let uxSpecs;
    try {
      const content = fs.readFileSync(uxSpecsPath, 'utf-8');
      uxSpecs = JSON.parse(content);
    } catch (e) {
      console.error(`Error: Failed to parse ux-specs.json: ${e.message}`);
      process.exit(1);
    }

    // Check if Storybook applies (skip if native/game engine)
    const packagePath = path.join(projectRoot, packageName);
    if (skipStorybook[packageName]) {
      console.log(`Skipping Storybook generation for ${packageName} (native/game engine detected)`);
      return;
    }

    // Detect framework for package
    const framework = frameworks[packageName] || 'html';
    console.log(`Detected framework: ${framework} for package: ${packageName}`);

    // Extract components from ux-specs.json
    const components = extractComponents(uxSpecs);

    if (components.length === 0) {
      console.log('No components found in ux-specs.json');
      return;
    }

    console.log(`Found ${components.length} components to generate stories for`);

    // Generate stories for each component
    for (const component of components) {
      await generateComponentStory(component, framework, packageName, uxSpecs);
    }

    console.log('Story generation complete!');
  } catch (error) {
    console.error('Error generating stories:', error.message);
    console.error(error.stack);
    // Don't exit with error code - allow workflow to continue
    process.exit(0);
  }
}

/**
 * Extract components from ux-specs.json
 */
function extractComponents(uxSpecs) {
  const components = new Set();

  // Extract from stateDesign.elements
  if (uxSpecs.passes?.stateDesign?.elements) {
    for (const element of uxSpecs.passes.stateDesign.elements) {
      if (element.component) {
        components.add(element.component);
      }
      if (element.name) {
        components.add(element.name);
      }
    }
  }

  // Extract from affordances.actions
  if (uxSpecs.passes?.affordances?.actions) {
    for (const action of uxSpecs.passes.affordances.actions) {
      if (action.component) {
        components.add(action.component);
      }
    }
  }

  // Extract from layout.componentComposition
  if (uxSpecs.layout?.componentComposition) {
    for (const comp of uxSpecs.layout.componentComposition) {
      if (comp.name) {
        components.add(comp.name);
      }
    }
  }

  return Array.from(components).filter(Boolean);
}

/**
 * Generate story file for a component
 */
async function generateComponentStory(componentName, framework, packageName, uxSpecs) {
  // Determine story location
  const isShared = isSharedComponent(componentName, uxSpecs);
  const storyDir = isShared
    ? path.join(projectRoot, 'storybook', 'stories', 'components')
    : path.join(projectRoot, 'storybook', 'stories', 'packages', packageName, 'components');

  // Ensure directory exists
  fs.mkdirSync(storyDir, { recursive: true });

  // Determine file extension based on framework
  const extMap = {
    'react': 'jsx',
    'vue': 'vue',
    'svelte': 'svelte',
    'html': 'js',
  };
  const ext = extMap[framework] || 'js';

  const storyPath = path.join(storyDir, `${componentName}.stories.${ext}`);

  // Check if story already exists with manual edits
  let existingContent = '';
  if (fs.existsSync(storyPath)) {
    existingContent = fs.readFileSync(storyPath, 'utf-8');
    // If file has manual edits marker, preserve it
    if (existingContent.includes('MANUAL EDIT')) {
      console.log(`Skipping ${componentName} - has manual edits`);
      return;
    }
  }

  // Extract component states
  const states = extractComponentStates(componentName, uxSpecs);

  // Extract design tokens
  const designTokens = extractDesignTokens(uxSpecs);

  // Extract accessibility level
  const accessibilityLevel = uxSpecs.technicalConstraints?.accessibilityLevel || 'AA';

  // Generate story content based on framework
  const storyContent = generateStoryContent(
    componentName,
    framework,
    packageName,
    states,
    designTokens,
    accessibilityLevel,
    isShared
  );

  // Write story file
  fs.writeFileSync(storyPath, storyContent, 'utf-8');
  console.log(`Generated: ${storyPath}`);
}

/**
 * Check if component is shared (used across packages)
 */
function isSharedComponent(componentName, uxSpecs) {
  // Simple heuristic: if mentioned in multiple packages or layout, it's shared
  // For now, default to package-specific
  return false;
}

/**
 * Extract states for a component
 */
function extractComponentStates(componentName, uxSpecs) {
  const states = ['default']; // Always include default

  if (uxSpecs.passes?.stateDesign?.elements) {
    for (const element of uxSpecs.passes.stateDesign.elements) {
      if (element.component === componentName || element.name === componentName) {
        if (element.states) {
          states.push(...element.states);
        } else {
          // Default states if not specified
          states.push('empty', 'loading', 'error', 'success');
        }
      }
    }
  }

  // Deduplicate
  return [...new Set(states)];
}

/**
 * Extract design tokens from ux-specs.json
 */
function extractDesignTokens(uxSpecs) {
  const tokens = {
    color: 'primary',
    spacing: 'spacing-4',
    typography: 'body',
  };

  if (uxSpecs.layout?.visualHierarchy?.colorUsage) {
    tokens.color = uxSpecs.layout.visualHierarchy.colorUsage.primary || tokens.color;
  }

  if (uxSpecs.layout?.spacingRhythm) {
    tokens.spacing = uxSpecs.layout.spacingRhythm.default || tokens.spacing;
  }

  if (uxSpecs.layout?.typographyHierarchy) {
    tokens.typography = uxSpecs.layout.typographyHierarchy.body || tokens.typography;
  }

  return tokens;
}

/**
 * Generate story content based on framework
 */
function generateStoryContent(componentName, framework, packageName, states, designTokens, accessibilityLevel, isShared) {
  const titlePrefix = isShared ? 'Components' : `Packages/${packageName.charAt(0).toUpperCase() + packageName.slice(1)}`;
  const importPath = isShared
    ? `../../${packageName}/components/${componentName}`
    : `../../../${packageName}/components/${componentName}`;

  switch (framework) {
    case 'react':
      return generateReactStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel);
    case 'vue':
      return generateVueStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel);
    case 'svelte':
      return generateSvelteStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel);
    case 'html':
    default:
      return generateHTMLStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel);
  }
}

/**
 * Generate React story
 */
function generateReactStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel) {
  const componentImport = `import ${componentName} from '${importPath}';`;
  const title = `${titlePrefix}/${componentName}`;

  let content = `// Auto-generated from ux-specs.json - DO NOT EDIT MANUALLY
// Regenerate by running: cd storybook && npm run generate-stories
// Framework: React

${componentImport}

export default {
  title: '${title}',
  component: ${componentName},
  parameters: {
    designSystem: {
      tokens: {
        color: '${designTokens.color}',
        spacing: '${designTokens.spacing}',
        typography: '${designTokens.typography}'
      }
    },
    accessibility: {
      level: '${accessibilityLevel}',
    }
  }
};

`;

  // Generate state variants
  for (const state of states) {
    const stateName = state.charAt(0).toUpperCase() + state.slice(1);
    content += `export const ${stateName} = {
  render: () => <${componentName} variant="${state}" />,
};
`;
  }

  return content;
}

/**
 * Generate Vue story
 */
function generateVueStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel) {
  const componentImport = `import ${componentName} from '${importPath}.vue';`;
  const title = `${titlePrefix}/${componentName}`;

  let content = `// Auto-generated from ux-specs.json - DO NOT EDIT MANUALLY
// Framework: Vue

${componentImport}

export default {
  title: '${title}',
  component: ${componentName},
  parameters: {
    designSystem: {
      tokens: {
        color: '${designTokens.color}',
        spacing: '${designTokens.spacing}',
        typography: '${designTokens.typography}'
      }
    },
    accessibility: {
      level: '${accessibilityLevel}',
    }
  }
};

`;

  // Generate state variants
  for (const state of states) {
    const stateName = state.charAt(0).toUpperCase() + state.slice(1);
    const disabled = state === 'loading' ? ' :disabled="true"' : '';
    content += `export const ${stateName} = {
  render: () => ({
    components: { ${componentName} },
    template: '<${componentName} variant="${state}"${disabled}>${stateName}</${componentName}>'
  }),
};
`;
  }

  return content;
}

/**
 * Generate Svelte story
 */
function generateSvelteStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel) {
  const componentImport = `import ${componentName} from '${importPath}.svelte';`;
  const title = `${titlePrefix}/${componentName}`;

  let content = `// Auto-generated from ux-specs.json - DO NOT EDIT MANUALLY
// Framework: Svelte

${componentImport}

export default {
  title: '${title}',
  component: ${componentName},
  parameters: {
    designSystem: {
      tokens: {
        color: '${designTokens.color}',
        spacing: '${designTokens.spacing}',
        typography: '${designTokens.typography}'
      }
    },
    accessibility: {
      level: '${accessibilityLevel}',
    }
  }
};

`;

  // Generate state variants
  for (const state of states) {
    const stateName = state.charAt(0).toUpperCase() + state.slice(1);
    const disabled = state === 'loading' ? ', disabled: true' : '';
    content += `export const ${stateName} = {
  component: ${componentName},
  props: {
    variant: '${state}'${disabled},
    children: '${stateName}'
  },
};
`;
  }

  return content;
}

/**
 * Generate HTML story
 */
function generateHTMLStory(componentName, titlePrefix, importPath, states, designTokens, accessibilityLevel) {
  const title = `${titlePrefix}/${componentName}`;

  let content = `// Auto-generated from ux-specs.json - DO NOT EDIT MANUALLY
// Framework: HTML/Alpine.js

export default {
  title: '${title}',
  parameters: {
    designSystem: {
      tokens: {
        color: '${designTokens.color}',
        spacing: '${designTokens.spacing}',
        typography: '${designTokens.typography}'
      }
    },
    accessibility: {
      level: '${accessibilityLevel}',
    }
  }
};

`;

  // Generate state variants
  for (const state of states) {
    const stateName = state.charAt(0).toUpperCase() + state.slice(1);
    const className = `component-${componentName.toLowerCase()}-${state}`;
    const disabled = state === 'loading' ? ' disabled' : '';
    const errorClass = state === 'error' ? ' error' : '';
    const successClass = state === 'success' ? ' success' : '';
    
    content += `export const ${stateName} = {
  render: () => \`<div class="${className}${disabled}${errorClass}${successClass}">${stateName}</div>\`,
};
`;
  }

  return content;
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  generateStories();
}

export { generateStories, extractComponents, extractComponentStates };
