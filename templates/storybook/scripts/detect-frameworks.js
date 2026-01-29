/**
 * Framework Detection Script for Storybook
 * 
 * Automatically detects the frontend framework used in a project by checking:
 * 1. TECH-STACK.md for explicit framework specification
 * 2. package.json for framework dependencies (React, Vue, Svelte, Angular)
 * 3. File extensions (.jsx, .vue, .svelte) in package directories
 * 4. Defaults to HTML if no framework detected
 * 
 * Also detects native/game engine packages (iOS, Android, Godot, Unity) to skip Storybook.
 * 
 * Used by:
 * - Storybook configuration (.storybook/main.js) to select correct framework
 * - Story generation script (generate-stories.js) to generate framework-appropriate stories
 * 
 * Exports:
 * - frameworks: { frontend: 'react'|'vue'|'svelte'|'html', mobile: ... }
 * - skipStorybook: { frontend: boolean, mobile: boolean }
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../../');

/**
 * Detect the project name from git repository or docs directory
 */
function detectProjectName() {
  // Try to get from git config
  try {
    const gitConfigPath = path.join(projectRoot, '.git', 'config');
    if (fs.existsSync(gitConfigPath)) {
      const gitConfig = fs.readFileSync(gitConfigPath, 'utf-8');
      const urlMatch = gitConfig.match(/url\s*=\s*.+[:/]([^/]+)\.git/);
      if (urlMatch) {
        return urlMatch[1];
      }
    }
  } catch (e) {
    // Ignore errors
  }

  // Try to detect from docs directory
  const docsDir = path.join(process.env.HOME || process.env.USERPROFILE || '', 'docs');
  if (fs.existsSync(docsDir)) {
    const dirs = fs.readdirSync(docsDir, { withFileTypes: true })
      .filter(d => d.isDirectory())
      .map(d => d.name);
    if (dirs.length > 0) {
      // Use first directory as project name (could be improved)
      return dirs[0];
    }
  }

  // Fallback: use directory name
  return path.basename(projectRoot);
}

/**
 * Detect framework for a package directory
 */
function detectFramework(packagePath) {
  if (!fs.existsSync(packagePath)) {
    return 'html'; // Default if package doesn't exist
  }

  const projectName = detectProjectName();
  const techStackPath = path.join(process.env.HOME || process.env.USERPROFILE || '', 'docs', projectName, 'TECH-STACK.md');
  
  // Check TECH-STACK.md first
  if (fs.existsSync(techStackPath)) {
    try {
      const techStack = fs.readFileSync(techStackPath, 'utf-8');
      const frameworkMatch = techStack.match(/framework[:\s]+(\w+)/i);
      if (frameworkMatch) {
        return frameworkMatch[1].toLowerCase();
      }
    } catch (e) {
      // Continue to other detection methods
    }
  }
  
  // Check package.json
  const packageJsonPath = path.join(packagePath, 'package.json');
  if (fs.existsSync(packageJsonPath)) {
    try {
      const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf-8'));
      const deps = { ...packageJson.dependencies, ...packageJson.devDependencies };
      
      if (deps.react || deps['react-dom']) return 'react';
      if (deps.vue) return 'vue';
      if (deps.svelte || deps['@sveltejs/kit']) return 'svelte';
      if (deps['@angular/core']) return 'angular';
    } catch (e) {
      // Continue to file extension detection
    }
  }
  
  // Check file extensions
  try {
    const files = fs.readdirSync(packagePath, { recursive: true });
    if (files.some(f => f.endsWith('.jsx') || f.endsWith('.tsx'))) return 'react';
    if (files.some(f => f.endsWith('.vue'))) return 'vue';
    if (files.some(f => f.endsWith('.svelte'))) return 'svelte';
  } catch (e) {
    // If we can't read directory, default to HTML
  }
  
  // Default to HTML
  return 'html';
}

/**
 * Check if package is native or game engine
 */
function isNativeOrGameEngine(packagePath) {
  if (!fs.existsSync(packagePath)) {
    return false;
  }

  try {
    // Check for native indicators
    const hasIOS = fs.existsSync(path.join(packagePath, 'ios')) || 
                   fs.existsSync(path.join(packagePath, 'Podfile'));
    const hasAndroid = fs.existsSync(path.join(packagePath, 'android')) ||
                       fs.existsSync(path.join(packagePath, 'build.gradle'));
    
    // Check for game engine indicators
    const hasGodot = fs.existsSync(path.join(packagePath, 'project.godot'));
    const hasUnity = fs.existsSync(path.join(packagePath, 'Assets')) ||
                     fs.existsSync(path.join(packagePath, 'ProjectSettings'));
    
    return hasIOS || hasAndroid || hasGodot || hasUnity;
  } catch (e) {
    return false;
  }
}

// Detect frameworks for each package
// When frontend/ doesn't exist or has no package.json, detect from project root (e.g. SvelteKit-at-root)
const frontendPath = path.join(projectRoot, 'frontend');
const frontendHasPackage = fs.existsSync(path.join(frontendPath, 'package.json'));
const frameworks = {
  frontend: fs.existsSync(frontendPath) && frontendHasPackage
    ? detectFramework(frontendPath)
    : detectFramework(projectRoot),
  mobile: detectFramework(path.join(projectRoot, 'mobile')),
};

// Check for native/game engines
const skipStorybook = {
  frontend: isNativeOrGameEngine(path.join(projectRoot, 'frontend')),
  mobile: isNativeOrGameEngine(path.join(projectRoot, 'mobile')),
};

export { frameworks, skipStorybook };
