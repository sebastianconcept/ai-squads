---
description: Docker Requirements - Pines UI Library Adoption
type: docker-requirements
status: planned
---

# Docker Requirements: Pines UI Library Adoption

## Overview

The Pines UI library adoption requires significant updates to the Docker infrastructure, particularly for the SaaS application which currently lacks asset building capabilities. This document outlines the specific Docker changes needed for both development and production environments.

## Current Docker State

### Site Application (Working)
- ✅ Multi-stage build with Node.js asset building
- ✅ Vite configuration for asset compilation
- ✅ Asset copying and serving in containers
- ✅ Development and production Docker configurations

### SaaS Application (Missing)
- ❌ No Node.js asset building stage
- ❌ No asset pipeline in Docker
- ❌ No asset copying or serving
- ❌ Missing development asset workflow

## Required Docker Changes

### 1. SaaS Dockerfile Updates

#### `Dockerfile.saas` - Add Asset Building Stage
```dockerfile
# Multi-stage build: Assets stage (NEW)
FROM node:18-alpine AS assets
WORKDIR /app
COPY crates/saas/package*.json ./
RUN npm ci --only=production
COPY crates/saas/assets ./assets
RUN npm run build

# Multi-stage build: Rust stage (EXISTING)
FROM bankflow/saas-base:${BASE_IMAGE_VERSION} as builder
# ... existing code ...

# Copy built assets from assets stage (NEW)
COPY --from=assets /app/dist ./crates/saas/dist

# Runtime stage updates (NEW)
COPY --from=builder /app/crates/saas/dist ./dist
```

#### `Dockerfile.saas-base` - Add Node.js Dependencies
```dockerfile
# Add Node.js to base image
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs
```

### 2. Docker Compose Updates

#### Development Configuration
```yaml
# docker-compose.dev.yml
services:
  saas:
    build:
      context: ./bankflow
      dockerfile: Dockerfile.saas.dev  # New development Dockerfile
    volumes:
      - ./bankflow/crates/saas/assets:/app/crates/saas/assets  # Asset hot reloading
      - ./bankflow/crates/saas/dist:/app/crates/saas/dist      # Asset output
    environment:
      - NODE_ENV=development
      - VITE_DEV_SERVER=true
```

#### Production Configuration
```yaml
# docker-compose.prod.yml
services:
  saas:
    build:
      context: ./bankflow
      dockerfile: Dockerfile.saas  # Updated production Dockerfile
    environment:
      - NODE_ENV=production
      - VITE_DEV_SERVER=false
```

### 3. New Docker Files Required

#### `Dockerfile.saas.dev` - Development Dockerfile
```dockerfile
# Development Dockerfile for SaaS with hot reloading
FROM node:18-alpine AS assets-dev
WORKDIR /app
COPY crates/saas/package*.json ./
RUN npm ci
COPY crates/saas/assets ./assets
# Keep assets running for hot reloading
CMD ["npm", "run", "dev"]

# Rust development stage
FROM bankflow/saas-base:${BASE_IMAGE_VERSION} as builder-dev
# ... development-specific build steps ...
```

## Implementation Tasks

### Phase 1: Docker Infrastructure Updates

#### 1.1 Update SaaS Base Image
- [ ] Add Node.js 18 to `Dockerfile.saas-base`
- [ ] Install npm and build tools
- [ ] Test base image builds

#### 1.2 Update SaaS Production Dockerfile
- [ ] Add Node.js asset building stage
- [ ] Implement asset copying and building
- [ ] Update runtime stage to include assets
- [ ] Test production builds

#### 1.3 Create SaaS Development Dockerfile
- [ ] Create `Dockerfile.saas.dev` for development
- [ ] Implement hot reloading support
- [ ] Add volume mounts for asset development
- [ ] Test development workflow

#### 1.4 Update Docker Compose Configurations
- [ ] Update `docker-compose.dev.yml` for SaaS
- [ ] Update `docker-compose.prod.yml` for SaaS
- [ ] Update `docker-compose.staging.yml` for SaaS
- [ ] Test all environments

### Phase 2: Asset Pipeline Integration

#### 2.1 Asset Building in Containers
- [ ] Test Vite builds in Docker containers
- [ ] Verify Tailwind CSS compilation
- [ ] Test Pines UI integration in containers
- [ ] Validate asset optimization

#### 2.2 Development Workflow
- [ ] Test hot reloading in Docker
- [ ] Verify asset serving in development
- [ ] Test component development workflow
- [ ] Validate debugging capabilities

#### 2.3 Production Optimization
- [ ] Test production asset builds
- [ ] Verify asset minification
- [ ] Test asset serving in production
- [ ] Validate performance

## Docker Build Process

### Development Build
```bash
# Build development images
docker-compose -f docker-compose.dev.yml build saas

# Start development environment
docker-compose -f docker-compose.dev.yml up saas

# Hot reloading enabled
# Assets automatically rebuild on changes
```

### Production Build
```bash
# Build production images
docker-compose -f docker-compose.prod.yml build saas

# Start production environment
docker-compose -f docker-compose.prod.yml up saas

# Optimized assets included
# No hot reloading
```

## Volume Mounts for Development

### Asset Development
```yaml
volumes:
  # Source assets for hot reloading
  - ./bankflow/crates/saas/assets:/app/crates/saas/assets:ro
  
  # Built assets for serving
  - ./bankflow/crates/saas/dist:/app/crates/saas/dist
  
  # Node modules for faster builds
  - saas_node_modules:/app/crates/saas/node_modules
```

### Rust Development
```yaml
volumes:
  # Rust source for hot reloading
  - ./bankflow/crates/saas/src:/app/crates/saas/src:ro
  
  # Cargo cache for faster builds
  - cargo_cache:/usr/local/cargo/registry
```

## Environment Variables

### Development
```yaml
environment:
  - NODE_ENV=development
  - VITE_DEV_SERVER=true
  - VITE_HMR_PORT=5173
  - RUST_LOG=debug
```

### Production
```yaml
environment:
  - NODE_ENV=production
  - VITE_DEV_SERVER=false
  - RUST_LOG=warn
```

## Testing Strategy

### Docker Build Testing
- [ ] Test base image builds
- [ ] Test production image builds
- [ ] Test development image builds
- [ ] Verify asset inclusion

### Runtime Testing
- [ ] Test asset serving in containers
- [ ] Test hot reloading in development
- [ ] Test production asset optimization
- [ ] Verify performance

### Integration Testing
- [ ] Test with existing services
- [ ] Test Docker Compose configurations
- [ ] Test deployment pipelines
- [ ] Verify health checks

## Performance Considerations

### Build Performance
- **Layer Caching**: Optimize Docker layer caching for assets
- **Parallel Builds**: Use multi-stage builds for parallel asset compilation
- **Build Context**: Minimize Docker build context size

### Runtime Performance
- **Asset Serving**: Optimize asset serving in containers
- **Memory Usage**: Monitor memory usage with Node.js processes
- **Startup Time**: Minimize container startup time

## Security Considerations

### Container Security
- **Non-root User**: Run containers as non-root user
- **Minimal Base Images**: Use minimal base images
- **Asset Permissions**: Set proper permissions for assets

### Asset Security
- **Source Maps**: Exclude source maps in production
- **Dependencies**: Audit npm dependencies
- **Build Tools**: Use minimal build tools in production

## Monitoring and Logging

### Build Monitoring
- [ ] Monitor Docker build times
- [ ] Track asset build performance
- [ ] Log build errors and warnings

### Runtime Monitoring
- [ ] Monitor asset serving performance
- [ ] Track container resource usage
- [ ] Log asset loading errors

## Rollback Strategy

### Docker Rollback
- [ ] Maintain previous Docker image versions
- [ ] Test rollback procedures
- [ ] Document rollback steps

### Asset Rollback
- [ ] Maintain previous asset versions
- [ ] Test asset rollback procedures
- [ ] Document asset rollback steps

## Success Criteria

### Docker Build Success
- [ ] All Docker images build successfully
- [ ] Asset building works in all environments
- [ ] No build errors or warnings

### Runtime Success
- [ ] Assets serve correctly in containers
- [ ] Hot reloading works in development
- [ ] Production assets are optimized

### Integration Success
- [ ] Works with existing Docker infrastructure
- [ ] Compatible with current deployment pipeline
- [ ] Maintains existing functionality
