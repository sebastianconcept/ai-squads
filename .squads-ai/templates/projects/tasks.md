---
description: Tasks Template - Hierarchical task management and execution tracking
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Project Tasks

## Overview

This document tracks all tasks required to complete the project work. Tasks are organized hierarchically with clear acceptance criteria and agent assignments. Each task should be specific, measurable, and actionable.

## Task Categories

### [ ] 1. [Category Name]
**Description**: [Brief description of what this category accomplishes]

- [ ] 1.1 [Specific task with clear acceptance criteria]
- [ ] 1.2 [Specific task with clear acceptance criteria]
- [ ] 1.3 [Specific task with clear acceptance criteria]
- [ ] 1.4 Verify all tasks in this category are complete

**Dependencies**: [What needs to be completed first]
**Estimated Effort**: [Small/Medium/Large]
**Priority**: [Low/Medium/High/Critical]

### [ ] 2. [Category Name]
**Description**: [Brief description of what this category accomplishes]

- [ ] 2.1 [Specific task with clear acceptance criteria]
- [ ] 2.2 [Specific task with clear acceptance criteria]
- [ ] 2.3 [Specific task with clear acceptance criteria]
- [ ] 2.4 Verify all tasks in this category are complete

**Dependencies**: [What needs to be completed first]
**Estimated Effort**: [Small/Medium/Large]
**Priority**: [Low/Medium/High/Critical]

### [ ] 3. [Category Name]
**Description**: [Brief description of what this category accomplishes]

- [ ] 3.1 [Specific task with clear acceptance criteria]
- [ ] 3.2 [Specific task with clear acceptance criteria]
- [ ] 3.3 [Specific task with clear acceptance criteria]
- [ ] 3.4 Verify all tasks in this category are complete

**Dependencies**: [What needs to be completed first]
**Estimated Effort**: [Small/Medium/Large]
**Priority**: [Low/Medium/High/Critical]

## Task Assignment by Squad Agent

### @agent:software-engineer
**Responsibilities**: Technical implementation, backend development, systems programming

- [ ] [Technical implementation task 1]
- [ ] [Technical implementation task 2]
- [ ] [Technical implementation task 3]

### @agent:ux-expert
**Responsibilities**: User experience research, design, and validation

- [ ] [User experience task 1]
- [ ] [User experience task 2]
- [ ] [User experience task 3]

### @agent:ui-implementor
**Responsibilities**: Frontend implementation, styling, and cross-platform compatibility

- [ ] [Frontend implementation task 1]
- [ ] [Frontend implementation task 2]
- [ ] [Frontend implementation task 3]

### @agent:git-workflow
**Responsibilities**: Version control, branching strategy, and release management

- [ ] [Version control task 1]
- [ ] [Version control task 2]
- [ ] [Version control task 3]

### @agent:collaboration
**Responsibilities**: Team coordination, quality assurance, and handoff management

- [ ] [Coordination task 1]
- [ ] [Coordination task 2]
- [ ] [Coordination task 3]

## Task Dependencies and Sequencing

### Phase 1: Foundation
**Dependencies**: None
**Tasks**: [List of foundation tasks that can start immediately]

### Phase 2: Core Implementation
**Dependencies**: Phase 1 completion
**Tasks**: [List of core implementation tasks]

### Phase 3: Integration and Testing
**Dependencies**: Phase 2 completion
**Tasks**: [List of integration and testing tasks]

### Phase 4: Deployment and Validation
**Dependencies**: Phase 3 completion
**Tasks**: [List of deployment and validation tasks]

## Quality Gates and Verification

### Code Quality Gates
- [ ] All code follows squad coding standards
- [ ] Code review completed and approved
- [ ] Static analysis passes without critical issues
- [ ] Test coverage meets minimum requirements

### Pre-Commit Quality Gates
**MANDATORY**: These must pass before any commit:

#### Rust Projects
- [ ] `cargo fmt` - Code formatting verified
- [ ] `cargo clippy --all-targets --all-features -- -D warnings` - Full Clippy with warnings as errors
- [ ] `cargo test` - All tests passing
- [ ] `cargo check` - Compilation successful

#### JavaScript/TypeScript Projects
- [ ] `npm run lint` - ESLint passes
- [ ] `npm run format` - Prettier applied
- [ ] `npm test` - All tests passing
- [ ] `npm run build` - Build successful

#### General Quality Gates
- [ ] Documentation updated for changes
- [ ] No TODO/FIXME in production code
- [ ] Commit message follows conventional format

### Testing Gates
- [ ] Unit tests written and passing
- [ ] Integration tests written and passing
- [ ] User acceptance tests completed
- [ ] Performance tests meet requirements

### Documentation Gates
- [ ] Code documentation updated
- [ ] API documentation updated
- [ ] User documentation updated
- [ ] README and project files updated

### Security and Performance Gates
- [ ] Security review completed
- [ ] Performance benchmarks met
- [ ] Accessibility requirements satisfied
- [ ] Cross-platform compatibility verified

## Progress Tracking

### Overall Progress
- **Total Tasks**: [X]
- **Completed**: [Y]
- **In Progress**: [Z]
- **Blocked**: [W]
- **Completion**: [Y/X]%

### Category Progress
- **Category 1**: [X/Y] tasks complete
- **Category 2**: [X/Y] tasks complete
- **Category 3**: [X/Y] tasks complete

### Agent Progress
- **@agent:software-engineer**: [X/Y] tasks complete
- **@agent:ux-expert**: [X/Y] tasks complete
- **@agent:ui-implementor**: [X/Y] tasks complete
- **@agent:git-workflow**: [X/Y] tasks complete
- **@agent:collaboration**: [X/Y] tasks complete

## Blocked Tasks and Issues

### Currently Blocked
- **Task**: [Task description]
  - **Blocked By**: [What's blocking this task]
  - **Resolution Plan**: [How to resolve the blockage]
  - **Estimated Resolution Time**: [When this will be resolved]

### Dependencies Waiting
- **Task**: [Task description]
  - **Waiting For**: [What this task is waiting for]
  - **Expected Completion**: [When the dependency will be ready]
  - **Contingency Plan**: [What to do if dependency is delayed]

## Risk Mitigation

### High-Risk Tasks
- **Task**: [Task description]
  - **Risk**: [What could go wrong]
  - **Mitigation**: [How to reduce or eliminate the risk]
  - **Contingency**: [What to do if the risk materializes]

### Time-Critical Tasks
- **Task**: [Task description]
  - **Deadline**: [When this must be completed]
  - **Critical Path**: [Why this affects other tasks]
  - **Acceleration Options**: [How to speed up if needed]

## Notes and Context

### Important Decisions
- [Decision 1]: [Context and rationale]
- [Decision 2]: [Context and rationale]
- [Decision 3]: [Context and rationale]

### External Dependencies
- [External dependency 1]: [Status and timeline]
- [External dependency 2]: [Status and timeline]
- [External dependency 3]: [Status and timeline]

### Team Notes
- [Important note 1]
- [Important note 2]
- [Important note 3]

## Task Template Examples

### Example 1: Feature Development Tasks
```markdown
### [ ] 1. User Authentication System
**Description**: Implement secure user authentication with JWT tokens

- [ ] 1.1 Design authentication database schema
- [ ] 1.2 Implement password hashing utilities
- [ ] 1.3 Create JWT token management
- [ ] 1.4 Build user registration endpoint
- [ ] 1.5 Build user login endpoint
- [ ] 1.6 Implement password reset functionality
- [ ] 1.7 Add authentication middleware
- [ ] 1.8 Write comprehensive tests
- [ ] 1.9 Verify all authentication features work correctly

**Dependencies**: None
**Estimated Effort**: Large
**Priority**: High
```

### Example 2: Bug Fix Tasks
```markdown
### [ ] 2. Performance Optimization
**Description**: Fix slow page load times and optimize performance

- [ ] 2.1 Identify performance bottlenecks
- [ ] 2.2 Implement database query optimization
- [ ] 2.3 Add caching layer
- [ ] 2.4 Optimize frontend bundle size
- [ ] 2.5 Implement lazy loading
- [ ] 2.6 Add performance monitoring
- [ ] 2.7 Verify performance improvements
- [ ] 2.8 Verify no regressions introduced

**Dependencies**: Performance analysis complete
**Estimated Effort**: Medium
**Priority**: High
```

### Example 3: Chore Tasks
```markdown
### [ ] 3. Code Quality Improvement
**Description**: Improve code quality and reduce technical debt

- [ ] 3.1 Update dependencies to latest versions
- [ ] 3.2 Fix linting issues
- [ ] 3.3 Improve test coverage
- [ ] 3.4 Refactor complex functions
- [ ] 3.5 Update documentation
- [ ] 3.6 Verify all improvements work correctly

**Dependencies**: None
**Estimated Effort**: Small
**Priority**: Medium
```

## Best Practices

### Task Creation
- **Be Specific**: Each task should have clear acceptance criteria
- **Be Measurable**: Tasks should be verifiable when complete
- **Be Actionable**: Tasks should start with action verbs
- **Be Realistic**: Tasks should be completable in reasonable time

### Feature Branch Creation
**ENGINEERS MUST FOLLOW THIS SEQUENCE BEFORE CREATING ANY FEATURE BRANCH:**

```bash
# 1. Check current git status
git status

# 2. Switch to main branch
git checkout main

# 3. Pull latest changes from origin/main
git pull origin main

# 4. Create feature branch from updated main
git checkout -b feature-[FEATURE_NAME]

# 5. Verify clean feature branch
git status
```

### Pre-Commit Quality Assurance
**ENGINEERS MUST RUN THESE COMMANDS BEFORE EVERY COMMIT:**

#### **Rust Projects**
```bash
# 0. Format code check
cargo fmt --all -- --check

# 1. Format code check
cargo fmt

# 2. Check compilation
cargo check

# 3. Run full Clippy with warnings as errors
cargo clippy --all-targets --all-features -- -D warnings

# 4. Run all tests
cargo test

# 5. Only if all above pass, then commit
git add .
git commit -m "descriptive message"
```

#### **JavaScript/TypeScript Projects**
```bash
# 1. Format code
npm run format

# 2. Lint code
npm run lint

# 3. Run tests
npm test

# 4. Build check
npm run build

# 5. Only if all above pass, then commit
git add .
git commit -m "descriptive message"
```

**ENFORCEMENT**: These quality gates are mandatory. No commit should happen without passing all checks.

### Task Management
- **Regular Updates**: Update task status regularly
- **Dependency Tracking**: Monitor and update dependency status
- **Progress Communication**: Share progress with the team
- **Risk Management**: Identify and mitigate risks early

### Quality Assurance
- **Verification Steps**: Include verification in each category
- **Quality Gates**: Use quality gates to ensure standards
- **Testing Integration**: Integrate testing into task completion
- **Documentation Updates**: Keep documentation current with code

### Team Coordination
- **Clear Assignments**: Assign tasks to appropriate agents
- **Handoff Protocols**: Define clear handoff procedures
- **Communication**: Maintain clear communication about task status
- **Collaboration**: Encourage collaboration between agents
