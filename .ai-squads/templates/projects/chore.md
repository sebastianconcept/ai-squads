---
description: Chore Template - Improvement and maintenance task planning
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Chore Template

## Overview

Chores are improvement and maintenance tasks that enhance the project without adding new features. They focus on code quality, performance, documentation, and technical debt reduction.

## Template Structure

### Chore Directory
```
projects/[PROJECT_NAME]/chore-[CHORE_NAME]/
├── subject.md          # What are we improving?
└── improvement.md      # How will we improve it?
```

## Template Files

### 1. `subject.md` - Improvement Subject

```markdown
---
description: Chore - [CHORE_NAME]
type: chore
priority: [low|medium|high]
effort: [small|medium|large]
---

# Chore: [CHORE_NAME]

## Improvement Subject

**What are we improving?**
[Clear description of the current state and what needs improvement]

## Current State

- **Code Quality**: [Describe current code quality issues]
- **Performance**: [Describe current performance concerns]
- **Documentation**: [Describe current documentation gaps]
- **Technical Debt**: [Describe accumulated technical debt]
- **Maintainability**: [Describe maintainability issues]

## Impact Assessment

- **User Impact**: [How does this affect users?]
- **Developer Impact**: [How does this affect developers?]
- **System Impact**: [How does this affect the system?]
- **Business Impact**: [How does this affect business goals?]

## Scope

- **Files Affected**: [List of files/components to improve]
- **Dependencies**: [What other work depends on this?]
- **Risks**: [What risks are involved?]
```

### 2. `improvement.md` - Improvement Plan

```markdown
---
description: Chore Improvement Plan - [CHORE_NAME]
type: chore-plan
status: [planned|in-progress|completed]
---

# Improvement Plan: [CHORE_NAME]

## Improvement Strategy

**How will we improve it?**
[Clear description of the improvement approach and methodology]

## Technical Approach

### Refactoring Strategy
- **Code Structure**: [How will we restructure the code?]
- **Patterns**: [What design patterns will we apply?]
- **Standards**: [How will we align with coding standards?]

### Quality Improvements
- **Testing**: [How will we improve test coverage?]
- **Documentation**: [How will we improve documentation?]
- **Performance**: [How will we optimize performance?]

### Technical Debt Reduction
- **Dependencies**: [How will we update/clean dependencies?]
- **Legacy Code**: [How will we modernize legacy code?]
- **Architecture**: [How will we improve system architecture?]

## Implementation Plan

### Phase 1: Analysis
- [ ] Assess current state
- [ ] Identify specific improvements
- [ ] Plan refactoring approach
- [ ] Estimate effort and timeline

### Phase 2: Implementation
- [ ] Implement improvements incrementally
- [ ] Maintain functionality during changes
- [ ] Update tests and documentation
- [ ] Validate improvements

### Phase 3: Validation
- [ ] Run comprehensive tests
- [ ] Performance benchmarking
- [ ] Code review and quality gates
- [ ] Documentation updates

## Success Criteria

### Quality Metrics
- **Code Coverage**: [Target test coverage percentage]
- **Performance**: [Target performance improvements]
- **Maintainability**: [Target maintainability score]
- **Documentation**: [Target documentation completeness]

### Deliverables
- [ ] Refactored code
- [ ] Updated tests
- [ ] Improved documentation
- [ ] Performance benchmarks
- [ ] Quality metrics report

## Resources Required

### Squad Agents
- **@agent:rusty** - Technical implementation
- **@agent:scribas** - Version control and branching
- **@agent:team** - Quality assurance and coordination

### Tools and Standards
- **Code Quality**: [Specific tools and standards to apply]
- **Testing**: [Testing frameworks and approaches]
- **Documentation**: [Documentation standards and tools]
- **Performance**: [Performance measurement tools]

## Timeline

- **Start Date**: [YYYY-MM-DD]
- **Estimated Duration**: [X days/weeks]
- **Completion Date**: [YYYY-MM-DD]

## Notes

[Additional context, considerations, or special instructions]
```

## Usage Examples

### Example 1: Code Quality Improvement
```
chore-improve-error-handling/
├── subject.md          # Current error handling is inconsistent
└── improvement.md      # Standardize error handling patterns
```

### Example 2: Performance Optimization
```
chore-optimize-database-queries/
├── subject.md          # Database queries are slow
└── improvement.md      # Add indexes and optimize query patterns
```

### Example 3: Documentation Update
```
chore-update-api-docs/
├── subject.md          # API documentation is outdated
└── improvement.md      # Generate comprehensive API documentation
```

## Best Practices

### Planning Chores
- **Prioritize by Impact**: Focus on high-impact, low-effort improvements
- **Batch Similar Work**: Group related improvements together
- **Consider Dependencies**: Plan improvements that enable future work
- **Measure Results**: Define clear success criteria

### Execution
- **Incremental Changes**: Make small, safe improvements
- **Maintain Quality**: Don't introduce new technical debt
- **Document Changes**: Update documentation as you go
- **Validate Results**: Ensure improvements meet success criteria

### Integration
- **Squad Coordination**: Work with appropriate squad agents
- **Quality Gates**: Ensure improvements meet squad standards
- **Knowledge Sharing**: Share learnings with the team
- **Continuous Improvement**: Use learnings to improve future chores
