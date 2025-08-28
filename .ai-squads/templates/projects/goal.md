---
description: Goal Template - Success criteria and objectives definition
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Goal Template

## Overview

Goals define the success criteria and objectives for any project work. They provide clear direction on what success looks like and how it will be measured.

## Template Structure

### Goal File
```
projects/[PROJECT_NAME]/[WORK_TYPE]-[WORK_NAME]/goal.md
```

**Work Types:**
- `feature-[FEATURE_NAME]/goal.md` - Feature success criteria
- `hotfix-[HOTFIX_NAME]/goal.md` - Hotfix success criteria  
- `chore-[CHORE_NAME]/goal.md` - Chore success criteria

## Template Content

```markdown
---
description: Goal - [WORK_NAME]
type: [feature|hotfix|chore]
priority: [low|medium|high]
status: [planned|in-progress|completed]
---

# Goal: [WORK_NAME]

## Success Definition

**What does success look like?**
[Clear, measurable description of what success means for this work]

## Objectives

### Primary Objective
[Main goal that this work aims to achieve]

### Secondary Objectives
- [Secondary goal 1]
- [Secondary goal 2]
- [Secondary goal 3]

## Success Criteria

### Functional Requirements
- [ ] [Requirement 1 - specific and testable]
- [ ] [Requirement 2 - specific and testable]
- [ ] [Requirement 3 - specific and testable]

### Quality Requirements
- [ ] **Performance**: [Specific performance targets]
- [ ] **Reliability**: [Specific reliability targets]
- [ ] **Security**: [Specific security requirements]
- [ ] **Accessibility**: [Specific accessibility standards]
- [ ] **Usability**: [Specific usability criteria]

### Technical Requirements
- [ ] **Code Quality**: [Specific code quality standards]
- [ ] **Test Coverage**: [Specific test coverage targets]
- [ ] **Documentation**: [Specific documentation requirements]
- [ ] **Standards Compliance**: [Specific standards to follow]

## Acceptance Criteria

### User Acceptance
**As a [user type], I want [capability] so that [benefit]**

- **Given**: [Precondition]
- **When**: [Action]
- **Then**: [Expected result]

### Technical Acceptance
- [ ] [Technical criterion 1]
- [ ] [Technical criterion 2]
- [ ] [Technical criterion 3]

### Business Acceptance
- [ ] [Business criterion 1]
- [ ] [Business criterion 2]
- [ ] [Business criterion 3]

## Measurement and Validation

### Quantitative Metrics
- **Performance**: [Specific metrics and targets]
- **Quality**: [Specific quality metrics]
- **Coverage**: [Specific coverage targets]
- **Efficiency**: [Specific efficiency metrics]

### Qualitative Assessment
- **User Experience**: [How will we assess UX?]
- **Code Quality**: [How will we assess code quality?]
- **Maintainability**: [How will we assess maintainability?]
- **Documentation**: [How will we assess documentation?]

### Testing Strategy
- **Unit Tests**: [What unit tests are needed?]
- **Integration Tests**: [What integration tests are needed?]
- **User Acceptance Tests**: [What UAT scenarios are needed?]
- **Performance Tests**: [What performance tests are needed?]

## Constraints and Limitations

### Technical Constraints
- **Performance**: [Performance limitations or requirements]
- **Scalability**: [Scalability constraints or requirements]
- **Compatibility**: [Compatibility requirements]
- **Security**: [Security constraints or requirements]

### Business Constraints
- **Timeline**: [Time constraints or deadlines]
- **Budget**: [Resource or budget constraints]
- **Dependencies**: [External dependencies or requirements]
- **Regulatory**: [Compliance or regulatory requirements]

### Quality Constraints
- **Standards**: [Quality standards that must be met]
- **Processes**: [Process requirements that must be followed]
- **Tools**: [Tool requirements or constraints]
- **Documentation**: [Documentation requirements]

## Risk Assessment

### High-Risk Areas
- **Risk 1**: [Description and mitigation strategy]
- **Risk 2**: [Description and mitigation strategy]
- **Risk 3**: [Description and mitigation strategy]

### Mitigation Strategies
- **Risk 1**: [Specific mitigation approach]
- **Risk 2**: [Specific mitigation approach]
- **Risk 3**: [Specific mitigation approach]

## Timeline and Milestones

### Key Milestones
- **Milestone 1**: [Date and deliverable]
- **Milestone 2**: [Date and deliverable]
- **Milestone 3**: [Date and deliverable]

### Dependencies
- **Internal**: [What internal work must be completed first?]
- **External**: [What external factors must be resolved?]
- **Technical**: [What technical prerequisites are needed?]
- **Business**: [What business approvals are required?]

## Success Validation

### Validation Methods
- **Testing**: [How will we test the solution?]
- **Review**: [How will we review the work?]
- **User Feedback**: [How will we gather user feedback?]
- **Performance Analysis**: [How will we analyze performance?]

### Sign-off Requirements
- **Technical Review**: [Who must approve the technical implementation?]
- **User Acceptance**: [Who must accept the user experience?]
- **Business Approval**: [Who must approve the business value?]
- **Quality Gate**: [What quality standards must be met?]

## Definition of Done

### Work Completion
- [ ] [All acceptance criteria met]
- [ ] [All tests passing]
- [ ] [Code review completed]
- [ ] [Documentation updated]
- [ ] [Performance requirements met]
- [ ] [Security requirements met]
- [ ] [Accessibility requirements met]

### Quality Gates
- [ ] [Code quality standards met]
- [ ] [Test coverage requirements met]
- [ ] [Documentation standards met]
- [ ] [Performance benchmarks met]
- [ ] [Security review completed]
- [ ] [Accessibility review completed]

### Deployment Readiness
- [ ] [All tests passing in staging]
- [ ] [Performance validated in staging]
- [ ] [Security scan completed]
- [ ] [Deployment plan approved]
- [ ] [Rollback plan prepared]
- [ ] [Monitoring configured]

## Notes

[Additional context, considerations, or special instructions]
```

## Usage Examples

### Feature Goal Example
```markdown
# Goal: User Authentication System

## Success Definition
A secure, user-friendly authentication system that allows users to sign up, log in, and manage their accounts.

## Success Criteria
- [ ] Users can create accounts with email verification
- [ ] Users can log in securely with password reset capability
- [ ] Session management works across browser tabs
- [ ] Authentication is secure against common attacks
```

### Hotfix Goal Example
```markdown
# Goal: Fix Login Performance Issue

## Success Definition
Login response time reduced from 5 seconds to under 1 second.

## Success Criteria
- [ ] Login response time < 1 second
- [ ] No regression in security features
- [ ] All existing functionality preserved
```

### Chore Goal Example
```markdown
# Goal: Improve Error Handling

## Success Definition
Consistent, user-friendly error handling across the application.

## Success Criteria
- [ ] All error messages are user-friendly
- [ ] Error logging provides sufficient debugging information
- [ ] Error handling follows consistent patterns
```

## Best Practices

### Writing Clear Goals
- **Be Specific**: Use concrete, measurable criteria
- **Be Realistic**: Set achievable but challenging targets
- **Be Testable**: Ensure success criteria can be validated
- **Be User-Focused**: Prioritize user value and experience

### Goal Management
- **Regular Review**: Review goals regularly to ensure they remain relevant
- **Progress Tracking**: Track progress against goals throughout development
- **Adjustment**: Be willing to adjust goals based on new information
- **Celebration**: Celebrate when goals are achieved

### Integration with Workflow
- **Planning Phase**: Define goals before starting implementation
- **Development Phase**: Reference goals during development decisions
- **Testing Phase**: Use goals to validate deliverables
- **Review Phase**: Assess success against defined goals
