---
description: Solution Template - Solution design and implementation approach
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Solution Template

## Overview

Solution documents define how a problem will be solved. They provide the technical approach, implementation plan, and design decisions that will guide development.

## Template Structure

### Solution File
```
projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/solution.md
```

## Template Content

```markdown
---
description: Feature Solution - [FEATURE_NAME]
type: feature-solution
status: [planned|in-progress|completed]
priority: [low|medium|high]
---

# Solution: [FEATURE_NAME]

## Solution Overview

**How will we solve it?**
[High-level description of the solution approach and methodology]

## Solution Design

### High-Level Architecture
[Overview of the system architecture and how components interact]

### Core Components
- **[Component 1]**: [Purpose and responsibility]
- **[Component 2]**: [Purpose and responsibility]
- **[Component 3]**: [Purpose and responsibility]

### Data Flow
[How data flows through the system and between components]

### Integration Points
[How this solution integrates with existing systems and components]

## Technical Approach

### Technology Choices
- **Frontend**: [Technology and rationale]
- **Backend**: [Technology and rationale]
- **Database**: [Technology and rationale]
- **Infrastructure**: [Technology and rationale]

### Design Patterns
- **[Pattern 1]**: [How and why it's used]
- **[Pattern 2]**: [How and why it's used]
- **[Pattern 3]**: [How and why it's used]

### Security Considerations
- **Authentication**: [Authentication approach and security measures]
- **Authorization**: [Authorization model and access control]
- **Data Protection**: [Data encryption and privacy measures]
- **Input Validation**: [Input validation and sanitization approach]

### Performance Considerations
- **Caching Strategy**: [Caching approach and implementation]
- **Database Optimization**: [Database performance optimization]
- **Frontend Optimization**: [Frontend performance optimization]
- **Scalability**: [Scalability considerations and approach]

## User Experience Design

### User Interface
- **Layout**: [Overall layout and structure]
- **Navigation**: [Navigation patterns and flow]
- **Responsiveness**: [Responsive design approach]
- **Accessibility**: [Accessibility features and compliance]

### User Interaction
- **Workflow**: [User workflow and interaction patterns]
- **Feedback**: [User feedback and status indicators]
- **Error Handling**: [Error messages and recovery]
- **Help and Documentation**: [User assistance and guidance]

### User Testing
- **Usability Testing**: [Usability testing approach]
- **User Feedback**: [How user feedback will be gathered]
- **Iteration**: [How the solution will be refined based on feedback]

## Implementation Plan

### Phase 1: Foundation
- [ ] [Task 1 - specific and actionable]
- [ ] [Task 2 - specific and actionable]
- [ ] [Task 3 - specific and actionable]

### Phase 2: Core Implementation
- [ ] [Task 1 - specific and actionable]
- [ ] [Task 2 - specific and actionable]
- [ ] [Task 3 - specific and actionable]

### Phase 3: Integration and Testing
- [ ] [Task 1 - specific and actionable]
- [ ] [Task 2 - specific and actionable]
- [ ] [Task 3 - specific and actionable]

### Phase 4: Deployment and Validation
- [ ] [Task 1 - specific and actionable]
- [ ] [Task 2 - specific and actionable]
- [ ] [Task 3 - specific and actionable]

## Dependencies

### Internal Dependencies
- **[Dependency 1]**: [What needs to be completed first and why]
- **[Dependency 2]**: [What needs to be completed first and why]
- **[Dependency 3]**: [What needs to be completed first and why]

### External Dependencies
- **[Dependency 1]**: [External system, service, or approval needed]
- **[Dependency 2]**: [External system, service, or approval needed]
- **[Dependency 3]**: [External system, service, or approval needed]

### Technical Dependencies
- **[Dependency 1]**: [Technical prerequisite or infrastructure needed]
- **[Dependency 2]**: [Technical prerequisite or infrastructure needed]
- **[Dependency 3]**: [Technical prerequisite or infrastructure needed]

## Risks and Mitigation

### Technical Risks
- **Risk 1**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]
- **Risk 2**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]
- **Risk 3**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]

### Business Risks
- **Risk 1**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]
- **Risk 2**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]

### User Experience Risks
- **Risk 1**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]
- **Risk 2**: [Description and potential impact]
  - **Mitigation**: [Specific approach to reduce or eliminate risk]

## Testing Strategy

### Unit Testing
- **Coverage Target**: [Target test coverage percentage]
- **Testing Approach**: [How unit tests will be written and organized]
- **Mocking Strategy**: [How dependencies will be mocked]

### Integration Testing
- **Testing Scope**: [What integration scenarios will be tested]
- **Testing Environment**: [Testing environment setup and configuration]
- **Data Management**: [How test data will be managed]

### User Acceptance Testing
- **Test Scenarios**: [Key user scenarios to test]
- **User Involvement**: [How users will be involved in testing]
- **Acceptance Criteria**: [Criteria for user acceptance]

### Performance Testing
- **Performance Targets**: [Specific performance metrics and targets]
- **Testing Tools**: [Tools and approaches for performance testing]
- **Load Testing**: [Load testing approach and scenarios]

## Quality Assurance

### Code Quality
- **Standards**: [Coding standards and best practices to follow]
- **Review Process**: [Code review process and requirements]
- **Static Analysis**: [Static analysis tools and configuration]

### Documentation
- **Code Documentation**: [Code documentation requirements]
- **API Documentation**: [API documentation requirements]
- **User Documentation**: [User documentation requirements]

### Monitoring and Observability
- **Logging**: [Logging strategy and implementation]
- **Metrics**: [Key metrics to track and monitor]
- **Alerting**: [Alerting strategy and thresholds]

## Deployment Strategy

### Environment Strategy
- **Development**: [Development environment setup]
- **Staging**: [Staging environment setup]
- **Production**: [Production environment setup]

### Deployment Process
- **Deployment Method**: [Deployment approach (CI/CD, manual, etc.)]
- **Rollback Strategy**: [Rollback approach and procedures]
- **Database Migrations**: [Database migration strategy]

### Release Strategy
- **Release Planning**: [Release planning and coordination]
- **Feature Flags**: [Feature flag strategy for gradual rollout]
- **User Communication**: [How users will be informed of changes]

## Success Metrics

### Technical Metrics
- **Performance**: [Specific performance metrics and targets]
- **Reliability**: [Reliability metrics and targets]
- **Security**: [Security metrics and targets]

### User Metrics
- **Adoption**: [User adoption metrics and targets]
- **Satisfaction**: [User satisfaction metrics and targets]
- **Engagement**: [User engagement metrics and targets]

### Business Metrics
- **Efficiency**: [Business efficiency metrics and targets]
- **Cost**: [Cost metrics and targets]
- **ROI**: [Return on investment metrics and targets]

## Timeline and Resources

### Timeline
- **Start Date**: [YYYY-MM-DD]
- **Phase 1**: [Duration and completion date]
- **Phase 2**: [Duration and completion date]
- **Phase 3**: [Duration and completion date]
- **Phase 4**: [Duration and completion date]
- **Total Duration**: [Total project duration]

### Resource Requirements
- **Development Team**: [Team composition and roles]
- **Infrastructure**: [Infrastructure requirements and costs]
- **Third-Party Services**: [External services and costs]
- **Training**: [Training requirements and costs]

## Squad Coordination

### Agent Assignments
- **@agent:software-engineer**: [Specific responsibilities and tasks]
- **@agent:ux-expert**: [Specific responsibilities and tasks]
- **@agent:ui-implementor**: [Specific responsibilities and tasks]
- **@agent:git-workflow**: [Specific responsibilities and tasks]
- **@agent:collaboration**: [Specific responsibilities and tasks]

### Handoff Protocols
- **Design to Development**: [How UX/UI designs are handed off to development]
- **Development to Testing**: [How developed features are handed off to testing]
- **Testing to Deployment**: [How tested features are handed off to deployment]

### Quality Gates
- **Design Review**: [Design review requirements and process]
- **Code Review**: [Code review requirements and process]
- **Testing Review**: [Testing review requirements and process]
- **Deployment Review**: [Deployment review requirements and process]

## Notes

[Additional context, considerations, or special instructions]
```

## Usage Examples

### Example 1: User Authentication Solution
```markdown
# Solution: User Authentication System

## Solution Overview
Implement a secure, scalable user authentication system using JWT tokens and bcrypt password hashing.

## Technical Approach
### Technology Choices
- **Backend**: Node.js with Express and JWT
- **Database**: PostgreSQL with bcrypt for password hashing
- **Security**: JWT tokens with refresh token rotation

### Implementation Plan
#### Phase 1: Foundation
- [ ] Set up authentication database schema
- [ ] Implement password hashing utilities
- [ ] Create JWT token management
```

### Example 2: Performance Optimization Solution
```markdown
# Solution: Page Load Performance Optimization

## Solution Overview
Implement comprehensive performance optimization including lazy loading, caching, and code splitting.

## Technical Approach
### Technology Choices
- **Frontend**: React with React.lazy and Suspense
- **Caching**: Redis for server-side caching
- **Build Tools**: Webpack with code splitting

### Implementation Plan
#### Phase 1: Foundation
- [ ] Implement code splitting strategy
- [ ] Set up Redis caching infrastructure
- [ ] Configure build optimization
```

### Example 3: Accessibility Solution
```markdown
# Solution: Accessibility Compliance

## Solution Overview
Implement comprehensive accessibility features including screen reader support, keyboard navigation, and ARIA compliance.

## Technical Approach
### Technology Choices
- **Frontend**: React with ARIA attributes and semantic HTML
- **Testing**: axe-core for automated accessibility testing
- **Standards**: WCAG 2.1 AA compliance

### Implementation Plan
#### Phase 1: Foundation
- [ ] Audit current accessibility state
- [ ] Implement semantic HTML structure
- [ ] Add ARIA attributes and labels
```

## Best Practices

### Solution Design
- **User-Centric**: Always consider user experience and needs
- **Technical Feasibility**: Ensure the solution is technically achievable
- **Scalability**: Design for future growth and expansion
- **Security**: Prioritize security in all design decisions

### Implementation Planning
- **Incremental**: Break work into manageable, testable increments
- **Dependencies**: Clearly identify and manage dependencies
- **Risk Management**: Proactively identify and mitigate risks
- **Quality Focus**: Maintain high quality throughout implementation

### Documentation
- **Clear and Complete**: Provide comprehensive documentation
- **Stakeholder Communication**: Ensure all stakeholders understand the solution
- **Technical Detail**: Include sufficient technical detail for implementation
- **Maintenance**: Keep documentation current as the solution evolves

### Validation
- **User Testing**: Validate the solution with actual users
- **Technical Review**: Ensure technical feasibility and quality
- **Stakeholder Approval**: Get approval from all relevant stakeholders
- **Continuous Improvement**: Be willing to refine the solution based on feedback
