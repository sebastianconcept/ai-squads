---
description: Problem Template - Problem definition and analysis
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Problem Template

## Overview

Problem documents define the issue, need, or opportunity that a feature aims to address. They provide clear context for why work is needed and what success looks like.

## Template Structure

### Problem File
```
projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/problem.md
```

## Template Content

```markdown
---
description: Feature Problem - [FEATURE_NAME]
type: feature-problem
priority: [low|medium|high]
status: [identified|analyzed|validated]
---

# Problem: [FEATURE_NAME]

## Problem Statement

**What problem are we solving?**
[Clear, concise description of the problem or need]

## Problem Context

### Current Situation
[Describe the current state and what's not working]

### User Impact
[How does this problem affect users? What are they experiencing?]

### Business Impact
[How does this problem affect business goals, metrics, or operations?]

### Technical Impact
[How does this problem affect the system, performance, or maintainability?]

## Problem Analysis

### Root Cause
[What is the underlying cause of this problem?]

### Scope
[How widespread is this problem? Who/what is affected?]

### Frequency
[How often does this problem occur?]

### Severity
[How critical is this problem? What are the consequences?]

## User Stories

### Primary User Story
**As a [user type], I want [capability] so that [benefit]**

### Additional User Stories
- **As a [user type], I want [capability] so that [benefit]**
- **As a [user type], I want [capability] so that [benefit]**
- **As a [user type], I want [capability] so that [benefit]**

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

### User Constraints
- **Accessibility**: [Accessibility requirements]
- **Usability**: [Usability constraints or requirements]
- **Learning Curve**: [User learning requirements]
- **Device Support**: [Device or platform requirements]

## Success Criteria

### Problem Resolution
- [ ] [Specific criterion 1 - measurable and testable]
- [ ] [Specific criterion 2 - measurable and testable]
- [ ] [Specific criterion 3 - measurable and testable]

### User Experience
- [ ] [UX criterion 1]
- [ ] [UX criterion 2]
- [ ] [UX criterion 3]

### Business Value
- [ ] [Business criterion 1]
- [ ] [Business criterion 2]
- [ ] [Business criterion 3]

## Related Problems

### Dependencies
- [Problem 1] - [How it relates]
- [Problem 2] - [How it relates]
- [Problem 3] - [How it relates]

### Blocked Problems
- [Problem 1] - [How this problem blocks it]
- [Problem 2] - [How this problem blocks it]
- [Problem 3] - [How this problem blocks it]

## Stakeholders

### Primary Stakeholders
- **[Stakeholder 1]**: [Role and interest in this problem]
- **[Stakeholder 2]**: [Role and interest in this problem]
- **[Stakeholder 3]**: [Role and interest in this problem]

### Secondary Stakeholders
- **[Stakeholder 1]**: [Role and interest in this problem]
- **[Stakeholder 2]**: [Role and interest in this problem]

## Research and Data

### User Research
- **User Interviews**: [Key findings from user interviews]
- **User Surveys**: [Key findings from user surveys]
- **User Analytics**: [Key metrics and data points]
- **User Feedback**: [Key feedback and complaints]

### Market Research
- **Competitor Analysis**: [How competitors handle this problem]
- **Industry Trends**: [Relevant industry trends and patterns]
- **Market Size**: [Market opportunity and potential impact]

### Technical Research
- **Performance Data**: [Current performance metrics]
- **Error Logs**: [Error frequency and patterns]
- **System Monitoring**: [System health and stability data]
- **Technical Debt**: [Accumulated technical debt impact]

## Risk Assessment

### High-Risk Areas
- **Risk 1**: [Description and potential impact]
- **Risk 2**: [Description and potential impact]
- **Risk 3**: [Description and potential impact]

### Mitigation Strategies
- **Risk 1**: [Specific mitigation approach]
- **Risk 2**: [Specific mitigation approach]
- **Risk 3**: [Specific mitigation approach]

## Timeline and Urgency

### Urgency Level
- **Critical**: [Immediate action required]
- **High**: [Action required within [timeframe]]
- **Medium**: [Action required within [timeframe]]
- **Low**: [Action required within [timeframe]]

### Impact Timeline
- **Immediate**: [Immediate consequences]
- **Short-term**: [Consequences within [timeframe]]
- **Long-term**: [Long-term consequences if not addressed]

## Notes

[Additional context, considerations, or special instructions]
```

## Usage Examples

### Example 1: User Authentication Problem
```markdown
# Problem: User Authentication System

## Problem Statement
Users cannot create accounts or log into the application, preventing access to core functionality.

## Problem Context
### Current Situation
The application has no user authentication system. Users cannot create accounts, log in, or access personalized features.

### User Impact
Users cannot access the application's core functionality, leading to frustration and abandonment.

### Business Impact
No user engagement, no user data collection, no monetization potential.
```

### Example 2: Performance Problem
```markdown
# Problem: Slow Page Load Times

## Problem Statement
Application pages take 5+ seconds to load, causing user frustration and abandonment.

## Problem Context
### Current Situation
Page load times average 5-8 seconds, with some pages taking up to 15 seconds.

### User Impact
Users abandon the application due to slow performance, leading to high bounce rates.

### Business Impact
Reduced user engagement, increased support tickets, negative user reviews.
```

### Example 3: Accessibility Problem
```markdown
# Problem: Poor Accessibility Support

## Problem Statement
The application is not accessible to users with disabilities, excluding a significant user base.

## Problem Context
### Current Situation
The application lacks proper accessibility features like screen reader support, keyboard navigation, and color contrast.

### User Impact
Users with disabilities cannot effectively use the application, excluding them from the user base.

### Business Impact
Legal compliance risks, reduced market reach, negative brand perception.
```

## Best Practices

### Problem Definition
- **Be Specific**: Clearly define what the problem is
- **Be Measurable**: Include metrics and data when possible
- **Be User-Focused**: Focus on user impact and experience
- **Be Actionable**: Ensure the problem can be addressed

### Problem Analysis
- **Root Cause**: Identify the underlying cause, not just symptoms
- **Scope**: Understand the full extent of the problem
- **Impact**: Assess both user and business impact
- **Urgency**: Determine how quickly the problem needs to be solved

### Documentation
- **Clear Language**: Use clear, understandable language
- **Structured Format**: Follow consistent formatting and structure
- **Evidence**: Include data, research, and user feedback
- **Stakeholders**: Identify all relevant stakeholders and their interests

### Validation
- **User Research**: Validate the problem with actual users
- **Data Analysis**: Use data to confirm problem scope and impact
- **Stakeholder Input**: Get input from all relevant stakeholders
- **Technical Review**: Ensure technical feasibility of solving the problem
