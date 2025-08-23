---
description: Planning Workflows - Squad planning and documentation system
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Planning Workflows

## Overview

This document defines the planning workflows for the SquadsAI system. The squad uses these workflows to plan and document work before execution, ensuring clear understanding and proper coordination.

## Planning Commands

### 1. `plan-project` - New Project Planning

**Purpose**: Plan a completely new project from scratch

**Workflow**:
1. **Project Initialization**
   - Create project directory: `.squads-ai/projects/[PROJECT_NAME]/`
   - Copy core templates: `mission.md`, `roadmap.md`, `tech-stack.md`, `decisions.md`, `README.md`
   - Ask user any questions that you need for project specifics

2. **Squad Assignment**
   - Identify appropriate squad for project type
   - Assign squad agents to project roles
   - Define handoff protocols between agents

3. **Documentation Creation**
   - **Mission**: Define project vision and purpose
   - **Roadmap**: Plan development phases and features
   - **Tech Stack**: Choose appropriate technologies
   - **Decisions**: Document key architectural choices
   - **Tasks**: Create comprehensive task breakdown with agent assignments

**Output Structure**:
```
projects/[PROJECT_NAME]/
├── mission.md          # Project vision and purpose
├── roadmap.md          # Development phases and features. It has a list of features.
├── tech-stack.md       # Technical architecture
├── dependencies.md     # Current external repositories that would need changes to add features or fixes to the project
├── decisions.md        # Decision log
├── tasks.md            # Comprehensive task breakdown with agent assignments
└── README.md           # Project overview
```

### 2. `adopt-project` - Existing Project Adoption

**Purpose**: Adopt an existing project into the SquadsAI system

**Workflow**:
1. **Project Analysis**
   - Analyze existing project structure and codebase
   - Identify current state and technical debt
   - Assess alignment with squad standards
   - Analyze existing project dependencies
   - Ask anything you need about external project dependencies

2. **Documentation Gap Analysis**
   - Identify missing documentation
   - Create missing project files
   - Update existing documentation to match standards
   - Create comprehensive task breakdown for existing work

3. **Squad Integration**
   - Assign appropriate squad and agents
   - Define quality gates and standards
   - Establish workflow integration

**Output Structure**:
```
projects/[PROJECT_NAME]/
├── mission.md          # Updated project vision
├── roadmap.md          # Current state and future plans. It has a list of features.
├── tech-stack.md       # Current and target architecture
├── dependencies.md     # Current external repositories that would need changes to add features or fixes to the project
├── decisions.md        # Historical and new decisions
├── tasks.md            # Comprehensive task breakdown for existing and planned work
├── README.md           # Updated project overview
└── adoption-plan.md    # Adoption strategy and timeline
```

### 3. `plan-feature` - Feature Planning

**Purpose**: Plan a new feature for an existing project

**Workflow**:
1. **Feature Analysis**
   - Define feature scope and requirements
   - Identify user stories and acceptance criteria
   - Assess technical complexity and dependencies

2. **Documentation Creation**
   - Create feature directory: `projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/`
   - Create required documents: `problem.md`, `solution.md`, `goal.md`, `tasks.md`

3. **Planning Validation**
   - Review with appropriate squad agents
   - Validate technical feasibility
   - Confirm resource availability

**Output Structure**:
```
projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/
├── problem.md          # What problem are we solving?
├── solution.md         # How will we solve it?
├── tasks.md            # List of tasks planed to be executed
└── goal.md             # What's the success criteria?
```

### 4. `plan-fix` - Problem Fix Planning

**Purpose**: Plan how to fix a problem in an existing project

**Workflow**:
1. **Problem Analysis**
   - Identify and document the problem
   - Assess impact and urgency
   - Determine root cause and scope

2. **Solution Planning**
   - Design fix approach and strategy
   - Identify risks and mitigation strategies
   - Plan testing and validation approach

3. **Documentation Creation**
   - Create hotfix directory: `projects/[PROJECT_NAME]/hotfix-[HOTFIX_NAME]/`
   - Create required documents: `issue.md`, `goal.md`, `tasks.md`

**Output Structure**:
```
projects/[PROJECT_NAME]/hotfix-[HOTFIX_NAME]/
├── issue.md            # What's the issue?
├── tasks.md            # List of tasks planed to be executed
└── goal.md             # What's the fix goal?
```

## Document Templates

### Feature Planning Documents

#### `problem.md` - Problem Definition
```markdown
---
description: Feature Problem - [FEATURE_NAME]
type: feature-problem
priority: [low|medium|high]
---

# Problem: [FEATURE_NAME]

## Problem Statement
[Clear description of the problem or need]

## User Impact
[How does this problem affect users?]

## Business Impact
[How does this problem affect business goals?]

## Current State
[What's the current situation?]

## Desired State
[What would the ideal situation look like?]

## Constraints
[What limitations or constraints exist?]
```

#### `solution.md` - Solution Design
```markdown
---
description: Feature Solution - [FEATURE_NAME]
type: feature-solution
status: [planned|in-progress|completed]
---

# Solution: [FEATURE_NAME]

## Solution Overview
[High-level description of the solution approach]

## Technical Approach
[Technical implementation details]

## User Experience
[How will users interact with this feature?]

## Implementation Plan
[Step-by-step implementation approach]

## Dependencies
[What needs to be built or resolved first?]

## Risks and Mitigation
[Potential risks and how to address them]
```

### Hotfix Planning Documents

#### `issue.md` - Issue Definition
```markdown
---
description: Hotfix Issue - [HOTFIX_NAME]
type: hotfix-issue
priority: [low|medium|high|critical]
---

# Issue: [HOTFIX_NAME]

## Issue Description
[Clear description of the issue]

## Impact Assessment
- **User Impact**: [How does this affect users?]
- **Business Impact**: [How does this affect business?]
- **System Impact**: [How does this affect the system?]

## Current Behavior
[What's happening now?]

## Expected Behavior
[What should happen?]

## Steps to Reproduce
[How can this issue be reproduced?]

## Environment
[Where does this issue occur?]
```

## Squad Planning Process

### Planning Phase
1. **Command Execution**: Execute planning command (`plan-project`, `adopt-project`, `plan-feature`, `plan-fix`)
2. **Squad Activation**: Activate appropriate squad agents for planning
3. **Documentation Creation**: Create required planning documents
4. **Task Breakdown**: Create comprehensive task breakdown with agent assignments
5. **Validation**: Review and validate planning with squad

### Execution Phase
1. **Work Assignment**: Assign work to appropriate squad agents
2. **Implementation**: Execute planned work following squad standards
3. **Quality Gates**: Ensure work meets defined quality standards
4. **Documentation Updates**: Update documentation as work progresses

### Review Phase
1. **Goal Validation**: Validate work against defined goals
2. **Quality Review**: Ensure quality standards are met
3. **Documentation Review**: Update final documentation
4. **Knowledge Transfer**: Share learnings with the team

## Quality Gates

### Planning Quality
- **Completeness**: All required documents created
- **Clarity**: Clear problem and solution definitions
- **Feasibility**: Technical and resource feasibility confirmed
- **Alignment**: Alignment with project and squad goals
- **Task Breakdown**: Comprehensive task breakdown with clear agent assignments

### Execution Quality
- **Standards Compliance**: Follow squad coding and quality standards
- **Testing**: Comprehensive testing coverage
- **Documentation**: Updated documentation and code comments
- **Performance**: Meet performance and quality requirements

### Review Quality
- **Goal Achievement**: All defined goals met
- **Quality Standards**: All quality standards satisfied
- **Documentation**: Complete and accurate documentation
- **Knowledge Transfer**: Learnings shared with the team

## Task Management Integration

### Task Creation Process
1. **Planning Phase**: Create tasks based on problem analysis and solution design
2. **Agent Assignment**: Assign tasks to appropriate squad agents based on capabilities
3. **Dependency Mapping**: Identify task dependencies and create execution sequence
4. **Effort Estimation**: Estimate effort and priority for each task category
5. **Quality Gates**: Include verification tasks for each major deliverable

### Task Execution Workflow
1. **Task Assignment**: Agents receive assigned tasks with clear acceptance criteria
2. **Progress Tracking**: Regular updates on task status and completion
3. **Dependency Management**: Monitor blocked tasks and dependencies
4. **Quality Validation**: Complete verification tasks before marking categories complete
5. **Handoff Coordination**: Coordinate handoffs between agents for dependent tasks

## Integration with Squad Workflows

### Elite Squad Integration
- **Rust Projects**: Use Rust-specific standards and workflows
- **Smalltalk Projects**: Use Smalltalk-specific standards and workflows
- **C++ Projects**: Use C++-specific standards and workflows
- **Full-Stack Projects**: Coordinate between backend and frontend agents

### Agent Coordination
- **Director Agent**: Overall planning coordination and oversight
- **Software Engineer**: Technical solution design and implementation
- **UX Expert**: User experience and interface design
- **UI Implementor**: Frontend implementation and styling
- **Git Workflow**: Version control and release management
- **Collaboration**: Team coordination and quality assurance

## Best Practices

### Planning Best Practices
- **Start with Problem**: Clearly define the problem before designing solutions
- **User-Focused**: Always consider user impact and experience
- **Technical Feasibility**: Validate technical approach before committing
- **Resource Planning**: Ensure adequate resources and time allocation
- **Risk Assessment**: Identify and plan for potential risks

### Documentation Best Practices
- **Clear and Concise**: Write clear, understandable documentation
- **Consistent Format**: Use consistent formatting and structure
- **Regular Updates**: Keep documentation current as work progresses
- **Accessible**: Make documentation easily accessible to the team
- **Version Control**: Track documentation changes in version control

### Execution Best Practices
- **Follow Standards**: Adhere to squad coding and quality standards
- **Regular Reviews**: Conduct regular code and quality reviews
- **Testing**: Maintain comprehensive testing coverage
- **Documentation**: Update documentation as code changes
- **Communication**: Maintain clear communication with the team
