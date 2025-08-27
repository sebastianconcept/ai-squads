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
   - Create project subdirectory structure for complete isolation
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
.squads-ai/projects/[PROJECT_NAME]/
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
.squads-ai/projects/[PROJECT_NAME]/
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

**Quick Start**: Use the automated script for consistent feature planning:
```bash
./scripts/plan-feature.sh <project_name> <feature_name> [options]
```

**Workflow**:
1. **Feature Analysis**
   - Define feature scope and requirements
   - Identify user stories and acceptance criteria
   - Assess technical complexity and dependencies

2. **JTBD Analysis**
   - @agent:jtbd-expert analyzes customer jobs and validates feature alignment
   - Identifies satisfaction gaps and unintended consequences
   - Ensures solution addresses real customer needs

3. **Documentation Creation**
   - Create feature directory: `.squads-ai/projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/`
   - Create required documents: `problem.md`, `solution.md`, `jtbd-analysis.md`, `goal.md`, `tasks.md`
   - Ensure project has all required planning files (mission.md, roadmap.md, tech-stack.md, decisions.md, tasks.md)

3. **Planning Validation**
   - Review with appropriate squad agents
   - Validate technical feasibility
   - Confirm resource availability
   - Validate JTBD analysis and customer job alignment

**Output Structure**:
```
.squads-ai/projects/[PROJECT_NAME]/feature-[FEATURE_NAME]/
├── problem.md          # What problem are we solving?
├── solution.md         # How will we solve it?
├── jtbd-analysis.md    # Customer jobs and satisfaction analysis
├── tasks.md            # List of tasks planned to be executed
├── goal.md             # What's the success criteria?
├── status.md           # Current status and progress tracking
├── implementation-status.md # Implementation phase tracking
└── .cursor-rule.md     # Cursor integration for the feature
```

**For detailed instructions and Cursor integration, see**: `.squads-ai/instructions/plan-feature.mdc`

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
   - Create hotfix directory: `.squads-ai/projects/[PROJECT_NAME]/hotfix-[HOTFIX_NAME]/`
   - Create required documents: `issue.md`, `goal.md`, `tasks.md`

**Output Structure**:
```
.squads-ai/projects/[PROJECT_NAME]/hotfix-[HOTFIX_NAME]/
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
3. **JTBD Analysis**: @agent:jtbd-expert validates customer job alignment
4. **Documentation Creation**: Create required planning documents including jtbd-analysis.md
5. **Task Breakdown**: Create comprehensive task breakdown with agent assignments
6. **Validation**: Review and validate planning with squad including JTBD validation

### Execution Phase
1. **Work Assignment**: Assign work to appropriate squad agents
2. **JTBD Validation**: @agent:jtbd-expert validates implementation alignment with customer jobs
3. **Implementation**: Execute planned work following squad standards
4. **Quality Gates**: Ensure work meets defined quality standards including JTBD validation
5. **Documentation Updates**: Update documentation as work progresses

### Review Phase
1. **Goal Validation**: Validate work against defined goals including customer job satisfaction
2. **JTBD Review**: @agent:jtbd-expert validates customer job satisfaction improvement
3. **Quality Review**: Ensure quality standards are met
4. **Documentation Review**: Update final documentation
5. **Knowledge Transfer**: Share learnings with the team

## Quality Gates

### Planning Quality
- **Completeness**: All required documents created
- **Clarity**: Clear problem and solution definitions
- **JTBD Validation**: Customer jobs identified and solution alignment validated
- **Feasibility**: Technical and resource feasibility confirmed
- **Alignment**: Alignment with project and squad goals
- **Task Breakdown**: Comprehensive task breakdown with clear agent assignments

### Pre-Commit Quality Gates
**MANDATORY**: These quality gates must pass before any commit:

#### Rust Projects
- [ ] `cargo fmt --all -- --check` - Check that code is properly formatted
- [ ] `cargo fmt` - Code is properly formatted
- [ ] `cargo clippy --all-targets --all-features -- -D warnings` - Full Clippy with warnings as errors
- [ ] `cargo test` - All tests passing
- [ ] `cargo check` - Code compiles without errors

#### JavaScript/TypeScript Projects
- [ ] `npm run lint` - ESLint passes without errors
- [ ] `npm run format` - Prettier formatting applied
- [ ] `npm test` - All tests passing
- [ ] `npm run build` - Build succeeds without errors

#### General Quality Gates
- [ ] Code review completed and approved
- [ ] Documentation updated for changes
- [ ] JTBD analysis completed and validated
- [ ] No TODO/FIXME comments left in production code
- [ ] Commit message follows conventional format

**ENFORCEMENT**: The git-workflow agent will refuse to commit if any quality gate fails. Issues must be resolved before committing.

## 🚨 **ENGINEER RESPONSIBILITIES**

### **Before Every Commit, Engineers MUST Run:**

#### **Rust Projects**
```bash
# 1. Format code
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

### **Quality Gate Enforcement**
- **@agent:git-workflow** will verify these commands were run and passed
- **@agent:jtbd-expert** will verify JTBD analysis is completed and validated
- **No exceptions**: Quality gates must pass before any commit
- **Team accountability**: Engineers are responsible for running these checks
- **Failure reporting**: Report any quality gate failures to the team

### **Feature Branch Workflow Enforcement**
- **@agent:git-workflow** will verify proper feature branch creation sequence
- **@agent:jtbd-expert** will verify JTBD analysis is completed before feature development
- **Mandatory sequence**: Switch to main → Pull latest → Create feature branch
- **No exceptions**: Feature development cannot start without proper git workflow and JTBD validation
- **Team accountability**: Engineers must follow git workflow and JTBD methodology before development

### Execution Quality
- **Standards Compliance**: Follow squad coding and quality standards
- **Testing**: Comprehensive testing coverage
- **Documentation**: Updated documentation and code comments
- **Performance**: Meet performance and quality requirements
- **JTBD Validation**: Solution implementation aligns with customer job satisfaction
- **Quality Gates**: All pre-commit quality gates must pass before any commit including JTBD validation

### Review Quality
- **Goal Achievement**: All defined goals met including customer job satisfaction
- **Quality Standards**: All quality standards satisfied
- **JTBD Validation**: Customer job satisfaction goals achieved and measured
- **Documentation**: Complete and accurate documentation including JTBD analysis
- **Knowledge Transfer**: Learnings shared with the team

## Task Management Integration

### Task Creation Process
1. **Planning Phase**: Create tasks based on problem analysis and solution design
2. **JTBD Analysis**: @agent:jtbd-expert validates customer job alignment and creates satisfaction metrics
3. **Agent Assignment**: Assign tasks to appropriate squad agents based on capabilities
4. **Dependency Mapping**: Identify task dependencies and create execution sequence
5. **Effort Estimation**: Estimate effort and priority for each task category
6. **Quality Gates**: Include verification tasks for each major deliverable including JTBD validation

### Task Execution Workflow
1. **Task Assignment**: Agents receive assigned tasks with clear acceptance criteria
2. **JTBD Validation**: @agent:jtbd-expert validates task alignment with customer jobs
3. **Progress Tracking**: Regular updates on task status and completion
4. **Dependency Management**: Monitor blocked tasks and dependencies
5. **Quality Validation**: Complete verification tasks before marking categories complete including JTBD validation
6. **Handoff Coordination**: Coordinate handoffs between agents for dependent tasks

## Integration with Squad Workflows

### Elite Squad Integration
- **Rust Projects**: Use Rust-specific standards and workflows
- **Smalltalk Projects**: Use Smalltalk-specific standards and workflows
- **C++ Projects**: Use C++-specific standards and workflows
- **Full-Stack Projects**: Coordinate between backend and frontend agents
- **JTBD Methodology**: All projects include customer job analysis and validation

### JTBD Integration
- **@agent:jtbd-expert**: Participates in all planning workflows
- **Customer Job Analysis**: Validates feature alignment with customer needs
- **Satisfaction Gap Identification**: Identifies and prioritizes customer satisfaction gaps
- **Solution Validation**: Ensures solutions address real customer jobs
- **Unintended Consequence Analysis**: Identifies potential negative side effects

### Agent Coordination
- **Director Agent**: Overall planning coordination and oversight
- **JTBD Expert**: Customer jobs analysis and solution validation
- **Software Engineer**: Technical solution design and implementation
- **UX Expert**: User experience and interface design
- **UI Implementor**: Frontend implementation and styling
- **Git Workflow**: Version control and release management
- **Collaboration**: Team coordination and quality assurance

## JTBD Methodology Integration

### Core JTBD Principles
- **Jobs, Not Products**: Focus on what customers are trying to accomplish
- **Progress, Not Satisfaction**: Measure improvement in job completion
- **Context Matters**: Understand job circumstances and constraints
- **Emotional and Social Jobs**: Address functional, emotional, and social needs
- **Unintended Consequences**: Identify and mitigate negative side effects

### JTBD Workflow Integration
- **Feature Planning**: @agent:jtbd-expert analyzes customer jobs and validates feature alignment
- **Solution Design**: JTBD validation ensures solutions address real satisfaction gaps
- **Project Scope**: JTBD analysis validates project alignment with customer needs
- **Quality Gates**: JTBD validation required before feature approval
- **Success Metrics**: Job satisfaction improvement measured and tracked

### JTBD Document Requirements
- **jtbd-analysis.md**: Required for all feature planning
- **Customer Jobs**: Clearly identified and articulated
- **Satisfaction Gaps**: Prioritized by impact and frequency
- **Solution Alignment**: Validated against customer jobs
- **Unintended Consequences**: Identified and mitigation strategies planned

## Best Practices

### Planning Best Practices
- **Start with Problem**: Clearly define the problem before designing solutions
- **JTBD Analysis**: Always analyze customer jobs and satisfaction gaps
- **User-Focused**: Always consider user impact and experience
- **Technical Feasibility**: Validate technical approach before committing
- **Resource Planning**: Ensure adequate resources and time allocation
- **Risk Assessment**: Identify and plan for potential risks
- **Customer Validation**: Ensure solutions address real customer needs

### Documentation Best Practices
- **Clear and Concise**: Write clear, understandable documentation
- **Consistent Format**: Use consistent formatting and structure
- **JTBD Integration**: Include customer job analysis in all feature documentation
- **Regular Updates**: Keep documentation current as work progresses
- **Accessible**: Make documentation easily accessible to the team
- **Version Control**: Track documentation changes in version control
- **Customer Focus**: Document customer jobs and satisfaction goals

### Execution Best Practices
- **Follow Standards**: Adhere to squad coding and quality standards
- **JTBD Validation**: Ensure implementation aligns with customer job satisfaction
- **Regular Reviews**: Conduct regular code and quality reviews
- **Testing**: Maintain comprehensive testing coverage
- **Documentation**: Update documentation as code changes
- **Communication**: Maintain clear communication with the team
- **Customer Success**: Measure and track customer job satisfaction improvement
