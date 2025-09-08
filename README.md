# SquadsAI

> **Planning-First, Execution-Second AI Agent Collaboration System**

SquadsAI is a sophisticated multi-agent collaboration system that structures development work through comprehensive planning and documentation before any code is written. It ensures high quality, clear communication, and successful project delivery through specialized AI agents working in coordinated squads.

## 🎯 **System Philosophy**

SquadsAI is built on a **"planning-first, execution-second"** philosophy. The squad plans and documents work comprehensively before any code is written, ensuring clear understanding, proper coordination, and high-quality deliverables.

## Applicability

SquadsAI unlocks experimentation with different **APP (Agents Process Project) dynamics**, enabling AI agent squads to work on projects using their preferred standards and skills. This approach allows for:
- **Customized Workflows**: Tailored to specific project requirements
- **Flexible Team Structures**: Different squad configurations for different needs
- **Adaptable Standards**: Project-specific quality and coding standards
- **Scalable Collaboration**: From small teams to large organizations
- **Optimized Tech Stacks**: Each project can select the best technologies for their specific goals
- **Strategic Team Assembly**: Projects can assemble the ideal team composition for their direction and objectives

## 🏗️ **System Architecture**

### **Core Components**
```
SquadsAI System
├── Workflows/             # Process definitions and coordination
├── Agents/                # Specialized AI agents
├── Squads/                # Team configurations
├── Standards/             # Quality and coding standards
├── Templates/             # Document templates
└── Projects/              # Project-specific documentation
```

### **Workflow Flow**
```
User Request → Planning Command → Squad Planning → Documentation → Execution → Quality Gates → Delivery
```

## 📋 **Planning Commands**

### **1. `plan-project`** - New Project Planning
**Purpose**: Plan a completely new project from scratch

**Workflow**:
1. **Project Initialization**: Create project structure and copy core templates
2. **Squad Assignment**: Identify appropriate squad and assign agents
3. **Documentation Creation**: Create mission, roadmap, tech stack, and decisions

**Output**: Complete project foundation with all core documents

### **2. `adopt-project`** - Existing Project Adoption
**Purpose**: Adopt an existing project into the SquadsAI system

**Workflow**:
1. **Project Analysis**: Analyze existing codebase and identify gaps
2. **Documentation Gap Analysis**: Create missing project files
3. **Squad Integration**: Assign squad and establish quality gates

**Output**: Updated project with complete documentation and squad integration

### **3. `plan-feature`** - Feature Planning
**Purpose**: Plan a new feature for an existing project

**Workflow**:
1. **Feature Analysis**: Define scope, requirements, and complexity
2. **Documentation Creation**: Create problem, solution, and goal documents
3. **Planning Validation**: Review with squad agents and validate feasibility

**Output**: Feature directory with complete planning documentation

### **4. `plan-fix`** - Problem Fix Planning
**Purpose**: Plan how to fix a problem in an existing project

**Workflow**:
1. **Problem Analysis**: Identify issue, assess impact, and determine scope
2. **Solution Planning**: Design fix approach and identify risks
3. **Documentation Creation**: Create issue and goal documents

**Output**: Hotfix directory with problem analysis and solution plan

## 📁 **Documentation Structure**

### **Project Structure**
```
.ai-squads/projects/[PROJECT_NAME]/
├── mission.md              # Project vision and purpose
├── roadmap.md              # Development phases and features
├── tech-stack.md           # Technical architecture
├── decisions.md            # Decision log
├── README.md               # Project overview
├── feature-[FEATURE_NAME]/ # Feature planning
│   ├── problem.md          # What problem are we solving?
│   ├── solution.md         # How will we solve it?
│   ├── jtbd-analysis.md    # Customer jobs and satisfaction analysis
│   ├── story-plan.md       # Narrative strategy and storytelling elements
│   ├── tasks.md            # Comprehensive task breakdown with agent assignments
│   └── goal.md             # What's the success criteria?
├── hotfix-[HOTFIX_NAME]/   # Problem fix planning
│   ├── issue.md            # What's the issue?
│   ├── tasks.md            # Task breakdown for fix implementation
│   └── goal.md             # What's the fix goal?
└── chore-[CHORE_NAME]/     # Improvement planning
    ├── subject.md          # What are we improving?
    ├── tasks.md            # Task breakdown for improvements
    └── improvement.md      # How will we improve it?
```

**Note**: All project files are created in `.ai-squads/projects/[PROJECT_NAME]/` directories to ensure project isolation and prevent conflicts between multiple projects.

### **Document Types**

#### **Core Project Documents**
- **`mission.md`**: Project vision, purpose, and success criteria
- **`roadmap.md`**: Development phases, feature prioritization, and timeline
- **`tech-stack.md`**: Technical architecture, technology choices, and standards
- **`decisions.md`**: Architectural decisions, rationale, and alternatives
- **`README.md`**: Project overview and getting started guide

#### **Feature Planning Documents**
- **`problem.md`**: Problem definition, context, and impact analysis
- **`solution.md`**: Solution design, technical approach, and implementation plan
- **`jtbd-analysis.md`**: Customer jobs analysis, satisfaction gaps, and solution validation
- **`story-plan.md`**: Narrative strategy, character development, and storytelling elements
- **`tasks.md`**: Comprehensive task breakdown with agent assignments and dependencies
- **`goal.md`**: Success criteria, acceptance criteria, and validation methods

#### **Hotfix Planning Documents**
- **`issue.md`**: Issue description, impact assessment, and reproduction steps
- **`tasks.md`**: Task breakdown for fix implementation and testing
- **`goal.md`**: Fix objectives, success criteria, and validation approach

#### **Chore Planning Documents**
- **`subject.md`**: Improvement subject, current state, and impact assessment
- **`tasks.md`**: Task breakdown for improvement implementation
- **`improvement.md`**: Improvement strategy, technical approach, and implementation plan

## 🤖 **Squad System**

### **Elite Squad** - High-Performance Systems Development
**Specialization**: Rust, Smalltalk/Pharo, C/C++, JavaScript/TypeScript, Ruby, CSS

**Capabilities**:
- **Backend Architecture**: High-performance, scalable backend systems
- **Systems Programming**: Low-level systems and performance optimization
- **Full-Stack Development**: Complete web application development
- **Object-Oriented Design**: Clean architecture and design patterns

**Agents**:
- **@agent:steve**: Project coordination and strategic oversight
- **@agent:moesta**: Customer jobs analysis and solution validation
- **@agent:godin**: Storytelling, character development, and narrative craft
- **@agent:rusty**: Full-stack development and systems programming
- **@agent:uxe**: User experience research and design
- **@agent:uidev**: Frontend implementation across platforms
- **@agent:product-planner**: Strategic product planning and development
- **@agent:scribas**: Version control, workflow management, and quality gate enforcement
- **@agent:team**: Team coordination and quality assurance

### **Other Squads**
- **Expendables**: Rapid prototyping and experimentation
- **Sulaco**: Enterprise and large-scale system development

## 🔄 **Workflow Process**

### **Phase 1: Planning**
1. **Command Execution**: User executes planning command
2. **Squad Activation**: Appropriate squad agents are activated
3. **Documentation Creation**: Required planning documents are created
4. **Validation**: Planning is reviewed and validated with squad

### **Phase 2: Execution**
1. **Work Assignment**: Work is assigned to appropriate squad agents
2. **Implementation**: Work is executed following squad standards
3. **Quality Gates**: Work is validated against quality standards
4. **Documentation Updates**: Documentation is updated as work progresses

### **Phase 3: Review**
1. **Goal Validation**: Work is validated against defined goals
2. **Quality Review**: Quality standards are verified
3. **Documentation Review**: Final documentation is updated
4. **Knowledge Transfer**: Learnings are shared with the team

## 🎯 **Quality Gates**

### **Planning Quality**
- **Completeness**: All required documents created
- **Clarity**: Clear problem and solution definitions
- **JTBD Validation**: Customer jobs identified and solution alignment validated
- **Story Validation**: Narrative strategy and character development planned
- **Feasibility**: Technical and resource feasibility confirmed
- **Alignment**: Alignment with project and squad goals

### **Execution Quality**
- **Standards Compliance**: Follow squad coding and quality standards
- **Testing**: Comprehensive testing coverage
- **Documentation**: Updated documentation and code comments
- **Performance**: Meet performance and quality requirements

### **Review Quality**
- **Goal Achievement**: All defined goals met
- **Quality Standards**: All quality standards satisfied
- **Documentation**: Complete and accurate documentation
- **Knowledge Transfer**: Learnings shared with the team

## 🚀 **Benefits of the System**

### **For Developers**
- **Clear Direction**: Always know what to build and why
- **Task Clarity**: Specific, actionable tasks with clear acceptance criteria
- **Progress Tracking**: Visual progress indicators and completion status
- **Quality Standards**: Consistent quality and coding standards
- **Knowledge Sharing**: Access to comprehensive documentation
- **Team Coordination**: Clear handoff protocols and responsibilities

### **For Projects**
- **Risk Reduction**: Problems identified and planned before execution
- **Quality Assurance**: Built-in quality gates and validation
- **Documentation**: Complete project documentation and decision records
- **Maintainability**: Clear architecture and design decisions

### **For Organizations**
- **Consistency**: Standardized approach across all projects
- **Scalability**: System scales with team and project growth
- **Knowledge Retention**: Project knowledge is captured and preserved
- **Quality Improvement**: Continuous improvement through feedback and iteration

## 📚 **Getting Started**

### **For New Users**
1. **Read `startup.md`**: Understand the basic startup process
2. **Choose Planning Command**: Select appropriate planning command for your needs
3. **Follow Workflow**: Use the planning workflow to create documentation
4. **Execute with Squad**: Work with squad agents to implement your plan

### **For Existing Projects**
1. **Use `adopt-project`**: Adopt existing project into the system
2. **Fill Documentation Gaps**: Create missing project documentation
3. **Plan Features**: Use `plan-feature` for new feature development
4. **Plan Fixes**: Use `plan-fix` for problem resolution

### **For New Features**
1. **Use `plan-feature`**: Plan new feature development
2. **Create Documents**: Create problem, solution, and goal documents
3. **Validate with Squad**: Review plan with appropriate squad agents
4. **Execute Plan**: Implement feature following documented plan

## 🔧 **Customization and Extension**

### **Adding New Squads**
1. **Create Squad Definition**: Define squad purpose and capabilities
2. **Assign Agents**: Select appropriate agents for squad
3. **Define Workflows**: Create squad-specific workflows
4. **Update Standards**: Add squad-specific standards and guidelines

### **Adding New Document Types**
1. **Create Template**: Create document template in templates directory
2. **Update Workflows**: Update planning workflows to include new document type
3. **Update Standards**: Add standards for new document type
4. **Train Squad**: Ensure squad understands new document requirements

### **Extending Agent Capabilities**
1. **Update Agent Definition**: Add new capabilities and workflows
2. **Update Integration**: Update agent integration patterns
3. **Update Standards**: Add standards for new capabilities
4. **Test Integration**: Verify new capabilities work with existing system

## 📈 **Continuous Improvement**

### **Feedback Collection**
- **User Feedback**: Collect feedback from system users
- **Agent Feedback**: Monitor agent performance and effectiveness
- **Quality Metrics**: Track quality metrics and identify improvement areas
- **Process Review**: Regular review of workflows and processes

### **System Evolution**
- **Template Updates**: Regular updates to document templates
- **Workflow Refinement**: Continuous refinement of planning workflows
- **Standard Updates**: Regular updates to quality and coding standards
- **Agent Enhancement**: Continuous enhancement of agent capabilities

### **Knowledge Sharing**
- **Best Practices**: Document and share best practices
- **Lessons Learned**: Capture and share lessons learned
- **Success Stories**: Document and share successful implementations
- **Community Building**: Build community around the system

## 📖 **Detailed Documentation**

- **`.ai-squads/workflows/`** - Complete workflow instructions and process definitions
- **`.ai-squads/agents/`** - Detailed agent specifications and capabilities
- **`.ai-squads/squads/`** - Squad configurations and team structures
- **`.ai-squads/standards/`** - Quality standards and coding guidelines
- **`.ai-squads/templates/`** - Document templates and examples

## 🎉 **Conclusion**

The SquadsAI system provides a comprehensive, planning-first approach to software development that ensures high quality, clear communication, and successful project delivery. By combining structured planning, specialized agents, quality standards, and comprehensive documentation, it creates a robust foundation for any development project.

The system is designed to be flexible, scalable, and continuously improving, adapting to the needs of different projects, teams, and organizations while maintaining consistent quality and standards.

---

**Ready to transform your development process? Start with `plan-project` and let the Elite squad guide you to success! 🚀**