# AI Squads

A config-oriented system for managing AI agent teams in software projects. Projects adopt ai-squads as a git submodule to get specialist agents, code style standards, and workflows for project planning and code review.

## Features

- **Specialist Agents**: Rust, Smalltalk, JavaScript, Jobs to be Done, UI/UX, UI Developer
- **Code Style Standards**: Consistent coding standards for Rust, Smalltalk, JavaScript, and htmx
- **Project Planning**: Structured workflows for feature planning with PRD, specs, and tasks
- **Code Review**: Agent-based code review using project's tech stack and style guides
- **Configuration-Driven**: All agents, standards, and workflows defined in `.mdc` files for easy adjustment

## Installation

### Global Installation

```bash
git clone <ai-squads-url>
cd ai-squads
./scripts/install.sh
```

This installs global commands to `~/.cursor/commands/`:

**Project Commands:**
- Adopt Project - Set up ai-squads in a new project
- Plan Feature - Create structured feature documentation
- Review Merge Request - Agent-based code review

**Specialist Agent Commands:**
- Rusty - Rust programming expert
- Alan - Smalltalk/Pharo expert
- UIDev - JavaScript/CSS/HTML expert
- Bob - Jobs to be Done expert
- Steve - Product & UX expert

### Adopt in a Project

1. Add ai-squads as a git submodule:

```bash
cd my-project
git submodule add <ai-squads-url> ai-squads
cd ai-squads
./scripts/install.sh  # If not already installed globally
cd ..
```

2. Run the "Adopt Project" command in Cursor

This will:
- Create `ai-squads-docs/` directory with project documentation templates
- Set up `.cursor/commands/` with project-specific commands
- Ask questions about mission, tech stack, and roadmap
- Configure agent team based on tech stack
- Save team configuration to `ai-squads-docs/team.md`

## Usage

### Plan a Feature

Run the "Plan Feature" command in Cursor. This will:
- Create feature directory in `ai-squads-docs/feature/{feature_name}/`
- Generate PRD.md, specs.md, and tasks.md templates
- Use your project's agent team to inform planning
- Customize planning documents with feature details

### Review Merge Request

Run the "Review Merge Request" command in Cursor. This will:
- Analyze git diff or selected files
- Load your project's agent team
- Invoke relevant agents based on file types
- Apply code style standards
- Generate comprehensive review feedback

### Invoke Agent

Run the "Invoke Agent" command in Cursor. This will:
- List available agents
- Allow selection of agent(s)
- Provide context-aware guidance
- Apply agent rules and style guides

## Project Structure

After adopting ai-squads, your project will have:

```
my-project/
├── ai-squads/                # Git submodule
│   ├── agents/               # Specialist agents
│   ├── commands/             # Command workflows
│   ├── scripts/              # Install scripts
│   ├── standards/code/       # Code style standards
│   ├── rules/                # System rules
│   └── templates/            # Documentation templates
├── .cursor/
│   └── commands/             # Installed commands (copied from ai-squads/commands/)
├── ai-squads-docs/           # Project planning docs
│   ├── mission.md
│   ├── roadmap.md
│   ├── tech-stack.md
│   ├── DECISIONS.md
│   ├── README.md
│   ├── team.md
│   └── feature/
│       └── {feature_name}/
│           ├── PRD.md
│           ├── specs.md
│           └── tasks.md
└── src/                      # Your project code
```

## Agents

### Rust Specialist
- Specializes in Rust programming
- References `rust-style.mdc` standards
- Provides code review, implementation guidance, architecture recommendations

### Smalltalk Specialist
- Specializes in Smalltalk/Pharo
- References `smalltalk-style.mdc` standards
- Provides Smalltalk-specific guidance and patterns

### JavaScript Specialist
- Specializes in JavaScript, Alpine.js, htmx
- References `javascript-style.mdc` and `htmx-style.mdc` standards
- Provides frontend development guidance

### Jobs to be Done
- Inspired by Bob Moesta
- Focuses on user needs and product strategy
- Helps with feature planning from a JTBD perspective

### UI/UX Agent
- Inspired by Steve Krug
- Focuses on usability and user experience
- Provides design and interaction guidance

### UI Developer
- Specializes in frontend implementation
- References JavaScript and htmx style guides
- Provides component architecture and implementation guidance

## Code Style Standards

Style guides are located in `ai-squads/standards/code/`:
- `rust-style.mdc` - Rust coding standards
- `smalltalk-style.mdc` - Smalltalk/Pharo standards
- `javascript-style.mdc` - JavaScript standards (covers Alpine, vanilla JS)
- `htmx-style.mdc` - htmx patterns and best practices

Agents automatically reference relevant style guides when providing guidance.

## Configuration

All configuration is done through `.mdc` files in the ai-squads submodule:
- **Agents**: `ai-squads/agents/*.mdc`
- **Standards**: `ai-squads/standards/code/*.mdc`
- **Commands**: `ai-squads/commands/*.mdc`
- **System Rules**: `ai-squads/rules/system.mdc`

Edit these files in the ai-squads submodule to customize agents, standards, and workflows for your needs.

## Team Configuration

Your project's agent team is configured in `ai-squads-docs/team.md`. This file lists:
- Assigned agents
- Agent roles and responsibilities
- Tech stack alignment

The team configuration is used by commands to provide relevant guidance.

## Contributing

This is a config-oriented system. To customize:
1. Edit agent definitions in `ai-squads/agents/`
2. Update style guides in `ai-squads/standards/code/`
3. Modify command workflows in `ai-squads/commands/`
4. Adjust templates in `ai-squads/templates/`
5. Update system rules in `ai-squads/rules/`

## License

[Add your license here]

## Support

For issues or questions, please [add your support channel here]

