# AI Squads

A config-oriented system for managing AI agent teams in software projects. Install ai-squads globally once to get specialist agents, code style standards, and workflows for project planning and code review across all your projects.

## Features

- **Specialist Agents**: Rust, Smalltalk, JavaScript, Jobs to be Done, UI/UX, UI Developer
- **Code Style Standards**: Consistent coding standards for Rust, Smalltalk, JavaScript, and htmx
- **Project Planning**: Structured workflows for feature planning with PRD, specs, and tasks
- **Code Review**: Agent-based code review using project's tech stack and style guides
- **Configuration-Driven**: All agents, standards, and workflows defined in `.md` files for easy adjustment

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

1. Run the "Adopt Project" command in Cursor from your project root

This will:
- Create `docs/` directory with project documentation templates
- Ask questions about mission, tech stack, and roadmap
- Configure agent team based on tech stack
- Save team configuration to `docs/team.md`

## Usage

### Plan a Feature

Run the "Plan Feature" command in Cursor. This will:
- Create feature directory in `docs/feature/{feature_name}/`
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
├── docs/                     # Project planning docs
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

Global installation (at `~/.cursor/`):
```
~/.cursor/
├── commands/                 # Command workflows
├── templates/                # Documentation templates
├── scripts/                  # Helper scripts
└── rules/                    # System rules (applied globally)
```

## Agents

### Rust Specialist
- Specializes in Rust programming
- References `rust-style.md` standards
- Provides code review, implementation guidance, architecture recommendations

### Smalltalk Specialist
- Specializes in Smalltalk/Pharo
- References `smalltalk-style.md` standards
- Provides Smalltalk-specific guidance and patterns

### JavaScript Specialist
- Specializes in JavaScript, Alpine.js, htmx
- References `javascript-style.md` and `htmx-style.md` standards
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

Style guides are located in the ai-squads source repository:
- `rust-style.md` - Rust coding standards
- `smalltalk-style.md` - Smalltalk/Pharo standards
- `javascript-style.md` - JavaScript standards (covers Alpine, vanilla JS)
- `htmx-style.md` - htmx patterns and best practices

Agents automatically reference relevant style guides when providing guidance.

## Configuration

All configuration is done through `.md` files in the ai-squads repository:
- **Agents**: `agents/*.md`
- **Standards**: `standards/code/*.md`
- **Commands**: `commands/*.md`
- **System Rules**: `rules/system.md`

Edit these files in the ai-squads repository, then re-run `./scripts/install.sh` to update your global installation.

## Team Configuration

Your project's agent team is configured in `docs/team.md`. This file lists:
- Assigned agents
- Agent roles and responsibilities
- Tech stack alignment

The team configuration is used by commands to provide relevant guidance.

## Contributing

This is a config-oriented system. To customize:
1. Edit agent definitions in `agents/`
2. Update style guides in `standards/code/`
3. Modify command workflows in `commands/`
4. Adjust templates in `templates/`
5. Update system rules in `rules/`
6. Re-run `./scripts/install.sh` to update global installation

## License

[Add your license here]

## Support

For issues or questions, please [add your support channel here]

