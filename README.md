# AI Squads

A config-oriented system for managing AI agent teams in software projects. Install ai-squads globally once to get specialist agents, code style standards, and workflows for project planning and code review across all your projects.

## Features

- **Specialist Agents**: Rust, Smalltalk, JavaScript, Jobs to be Done, UI/UX, UI Developer, Strategic Designer, Brazilian SaaS Copywriter, Financial Advisor
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
- Rian - Strategic Designer & Cognitive Biases expert
- Clovis - Brazilian SaaS Copywriting & Growth expert
- Gustavo - Financial Advisor & Wealth Management expert

### Adopt in a Project

1. Run the `/adopt-project` command in Cursor from your project root

This will:
- Create `docs/` directory with project documentation
- Ask questions about mission, tech stack, and roadmap
- Configure agent team based on tech stack
- Save team configuration to `docs/team.md`
- Based on templates, generate mission, roadmap, desicions

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

### Invoke Specialist Agents

Invoke specialist agents directly by their command names in Cursor:
- `@rusty` - Rust programming expert
- `@alan` - Smalltalk/Pharo expert
- `@uidev` - JavaScript/CSS/HTML expert
- `@bob` - Jobs to be Done expert
- `@steve` - Product & UX expert
- `@rian` - Strategic Designer & Cognitive Biases expert
- `@clovis` - Brazilian SaaS Copywriting & Growth expert
- `@gustavo` - Financial Advisor & Wealth Management expert

Each agent provides:
- Context-aware guidance based on your current file and selection
- Agent-specific rules and best practices
- Relevant style guide enforcement
- Specialized expertise in their domain

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

### Strategic Designer
- Inspired by Rian Dutra's "Enviesados"
- Specializes in cognitive biases and ethical influence
- Applies psychological principles to help users make better decisions
- Detects and prevents dark patterns
- Optimizes choice architecture and decision-making flows

### Brazilian SaaS Copywriter & Growth
- Specializes in Brazilian SaaS market copywriting and growth strategies
- Expert in cultural localization for Brazilian audiences
- Applies persuasion principles from Cialdini, SPIN Selling, and negotiation tactics
- Influenced by Betina Rudolph, Jeffrey Gitomer, Chris Voss, and others
- Provides conversion-optimized copy in Brazilian Portuguese
- Platform expertise: WhatsApp, Instagram, LinkedIn for Brazilian market
- Focuses on ethical growth strategies and long-term relationship building

### Financial Advisor & Wealth Management
- Specializes in Brazilian financial advisory, accounting, and wealth management
- Expert in transaction categorization for tax reporting and financial analysis
- Provides cost optimization and revenue expansion strategies
- Focuses on personal and family wealth building
- Influenced by Gustavo Cerbasi's financial education methodology
- Bilingual approach: Brazilian Portuguese for financial advice, English for technical matters
- Explains complex financial concepts in simple, accessible language for non-accountants
- Expertise in Brazilian tax regulations (IRPF, IRPJ, Simples Nacional) and accounting standards
- Helps with budget planning, investment strategies, and retirement planning
- Emphasizes financial education and empowerment

## Code Style Standards

Style guides are located in the ai-squads source repository at `standards/code/`:
- `rust-style.md` - Rust coding standards
- `smalltalk-style.md` - Smalltalk/Pharo standards
- `javascript-style.md` - JavaScript standards (covers Alpine, vanilla JS)
- `htmx-style.md` - htmx patterns and best practices

Agents automatically reference relevant style guides when providing guidance. Commands reference these files from the ai-squads repository, so they must remain accessible.

## Configuration

All configuration is done through `.md` files in the ai-squads repository:
- **Agents**: `agents/*.md` - Agent definitions and rules
- **Standards**: `standards/code/*.md` - Code style guides
- **Commands**: `commands/*.md` - Command workflows (installed to `~/.cursor/commands/`)
- **Templates**: `templates/*.md` - Documentation templates (installed to `~/.cursor/templates/`)
- **Scripts**: `scripts/*.sh` - Helper scripts (installed to `~/.cursor/scripts/`)
- **System Rules**: `rules/system.md` - Global system rules (installed to `~/.cursor/rules/`)

**Note**: Agents and standards remain in the ai-squads repository and are referenced by commands via relative paths. Commands, templates, scripts, and rules are copied to `~/.cursor/` during installation.

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

