# AI Squads

A spec-oriented system for managing AI agent teams in software projects. Install ai-squads globally once to get specialist agents, code style standards, and workflows for project planning and code review across all your projects.

## Quick Start

```bash
# 1. Clone and install globally
git clone <ai-squads-url>
cd ai-squads
./scripts/install.sh

# 2. Adopt in your project
cd /path/to/your-project
# Run /adopt-project command in Cursor

# 3. Start using agents and workflows
# @rusty for Rust help, @steve for UX guidance, @ops for infrastructure
# /diagnose-issue to investigate problems
# /ideate-solution to explore approaches
# /plan-feature to create feature specs ready to execute
# /execute-feature to autonomously implement planned features
```

## Features

- **Specialist Agents**: Rust, Smalltalk, JavaScript, Jobs to be Done, UI/UX, UI Developer, Strategic Designer, Brazilian SaaS Copywriter, Financial Advisor, DevOps, Video Game Development
- **Code Style Standards**: Consistent coding standards for Rust, Smalltalk, JavaScript, and htmx
- **Project Planning**: Structured workflows for feature planning with PRD, specs, and machine-readable execution format (prd.json)
- **Autonomous Execution**: Execute planned features autonomously with dependency resolution, quality checks, and progress tracking
- **Code Review**: Agent-based code review using project's tech stack and style guides
- **Skills System**: Specialized capabilities like browser verification for frontend work
- **Configuration-Driven**: All agents, standards, and workflows defined in `.md` files for easy adjustment

## Installation

### Global Installation

```bash
git clone <ai-squads-url>
cd ai-squads
./scripts/install.sh
```

This installs global commands, templates, scripts, rules, and skills to `~/.cursor/`:

**Project Commands:**
- Adopt Project - Set up ai-squads in a new project
- Diagnose Issue - Investigate problems with hypothesis-driven analysis
- Explain System - Generate narrative understanding of architecture
- Ideate Solution - Explore solution approaches from minimal to comprehensive
- Plan Feature - Create structured feature documentation (PRD, specs, prd.json)
- Execute Feature - Autonomously execute planned features with quality checks and progress tracking
- Plan Game - Create Game Design Document (GDD) for game projects
- Review Merge Request - Agent-based code review
- Team Lately - Analyze team git activity and generate reports

**Specialist Agent Commands:**
- Rusty - Rust programming expert
- Alan - Smalltalk/Pharo expert
- UIDev - JavaScript/CSS/HTML expert
- Bob - Jobs to be Done expert
- Steve - Product & UX expert
- Rian - Strategic Designer & Cognitive Biases expert
- Clovis - Brazilian SaaS Copywriting & Growth expert
- Gustavo - Financial Advisor & Wealth Management expert
- Ops - DevOps & Infrastructure expert
- Eric - Video Game Development expert

### Adopt in a Project

1. Run the `/adopt-project` command in Cursor from your project root

This will:
- Create `docs/` directory with project documentation
- Ask questions about mission, tech stack, and roadmap
- Configure agent team based on tech stack
- Save team configuration to `docs/team.md`
- Based on templates, generate mission, roadmap, decisions

## Usage

### Diagnose an Issue

Run the `/diagnose-issue` command when investigating problems. This will:
- Gather evidence (logs, errors, symptoms)
- Generate hypotheses ordered by likelihood
- Guide investigation with specific checks
- Produce clear problem definitions
- Hand off to `/ideate-solution` when root cause is found

### Explain the System

Run the `/explain-system` command to understand architecture. This will:
- Synthesize project documentation into narrative
- Provide overview or deep-dive into subsystems
- Trace data flows and dependencies
- Surface design decisions and rationale
- Help onboard quickly with coherent mental models

### Ideate Solutions

Run the `/ideate-solution` command to explore approaches. This will:
- Generate multiple solutions from minimal to comprehensive
- Show clear trade-offs and effort estimates
- Provide comparison matrix across approaches
- Hand off to `/plan-feature` with optimized context

### Plan a Feature

Run the `/plan-feature` command in Cursor. This will:
- Create feature directory in `docs/feature/{feature_name}/`
- Generate PRD.md, specs.md, and prd.json (machine-readable execution format)
- Use your project's agent team to inform planning
- Customize planning documents with feature details
- Create user stories with acceptance criteria, dependencies, and agent assignments

**Note**: `tasks.md` is deprecated. `prd.json` replaces it entirely and is required for autonomous execution.

### Execute a Feature

Run the `/execute-feature` command in Cursor to autonomously implement a planned feature. This will:
- Read feature plan from `docs/feature/{feature_name}/prd.json`
- Resolve dependencies and execute user stories in order
- Route stories to appropriate agents based on type and tech stack
- Run quality checks (typecheck, lint, format, test) before commits
- Perform browser verification for frontend stories
- Track progress and commit each completed story
- Archive feature when all stories pass

**Dry-run mode**: Use `--dry-run` flag to preview execution prompts without running them.

**Automatic mode**: Run without a feature name to automatically find and execute features with incomplete stories.

### Plan a Game

Run the `/plan-game` command in Cursor. This will:
- Create game directory in `docs/game/{game_name}/`
- Generate GDD.md (Game Design Document) template
- Use Eric (Video Game Specialist) to guide GDD creation
- Apply game design principles from industry canon (Schell, Koster, Swink)
- Cover core gameplay, player experience, art direction, monetization, and more
- Create a comprehensive design document ready for team implementation

### Review Merge Request

Run the "Review Merge Request" command in Cursor. This will:
- Analyze git diff or selected files
- Load your project's agent team
- Invoke relevant agents based on file types
- Apply code style standards
- Generate comprehensive review feedback

### Analyze Team Activity

Run the `/team-lately` command to understand what the team has been working on. This will:
- Fetch all remote branches to get latest activity
- Analyze commits per author over configurable period (default: 7 days)
- Generate conceptual summaries of each branch's purpose
- Output report to `$HOME/docs/{project}/team-lately/`

**Example usage:**
```
@team-lately
@team-lately last 2 weeks
@team-lately 1 month
```

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
- `@ops` - DevOps & Infrastructure expert
- `@eric` - Video Game Development expert

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
│   ├── quality.md            # Quality check commands
│   ├── feature/
│   │   └── {feature_name}/
│   │       ├── PRD.md
│   │       ├── specs.md
│   │       └── prd.json      # Machine-readable execution format
│   ├── game/
│   │   └── {game_name}/
│   │       └── GDD.md
│   └── archived/             # Completed features
│       └── YYYY-MM-DD-{feature_name}/
└── src/                      # Your project code
```

Global installation (at `~/.cursor/`):
```
~/.cursor/
├── commands/                 # Command workflows
├── templates/                # Documentation templates
├── scripts/                  # Helper scripts
├── rules/                    # System rules (applied globally)
└── skills/                   # Specialized capabilities (e.g., browser-verification)
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

### DevOps Specialist
- Specializes in infrastructure, deployment, and operational excellence
- Expert in Nomad orchestration and Docker containerization
- Focuses on vendor-agnostic solutions for platform portability
- Provides guidance on CI/CD pipelines, monitoring, and observability
- Helps structure services for deployment (DigitalOcean droplets, or any platform)
- Infrastructure as Code practices with Terraform and Ansible
- Secrets management, security hardening, and cost optimization

### Video Game Development Specialist
- Specializes in indie game development from concept to launch
- Expert in Godot engine (GDScript, scene composition, signals)
- Mastery in game design across all genres (RPG, platformer, puzzle, strategy, roguelike, etc.)
- Art direction for 2D, 3D, and hybrid visual styles
- Music composition and sound design guidance
- Player psychology and engagement optimization (Flow theory, Octalysis, Bartle's types)
- Ethical monetization strategies and dark pattern prevention
- Game analytics and retention metrics (D1/D7/D30, DAU/MAU, funnel analysis)
- Platform strategy (Steam, itch.io, console, mobile)
- Team coaching and milestone planning for indie teams
- Community building and live service planning
- Comprehensive knowledge of game design canon (Schell, Koster, Swink, and more)

## Code Style Standards

Style guides are located in the ai-squads source repository at `standards/code/`:
- `rust-style.md` - Rust coding standards
- `smalltalk-style.md` - Smalltalk/Pharo standards
- `javascript-style.md` - JavaScript standards (covers Alpine, vanilla JS)
- `htmx-style.md` - htmx patterns and best practices

Agents automatically reference relevant style guides when providing guidance. Commands reference these files from the ai-squads repository, so they must remain accessible.

## Skills System

Skills are specialized capabilities that agents can use during execution. Skills are located in `skills/` and installed to `~/.cursor/skills/`:

- **browser-verification**: Verifies frontend changes work correctly in a browser. Required for frontend user stories. Navigates to pages, interacts with UI elements, and confirms expected behavior.

Skills are automatically invoked when relevant (e.g., browser verification for frontend stories with "Verify in browser" in acceptance criteria).

## Configuration

All configuration is done through `.md` files in the ai-squads repository:
- **Agents**: `agents/*.md` - Agent definitions and rules
- **Standards**: `standards/code/*.md` - Code style guides
- **Commands**: `commands/*.md` - Command workflows (installed to `~/.cursor/commands/`)
- **Templates**: `templates/*.md` - Documentation templates (installed to `~/.cursor/templates/`)
- **Scripts**: `scripts/*.sh` - Helper scripts (installed to `~/.cursor/scripts/`)
- **System Rules**: `rules/system.md` - Global system rules (installed to `~/.cursor/rules/`)
- **Skills**: `skills/*/skill.md` - Specialized capabilities (installed to `~/.cursor/skills/`)

**Note**: Agents and standards remain in the ai-squads repository and are referenced by commands via relative paths. Commands, templates, scripts, rules, and skills are copied to `~/.cursor/` during installation.

Edit these files in the ai-squads repository, then re-run `./scripts/install.sh` to update your global installation.

## Team Configuration

Your project's agent team is configured in `docs/team.md`. This file lists:
- Assigned agents
- Agent roles and responsibilities
- Tech stack alignment

The team configuration is used by commands to provide relevant guidance.

## Quality Checks

Quality checks are defined in `docs/quality.md` in your project. These commands are copied to `prd.json.quality` when planning features and executed during feature execution. Common quality checks include:
- Type checking (e.g., `tsc --noEmit`, `cargo check`)
- Linting (e.g., `npm run lint`, `cargo clippy`)
- Formatting (e.g., `npm run format`, `cargo fmt`)
- Testing (e.g., `npm test`, `cargo test`)

Quality checks must pass before a story is committed. Commands in `prd.json.quality` are executed directly in your project directory, so only include trusted commands.

## Contributing

This is a config-oriented system. To customize:
1. Edit agent definitions in `agents/`
2. Update style guides in `standards/code/`
3. Modify command workflows in `commands/`
4. Adjust templates in `templates/`
5. Update system rules in `rules/`
6. Re-run `./scripts/install.sh` to update global installation

## License

MIT License - See [LICENSE](LICENSE) for details.

## Support

For issues or questions:
- Open an issue on GitHub
- Check existing documentation in `docs/`

