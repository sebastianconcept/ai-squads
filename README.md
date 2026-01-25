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
- **Storybook Integration**: Framework-aware component documentation with automatic story generation from UX specifications
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
- Adopt Project - Set up ai-squads in a new project (includes Storybook initialization for frontend projects)
- Project Starter - Organize business ideas into structured projects with pitch deck and lean canvas generation
- Diagnose Issue - Investigate problems with hypothesis-driven analysis
- Explain System - Generate narrative understanding of architecture
- Ideate Solution - Explore solution approaches from minimal to comprehensive
- Plan Feature - Create structured feature documentation (PRD, specs, prd.json, Storybook stories for frontend)
- Execute Feature - Autonomously execute planned features with quality checks and progress tracking
- Plan Game - Create Game Design Document (GDD) for game projects
- Review Merge Request - Agent-based code review
- Team Lately - Analyze team git activity and generate reports
- Update Docs - Maintain documentation consistency by tracking changes and updating docs via AI

**Specialist Agent Commands:**
- Ben - Startup Advisor & Business Planning expert
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

### Start a New Business Project (or Add Business Planning to Existing Project)

1. Run the `/project-starter` command in Cursor

**For new business ideas:**
- Prompt for business name and validate it
- Create `~/docs/{project-name}/` directory with project documentation
- Use a multi-agent team (Ben - Startup Advisor, Bob, Steve, Rian, Gustavo) to guide you through business planning
- Generate `LEAN-CANVAS.md` and `PITCH-DECK.md` based on your business idea
- Create initial project documentation (MISSION.md, ROADMAP.md, TEAM.md)
- Set up conversation tracking for ongoing business planning

**For existing projects:**
- If project already exists, command will detect it and add business planning documents
- Won't overwrite existing documentation (MISSION.md, ROADMAP.md, TECH-STACK.md)
- Creates `LEAN-CANVAS.md` and `PITCH-DECK.md` alongside existing docs
- Adds Ben (Startup Advisor) to TEAM.md
- Useful for: Adding business planning to existing products, preparing for fundraising, refining business model

**Ongoing guidance:** After initial setup, you can invoke `@ben` (Startup Advisor) anytime to:
- Continue refining your business planning documents
- Stress-test your idea with adversarial questioning
- Evaluate features as business hypotheses
- Plan MVP strategy and validation experiments
- Get growth guidance as your business evolves

### Adopt in a Project

1. Run the `/adopt-project` command in Cursor from your project root

This will:
- Create `~/docs/{project-name}/` directory with project documentation (where {project-name} is derived from git repository name)
- Ask questions about mission, tech stack, and roadmap
- Configure agent team based on tech stack
- Save team configuration to `~/docs/{project-name}/TEAM.md`
- Based on templates, generate mission, roadmap, decisions

**Note:** `/project-starter` is for business planning before code exists. `/adopt-project` is for adopting ai-squads in an existing code repository. If you've used `/project-starter` and later create a code repo, you can run `/adopt-project` to merge business planning docs with code repo docs.

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
- Create feature directory in `~/docs/{project-name}/feature/{feature_name}/`
- Generate PRD.md, SPECS.md, and prd.json (machine-readable execution format)
- Use your project's agent team to inform planning
- Customize planning documents with feature details
- Create user stories with acceptance criteria, dependencies, and agent assignments
- **For frontend features**: Automatically initialize Storybook if not already set up, then generate component stories from UX specifications

**Storybook Integration**: When planning frontend features, Storybook is automatically initialized (if needed) and component stories are generated from `ux-specs.json`. Stories are framework-aware (React, Vue, Svelte, HTML/Alpine.js) and stay in sync with UX specifications automatically.

**Note**: `tasks.md` is deprecated. `prd.json` replaces it entirely and is required for autonomous execution.

### Execute a Feature

Run the `/execute-feature` command in Cursor to autonomously implement a planned feature. This will:
- Read feature plan from `~/docs/{project-name}/feature/{feature_name}/prd.json`
- Resolve dependencies and execute user stories in order
- Route stories to appropriate agents based on type and tech stack
- **Intelligent Mode Selection**: Automatically use `/plan` mode for complex stories and `/ask` mode for failure diagnosis
- **Dynamic Model Selection**: Select between fast and best models based on story characteristics (defaults to `auto`)
- **Parallel Execution**: Execute independent stories in parallel using local Git worktrees for isolation
- **Failure Diagnosis Loop**: Automatically diagnose failures and ideate solutions to inform the next iteration
- **Non-Interactive Mode**: Support for automated environments via `--non-interactive` flag
- Run quality checks (typecheck, lint, format, test) before commits
- Perform browser verification for frontend stories
- Track progress and commit each completed story
- Archive feature when all stories pass

**Dry-run mode**: Use `--dry-run` flag to preview execution prompts without running them.

**Non-interactive mode**: Use `--non-interactive` to skip confirmation prompts (e.g., for MCP server allowlist).

**Automatic mode**: Run without a feature name to automatically find and execute features with incomplete stories.

### Plan a Game

Run the `/plan-game` command in Cursor. This will:
- Create game directory in `~/docs/{project-name}/game/{game_name}/`
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

### Start a Business Project

Run the `/project-starter` command to organize your business idea into a structured project. This will:
- Guide you through business planning with a multi-agent team (Startup Advisor, Bob, Steve, Rian, Gustavo)
- Ask strategic questions from multiple perspectives (Jobs to be Done, UX, Growth, Financial)
- Generate a Lean Canvas (`LEAN-CANVAS.md`) - one-page business model
- Generate a Pitch Deck (`PITCH-DECK.md`) - investor-ready presentation
- Create project documentation structure ready for development planning
- Set up conversation tracking so you can resume planning anytime

**Multi-agent collaboration:** The team provides both supportive and adversarial perspectives to help you mature your business idea:
- **Startup Advisor** orchestrates conversation and ensures comprehensive coverage
- **Bob (Jobs to be Done)** ensures your business serves core jobs and understands user motivations
- **Steve (UI/UX)** ensures your business considers user experience and usability
- **Rian (Strategic Designer)** ensures your business has growth strategy and engagement loops
- **Gustavo (Financial Advisor)** ensures your business model is financially viable with working unit economics

**Example usage:**
```
/project-starter
```

After initial setup, continue refining your business plan:
```
@ben - Continue business planning conversation (Startup Advisor)
@bob - Explore Jobs to be Done perspective
@steve - Think about user experience
@rian - Explore growth strategy
@gustavo - Validate business model and unit economics
```

**Next steps after business planning:**
- Continue refining business documents with agents
- Run `/discover-product` to translate business planning into product strategy
- Plan features using `/plan-feature` (which uses your business planning as context)

### Update Documentation

Run the `/update-docs` command to maintain documentation consistency. This will:
- Track when each doc was last updated using commit hashes
- Analyze git diffs to detect code changes
- Automatically update docs to reflect current system behavior
- Exclude archived docs from processing (historical records)
- Work on older projects without metadata (auto-initializes)

**Features:**
- **Idempotent**: Running multiple times with no changes won't modify docs
- **Backward Compatible**: Auto-initializes metadata for older projects
- **Smart Updates**: Only updates docs when code changes are detected
- **Content Hashing**: Uses SHA256 to prevent unnecessary writes

**Example usage:**
```
/update-docs
```

The command will:
1. Scan all docs in `docs/` (excluding `archived/`)
2. Check which docs need updates based on git diffs
3. Ask if you want to include uncommitted changes
4. Update docs that have changed via AI
5. Update metadata with new commit hashes

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

**Project Repository** (`my-project/`):
```
my-project/
├── storybook/                 # Storybook (if frontend project)
│   ├── .storybook/           # Storybook configuration
│   ├── stories/              # Component stories (auto-generated)
│   └── package.json          # Storybook dependencies
└── src/                      # Your project code
```

**Project Documentation** (`$HOME/docs/{project-name}/`):
```
$HOME/docs/{project-name}/
├── MISSION.md
├── ROADMAP.md
├── TECH-STACK.md
├── DECISIONS.md
├── README.md
├── TEAM.md
├── QUALITY.md                # Quality check commands
├── STORYBOOK.md              # Storybook documentation (if frontend project)
├── feature/
│   └── {feature_name}/
│       ├── PRD.md
│       ├── SPECS.md
│       ├── prd.json          # Machine-readable execution format
│       ├── ux-specs.json     # UX specifications (if frontend feature)
│       └── UX-SPECS.md       # Human-readable UX specs
├── game/
│   └── {game_name}/
│       └── GDD.md
└── archived/                 # Completed features
    └── YYYY-MM-DD-{feature_name}/
```

**Global Installation** (at `~/.cursor/`):
```
~/.cursor/
├── commands/                 # Command workflows
├── templates/                # Documentation templates (including Storybook templates)
├── scripts/                  # Helper scripts (including init-storybook.sh)
├── rules/                    # System rules (applied globally)
└── skills/                   # Specialized capabilities (e.g., browser-verification)
```

**Note**: Project documentation lives in `$HOME/docs/{project-name}/` (outside the repository), while Storybook lives in `storybook/` within the project repository. This separation keeps planning docs separate from code while allowing Storybook to import components from the codebase.

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
- Generates UX specifications (ux-specs.json) that automatically create Storybook stories
- Coordinates 6-pass UX methodology for comprehensive feature design

### UI Developer
- Specializes in frontend implementation
- References JavaScript and htmx style guides
- Provides component architecture and implementation guidance
- Integrates with Storybook for component documentation and testing
- Uses Storybook for component discovery and isolated development

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

## Storybook Integration

Storybook is automatically integrated into the UX workflow for frontend features:

- **Automatic Initialization**: Storybook is initialized when adopting a project with frontend code or when planning frontend features
- **Framework-Aware**: Automatically detects framework (React, Vue, Svelte, HTML/Alpine.js) and generates appropriate stories
- **Auto-Generated Stories**: Component stories are automatically generated from `ux-specs.json` during feature planning
- **Framework Detection**: Checks TECH-STACK.md, package.json, and file extensions to determine framework
- **Native/Game Engine Handling**: Automatically skips Storybook for iOS/Android/Godot/Unity projects
- **Isolated Setup**: Storybook lives in `storybook/` directory, separate from product code
- **Monorepo Support**: Works with flexible monorepo structures (no packages/ wrapper required)

**Usage:**
- Stories are generated automatically during `/plan-feature` for frontend features
- Start Storybook: `cd storybook && npm run storybook`
- Manual story generation: `cd storybook && npm run generate-stories <feature-path> [package-name]`

**Documentation:** See `~/docs/{project-name}/STORYBOOK.md` for complete Storybook documentation.

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

Your project's agent team is configured in `docs/TEAM.md`. This file lists:
- Assigned agents
- Agent roles and responsibilities
- Tech stack alignment

The team configuration is used by commands to provide relevant guidance.

## Quality Checks

Quality checks are defined in `docs/QUALITY.md` in your project. These commands are copied to `prd.json.quality` when planning features and executed during feature execution. Common quality checks include:
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

