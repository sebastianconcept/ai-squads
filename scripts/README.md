# AI Squads Scripts

This directory contains utility scripts for managing and working with the AI Squads system.

## Available Scripts

### `plan-feature.sh` - Feature Planning
**Purpose**: Create comprehensive feature planning documentation using AI Squads workflows

**Usage**:
```bash
./scripts/plan-feature.sh <project_name> <feature_name> [options]
```

**Options**:
- `-p, --priority`: Set feature priority (low/medium/high, default: medium)
- `-s, --squad`: Specify squad to use (default: auto-detect)
- `-t, --template`: Custom template directory (default: templates/projects/)
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Show help message

**Examples**:
```bash
# Basic usage
./scripts/plan-feature.sh stui user-authentication

# With high priority
./scripts/plan-feature.sh myapp api-rate-limiting --priority high

# With specific squad
./scripts/plan-feature.sh backend user-management --squad elite
```

**What it creates**:
- `feature-[FEATURE_NAME]/` directory structure
- Complete planning documents (problem.md, solution.md, goal.md, tasks.md)
- Status tracking files (status.md, implementation-status.md)
- Cursor integration (.cursor-rule.md)
- Updates project status

### `link.sh` - Cursor Rules Integration
**Purpose**: Link .cursor/rules from .ai-squads into target repository

**Usage**:
```bash
./scripts/link.sh <target_repo_path>
```

**What it does**:
- Creates symlinks to all agent and project rules
- Converts .md files to .mdc for Cursor compatibility
- Sets up automatic updates when AI Squads is updated
- Updates .gitignore to exclude symlinked directories

### `create-squad.sh` - Squad Creation
**Purpose**: Create new squad configuration

**Usage**:
```bash
./scripts/create-squad.sh <squad_name>
```

### `create-resource.sh` - Resource Creation
**Purpose**: Create new resource definition

**Usage**:
```bash
./scripts/create-resource.sh <resource_name>
```

### `create-jtbd-agent.sh` - JTBD Agent Setup
**Purpose**: Set up JTBD Expert Agent for AI Squads projects

**Usage**:
```bash
./scripts/create-jtbd-agent.sh <project_name> [options]
```

**Options**:
- `-f, --force`: Force setup even if agent already exists
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Show help message

**Examples**:
```bash
# Basic usage
./scripts/create-jtbd-agent.sh myproject

# Force reinstall
./scripts/create-jtbd-agent.sh backend-api --force
```

**What it creates**:
- JTBD agent setup documentation
- JTBD analysis templates and directory structure
- Workflow integration documentation
- Updated project configuration for JTBD methodology

### `create-writer-agent.sh` - Writer Agent Setup
**Purpose**: Set up Writer Agent for AI Squads projects

**Usage**:
```bash
./scripts/create-writer-agent.sh <project_name> [options]
```

**Options**:
- `-f, --force`: Force setup even if agent already exists
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Show help message

**Examples**:
```bash
# Basic usage
./scripts/create-writer-agent.sh myproject

# Force reinstall
./scripts/create-writer-agent.sh backend-api --force
```

**What it creates**:
- Writer agent setup documentation
- Content creation templates and directory structure
- Workflow integration documentation
- Updated project configuration for content creation

### `setup-project-docs.sh` - Project Documentation Setup
**Purpose**: Set up standardized documentation directory structure for all projects

**Usage**:
```bash
./scripts/setup-project-docs.sh [project_name] or ./scripts/setup-project-docs.sh --all
```

**Options**:
- `project_name`: Set up docs for specific project
- `--all`: Set up docs for all existing projects

**Examples**:
```bash
# Set up docs for specific project
./scripts/setup-project-docs.sh stui

# Set up docs for all projects
./scripts/setup-project-docs.sh --all
```

**What it creates**:
- Standardized `docs/` directory structure in all projects
- Template documentation files (README.md, setup.md, overview.md, etc.)
- Updated project templates to reference documentation
- Documentation integration script for management

### `docs-integration.sh` - Documentation Management
**Purpose**: Manage and maintain project documentation

**Usage**:
```bash
./scripts/docs-integration.sh [command] [project_name]
```

**Commands**:
- `list [project]`: List all documentation for project
- `search [project]`: Search documentation content
- `update [project]`: Update documentation timestamps
- `validate [project]`: Validate documentation completeness
- `archive [project]`: Archive outdated documentation

**Examples**:
```bash
# List all documentation for a project
./scripts/docs-integration.sh list stui

# Search for specific content
./scripts/docs-integration.sh search stui 'deployment'

# Update timestamps
./scripts/docs-integration.sh update stui

# Validate documentation completeness
./scripts/docs-integration.sh validate stui

# Archive outdated documentation
./scripts/docs-integration.sh archive stui
```

**What it provides**:
- Documentation inventory and search capabilities
- Automated timestamp updates
- Documentation completeness validation
- Automated archiving of outdated content

**Usage**:
```bash
./scripts/create-writer-agent.sh <project_name> [options]
```

**Options**:
- `-f, --force`: Force setup even if agent already exists
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Show help message

**Examples**:
```bash
# Basic usage
./scripts/create-writer-agent.sh myproject

# Force reinstall
./scripts/create-writer-agent.sh backend-api --force
```

**What it creates**:
- Writer agent setup documentation
- Story planning templates and directory structure
- Workflow integration documentation
- Updated project configuration for storytelling methodology

### `list-resources.sh` - Resource Listing
**Purpose**: List available resources

**Usage**:
```bash
./scripts/list-resources.sh
```

## Integration with Cursor

The `plan-feature.sh` script integrates seamlessly with Cursor through:

1. **Automatic Cursor Rule Creation**: Generates `.cursor-rule.md` for each feature
2. **Template Integration**: Uses established document templates
3. **Workflow Consistency**: Follows AI Squads planning workflows
4. **Quality Standards**: Maintains established quality gates

## Workflow Integration

All scripts follow the AI Squads "planning-first, execution-second" philosophy with JTBD methodology and storytelling integration:

1. **Planning Phase**: Create comprehensive documentation and task breakdown
2. **JTBD Analysis**: Validate customer jobs and solution alignment
3. **Story Planning**: Create narrative strategy and character development
4. **Validation Phase**: Review with squad agents and validate feasibility
5. **Execution Phase**: Implement following documented plan and quality gates
6. **Review Phase**: Validate against goals and update documentation

## Best Practices

1. **Always Use Scripts**: Use the provided scripts for consistency
2. **Follow Standards**: Adhere to AI Squads quality gates and coding standards
3. **JTBD Validation**: Ensure all features address real customer jobs and satisfaction gaps
4. **Story Validation**: Ensure all features include engaging narrative strategy and character development
5. **Update Documentation**: Keep all documents current as work progresses
6. **Squad Coordination**: Work with assigned agents for specialized tasks
7. **Quality Gates**: Ensure all work passes quality standards before proceeding

## Troubleshooting

### Common Issues

**Script Permission Denied**:
```bash
chmod +x scripts/plan-feature.sh
```

**Project Not Found**:
- Ensure project exists in `.ai-squads/projects/`
- Check project has required files (README.md, mission.md, roadmap.md)

**Template Issues**:
- Verify `templates/projects/` directory exists
- Check template files are present and readable

**Cursor Integration Issues**:
- Use `./scripts/link.sh` to set up Cursor rules
- Ensure `.cursor/rules/` directory exists in target repository

## Examples

### Complete Feature Planning Workflow

```bash
# 1. Plan a new feature
./scripts/plan-feature.sh stui user-authentication --priority high

# 2. Review generated documents
cat .ai-squads/projects/stui/feature-user-authentication/problem.md
cat .ai-squads/projects/stui/feature-user-authentication/tasks.md

# 3. Customize content with specific details
# Edit the generated files with your feature specifics

# 4. Begin implementation following task breakdown
```

### Setting Up Cursor Integration

```bash
# Link AI Squads rules to your project
./scripts/link.sh /path/to/your/project

# Now you can use plan-feature in your project
cd /path/to/your/project
../.ai-squads/scripts/plan-feature.sh myapp new-feature
```

## Contributing

When adding new scripts:

1. **Follow Naming Convention**: Use descriptive names with `.sh` extension
2. **Include Help**: Always provide `--help` option with usage information
3. **Error Handling**: Use proper error handling and colored output
4. **Documentation**: Update this README with new script information
5. **Testing**: Test scripts with various inputs and edge cases

## Dependencies

All scripts require:
- Bash shell
- Git repository structure
- AI Squads directory structure
- Template files in `templates/projects/`

---

**Ready to plan your next feature? Use `./scripts/plan-feature.sh` and let AI Squads guide you to success! 🚀**
