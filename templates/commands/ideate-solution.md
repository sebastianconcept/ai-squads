# Ideate Solution

Generate multiple solution approaches to a problem, from minimal to comprehensive.

This command will:
- Clarify the problem and scope
- Generate 2+ solution approaches (minimal → comprehensive)
- Provide pros/cons and effort estimates for each
- Offer comparison matrix across approaches
- Optionally prepare a `plan-feature` handoff prompt

## Usage

Run this command from your project root directory.

The command reads your project context from `~/docs/{project-name}/` to provide relevant guidance:
- `~/docs/{project-name}/README.md` - Project overview
- `~/docs/{project-name}/MISSION.md` - Project purpose and goals
- `~/docs/{project-name}/TECH-STACK.md` - Technologies and constraints
- `~/docs/{project-name}/DECISIONS.md` - Existing architectural decisions
- `~/docs/{project-name}/TEAM.md` - Available agent expertise

## Templates Used

Output templates from `ai-squads/templates/ideation/`:
- `approach.md` - Structure for each solution approach
- `comparison-matrix.md` - Quick comparison table
- `plan-feature-prompt.md` - Handoff to feature planning

See `ai-squads/commands/ideate-solution.md` for detailed workflow.

