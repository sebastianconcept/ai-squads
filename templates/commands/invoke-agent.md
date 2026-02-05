# Invoke Agent

Invoke a specific AI agent for help with your current context.

This command will:
- List available agents from ai-squads
- Allow selection of one or more agents
- Provide context (current file, selection, project)
- Apply agent rules and corresponding style guides
- Generate specialized response

## Usage

Run this command from your project root directory.

Available agents include:
- Rust Specialist
- Smalltalk Specialist
- JavaScript Specialist
- Jobs to be Done
- UI/UX Agent
- UI Developer
- DevOps Specialist

The command uses your current file context and selection to provide relevant guidance.

See `ai-squads/definitions/commands/invoke-agent.md` for detailed workflow.

