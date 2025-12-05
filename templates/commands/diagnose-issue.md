# Diagnose Issue

Investigate problems by analyzing evidence, generating hypotheses, and finding root causes.

This command will:
- Gather and analyze evidence (logs, errors, descriptions)
- Generate hypotheses ordered by likelihood
- Guide investigation to narrow down the cause
- Formulate a clear problem definition when root cause is found
- Offer handoff to `ideate-solution` for fix exploration

## Usage

Run this command from your project root directory.

The command reads your project context from `docs/` for technical understanding:
- `docs/README.md` - Project overview
- `docs/tech-stack.md` - Technologies and architecture
- `docs/DECISIONS.md` - Design decisions that may be relevant

## Workflow

```
[Problem + Evidence] → diagnose-issue → [Root Cause] → ideate-solution → [Fix Approaches]
```

## Templates Used

Output templates from `ai-squads/templates/diagnosis/`:
- `hypothesis.md` - Structure for each hypothesis
- `problem-definition.md` - Clear problem statement format
- `ideate-prompt.md` - Handoff to solution ideation

See `ai-squads/commands/diagnose-issue.md` for detailed workflow.

