---
description: Common Startup Steps for SquadsAI Instructions
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Startup Rules

## Session Kickoff
- **FIRST STEP**: Always start with the Director Agent (@~/.squads-ai/instructions/agents/director.md) to assess current project state and guide the session
- The Director Agent provides project status, roadmap analysis, and session guidance before proceeding with any other work

## Agent Usage
- IMPORTANT: For any step that specifies a subagent in the subagent="" XML attribute you MUST use the specified subagent to perform the instructions for that step.

## Process Flow
- Process XML blocks sequentially
- Use exact templates as provided
- Follow the Director Agent's session guidance for workflow selection
