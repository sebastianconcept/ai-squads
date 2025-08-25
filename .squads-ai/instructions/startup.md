---
description: Common Startup Steps for SquadsAI Instructions
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# Startup Rules

## Session Kickoff
- **FIRST STEP**: Always start with the Director Agent (@~/.squads-ai/agents/director.md) to assess current project state and guide the session
- The Director Agent identifies the active squad, provides project status, roadmap analysis, and squad-specific session guidance before proceeding with any other work

## Squad Context
- **SQUAD IDENTIFICATION**: The Director Agent must identify which squad is active (e.g., Elite squad for Rust/Smalltalk projects)
- **AGENT AVAILABILITY**: Only agents available in the active squad can be activated
- **SQUAD CAPABILITIES**: Workflows and recommendations must align with the squad's specialized focus areas

## Agent Usage
- IMPORTANT: For any step that specifies a subagent in the subagent="" XML attribute you MUST use the specified subagent to perform the instructions for that step.
- **SQUAD AGENTS ONLY**: Only activate agents that are available in the current squad
- **AGENT COORDINATION**: The Director Agent coordinates between squad agents based on project needs
- **JTBD INTEGRATION**: @agent:jtbd-expert participates in all planning workflows for customer job validation

## Process Flow
- Process XML blocks sequentially
- Use exact templates as provided
- Follow the Director Agent's squad-specific session guidance for workflow selection
- Ensure all agent activations are within the squad's available agent set
- Include JTBD analysis in all planning workflows for customer job validation

## Squad-Specific Workflows
- **Elite Squad**: Specialized in Rust and Smalltalk/Pharo projects with JTBD methodology
- **Other Squads**: Follow their specific focus areas and available agents
- **Project Context**: Always check .squads-ai/projects/[PROJECT_NAME] for project-specific instructions
- **JTBD Integration**: All planning workflows include customer job validation
