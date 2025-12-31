---
name: plan-game
alwaysApply: false
---

# Plan Game Workflow

This workflow guides the creation of a Game Design Document (GDD) for a new game project using Eric, the Video Game Development Specialist.

## Prerequisites

1. Project must have been adopted (have `docs/` directory)
2. Project should have `docs/team.md` (optional, but helpful for team context)

## Steps

### 1. Get Game Name
- Prompt user for game name
- Validate name (no special characters, spaces become hyphens)
- Check if game documentation already exists

### 2. Create Game Structure
- Run `~/.cursor/scripts/create-game-docs.sh {game_name}` from project root
- This creates `docs/game/{game_name}/` directory
- Copies GDD.md template

### 3. Load Project Context (if available)
- Read `docs/team.md` to understand team composition
- Read `docs/mission.md` for project alignment (if game is part of larger project)
- Read `docs/tech-stack.md` for technical context

### 4. Activate Eric (Video Game Specialist)
- Load Eric agent: `../../ai-squads/agents/video-game-specialist.md`
- Eric will guide the GDD creation process

### 5. Gather Game Information

Eric will ask questions to understand the game concept:

**Core Concept:**
- What is the game about? (genre, core idea)
- What makes it unique or interesting?
- Who is the target audience?
- What inspired this game?

**Gameplay:**
- What is the core loop? (the fundamental cycle players repeat)
- What are the key mechanics?
- How does the game progress? (tutorial → early → mid → end game)
- What is the difficulty curve?

**Player Experience:**
- What player motivations does this serve? (Achievers, Explorers, Socializers, Killers)
- How does it create flow state?
- What's the first-time user experience?

**Art & Audio:**
- What visual style? (pixel art, hand-drawn, 3D, etc.)
- What music style and mood?
- Any art/music references?

**Technical:**
- What engine? (Godot recommended for MVP)
- What platforms? (PC, mobile, console)
- Performance requirements?

**Business:**
- Monetization model? (Premium, F2P, etc.)
- Platform strategy? (Steam, itch.io, mobile, console)
- Team size and timeline?

### 6. Generate Game Design Document

Eric will help create a comprehensive GDD.md covering:

**Core Sections:**
- Executive Summary - One-paragraph pitch
- Game Concept - Core idea and vision
- Core Gameplay - Loop, mechanics, flow, difficulty
- Player Experience - Audience, motivations, flow state, FTUE
- Progression Systems - How players grow and are rewarded
- Art & Visual Style - Direction, references, palette
- Audio & Music - Style, sound design, implementation
- Story & Narrative - If applicable
- Technical Requirements - Engine, platforms, performance
- Monetization - Business model and ethical strategy
- Platform Strategy - Launch platforms and approach
- Scope & Team - Team size, timeline, MVP scope
- Success Metrics - Engagement, business, quality metrics
- Risks & Mitigation - Technical, design, scope, market risks
- Out of Scope - What's not in MVP
- References & Inspiration - Games and design principles

### 7. Apply Game Design Principles

Eric applies knowledge from:
- **Jesse Schell's "Art of Game Design"** - 100+ lenses for analyzing mechanics
- **Raph Koster's "Theory of Fun"** - Why games are fun neurologically
- **Steve Swink's "Game Feel"** - The "juice" and satisfying feel
- **Flow Theory (Csikszentmihalyi)** - Challenge-skill balance
- **Octalysis Framework (Yu-kai Chou)** - 8 core player motivations
- **Bartle's Player Types** - Achievers, Explorers, Socializers, Killers

### 8. Validate Design Decisions

Eric will evaluate:
- **Mechanics**: Are they engaging and balanced?
- **Core Loop**: Is it satisfying and repeatable?
- **Player Experience**: Does it respect player time?
- **Flow State**: Is challenge balanced with skill?
- **Ethics**: Are monetization practices player-first?
- **Scope**: Is it realistic for the team?
- **Polish**: Is the core loop refined before expansion?

### 9. Review and Refine
- Present generated GDD
- Allow user to refine sections
- Update document based on feedback
- Iterate on design decisions

## Output

After completion, the game should have:
- `docs/game/{game_name}/GDD.md` - Complete Game Design Document

## Cross-Agent Collaboration

Eric works best when combined with other specialists:

### When to Use @rian (Strategic Designer)
- **Player Psychology Deep Dives**: Applying cognitive biases ethically
- **Engagement Loop Design**: Habit formation and retention
- **Monetization Psychology**: Ethical persuasion for F2P games
- **Analytics Strategy**: Defining and tracking growth metrics

### When to Use @steve (UI/UX Expert)
- **Game UI/UX**: Menu systems, HUD design, inventory interfaces
- **Tutorial Design**: Making tutorials intuitive
- **Accessibility**: Ensuring interfaces are usable for all

### When to Use @bob (Jobs to be Done)
- **Understanding Player Motivations**: What "job" is the player hiring your game to do?
- **Feature Prioritization**: Which features serve the core player job?

## Notes

- Reference game design canon (Schell, Koster, Swink, etc.)
- Keep GDD focused and actionable
- Prioritize core loop before expanding scope
- Ensure ethical monetization practices
- Consider team size and realistic scope
- Align with project mission (if part of larger project)

## Example Usage

```
/plan-game
# Follow prompts to create GDD for your game

# Or combine with other agents:
@eric @rian help me design ethical monetization for my F2P game
@eric @steve review my game's UI/UX design
@eric @bob what job is my game helping players accomplish?
```

