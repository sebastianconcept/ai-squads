---
name: project-starter
alwaysApply: false
---

# Project Starter Workflow

This workflow helps founders organize their business idea into a structured project with pitch deck and lean canvas generation. It uses a multi-agent team (Ben - Startup Advisor, Bob, Steve, Rian, Gustavo) to guide users through a conversational process that both supports and challenges them to mature their business concept from multiple perspectives.

## Prerequisites

1. Global ai-squads installation must be present (run `./scripts/install.sh` from ai-squads directory)
2. Agent team must be available:
   - Startup advisor agent (`agents/startup-advisor.md`) - Ben
   - Jobs to be Done agent (`agents/jobs-to-be-done.md`) - Bob
   - UI/UX agent (`agents/ui-ux.md`) - Steve
   - Strategic Designer agent (`agents/strategic-designer.md`) - Rian
   - Financial Advisor agent (`agents/financial-advisor.md`) - Gustavo

## Steps

### 1. Verify Global Installation
- Check if `~/.cursor/templates/` directory exists
- Verify it contains project templates
- Verify startup advisor agent is available
- If not found, prompt user to run global installation first

### 2. Validate Project Name
- Prompt user for project name (business name)
- Validate name format:
  - No special characters except hyphens
  - Spaces automatically converted to hyphens
  - Must be a valid directory name
- Check if project already exists in `~/docs/{project-name}/`
- If exists, warn user and allow:
  - Override (continue with existing project)
  - Choose different name
  - Cancel
- Store validated project name for use in subsequent steps

### 3. Initialize Project Structure
- Create `~/docs/{project-name}/` directory if it doesn't exist
- Copy project templates to `~/docs/{project-name}/`:
  - MISSION.md (will be populated from business concept)
  - ROADMAP.md (will be populated from business plan)
  - TECH-STACK.md (placeholder, to be filled later)
  - DECISIONS.md (empty initially)
  - README.md (will be populated with project overview)
  - TEAM.md (will include startup advisor agent)
- Create `~/docs/{project-name}/notes/` directory for conversation state
- Create initial `~/docs/{project-name}/PROGRESS.md` using `update-progress.sh` script
  - Script detects business planning mode (checks for LEAN-CANVAS.md, PITCH-DECK.md, or business-planning notes)
  - Script creates PROGRESS.md with business planning structure
  - Status: "Business planning in progress"
  - Current focus: "Completing business planning with startup advisor"
  - Next actions: Track conversation progress

### 4. Multi-Agent Business Information Gathering

**Agent Team:**
- **Ben (Startup Advisor)** (orchestrates conversation, ensures comprehensive coverage)
- **Bob (Jobs to be Done)** (ensures business serves core jobs, understands user motivations)
- **Steve (UI/UX)** (ensures business considers user experience, usability)
- **Rian (Strategic Designer)** (ensures business has growth strategy, engagement loops)
- **Gustavo (Financial Advisor)** (ensures business model is financially viable, unit economics work)

**Agent Invocation:**
- Agents can be invoked in two ways:
  1. **During command execution** (automatic): Agents are invoked as part of this workflow
  2. **Manually by user** (anytime): User can invoke agents directly to continue conversations

**Conversation Flow - Parallel Questions Per Topic:**
- **Ben (Startup Advisor)** starts with open-ended question: "Tell me about your business idea"
- **All Agents** analyze response from their perspectives:
  - **Ben:** Maturity level, information gaps, market context
  - **Bob:** What job does this serve? What progress are users trying to make?
  - **Steve:** How will users experience this? Is it usable?
  - **Rian:** How will this grow? What's the growth strategy?
  - **Gustavo:** Is this financially viable? Do unit economics work?

- **Parallel Questions Per Topic:** When user interaction is needed, agents present questions in parallel:
  1. **Identify Topic:** Ben (Startup Advisor) identifies topic needing user input (e.g., "Problem", "Solution", "Market", "Business Model")
  2. **Formulate Questions:** All agents formulate their question about this topic from their perspective
  3. **Create Answer Options:** Each agent provides 3-4 answer options plus "Other" to guide user thinking
  4. **Present Together:** All questions are presented together in a structured format:
     ```
     Topic: [Topic Name]
     
     [Ben - Startup Advisor] Question: [Question text]
     Options:
       A) [Option A]
       B) [Option B]
       C) [Option C]
       D) [Other - please specify]
     
     [Bob - Jobs to be Done] Question: [Question text]
     Options:
       A) [Option A]
       B) [Option B]
       C) [Option C]
       D) [Other - please specify]
     
     [Steve - UI/UX] Question: [Question text]
     Options:
       A) [Option A]
       B) [Option B]
       C) [Option C]
       D) [Other - please specify]
     
     [Rian - Strategic Designer] Question: [Question text]
     Options:
       A) [Option A]
       B) [Option B]
       C) [Option C]
       D) [Other - please specify]
     
     [Gustavo - Financial Advisor] Question: [Question text]
     Options:
       A) [Option A]
       B) [Option B]
       C) [Option C]
       D) [Other - please specify]
     
     Please answer all questions above, then we'll move to the next topic.
     ```
  5. **User Answers:** User answers all questions (can select options or provide free-form responses)
  6. **Agents Analyze:** All agents analyze responses from their perspectives
  7. **Move to Next Topic:** Startup advisor identifies next topic and process repeats

- **Question Types (Mixed in Options):**
  - **Supportive Questions:** Help founders articulate and build on ideas
  - **Adversarial Questions:** Challenge assumptions and identify weaknesses
  - **Discovery Questions:** Explore possibilities and pivot opportunities

- **Benefits of Parallel Questions:**
  - **Expands User Mind:** Multiple perspectives presented together
  - **Comprehensive Coverage:** All aspects of topic covered at once
  - **Efficient:** User answers all questions about topic before moving on
  - **Guided:** Answer options help users think through possibilities
  - **Coherent:** Ben (Startup Advisor) orchestrates to maintain conversation flow

- **Agents collaborate** to build comprehensive understanding
- **Agents challenge** assumptions from their specializations
- **Agents document** insights in notes with attribution
- **Agents suggest** updates to business documents

**Question Style Balance:**
- **Supportive (Favorable):** Help founders articulate ideas, explore possibilities, build on concepts
- **Adversarial (Challenging):** Stress-test assumptions, identify weaknesses, discover gaps
- **Discovery:** Explore alternatives, pivot opportunities, different approaches

**Note-Taking During Conversation (Multi-Agent):**
- **All agents** maintain conversation state in notes with category: "business-planning" in frontmatter/metadata
- Notes can use grouped structure (`notes/business-planning/CONTEXT.md`) or flat structure (`notes/business-planning-CONTEXT.md`)
- Category is stored in frontmatter, NOT in directory path
- Standard note files:
  - `CONTEXT.md`: Business idea context, goals, current stage (category: "business-planning" in frontmatter)
    - Includes perspectives from all agents (Jobs to be Done, UX, Growth, Financial)
  - `TODOS.md`: Information gaps, follow-up questions, pending items (category: "business-planning" in frontmatter)
    - Organized by agent perspective when relevant
  - `insights.json`: Key insights, assumptions, decisions discovered (category: "business-planning" in metadata)
    - Includes insights from all agents with source attribution
    - Example: `{"insight": "...", "source": "bob", "type": "jobs-to-be-done"}`
  - Conversation history and key responses
  - **Agent Contributions:** Each agent documents their contributions and perspectives for product discovery
- Agent reads `~/docs/{project-name}/PROGRESS.md` for context:
  - Current status of business planning conversation
  - Next step or question to address
  - What has been completed so far
  - What information gaps remain
- Agent updates `~/docs/{project-name}/PROGRESS.md` during conversation:
  - Updates "Current Position" status to reflect conversation progress
  - Updates "Next Actions" to show next step in conversation
  - Tracks which lean canvas components have been discussed
  - Notes information gaps that need follow-up
- When significant insights emerge:
  - Agent documents them in notes
  - Agent suggests updating business documents:
    - "This insight should be reflected in your LEAN-CANVAS.md"
    - "Consider updating PITCH-DECK.md with this information"
    - "This affects your ROADMAP.md - should we update it?"
- Agent tracks which lean canvas components have been discussed
- Agent notes gaps in information that need follow-up

**Resuming Conversations:**
- Users can resume conversations anytime by invoking `@ben` (or `@startup-advisor` for backward compatibility)
- Agent reads `~/docs/{project-name}/PROGRESS.md` FIRST to understand:
  - Current status of business planning
  - Next step or question to address
  - What has been completed
- Agent reads notes with category: "business-planning" in frontmatter/metadata to restore deeper context
- Agent references previous discussions and builds on them
- Agent can continue from where conversation left off (as indicated in PROGRESS.md)
- Agent tracks evolution of business idea over time
- Agent updates PROGRESS.md to reflect conversation progress

**Question Categories Covered (Multi-Agent Perspective):**

1. **Problem & Solution:**
   - **Ben (Startup Advisor):** What problem? For whom? How?
   - **Bob:** What job does this problem relate to? Does solution serve the job?
   - **Steve:** How does problem manifest in UX? Is solution usable?
   - **Rian:** How urgent is problem? How does solution enable growth?
   - **Gustavo:** What's financial impact of problem? Is solution financially viable?

2. **Market & Customers:**
   - **Ben (Startup Advisor):** Who are customers? Market size? How to reach them?
   - **Bob:** What jobs do customers have? What progress are they trying to make?
   - **Steve:** How do customers interact with products? What's their UX context?
   - **Rian:** How do we reach customers? What's acquisition strategy?
   - **Gustavo:** What's LTV of customers? What's CAC?

3. **Value Proposition:**
   - **Ben (Startup Advisor):** What makes solution unique? Why choose you?
   - **Bob:** What job does value proposition serve?
   - **Steve:** Is value proposition clear to users?
   - **Rian:** How does value proposition drive growth?
   - **Gustavo:** What's financial value of proposition?

4. **Business Model:**
   - **Ben (Startup Advisor):** How do you make money? Costs?
   - **Bob:** How does revenue model relate to jobs? What are users willing to pay?
   - **Steve:** How does pricing affect user experience?
   - **Rian:** How does revenue model enable growth?
   - **Gustavo:** Is revenue model sustainable? Do unit economics work? (CAC < LTV)

5. **Competitive Advantage:**
   - **Ben (Startup Advisor):** What's your unfair advantage?
   - **Bob:** How does advantage relate to serving jobs?
   - **Steve:** How does advantage affect user experience?
   - **Rian:** How does advantage enable growth?
   - **Gustavo:** How does advantage affect financials?

6. **Metrics & Validation:**
   - **Ben (Startup Advisor):** How do you measure success? What traction?
   - **Bob:** What metrics prove we're serving the job?
   - **Steve:** What metrics prove good user experience?
   - **Rian:** What metrics prove growth?
   - **Gustavo:** What financial metrics matter?

7. **Team & Resources:**
   - **Ben (Startup Advisor):** Who's on team? What resources needed?
   - **Bob:** What skills needed to serve the job?
   - **Steve:** What UX/design skills needed?
   - **Rian:** What growth/marketing skills needed?
   - **Gustavo:** What financial/business skills needed?

8. **Funding & Growth:**
   - **Ben (Startup Advisor):** What stage? What do you need to grow?
   - **Bob:** How does funding help serve jobs better?
   - **Steve:** How does funding improve user experience?
   - **Rian:** What's growth strategy? How will you acquire customers?
   - **Gustavo:** What's financial need? What are unit economics?

**Handling Partial Information:**
- If users don't have answers yet, agent:
  - Notes the gap in `TODOS.md`
  - Marks sections as "TBD" in generated documents
  - Allows users to return later to fill gaps
  - Suggests when to revisit specific questions

**Multi-Agent Adversarial Questioning (Optional but Recommended):**
- After initial business idea capture, agents can offer: "Would you like to stress-test your idea with adversarial questions from multiple perspectives? This will help identify weaknesses, validate assumptions, and discover pivot opportunities."
- If user accepts:
  - **All agents** facilitate adversarial questioning session using co-founder adversarial questioning framework
  - **Each agent** asks hard questions that stress-test business assumptions from their perspective:
    - **Co-Founder Alignment:** Vision, commitment, roles, values alignment (Ben)
    - **Product Adversarial:** 
      - **Bob:** Does solution serve the job? What if users don't have this job?
      - **Steve:** Is solution usable? What if users get confused?
      - **Rian:** How will this grow? What if users don't activate?
      - **Gustavo:** Do unit economics work? What if costs are too high?
      - **Ben (Startup Advisor):** Is this the best solution? What are alternatives?
    - **Market Validation:**
      - **Bob:** Are we targeting right customers? What if they don't have this job?
      - **Steve:** How do customers experience products? What's their UX context?
      - **Rian:** How do we reach customers? What if acquisition doesn't work?
      - **Gustavo:** What's customer LTV? What if CAC is too high?
    - **Business Model Validation:**
      - **Bob:** Will users pay for this? What are they willing to pay?
      - **Steve:** How does pricing affect user experience?
      - **Rian:** How does revenue model enable growth?
      - **Gustavo:** Is revenue model sustainable? Do unit economics work?
    - **Competitive & Market:** Competition analysis, market dynamics (all agents)
    - **Team & Execution:** Team capabilities, execution risks (all agents)
  - **Agents balance** opportunity exploration with critical challenges
  - **Agents help** identify weaknesses and pivot opportunities
  - **Agents assess** if business idea is "solid enough" using defined criteria:
    - Problem validation (must have) - all agents contribute
    - Solution validation (must have) - all agents contribute
    - Market validation (must have) - all agents contribute
    - Business model validation (must have) - Gustavo + Rian + Ben
    - Team validation (must have) - Ben
    - Competitive position (should have) - all agents contribute
    - Risk assessment (should have) - all agents contribute
  - **Agents help** determine when to proceed vs. pivot vs. keep exploring
  - **Agents document** adversarial questioning session and outcomes in notes
  - **Agents update** business documents based on insights and pivots discovered
  - **Agents help** plan validation for assumptions that need testing
- Adversarial questioning can happen:
  - During initial business planning (recommended)
  - When idea needs to pivot
  - When co-founders need alignment
  - Anytime by invoking agents and requesting adversarial questioning

### 5. Generate and Update Lean Canvas Document

**Template Usage:**
- Start with template from `~/.cursor/templates/lean-canvas.md` (if exists)
- Or use standard lean canvas structure defined in SPECS.md
- Agent reads template and creates initial document when sufficient information is gathered

**Document Creation and Editing (Multi-Agent Enriched):**
- **Agents collaborate** to create `~/docs/{project-name}/LEAN-CANVAS.md` when enough information is available
- **Agents edit** the file directly using file operations (no regeneration needed)
- **Agents populate** all standard sections incrementally with enriched content:
  - **Problem** (top 3 problems customers face)
    - Enriched with: Jobs to be Done perspective (Bob), UX considerations (Steve), growth implications (Rian), financial impact (Gustavo)
  - **Solution** (top 3 features/solutions)
    - Enriched with: Job-serving analysis (Bob), usability considerations (Steve), growth enablement (Rian), financial viability (Gustavo)
  - **Key Metrics** (key activities to track)
    - Enriched with: Jobs metrics (Bob), UX metrics (Steve), growth metrics (Rian), financial metrics (Gustavo)
  - **Unique Value Proposition** (single, clear, compelling message)
    - Enriched with: Jobs perspective (Bob), clarity assessment (Steve), growth potential (Rian), financial value (Gustavo)
  - **Unfair Advantage** (something that cannot be easily copied)
    - Enriched with: Jobs perspective (Bob), UX advantage (Steve), growth advantage (Rian), financial advantage (Gustavo)
  - **Channels** (path to customers)
    - Enriched with: Jobs timing (Bob), UX considerations (Steve), growth strategy (Rian), financial efficiency (Gustavo)
  - **Customer Segments** (target customers)
    - Enriched with: Jobs analysis (Bob), UX context (Steve), acquisition strategy (Rian), financial metrics (Gustavo)
  - **Cost Structure** (fixed and variable costs)
    - Enriched with: Jobs necessity (Bob), UX costs (Steve), growth costs (Rian), financial optimization (Gustavo)
  - **Revenue Streams** (revenue model)
    - Enriched with: Jobs willingness to pay (Bob), UX impact (Steve), growth enablement (Rian), financial sustainability (Gustavo)
- **Agents mark** sections with missing information as "TBD" or "To be determined"
- **Agents update** sections in-place as new information emerges during conversation
- **Agents document** their contributions and perspectives in notes for product discovery

**Content Evolution:**
- Agent edits document directly as conversation progresses
- Agent can refine and evolve content based on:
  - New insights from continued conversations
  - Market research or validation
  - Customer feedback
- Documents are living documents that evolve with the business idea
- No regeneration needed - agent updates files incrementally

### 6. Generate and Update Pitch Deck Document

**Template Usage:**
- Start with template from `~/.cursor/templates/pitch-deck.md` (if exists)
- Or use standard pitch deck structure defined in SPECS.md
- Agent reads template and creates initial document when sufficient information is gathered

**Document Creation and Editing (Multi-Agent Enriched):**
- **Agents collaborate** to create `~/docs/{project-name}/PITCH-DECK.md` when enough information is available
- **Agents edit** the file directly using file operations (no regeneration needed)
- **Agents populate** standard sections incrementally (each as a "slide") with enriched content:
  - **Slide 1: Problem**
    - Enriched with: Jobs to be Done perspective (Bob), UX considerations (Steve), growth implications (Rian), financial impact (Gustavo)
  - **Slide 2: Solution**
    - Enriched with: Job-serving analysis (Bob), usability considerations (Steve), growth enablement (Rian), financial viability (Gustavo)
  - **Slide 3: Market Opportunity**
    - Enriched with: Jobs market size (Bob), UX market context (Steve), growth strategy (Rian), financial analysis (Gustavo)
  - **Slide 4: Business Model**
    - Enriched with: Jobs revenue model (Bob), UX pricing impact (Steve), growth enablement (Rian), unit economics (Gustavo)
  - **Slide 5: Traction/Validation**
    - Enriched with: Jobs validation (Bob), UX validation (Steve), growth metrics (Rian), financial metrics (Gustavo)
  - **Slide 6: Team**
    - Enriched with: Jobs skills needed (Bob), UX skills needed (Steve), growth skills needed (Rian), financial skills needed (Gustavo)
  - **Slide 7: Financial Projections**
    - Enriched with: Growth assumptions (Rian), unit economics (Gustavo), financial viability (Gustavo)
  - **Slide 8: The Ask**
    - Enriched with: Growth plans (Rian), financial needs (Gustavo)
- **Agents tailor** content based on:
  - Business stage (pre-seed, seed, Series A)
  - Available information (some sections may be lighter for early stage)
  - Market context
- **Agents mark** sections with missing information appropriately
- **Agents update** sections in-place as new information emerges during conversation
- **Agents document** their contributions and perspectives in notes for product discovery

**Content Evolution:**
- Agent edits document directly as conversation progresses
- Agent can refine and evolve content based on:
  - New insights from continued conversations
  - Traction and validation data
  - Fundraising feedback
- Documents are living documents that evolve with the business idea
- No regeneration needed - agent updates files incrementally

### 7. Populate Project Documentation

**MISSION.md:**
- Derive from business concept and value proposition
- Include problem statement and target audience
- Populate from information gathered during Q&A

**ROADMAP.md:**
- Create initial roadmap based on business plan
- Include:
  - Immediate priorities (MVP, validation, first customers)
  - Short-term goals (traction, product-market fit)
  - Long-term vision
- Populate from business planning conversation

**TEAM.md:**
- Configure agent team for business planning and product development:
  - **Ben (Startup Advisor):** Business planning, validation strategy, and growth guidance
  - **Bob (Jobs to be Done):** Product strategy, user needs analysis, feature prioritization
  - **Steve (UI/UX):** User interface and experience design, usability
  - **Rian (Strategic Designer):** Growth strategy, engagement loops, product-market fit metrics
  - **Gustavo (Financial Advisor):** Business model validation, unit economics, financial viability
- Note that agents can be invoked anytime for:
  - Continued business planning (multi-agent collaboration)
  - Product discovery (translating business planning to product strategy)
  - Feature hypothesis evaluation (treating features as business hypotheses)
  - MVP strategy and validation planning
  - Growth strategy guidance
  - Progress evaluation and business growth

**README.md:**
- Create project overview
- Include business concept summary
- Reference generated documents (LEAN-CANVAS.md, PITCH-DECK.md)
- Note that Ben (Startup Advisor) can be invoked for continued planning

**TECH-STACK.md:**
- Leave as placeholder
- Note: "To be determined during development planning"

**DECISIONS.md:**
- Leave empty initially
- Note: "Business planning decisions will be documented here"

### 8. Update PROGRESS.md for Completion or Continuation

**If Business Planning is Complete:**
- Update `~/docs/{project-name}/PROGRESS.md`:
  - Set status to "Business planning complete"
  - Set current focus to first item in ROADMAP.md (execution phase)
  - Update "Next Actions" to reflect roadmap priorities
  - Note that business planning is done and project is ready for execution

**If Business Planning is Incomplete:**
- Update `~/docs/{project-name}/PROGRESS.md`:
  - Set status to "Business planning in progress"
  - Set current focus to "Completing business planning with startup advisor"
  - Update "Next Actions" to show next step in conversation:
    - Next question to address
    - Information gap to fill
    - Lean canvas component to discuss
  - Note that user can resume by invoking `@ben`

**PROGRESS.md Structure for Business Planning:**
```markdown
## Current Position

**Active Feature:** Business Planning
**Current Story:** Completing business planning conversation
**Status:** Business planning in progress
**Last activity:** [Timestamp] — [Last question/insight]

## Next Actions

**Immediate:**
- [ ] [Next step in conversation - e.g., "Discuss revenue model", "Define customer segments", "Complete lean canvas"]

**Upcoming:**
- [ ] Generate LEAN-CANVAS.md (when information complete)
- [ ] Generate PITCH-DECK.md (when information complete)
- [ ] Begin roadmap execution (when business planning complete)
```

### 9. Finalize and Present Summary

**Summary Presentation:**
- Show user what was created:
  - Project directory: `~/docs/{project-name}/`
  - Lean Canvas: `~/docs/{project-name}/LEAN-CANVAS.md` (if complete)
  - Pitch Deck: `~/docs/{project-name}/PITCH-DECK.md` (if complete)
  - Project documentation structure
  - Notes directory: `~/docs/{project-name}/notes/` (category: "business-planning" in frontmatter/metadata)
  - Progress tracking: `~/docs/{project-name}/PROGRESS.md`

**Next Steps - Offer Two Paths:**

**If business planning is complete or mostly complete:**
- **Assess Completeness First:**
  - Check which sections of LEAN-CANVAS.md are complete vs. TBD
  - Check which sections of PITCH-DECK.md are complete vs. TBD
  - Provide summary: "X of Y key sections complete"
  - Note which key sections are missing (if any)

- Present two clear options with completeness context:

  **Option 1: Continue Refining Business Planning**
  - "Would you like to continue refining your business planning documents?"
  - Explain that user can:
    - Resume conversation with Ben (Startup Advisor): `@ben`
    - Review and refine LEAN-CANVAS.md, PITCH-DECK.md, ROADMAP.md
    - Go through adversarial questioning to stress-test the idea
    - Evolve business documents as the idea matures
- Explain that agents can be invoked anytime:
  - `@ben` - Business planning, validation strategy, growth guidance (Startup Advisor)
  - `@bob` - Jobs to be Done, product strategy, user needs
  - `@steve` - UI/UX design, usability, user experience
  - `@rian` - Growth strategy, engagement loops, product-market fit metrics
  - `@gustavo` - Business model validation, unit economics, financial viability
- Explain ongoing role of agents:
  - **Business plan refinement**: Continue evolving lean canvas, pitch deck, roadmap (all agents)
  - **Adversarial questioning**: Stress-test business idea from multiple perspectives (all agents)
  - **Product discovery**: Translate business planning to product strategy (all agents)
  - **Feature hypothesis evaluation**: Help evaluate features as business hypotheses (all agents)
  - **MVP strategy**: Guide MVP development to validate hypotheses efficiently (all agents)
  - **Validation planning**: Design validation experiments for features and assumptions (all agents)
    - **Growth strategy**: Connect validation learnings to growth strategies (Rian, Ben)
  - **Progress evaluation**: Evaluate business progress and identify growth opportunities (all agents)
  - **Ongoing growth guidance**: Continuous business growth guidance beyond initial planning (all agents)

  **Option 2: Start Product Discovery**
  - "Would you like to translate your business planning into a Product Discovery Document?"
  - **Completeness Context:**
    - If mostly complete: "Your business planning looks solid. Ready for product discovery!"
    - If partially complete: "Your business planning has some incomplete sections (e.g., [list key missing sections]). You can start product discovery now (we'll ask questions to fill gaps), or complete business planning first for a more complete product strategy."
  - Explain that product discovery will:
    - Translate business strategy to product strategy
    - Help decide what to build first (MVP definition)
    - Create feature prioritization framework (what to build, what not to build)
    - Guide product development toward product-market fit
    - Bridge business planning to feature planning
    - **Ask questions to fill any gaps** in business planning documents
  - Explain that user can start product discovery by running: `/discover-product`
  - Note that product discovery uses business planning documents as input:
    - LEAN-CANVAS.md → Product vision, customer segments, value proposition
    - PITCH-DECK.md → Market opportunity, business model
    - MISSION.md → Product mission
  - **If sections are incomplete:** Explain that product discovery will ask questions to fill gaps during the process
  - Explain that after product discovery, user can:
    - Plan features using `/plan-feature` (which will use PRODUCT-DISCOVERY.md as context)
    - Continue refining business planning documents
    - Return to product discovery anytime to update product strategy

- Explain that both paths are available:
  - Users can refine business planning documents anytime
  - Users can start product discovery when ready
  - Users can do both (refine business planning, then do product discovery, then refine again)
  - Documents are living documents that evolve over time

**If business planning is incomplete:**
- Explain that user can resume by invoking agents:
  - `@ben` - Continue business planning conversation (Startup Advisor)
  - `@bob` - Explore Jobs to be Done perspective
  - `@steve` - Think about user experience
  - `@rian` - Explore growth strategy
  - `@gustavo` - Validate business model and unit economics
- Point to PROGRESS.md to see next step in conversation
- Note that agents will read PROGRESS.md and notes to continue seamlessly
- Explain that once business planning is more complete, user can:
  - Continue refining business planning documents (with all agents)
  - Start product discovery (if enough information is available)
- Explain that agents can be invoked anytime for continued collaboration

**General Guidance:**
- Explain that documents can be evolved through continued conversations
- Suggest reviewing generated documents
- Note that users can return to refine lean canvas, pitch deck, and roadmap anytime
- Mention that project is ready for development planning (can use `/adopt-project` if code repo is created)

**Resume Capability:**
- Emphasize that users can resume conversations with any agent
- **All agents** will read PROGRESS.md FIRST to understand current state
- **All agents** will read notes to restore deeper context
- **Agents** can continue from where conversation left off (as indicated in PROGRESS.md)
- **Agents** can help evolve all business documents over time
- **Agents** collaborate to provide comprehensive input
- Once business planning basics are complete, PROGRESS.md transitions to track roadmap execution

## Output

After completion, the project should have:
- `~/docs/{project-name}/` directory with project documentation
- `~/docs/{project-name}/LEAN-CANVAS.md` - One-page business model
- `~/docs/{project-name}/PITCH-DECK.md` - Pitch deck in markdown
- `~/docs/{project-name}/MISSION.md` - Business mission
- `~/docs/{project-name}/ROADMAP.md` - Initial roadmap
- `~/docs/{project-name}/TEAM.md` - Team configuration (includes startup advisor)
- `~/docs/{project-name}/README.md` - Project overview
- `~/docs/{project-name}/notes/` - Conversation notes and insights (category: "business-planning" in frontmatter/metadata)
- `~/docs/{project-name}/PROGRESS.md` - Tracks business planning conversation state, transitions to roadmap execution when complete
- Other standard project documentation files

## Notes

- All paths should be relative to project root or use `~/docs/` for user's docs directory
- **All agents** can be invoked manually anytime: `@ben`, `@bob`, `@steve`, `@rian`, `@gustavo`
- Conversations are maintained in notes for continuity
- PROGRESS.md tracks conversation state and next steps
- **All agents** read PROGRESS.md FIRST to understand current state, then read notes for deeper context
- Documents can be evolved through continued conversations with all agents
- Templates are starting points - agents can evolve content based on insights
- Once business planning basics are complete, PROGRESS.md transitions to track roadmap execution (first item in ROADMAP.md)
- **Multi-agent collaboration** enriches business planning documents with multiple perspectives:
  - Jobs to be Done perspective (Bob)
  - User experience considerations (Steve)
  - Growth strategy and engagement (Rian)
  - Financial viability and unit economics (Gustavo)
- **Richer documents** provide better input for product discovery
- **Ongoing role**: Agents can be consulted anytime to:
  - Continue business planning with multi-agent collaboration
  - Evaluate features as business hypotheses
  - Guide MVP strategy and validation planning
  - Connect validation learnings to growth strategies
  - Evaluate business progress and identify growth opportunities
  - Provide continuous business growth guidance
