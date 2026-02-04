---
name: discover-product
alwaysApply: false
---

# Discover Product Workflow

This workflow helps founders translate business planning into actionable product strategy. It creates a Product Discovery Document (PDD) that guides what to build (and what not to build) for product-market fit.

**Analogous to:** Game Design Document (GDD) for games, but for general products.

## Prerequisites

1. Project must have been created (have `~/docs/{project-name}/` directory)
2. Business planning should be complete or mostly complete:
   - `LEAN-CANVAS.md` exists (or mostly complete)
   - `PITCH-DECK.md` exists (or mostly complete)
   - `MISSION.md` exists
3. Project must have `~/docs/{project-name}/TEAM.md` with agent team

## When to Use

- After business planning is complete (or mostly complete)
- When founders need to translate business strategy to product strategy
- When deciding what to build first
- When prioritizing features
- When validating product-market fit
- When bridging business planning to feature planning

## Agent Team for Product Discovery

Product discovery uses a collaborative multi-agent approach. Each agent provides valuable input from their specialization:

### Bob (Jobs to be Done)
**Role:** Ensures product serves core jobs users hire it to do
**Contributions:**
- Identifies what job the product is hired to do (functional, emotional, social)
- Understands what progress users are trying to make
- Prioritizes features by how well they serve core jobs
- Identifies competing solutions and switching forces
- Reviews product vision, feature prioritization, user experience from Jobs to be Done perspective

### Steve (UI/UX)
**Role:** Ensures product is usable, clear, and follows UX best practices
**Contributions:**
- Reviews product flow for usability and clarity
- Ensures user journey is intuitive and follows conventions
- Designs first-time user experience (FTUE) for clarity
- Reviews information architecture
- Identifies usability concerns and provides UX feedback

### Rian (Strategic Designer)
**Role:** Defines growth strategy, engagement loops, and product-market fit metrics
**Contributions:**
- Defines product-market fit metrics from growth perspective
- Designs ethical engagement loops and habit formation
- Creates activation and retention strategies
- Defines North Star metric and growth metrics
- Optimizes go-to-market strategy with growth focus
- Reviews product strategy, metrics, and roadmap from growth perspective

### Gustavo (Financial Advisor)
**Role:** Validates business model, unit economics, and financial viability
**Contributions:**
- Validates unit economics (CAC < LTV, positive margins)
- Assesses financial viability of product-market fit assumptions
- Reviews revenue model and cost structure
- Defines financial metrics (CAC, LTV, unit economics, revenue metrics)
- Validates MVP financial viability
- Reviews go-to-market strategy from financial perspective
- Assesses financial risks and mitigation plans

### Startup Advisor
**Role:** Guides overall product strategy and ensures alignment with business planning
**Contributions:**
- Translates business planning documents to product strategy
- Defines product-market fit strategy and validation plans
- Guides MVP definition and success criteria
- Reviews overall product strategy and alignment
- Ensures evidence-based approach (derived from business planning)

## Steps

### 1. Verify Prerequisites and Assess Completeness
- Check if `~/docs/{project-name}/` directory exists
- Check if business planning documents exist:
  - `LEAN-CANVAS.md` (required)
  - `PITCH-DECK.md` (required)
  - `MISSION.md` (required)
- **Assess Completeness:**
  - Check which sections of LEAN-CANVAS.md are complete vs. TBD:
    - Required: Problem, Solution, Customer Segments (minimum for product discovery)
    - Important: Value Proposition, Channels, Key Metrics, Revenue Streams, Cost Structure
  - Check which sections of PITCH-DECK.md are complete vs. TBD:
    - Required: Problem, Solution (minimum for product discovery)
    - Important: Market Opportunity, Business Model, Traction/Validation
  - Provide summary: "Business planning completeness: X of Y key sections complete"
- **If documents don't exist:**
  - Prompt user: "Business planning documents are missing. Would you like to complete business planning first using `/project-starter`?"
  - Allow user to proceed anyway if they want (documents can be created during discovery)
- **If documents exist but are incomplete:**
  - Inform user: "Some business planning sections are incomplete (TBD). Product discovery will ask questions to fill gaps during the process."
  - Optionally offer: "Would you like to complete business planning first, or proceed with product discovery and fill gaps as we go?"
  - Allow user to proceed (product discovery handles incomplete information gracefully)

### 2. Load Business Context
- Read `~/docs/{project-name}/LEAN-CANVAS.md`:
  - Extract: Problem, Solution, Customer Segments, Value Proposition, Channels, Key Metrics
  - **Note which sections are TBD or incomplete**
- Read `~/docs/{project-name}/PITCH-DECK.md`:
  - Extract: Market Opportunity, Business Model, Traction/Validation
  - **Note which sections are TBD or incomplete**
- Read `~/docs/{project-name}/MISSION.md`:
  - Extract: Business mission and vision
- Read `~/docs/{project-name}/ROADMAP.md` (if exists):
  - Extract: Business roadmap priorities
- Read `~/docs/{project-name}/TEAM.md`:
  - Get agent team for product discovery
- **Identify Information Gaps:**
  - List which information is missing or incomplete
  - Plan to ask questions during product discovery to fill gaps
  - Document what comes from business planning vs. what needs to be discovered

### 3. Create Product Discovery Document Structure
- Create `~/docs/{project-name}/PRODUCT-DISCOVERY.md` using template
- **Cursor**: Template location: `~/.cursor/templates/product/PRODUCT-DISCOVERY.md`
- **Claude/Gemini**: Template location: `<ai-squads>/templates/product/PRODUCT-DISCOVERY.md` (where `<ai-squads>` is your local clone path)
- Or use standard structure from PRODUCT-DISCOVERY-FRAMEWORK.md

### 4. Product Vision Definition
**Agents:** Startup advisor + **Bob (Jobs to be Done)** + **Rian (Strategic Designer)**

**Process:**
- Derive product vision from business planning documents
- **Bob's Contribution:**
  - Identify what job the product is hired to do (functional, emotional, social jobs)
  - Understand what progress users are trying to make
  - Identify competing solutions and switching forces
  - Ask: "What job is the product hired to do? What progress are users trying to make?"
- **Rian's Contribution:**
  - Review value proposition from strategic design perspective
  - Consider cognitive biases and user decision-making
  - Ask: "How does the value proposition align with user psychology?"
- Ask clarifying questions if needed:
  - "What is the product? (Derived from solution in lean canvas)"
  - "Who is it for? (Derived from customer segments)"
  - "What makes it unique? (Derived from value proposition)"
  - "What job is the product hired to do? (Jobs to be Done)"

**Output:** Populate Product Vision section of PRODUCT-DISCOVERY.md

### 5. Product-Market Fit Strategy
**Agents:** Startup advisor + **Gustavo (Financial Advisor)** + **Rian (Strategic Designer)**

**Process:**
- Identify core assumptions that must be true for product-market fit
- **Gustavo's Contribution:**
  - Validate business model assumptions (unit economics, revenue model, cost structure)
  - Assess financial viability of product-market fit assumptions
  - Ask: "Do the unit economics work? (CAC < LTV, positive margins)"
  - Ask: "Is the revenue model sustainable? What are the cost implications?"
- **Rian's Contribution:**
  - Define product-market fit metrics from growth perspective
  - Consider activation and retention metrics
  - Ask: "What metrics prove product-market fit? How do we track them?"
- Define validation plan for each assumption
- Define success metrics that prove product-market fit
- Ask questions:
  - "What assumptions must be true for product-market fit?"
  - "How do we validate these assumptions?"
  - "What metrics prove product-market fit?"
  - "What's the minimum version needed to test?"

**Output:** Populate Product-Market Fit Strategy section of PRODUCT-DISCOVERY.md

### 6. Core Product Definition
**Agents:** **Rian (Strategic Designer)** + **Bob (Jobs to be Done)** + **Steve (UI/UX)**

**Process:**
- Define core product loop (fundamental cycle users repeat)
- **Bob's Contribution:**
  - Identify jobs the product serves in the core loop
  - Understand what progress users make in each loop iteration
  - Ask: "What job does each step of the loop serve? What progress do users make?"
- **Rian's Contribution:**
  - Design engagement loops with ethical cognitive bias application
  - Consider habit formation and retention strategies
  - Ask: "How do we create ethical engagement loops? What triggers and rewards?"
- **Steve's Contribution:**
  - Review product flow for usability and clarity
  - Ensure user journey is intuitive and follows conventions
  - Ask: "Is the product flow clear? Are there usability concerns?"
- Identify key product mechanics (core features/functionality)
- Map product flow (onboarding → activation → engagement → retention)
- Define user journey (key moments and touchpoints)
- Ask questions:
  - "What's the core product loop? (What do users do repeatedly?)"
  - "What are the key product mechanics? (Core features/functionality)"
  - "How do users progress? (Onboarding → Activation → Engagement)"
  - "What's the user journey?"

**Output:** Populate Product Strategy section of PRODUCT-DISCOVERY.md

### 7. MVP Definition
**Agents:** Startup advisor + **Rian (Strategic Designer)** + **Gustavo (Financial Advisor)** + **Bob (Jobs to be Done)**

**Process:**
- Define MVP scope (minimum features needed to validate product-market fit)
- **Bob's Contribution:**
  - Ensure MVP serves core jobs users hire the product to do
  - Prioritize features that serve primary jobs
  - Ask: "Does the MVP serve the core job? What's the minimum to serve it?"
- **Rian's Contribution:**
  - Define MVP success criteria from growth and engagement perspective
  - Consider activation and retention metrics for MVP
  - Ask: "What engagement metrics prove MVP success?"
- **Gustavo's Contribution:**
  - Validate MVP financial viability (costs, revenue potential)
  - Assess unit economics for MVP
  - Ask: "Are MVP costs sustainable? Does revenue model work at MVP scale?"
- Define MVP success criteria
- Create MVP validation plan
- Plan post-MVP roadmap
- Ask questions:
  - "What's the minimum version needed to validate product-market fit?"
  - "What features are essential vs. nice-to-have?"
  - "What's the MVP success criteria?"
  - "How do we validate MVP assumptions?"

**Output:** Populate MVP Definition section of PRODUCT-DISCOVERY.md

### 8. Feature Prioritization Framework
**Agents:** **Bob (Jobs to be Done)** + **Rian (Strategic Designer)** + **Gustavo (Financial Advisor)**

**Process:**
- Identify jobs the product serves (Jobs to be Done)
- **Bob's Contribution:**
  - Define feature evaluation criteria based on jobs
  - Prioritize features that serve core jobs
  - Ask: "What jobs does the product serve? How do features serve these jobs?"
- **Rian's Contribution:**
  - Evaluate features from growth and engagement perspective
  - Consider which features drive activation, retention, and growth
  - Ask: "Which features drive product-market fit metrics? Which drive growth?"
- **Gustavo's Contribution:**
  - Evaluate features from financial perspective (cost, revenue impact)
  - Consider ROI of features
  - Ask: "What's the financial impact of each feature? What's the ROI?"
- Create feature evaluation criteria
- Define build vs. buy vs. partner decisions
- Identify what NOT to build (explicit non-goals)
- Ask questions:
  - "What jobs does the product serve?"
  - "How do we evaluate features against these jobs?"
  - "What should we NOT build? Why?"
  - "Build vs. Buy vs. Partner decisions?"

**Output:** Populate Feature Prioritization Framework section of PRODUCT-DISCOVERY.md

### 9. User Experience Strategy
**Agents:** **Rian (Strategic Designer)** + **Steve (UI/UX)** + **Bob (Jobs to be Done)**

**Process:**
- Create target user personas
- **Bob's Contribution:**
  - Identify user motivations from Jobs to be Done perspective
  - Understand functional, emotional, and social jobs
  - Ask: "What motivates users? What jobs are they hiring the product to do?"
- **Rian's Contribution:**
  - Design FTUE with ethical cognitive bias application
  - Create engagement strategy with habit formation
  - Ask: "How do we ethically activate users? What engagement loops work?"
- **Steve's Contribution:**
  - Design FTUE for clarity and usability
  - Ensure user journey is intuitive and follows conventions
  - Review personas for usability implications
  - Ask: "Is the FTUE clear? Are there usability concerns? What's the information architecture?"
- Identify user motivations (functional, emotional, social jobs)
- Design first-time user experience (FTUE)
- Define engagement strategy
- Ask questions:
  - "Who are the target users? (Personas)"
  - "What motivates users? (Jobs to be Done)"
  - "What's the first-time user experience?"
  - "How do we keep users engaged?"

**Output:** Populate User Experience Strategy section of PRODUCT-DISCOVERY.md

### 10. Go-to-Market Strategy
**Agents:** Startup advisor + **Rian (Strategic Designer)** + **Gustavo (Financial Advisor)**

**Process:**
- Define distribution channels (from lean canvas)
- **Rian's Contribution:**
  - Create acquisition strategy with growth optimization
  - Design activation and retention strategies with engagement loops
  - Consider channel-specific creative and messaging
  - Ask: "What channels are most effective? How do we optimize acquisition?"
- **Gustavo's Contribution:**
  - Validate acquisition costs and unit economics
  - Assess financial viability of channels
  - Ask: "What's the CAC for each channel? Do unit economics work?"
- Create acquisition strategy
- Create activation strategy
- Create retention strategy
- Ask questions:
  - "How do users discover the product? (Channels from lean canvas)"
  - "How do we acquire customers?"
  - "How do we activate users?"
  - "How do we retain users?"

**Output:** Populate Go-to-Market Strategy section of PRODUCT-DISCOVERY.md

### 11. Product Metrics Definition
**Agents:** **Rian (Strategic Designer)** + Startup advisor + **Gustavo (Financial Advisor)**

**Process:**
- Define North Star metric
- **Rian's Contribution:**
  - Define product-market fit metrics from growth perspective
  - Define engagement metrics (activation, retention, habit formation)
  - Define growth metrics (acquisition, viral coefficient, etc.)
  - Ask: "What's the North Star metric? What engagement metrics matter?"
- **Gustavo's Contribution:**
  - Define financial metrics (CAC, LTV, unit economics, revenue metrics)
  - Ensure metrics align with business model
  - Ask: "What financial metrics prove viability? How do we track unit economics?"
- Define product-market fit metrics
- Define engagement metrics
- Define growth metrics
- Ask questions:
  - "What's the North Star metric?"
  - "What metrics prove product-market fit?"
  - "What engagement metrics matter?"
  - "What growth metrics matter?"

**Output:** Populate Product Metrics section of PRODUCT-DISCOVERY.md

### 12. Risk Assessment
**Agents:** Startup advisor + **Gustavo (Financial Advisor)** + **Rian (Strategic Designer)** + **Bob (Jobs to be Done)**

**Process:**
- Identify product risks
- **Gustavo's Contribution:**
  - Identify financial risks (unit economics, cash flow, revenue model)
  - Assess cost structure risks
  - Ask: "What financial risks exist? What if unit economics don't work?"
- **Rian's Contribution:**
  - Identify growth and engagement risks
  - Consider market adoption risks
  - Ask: "What if users don't activate? What if retention is low?"
- **Bob's Contribution:**
  - Identify risks related to jobs not being served
  - Consider competitive risks from Jobs to be Done perspective
  - Ask: "What if the product doesn't serve the job well? What if competitors serve it better?"
- Identify market risks
- Identify technical risks
- Create mitigation plans
- Ask questions:
  - "What could go wrong with the product?"
  - "What could go wrong with the market?"
  - "What could go wrong technically?"
  - "How do we mitigate these risks?"

**Output:** Populate Risk Assessment section of PRODUCT-DISCOVERY.md

### 13. Product Roadmap
**Agents:** Startup advisor + **Rian (Strategic Designer)** + **Gustavo (Financial Advisor)** + **Bob (Jobs to be Done)**

**Process:**
- Define Phase 1: MVP (Validation focus)
- **Bob's Contribution:**
  - Ensure roadmap phases serve core jobs progressively
  - Ask: "How does each phase better serve the core jobs?"
- **Rian's Contribution:**
  - Define growth and engagement goals for each phase
  - Consider metrics and milestones
  - Ask: "What growth goals for each phase? What engagement milestones?"
- **Gustavo's Contribution:**
  - Validate financial viability of each phase
  - Consider unit economics and revenue goals
  - Ask: "What are the financial goals for each phase? When do unit economics work?"
- Define Phase 2: Product-Market Fit (Iteration focus)
- Define Phase 3: Growth (Scale focus)
- Define Phase 4: Optimization (Efficiency focus)
- Ask questions:
  - "What's Phase 1 (MVP)? What's the goal?"
  - "What's Phase 2 (Product-Market Fit)? What's the goal?"
  - "What's Phase 3 (Growth)? What's the goal?"
  - "What's Phase 4 (Optimization)? What's the goal?"

**Output:** Populate Product Roadmap section of PRODUCT-DISCOVERY.md

### 14. Collaborative Review and Refine
**Agents:** All agents review together (Startup advisor, **Bob**, **Steve**, **Rian**, **Gustavo**)

**Process:**
- **Bob's Review:**
  - Review: Does product serve core jobs? Are features prioritized by jobs?
  - Check: Is Jobs to be Done perspective integrated throughout?
  - Provide feedback on product vision, feature prioritization, user experience
- **Steve's Review:**
  - Review: Is user experience clear and usable? Are flows intuitive?
  - Check: Are there usability concerns? Is information architecture sound?
  - Provide feedback on product flow, user journey, FTUE
- **Rian's Review:**
  - Review: Are growth metrics defined? Are engagement loops ethical?
  - Check: Is product-market fit strategy sound? Are activation/retention strategies clear?
  - Provide feedback on metrics, engagement, go-to-market strategy
- **Gustavo's Review:**
  - Review: Do unit economics work? Is business model financially viable?
  - Check: Are costs and revenue streams realistic? Are financial metrics defined?
  - Provide feedback on business model, unit economics, financial viability
- **Startup Advisor's Review:**
  - Review: Is product-market fit strategy complete? Are assumptions validated?
  - Check: Is MVP definition clear? Is roadmap realistic?
  - Provide feedback on overall product strategy and alignment with business planning
- Present generated PRODUCT-DISCOVERY.md with all agent feedback
- Allow user to refine sections based on agent feedback
- Update document based on feedback
- Ensure document is evidence-based (derived from business planning)
- Ensure document is actionable (guides what to build and what not to build)
- **Iterative Refinement:** Agents can review again after updates to ensure quality

## Output

After completion, the project should have:
- `~/docs/{project-name}/PRODUCT-DISCOVERY.md` - Complete Product Discovery Document

## Integration with Other Commands

### Document Flow
```
Business Planning (/project-starter)
├── LEAN-CANVAS.md (business model)
├── PITCH-DECK.md (business pitch)
└── MISSION.md (business mission)
         ↓
Product Discovery (/discover-product)
├── PRODUCT-DISCOVERY.md (product strategy)
         ↓
Feature Planning (/plan-feature)
├── feature/{name}/PRD.md (feature requirements)
├── feature/{name}/SPECS.md (feature specs)
└── feature/{name}/prd.json (feature execution)
```

### How Feature Planning Uses Product Discovery
- Feature planning reads `PRODUCT-DISCOVERY.md` for context
- Features are evaluated against product strategy
- Features are prioritized using feature prioritization framework
- Features align with product roadmap phases

## Notes

- **Multi-Agent Collaboration:** Product discovery uses multiple agents (Bob, Steve, Rian, Gustavo) throughout the process, each providing valuable input from their specialization
- **Evidence-Based:** Product discovery should be derived from business planning documents
- **Product-Market Fit Focus:** Everything should serve product-market fit validation
- **Actionable:** PRODUCT-DISCOVERY.md should guide feature decisions
- **Living Document:** PRODUCT-DISCOVERY.md should be updated as product evolves
- **Iterative:** Product discovery evolves as product learns
- **Agent Contributions:**
  - **Bob (Jobs to be Done):** Ensures product serves core jobs, prioritizes features by jobs, understands user motivations
  - **Steve (UI/UX):** Ensures usability, clarity, and intuitive user experience
  - **Rian (Strategic Designer):** Defines growth metrics, engagement loops, activation/retention strategies
  - **Gustavo (Financial Advisor):** Validates business model, unit economics, financial viability

## Success Criteria

**For the Product Discovery Document:**
- ✅ Clear product vision derived from business strategy
- ✅ Product-market fit strategy with validation plan
- ✅ MVP definition with success criteria
- ✅ Feature prioritization framework
- ✅ Clear roadmap from MVP to product-market fit
- ✅ Evidence-based (derived from business planning)
- ✅ Actionable (guides what to build and what not to build)
