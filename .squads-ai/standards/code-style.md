# Code Style Guide

## Context

Universal code style rules for SquadsAI projects. Language-specific conventions are handled by dedicated style guides in the `code-style/` directory.

## Available Language-Specific Style Guides

- **Ruby**: `code-style/ruby-style.md` - Ruby and Rails conventions
- **JavaScript**: `code-style/javascript-style.md` - JavaScript and frontend conventions  
- **CSS**: `code-style/css-style.md` - CSS and TailwindCSS conventions
- **HTML**: `code-style/html-style.md` - HTML structure and semantics
- **Rust**: `code-style/rust-style.md` - Rust language conventions
- **Smalltalk**: `code-style/smalltalk-style.md` - Smalltalk language conventions with Pharo headless execution patterns

<conditional-block context-check="general-formatting">
IF this General Formatting section already read in current context:
  SKIP: Re-reading this section
  NOTE: "Using General Formatting rules already in context"
ELSE:
  READ: The following formatting rules

## General Formatting

### Indentation
- Use 2 spaces for indentation (never tabs)
- Maintain consistent indentation throughout files
- Align nested structures for readability

### Naming Conventions
- Use meaningful names that inspire their purpose
- Focus on what it is first and why it exists second
- Preserve always good Separation of Concerns for flexibility
- Use temporary variables for clarity without abbreviations
- Break complex operations into smaller, readable steps
- Use meaningful variable names without abbreviations

### String Formatting
- Prefer interpolation to concatenation when available

### Code Comments
- Add brief comments when the method or function name is not easily self-evident
- Document complex algorithms or calculations
- When commenting a method, say "what" it does and what it returns omitting how is done
- Explain the "why" behind implementation choices
- Correct comments you find incorrect
- Never remove existing comments unless removing the associated code
- Update comments when modifying code to maintain accuracy
- Keep comments concise and readable

## Project Documentation Standards

### Architecture Decision Records (`decisions.md`)

All projects should maintain an Architecture Decision Records (ADR) document to track significant technical decisions.

#### ADR Format
```markdown
# Architecture Decision Records

This document records significant architectural decisions made during the project's development.

## ADR-001: [Decision Title]
**Date:** YYYY-MM-DD  
**Status:** [Accepted/Deprecated/Superseded]  
**Context:** [Problem statement and context]

**Decision:** [Clear statement of the decision made]

**Consequences:**
- ✅ [Positive consequences]
- ❌ [Negative consequences or trade-offs]

**Alternatives Considered:**
- [Alternative 1]: [Why it was rejected]
- [Alternative 2]: [Why it was rejected]

**Implementation Details:**
- [Specific implementation notes]
- [Tools, patterns, configurations]
```

#### When to Create ADRs
- **Architecture decisions** that affect multiple components
- **Technology stack** choices (databases, frameworks, tools)
- **Design pattern** implementations
- **Infrastructure** decisions (cloud vs on-premise, scaling strategies)
- **Security and compliance** approaches
- **Performance optimization** strategies

#### ADR Best Practices
- **Use consistent format** for all ADRs (date, status, context, decision, consequences)
- **Include context** and problem statement clearly
- **Document alternatives** considered with rationale for rejection
- **Note consequences** (positive and negative) with impact assessment
- **Include implementation details** when relevant (tools, patterns, configurations)
- **Update status** as decisions evolve (Accepted, Deprecated, Superseded)
- **Reference related issues** and discussions (GitHub issues, design docs, etc.)
- **Include technical constraints** and business requirements
- **Document rollback strategy** for critical decisions
- **Set review dates** for decisions that may need reconsideration

#### Benefits
- **Knowledge preservation** across team changes
- **Onboarding acceleration** for new developers
- **Audit trail** for compliance and debugging
- **Decision justification** for stakeholders
- **Pattern reuse** across similar projects
- **Risk assessment** and mitigation planning

### Changelog Standards (`changes.md`)

All projects should maintain a changelog to track version changes and updates.

#### Changelog Format
```markdown
# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]
### Added
- New feature description

### Changed
- Changed feature description

### Deprecated
- Deprecated feature description

### Removed
- Removed feature description

### Fixed
- Bug fix description

### Security
- Security fix description

## [1.2.0] - YYYY-MM-DD
### Added
- New feature description

### Changed
- Changed feature description

### Breaking Changes
- Clear description of breaking changes with migration steps
```

#### When to Create Changelog Entries
- **All software projects** regardless of technology stack
- **Library and framework** releases
- **API and service** updates
- **Infrastructure and deployment** changes
- **Security and compliance** updates

#### Changelog Best Practices
- **Include technical details** relevant to developers (API changes, database schema, etc.)
- **Reference issue numbers** when applicable (GitHub issues, JIRA tickets, etc.)
- **Note performance implications** of changes (memory usage, response times, etc.)
- **Document breaking changes** clearly with migration steps
- **Include security implications** for security-related changes
- **Note dependency updates** and version requirements
- **Reference documentation** updates when applicable
- **Include testing notes** for complex changes
</conditional-block>

<conditional-block task-condition="html-css-tailwind" context-check="html-css-style">
IF current task involves writing or updating HTML, CSS, or TailwindCSS:
  IF html-style.md AND css-style.md already in context:
    SKIP: Re-reading these files
    NOTE: "Using HTML/CSS style guides already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get HTML formatting rules from code-style/html-style.md"
        REQUEST: "Get CSS and TailwindCSS rules from code-style/css-style.md"
        PROCESS: Returned style rules
      ELSE:
        READ the following style guides (only if not already in context):
        - @~/.agent-os/standards/code-style/html-style.md (if not in context)
        - @~/.agent-os/standards/code-style/css-style.md (if not in context)
    </context_fetcher_strategy>
ELSE:
  SKIP: HTML/CSS style guides not relevant to current task
</conditional-block>

<conditional-block task-condition="javascript" context-check="javascript-style">
IF current task involves writing or updating JavaScript:
  IF javascript-style.md already in context:
    SKIP: Re-reading this file
    NOTE: "Using JavaScript style guide already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get JavaScript style rules from code-style/javascript-style.md"
        PROCESS: Returned style rules
      ELSE:
        READ: @~/.agent-os/standards/code-style/javascript-style.md
    </context_fetcher_strategy>
ELSE:
  SKIP: JavaScript style guide not relevant to current task
</conditional-block>

<conditional-block task-condition="ruby" context-check="ruby-style">
IF current task involves writing or updating Ruby or Ruby on Rails code:
  IF ruby-style.md already in context:
    SKIP: Re-reading this file
    NOTE: "Using Ruby style guide already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get Ruby style rules from code-style/ruby-style.md"
        PROCESS: Returned style rules
      ELSE:
        READ: @~/.agent-os/standards/code-style/ruby-style.md
    </context_fetcher_strategy>
ELSE:
  SKIP: Ruby style guide not relevant to current task
</conditional-block>

<conditional-block task-condition="rust" context-check="rust-style">
IF current task involves writing or updating Rust code:
  IF rust-style.md already in context:
    SKIP: Re-reading this file
    NOTE: "Using Rust style guide already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get Rust style rules from code-style/rust-style.md"
        PROCESS: Returned style rules
      ELSE:
        READ: @~/.agent-os/standards/code-style/rust-style.md
    </context_fetcher_strategy>
ELSE:
  SKIP: Rust style guide not relevant to current task
</conditional-block>

<conditional-block task-condition="smalltalk" context-check="smalltalk-style">
IF current task involves writing or updating Smalltalk or Pharo code:
  IF smalltalk-style.md already in context:
    SKIP: Re-reading this file
    NOTE: "Using Smalltalk style guide already in context"
  ELSE:
    <context_fetcher_strategy>
      IF current agent is Claude Code AND context-fetcher agent exists:
        USE: @agent:context-fetcher
        REQUEST: "Get Smalltalk and Pharo style rules from code-style/smalltalk-style.md"
        PROCESS: Returned style rules
      ELSE:
        READ: @~/.agent-os/standards/code-style/smalltalk-style.md
    </context_fetcher_strategy>
ELSE:
  SKIP: Smalltalk/Pharo style guide not relevant to current task
</conditional-block>
