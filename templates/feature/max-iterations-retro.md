# Max Iterations Retrospective

**Feature**: {FEATURE_NAME}  
**Max Iterations Reached**: {MAX_ITERATIONS}  
**Date**: {TIMESTAMP}

## Summary

This feature reached the maximum iteration limit ({MAX_ITERATIONS} iterations) without completing all stories. This retrospective documents what happened, what was learned, and recommendations for moving forward.

## Execution Statistics

- **Total Stories**: {TOTAL_STORIES}
- **Completed Stories**: {COMPLETED_STORIES}
- **Incomplete Stories**: {TOTAL_STORIES} - {COMPLETED_STORIES}
- **Iterations Used**: {MAX_ITERATIONS}

## What Happened

[Document the execution journey - what stories were completed, what patterns emerged, what blockers were encountered]

## Root Cause Analysis

### Why Did We Hit the Limit?

[Analyze why the feature didn't complete within the iteration limit. Consider:]
- Were stories too large or complex?
- Were there recurring quality check failures?
- Were dependencies blocking progress?
- Was the approach fundamentally flawed?
- Were there external blockers (missing dependencies, unclear requirements)?

### Patterns Observed

[Document patterns from introspection across all attempts:]
- What approaches were tried repeatedly?
- What errors or failures recurred?
- What worked consistently?
- What never worked?

## Key Learnings

### Technical Learnings

[What did we learn about:]
- The codebase or architecture
- Dependencies or integrations
- Quality checks or testing
- Performance or scalability

### Process Learnings

[What did we learn about:]
- Story sizing or decomposition
- Dependency management
- Quality gate configuration
- Execution approach

## New Open Questions

During execution, new questions emerged that weren't addressed in the original OPEN-QUESTIONS.md. These questions need answers before proceeding:

### Technical Questions

[Questions about:]
- Architecture or design decisions that need clarification
- Dependencies or integrations that need investigation
- Quality checks or testing approaches that need validation
- Performance or scalability concerns that need analysis

### Process Questions

[Questions about:]
- Story decomposition or sizing strategies
- Dependency management approaches
- Quality gate configuration
- Execution methodology

### Product/Feature Questions

[Questions about:]
- Requirements that need clarification
- User needs that need validation
- Feature scope that needs re-evaluation
- Success criteria that need refinement

### Next Steps for Questions

1. [ ] Review original OPEN-QUESTIONS.md to see which questions were resolved
2. [ ] Add new questions to OPEN-QUESTIONS.md for future planning
3. [ ] Prioritize questions by impact on feature completion
4. [ ] Determine which questions need answers before continuing vs. can be answered during execution

## Recommendations

### Immediate Actions

1. [ ] Review incomplete stories and determine if they're still needed
2. [ ] Break down remaining stories into smaller, more focused tasks
3. [ ] Review and adjust quality checks if they're too strict
4. [ ] Document blockers and dependencies that need resolution
5. [ ] Consider manual intervention for stories that are close to completion

### Process Improvements

[Recommendations for improving the execution process:]
- Story decomposition strategies
- Quality check adjustments
- Dependency management improvements
- Approach changes for similar features

### Technical Improvements

[Recommendations for technical changes:]
- Architecture adjustments
- Dependency updates
- Test improvements
- Code quality improvements

## Next Steps

[What should happen next:]
- Should this feature be paused and revisited later?
- Should stories be re-scoped or re-planned?
- Should manual intervention be used to complete remaining stories?
- Should the feature be cancelled or significantly changed?

## Evidence

[Reference specific evidence that supports the analysis:]
- Quality check logs
- Error messages
- Test results
- Code review findings
- Introspection from insights.json

## Related Documentation

- Feature PRD: `~/docs/{project-name}/feature/{FEATURE_NAME}/PRD.md`
- Feature Specs: `~/docs/{project-name}/feature/{FEATURE_NAME}/SPECS.md`
- Open Questions: `~/docs/{project-name}/feature/{FEATURE_NAME}/OPEN-QUESTIONS.md`
- Execution Insights: `~/docs/{project-name}/notes/{FEATURE_NAME}/insights.json`
- Dev Notes: `~/docs/{project-name}/notes/{FEATURE_NAME}/`
