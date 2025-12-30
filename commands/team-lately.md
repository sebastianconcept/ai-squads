---
name: team-lately
alwaysApply: false
---

# Team Lately Command

This command analyzes git repository activity over a configurable time period and generates a structured report summarizing team contributions, metrics, and the qualitative impact of each author's work.

## Prerequisites

1. Current directory must be a git repository
2. Repository should have a remote configured (origin)
3. Git must be installed and accessible

**Note:** Unlike other commands, this does NOT require project adoption (no `docs/` directory needed). It works on any git repository.

## When to Use

Invoke this command when you need to:
- Understand what the team has been working on recently
- Generate a summary of team activity for stakeholders
- Review team velocity and focus areas
- Get conceptual overview of recent changes across branches
- Track individual contributions and their impact
- Identify areas of the system that have evolved

## How It Works

### 1. Period Parsing

Accept time period from user or use default:
- **Default:** 7 days
- **Accept natural language:** "2 weeks", "1 month", "6 months", "last 30 days", "3 weeks"
- Parse and calculate start date and end date for the report
- Validate that the period is reasonable (not too far in the past)

### 2. Git Operations

Execute the following git commands to gather data:

```bash
# Fetch all remote branches to get latest activity
git fetch --all

# Get project name from git remote or directory name
# Use: git remote get-url origin | sed -E 's/.*\/([^\/]+)\.git$/\1/' 
# Or fallback to: basename $(pwd)

# Get all remote branches with activity in the period
git for-each-ref --sort=-committerdate refs/remotes/origin/ --format='%(refname:short)'

# For each branch, get commits in period with detailed info
git log --since="{start_date}" --until="{end_date}" \
  --format="%H|%an|%ae|%s|%ad|%D" \
  --date=short \
  --all \
  --remotes=origin

# Get file change statistics per commit
git log --since="{start_date}" --until="{end_date}" \
  --numstat \
  --format="%H|%an" \
  --all \
  --remotes=origin
```

### 3. Data Aggregation

Collect and organize the following metrics:

**Team-Level Metrics:**
- Total commits in period
- Number of active authors
- Number of active branches
- Total lines added/removed
- Most active areas (directories/modules based on file paths)
- Date range covered

**Per-Author Metrics:**
- Commits count
- Lines added/removed (sum across all commits)
- Branches worked on (unique branch names)
- Primary areas of focus (directories with most changes)
- Email address (for identification)

**Per-Branch Analysis:**
- Branch name
- Author(s) who worked on it
- Commit count
- Date range of activity
- Files changed
- Commit messages (for context)

### 4. Conceptual Analysis

For each branch with activity, the AI should:

1. **Read commit context:**
   - Review commit messages in chronological order
   - Examine file paths changed (to understand affected areas)
   - Note patterns in changes (new features, fixes, refactors)

2. **Infer the problem being solved:**
   - What user/system pain does this address?
   - What gap or need is being filled?
   - What improvement is being made?

3. **Describe new system behavior:**
   - How does the system behave differently now?
   - What new capabilities exist?
   - What changed from a user/system perspective?

4. **Keep descriptions conceptual:**
   - Focus on "what" and "why", not "how"
   - Avoid implementation details (specific functions, classes)
   - Use domain language appropriate to the project
   - Think in terms of user/system outcomes

### 5. Report Generation

Generate a Markdown report with the following structure:

```markdown
# Team Activity Report
**Project:** {project_name}
**Period:** {start_date} to {end_date}

## Team Summary

**Metrics:**
- Total Commits: {total_commits}
- Active Authors: {author_count}
- Active Branches: {branch_count}
- Lines Changed: +{additions}/-{deletions}

**Overview:**
[Qualitative summary of team velocity, focus areas, and overall activity patterns]

## Areas of Impact

[Conceptual breakdown of which parts of the system evolved, organized by domain/area]

---

## Author: {author_name}
**Email:** {email}  
**Commits:** {count} | **Lines:** +{additions}/-{deletions} | **Branches:** {branch_count}

**Primary Focus Areas:**
- {area1} ({percentage}% of changes)
- {area2} ({percentage}% of changes)

### Branch: {branch_name}
**Problem:** [What user/system pain this addresses - conceptual description]

**New Behavior:** [How the system now behaves differently - conceptual description]

**Activity:** {commit_count} commits, {date_range}

---

[Repeat for each author]
```

### 6. Output Location

Save the report to:
```
$HOME/docs/{project_name}/team-lately/{start_date}_to_{end_date}-report.md
```

**Directory Creation:**
- Create `$HOME/docs/` if it doesn't exist
- Create `$HOME/docs/{project_name}/` if it doesn't exist
- Create `$HOME/docs/{project_name}/team-lately/` if it doesn't exist

**File Naming:**
- Format: `{start_date}_to_{end_date}-report.md`
- Date format: `YYYY-MM-DD` (e.g., `2024-01-15_to_2024-01-22-report.md`)

**Project Name Derivation:**
1. Try: `git remote get-url origin | sed -E 's/.*\/([^\/]+)\.git$/\1/'`
2. Fallback: `basename $(pwd)`

## Response Patterns

### Initial Period Prompt

```markdown
## Team Activity Analysis

I'll analyze the team's git activity and generate a report.

**Time Period:**
[If user specified: "Analyzing activity for: {period}"]
[If not specified: "Using default period: last 7 days"]

Fetching remote branches and analyzing commits...
```

### Data Collection

```markdown
**Gathering data:**
- Fetching all remote branches...
- Analyzing commits from {start_date} to {end_date}...
- Processing {N} commits across {M} branches...
- Organizing by {K} authors...
```

### Report Generation

```markdown
## Report Generated ✓

**Summary:**
- {total_commits} commits by {author_count} authors
- {branch_count} active branches
- Report saved to: `{full_path}`

[Present key insights from the report]
```

## Example Usage

```
@team-lately

@team-lately last 2 weeks

@team-lately 1 month

@team-lately analyze the last 6 months

@team-lately what has the team been working on in the past 30 days?
```

## Quality Checklist

Before finalizing the report, verify:

- [ ] All remote branches have been fetched
- [ ] Period calculation is correct (start and end dates)
- [ ] All commits in the period are included
- [ ] Metrics are accurate (commits, lines, branches)
- [ ] Each author's contributions are properly attributed
- [ ] Branch descriptions are conceptual (not implementation-focused)
- [ ] Problem statements clearly explain "why" not "how"
- [ ] New behavior descriptions focus on system/user outcomes
- [ ] Report file has been saved to correct location
- [ ] Project name is correctly derived

## Pro Tips

1. **Focus on Impact**: When describing branches, think about what changed from a user or system perspective, not code structure
2. **Group Related Work**: If an author worked on multiple related branches, note the connection
3. **Identify Patterns**: Surface if the team is focused on a particular area (e.g., "heavy focus on authentication improvements")
4. **Be Concise**: Each branch description should be 2-3 sentences max
5. **Use Domain Language**: Match the terminology used in the project (from commit messages and file paths)
6. **Handle Edge Cases**: If a branch has no clear purpose from commits, note it as "maintenance" or "refactoring"
7. **Respect Privacy**: Only use information available in git history (names, emails from commits)

## Related Commands

- `explain-system` - Understand the system architecture before analyzing changes
- `review-merge-request` - Review specific changes in detail
- `diagnose-issue` - Investigate problems that might be visible in recent commits

