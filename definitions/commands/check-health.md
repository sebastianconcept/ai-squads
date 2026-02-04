---
name: check-health
alwaysApply: false
---

# Check Health Command - Acceptance Criteria

This document defines the acceptance criteria for a new command that allows developers to ask any agent to run health checks that verify if the system is currently running well.

## Overview

The `check-health` command enables developers to verify system health across different environments (local, staging, production) at any point during development. Health checks are project-specific and maintained in project documentation, allowing each project to define what "healthy" means for their system.

## Goals

1. **Provide quick health verification** during intermediate work and work-in-progress states
2. **Validate system state** after executing features or bringing systems up locally/remotely
3. **Support multiple environments** (local, staging, production) based on user request
4. **Enable any agent** to execute health checks using project-defined commands
5. **Integrate with project adoption** and documentation update workflows

## Prerequisites

1. Project must have been adopted (have `~/docs/{project-name}/` directory)
2. Project must have `~/docs/{project-name}/HEALTH-CHECKS.md` file (or similar) defining health check commands
3. Agent must be available (any agent can run health checks)

## Core Functionality

### 1. Health Check Definition Location

**Acceptance Criteria:**
- [ ] Health checks are defined in `~/docs/{project-name}/HEALTH-CHECKS.md`
- [ ] File follows similar structure to `QUALITY.md` (markdown with labeled bash commands)
- [ ] Each health check has:
  - A descriptive label (e.g., "API Health", "Database Connectivity", "Service Status")
  - A bash command to execute
  - Optional: Environment-specific commands (local, staging, production)
  - Optional: Expected output or exit code documentation
- [ ] Health checks can be environment-agnostic or environment-specific
- [ ] File is created during project adoption (if not exists, template copied)
- [ ] File is maintained during `update-docs` workflow (can be updated when system changes)

**Example Structure:**
```markdown
# Health Checks

This file defines health check commands that verify the system is running correctly.

## API Health

### Local
```bash
curl -f http://localhost:8080/health || exit 1
```

### Staging
```bash
curl -f https://staging.example.com/health || exit 1
```

### Production
```bash
curl -f https://api.example.com/health || exit 1
```

## Database Connectivity

### All Environments
```bash
psql $DATABASE_URL -c "SELECT 1" || exit 1
```

## Service Status

### Local
```bash
docker ps | grep -q my-service || exit 1
```

### Staging/Production
```bash
nomad job status my-service | grep -q running || exit 1
```
```

### 2. Environment Detection and Selection

**Acceptance Criteria:**
- [ ] Command accepts optional `--env` flag: `--env local|staging|production`
- [ ] If `--env` not specified, prompt user: "Which environment should we check? (local/staging/production)"
- [ ] Default to `local` if user doesn't specify and no flag provided
- [ ] Command validates environment value (must be one of: local, staging, production)
- [ ] When environment-specific commands exist in `HEALTH-CHECKS.md`, use those; otherwise use environment-agnostic commands
- [ ] If no commands exist for selected environment, show clear error message

**Usage Examples:**
```
@check-health
@check-health --env staging
@check-health --env production
@check-health local
```

### 3. Health Check Execution

**Acceptance Criteria:**
- [ ] Read `~/docs/{project-name}/HEALTH-CHECKS.md`
- [ ] Parse health check commands for selected environment
- [ ] Execute each health check command sequentially
- [ ] All commands must pass (exit code 0) for system to be considered healthy
- [ ] If any check fails, stop execution and report which check failed
- [ ] Show output from each check (stdout and stderr)
- [ ] Provide clear success/failure summary at the end
- [ ] Commands execute in project root directory (or specified working directory)
- [ ] Support timeout for long-running checks (configurable, default: 60 seconds per check)

**Execution Flow:**
1. Load project context (`~/docs/{project-name}/`)
2. Read `HEALTH-CHECKS.md`
3. If staging or production: optionally read TECH-STACK **Environments** (or equivalent project doc) for hostnames and SSH so health checks that run on a remote host (e.g. droplet) use the correct target (e.g. `ssh user@staging.example.com '…'`).
4. Determine environment (from flag or prompt)
5. Filter/select appropriate commands for environment
6. Execute each command in sequence (local or remote per HEALTH-CHECKS.md and TECH-STACK Environments)
7. Collect results (pass/fail, output, duration)
8. Generate summary report

### 4. Integration with Project Adoption

**Acceptance Criteria:**
- [ ] When running `/adopt-project`, check if `HEALTH-CHECKS.md` exists
- [ ] If missing, create from template `templates/project/HEALTH-CHECKS.md`
- [ ] Template includes examples for common tech stacks (Rust, JavaScript, Python, etc.)
- [ ] Template includes examples for common environments (local, staging, production)
- [ ] Template includes instructions on how to customize for project needs
- [ ] Template includes examples for common health check types:
  - API endpoints
  - Database connectivity
  - Service status
  - Resource availability (disk, memory)
  - External service dependencies

### 5. Integration with Update Docs Workflow

**Acceptance Criteria:**
- [ ] `HEALTH-CHECKS.md` is included in `update-docs` workflow
- [ ] When system changes (detected via git diff), `update-docs` can suggest updates to health checks
- [ ] AI analyzes code changes and suggests relevant health check updates
- [ ] User can review and accept/reject suggested updates
- [ ] Health checks are tracked in `docs-metadata.json` like other docs

### 6. Agent Execution

**Acceptance Criteria:**
- [ ] Any agent can execute health checks (not limited to specific agents)
- [ ] Agent receives context:
  - Project name and location
  - Selected environment
  - Health check commands to execute
  - Project tech stack (from `TECH-STACK.md`); if TECH-STACK has an **Environments** section, use it for staging/production hostnames and SSH when running remote health checks
  - Project mission (from `MISSION.md`) for context
- [ ] Agent executes commands and interprets results
- [ ] Agent provides helpful output:
  - Clear indication of which checks passed/failed
  - Interpretation of failures (what might be wrong)
  - Suggestions for fixing issues
  - Next steps if checks fail

### 7. Use Cases and Scenarios

**Acceptance Criteria:**

#### Scenario 1: After Feature Execution
- [ ] User runs `/execute-feature` and feature completes
- [ ] User runs `@check-health` to verify system still works
- [ ] Health checks run against local environment
- [ ] If checks fail, agent suggests what might have broken

#### Scenario 2: Before Deployment
- [ ] User runs `@check-health --env staging` before deploying
- [ ] Health checks verify staging environment is healthy
- [ ] If checks fail, user knows not to deploy

#### Scenario 3: After System Startup
- [ ] User brings system up locally (`docker-compose up` or similar)
- [ ] User runs `@check-health` to verify all services are running
- [ ] Health checks verify each service is accessible and responding

#### Scenario 4: During Development
- [ ] User is working on a feature and wants to verify system state
- [ ] User runs `@check-health` to check if their changes broke anything
- [ ] Health checks run quickly and provide immediate feedback

#### Scenario 5: Production Health Check
- [ ] User runs `@check-health --env production` to verify production health
- [ ] Health checks use production URLs/credentials
- [ ] Results indicate if production system is healthy

### 8. Error Handling

**Acceptance Criteria:**
- [ ] If `HEALTH-CHECKS.md` doesn't exist, show helpful error:
  - "Health checks not configured. Run `/adopt-project` to set up health checks, or create `~/docs/{project-name}/HEALTH-CHECKS.md` manually."
- [ ] If no commands exist for selected environment, show clear message:
  - "No health checks defined for {environment}. Add commands to `HEALTH-CHECKS.md`."
- [ ] If command execution fails (not the check itself, but execution error):
  - Show error message
  - Continue with remaining checks if possible
  - Report which checks couldn't be executed
- [ ] If health check command times out:
  - Report timeout
  - Suggest increasing timeout or optimizing check
  - Continue with remaining checks

### 9. Output Format

**Acceptance Criteria:**
- [ ] Show clear header: "Health Check Results for {project-name} ({environment})"
- [ ] For each check:
  - Show check label
  - Show command being executed
  - Show execution status (✓ pass / ✗ fail / ⏱ timeout)
  - Show output (if any)
  - Show duration
- [ ] Show summary at end:
  - Total checks: X
  - Passed: Y
  - Failed: Z
  - Overall status: HEALTHY / UNHEALTHY
- [ ] If unhealthy, show which checks failed and suggestions for fixing

**Example Output:**
```
## Health Check Results for my-project (local)

### API Health ✓ (0.2s)
Command: curl -f http://localhost:8080/health
Output: {"status": "ok"}

### Database Connectivity ✓ (0.1s)
Command: psql $DATABASE_URL -c "SELECT 1"
Output: 1

### Service Status ✗ (5.1s)
Command: docker ps | grep -q my-service
Error: Service not found in running containers

---

**Summary:**
- Total checks: 3
- Passed: 2
- Failed: 1
- **Status: UNHEALTHY**

**Failed Checks:**
- Service Status: Service not running. Try: `docker-compose up my-service`
```

### 10. Configuration and Customization

**Acceptance Criteria:**
- [ ] Health checks are fully customizable per project
- [ ] No hardcoded checks - all checks come from `HEALTH-CHECKS.md`
- [ ] Support for complex checks (scripts, multiple commands, conditional logic)
- [ ] Support for environment variables in commands
- [ ] Support for secrets/credentials (via environment variables, not hardcoded)
- [ ] Timeout per check is configurable (default: 60 seconds)
- [ ] Can skip specific checks via `--skip` flag: `@check-health --skip "Service Status"`

### 11. Performance Considerations

**Acceptance Criteria:**
- [ ] Health checks should complete quickly (< 30 seconds total for typical project)
- [ ] Long-running checks should have appropriate timeouts
- [ ] Parallel execution of independent checks (if possible) to speed up
- [ ] Show progress during execution (which check is running)
- [ ] Don't block on slow checks - show progress and continue

### 12. Security Considerations

**Acceptance Criteria:**
- [ ] Health check commands execute in project directory with project's environment
- [ ] No arbitrary code execution - commands come from trusted `HEALTH-CHECKS.md` file
- [ ] Environment variables are respected (don't override user's environment)
- [ ] Sensitive commands (production) require explicit `--env production` flag
- [ ] Warn user before running production health checks: "Running production health checks. Continue? (y/n)"

### 13. Documentation

**Acceptance Criteria:**
- [ ] Command documentation in `definitions/commands/check-health.md` (this file)
- [ ] Template documentation in `templates/project/HEALTH-CHECKS.md`
- [ ] README.md updated with command usage
- [ ] Examples in documentation show common use cases
- [ ] Template includes comments explaining how to structure health checks

## Implementation Notes

### File Structure
- Command file: `definitions/commands/check-health.md` (this file)
- Template file: `templates/project/HEALTH-CHECKS.md` (to be created)
- Integration: Update `definitions/commands/adopt-project.md` to create `HEALTH-CHECKS.md`
- Integration: Update `definitions/commands/update-docs.md` to include `HEALTH-CHECKS.md` in updates

### Dependencies
- Project must be adopted (have docs directory)
- Agent system (any agent can execute)
- Bash for command execution
- Project's environment setup (docker, nomad, etc. as needed)

### Testing Considerations
- Test with projects that have health checks defined
- Test with projects missing health checks
- Test with different environments (local, staging, production)
- Test with failing health checks
- Test with timeouts
- Test with missing environment-specific commands

## Success Criteria

The command is considered successful when:
1. ✅ Developers can run health checks on any environment (local/staging/production)
2. ✅ Health checks are project-specific and customizable
3. ✅ Health checks integrate with project adoption workflow
4. ✅ Health checks are maintained during documentation updates
5. ✅ Any agent can execute health checks
6. ✅ Health checks provide clear, actionable feedback
7. ✅ Health checks are fast and non-intrusive
8. ✅ Health checks work during intermediate development work
9. ✅ Health checks help verify system state after feature execution
10. ✅ Health checks help verify system state when bringing systems up

## Future Enhancements (Out of Scope for MVP)

- Health check history/logging
- Health check scheduling/automation
- Health check dashboards
- Health check alerts/notifications
- Health check comparison across environments
- Health check performance metrics over time
