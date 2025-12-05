## Approach {N}: {Descriptive Name}

**Philosophy:** {One-line summary of the approach mindset}

### Summary
{2-3 sentences explaining what this approach does}

### Changes Required
- {List of specific changes: files, modules, APIs}
- {Estimated scope: lines of code, files affected}

### Pros
- {Benefits of this approach}

### Cons
- {Drawbacks, risks, or trade-offs}

### Performance Impact
- **Async Runtime:** {Does this block the executor? Need `spawn_blocking`? Starves other tasks under load?}
- **Database Cost:** {New queries? Lock contention? Transaction scope changes? Connection pool pressure?}
- **External Calls:** {New network hops? Retry storms? Timeout implications?}
- **Memory:** {Allocations? Buffering? Cache growth?}
- **CPU:** {Compute-intensive? Serialization overhead? Crypto operations?}
- **Under Load:** {Degrades gracefully? Backpressure handling? Queue depth concerns?}

### Effort Estimate
- **Time:** {Rough estimate: hours/days/weeks}
- **Complexity:** {Low/Medium/High}
- **Risk:** {Low/Medium/High}

### Technical Considerations
- {Architecture implications}
- {Dependencies affected}
- {Testing requirements}

### When to Choose This
{Specific scenarios where this approach makes sense}

