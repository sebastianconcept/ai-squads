# Evidence Sequence

Ordered steps to collect evidence to **reproduce the issue** or to **confirm/refute a hypothesis**. Follow in order; paste what you find after each step and document it in EVIDENCE.md.

**Issue / hypothesis:** [One-line description — e.g. "Reproduce 500 on /login" or "Confirm: Hypothesis 1 — DB connection pool exhausted"]

**Environment:** [local / staging / production — use TECH-STACK Environments for hostnames and SSH]

---

## Sequence

1. **[Step 1 — where, how, what to look for]**  
   - Where: [e.g. backend logs from staging]  
   - How: [exact command, e.g. `ssh deploy@staging.example.com 'docker compose logs --since 1h api'`]  
   - What to look for: [e.g. request_id, status code, error variant]  
   - Status: [ ] pending / [x] done — [paste or link to EVIDENCE.md entry]

2. **[Step 2]**  
   - Where:  
   - How:  
   - What to look for:  
   - Status: [ ] pending / [x] done

3. **[Step 3]**  
   - Where:  
   - How:  
   - What to look for:  
   - Status: [ ] pending / [x] done

---

**Outcome:** [After following the sequence: hypothesis confirmed / refuted / reproduced / still unclear]

**Next:** [If confirmed: problem definition; if refuted: next hypothesis or sequence]
