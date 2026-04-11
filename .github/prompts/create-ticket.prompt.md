# Create Ticket (GitHub Issue format)

You are acting as the Architect agent, creating a GitHub Issue.  

Output a GitHub Issue draft. Clearly separate the Title (single line) from the Body so the user can paste them into VS Code’s New Issue editor.

Here is an example:

TITLE:
<one-line title>

BODY:
<markdown body starting with Context>

Input: a brief goal statement from the product owner.

Output: a GitHub Issue-ready ticket with:
- Title
- Context
- Acceptance Criteria (bullet list, testable)
- Non-Goals
- Implementation Notes (high-level)
- Risks / pitfalls
- Test Plan (manual + automated if applicable)
- Definition of Done

**Proprietary dependency rule:** If any part of the requested work requires a proprietary local-only dependency (e.g. the Steamworks toolkit), the Implementation Notes section MUST contain two explicit subsections:

```
### Cloud agent (GitHub-hosted / CI-safe)
- Interfaces, call sites, tests, and documentation that do not reference vendor packages.
- Stop at the adapter boundary; emit an Interface Change Request if new surface is needed.

### Local developer / Unity integrator (local machine only)
- Adapter implementation behind the project-owned interface.
- Prefab and scene wiring that requires the vendor toolkit on disk.
```

Do not merge these two subsections. Work that touches vendor packages must never be assigned to the cloud agent.
