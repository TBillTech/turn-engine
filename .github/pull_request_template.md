<!-- Keep PRs small, focused, and tied to a single Issue. -->
## Summary
Brief description of what this PR changes and why (tie directly to the Issue context).

## Related Issue
- Fixes #<issue-number>  
  or  
- Refs #<issue-number> (partial / groundwork only)

<!--
Replace <issue-number> above (do not leave a placeholder).
If this PR is truly not tied to an issue (rare), write "N/A" and explain why.
-->

(One primary issue per PR unless explicitly documented.)

## What Changed
- Bullet list of concrete changes (no speculation, no future work)

## How to Test
- [ ] Open Unity project
- [ ] Load scene: `<scene-name>`
- [ ] Perform steps:
      1. …
      2. …
- [ ] Expected result:
      - …
- [ ] No new console errors or warnings

## Architecture Notes
- New dependencies introduced? (Yes/No — list if yes)
- Any rule or state logic added outside explicit turn/state flow? (Yes/No)
- Any lifecycle usage changes? (Awake / Start / Update / Coroutines)
- Any serialization or ScriptableObject changes? (Describe or “None”)

## Non-Goals
Explicitly list what this PR does NOT attempt to address.

## Risks / Follow-ups
- Known risks, edge cases, or deferred work
- Follow-up issues (if any): #<issue-number>

## Performance / GC Notes
- Any per-frame allocations introduced? (Yes/No)
- Any new Update()-path logic? (Yes/No)
- If yes to either, explain briefly.

## Checklist
- [ ] Changes match Issue acceptance criteria
- [ ] Non-goals respected (no scope creep)
- [ ] No unrelated refactors
- [ ] No rule logic in `Update()`
- [ ] No new runtime `Find*` lookups
- [ ] Scene/prefab changes documented (or none)
- [ ] Unity play-mode tested

## Proprietary / Local-Only Dependencies
- [ ] This PR does **not** require a proprietary local-only dependency (e.g. Steamworks toolkit) to build or validate — **expected answer: checked**
- [ ] This PR does **not** touch any vendor or toolkit package folder — **expected answer: checked**

> If either box above is unchecked, describe what is blocked and include an **Interface Change Request** (see `.github/copilot-instructions.md`) in the PR body so a local developer can complete the adapter work.
