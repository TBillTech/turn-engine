# Challenge the Zodiak — Copilot Instructions

This is a turn-based digital board-game platform.
Correctness, determinism, and rule clarity are higher priority than visuals or convenience.

---

## Non-negotiables
- No hidden global state. Avoid singletons, static caches, or DontDestroyOnLoad unless explicitly approved.
- Game rules, state, and AI must be pure C# (no MonoBehaviour, no UnityEngine types).
- Unity code is an adapter layer only: input → actions, state → UI.
- No scene-object Find(), GetComponent in runtime paths; use serialized references or explicit wiring.
- Scene loading and navigation must remain explicit and deterministic.

---

## Turn-Based & Determinism Rules
- All game state changes must occur via explicit actions.
- Setup is a real phase with explicit setup actions; do not “initialize silently.”
- Action legality must depend on (phase, actor, action).
- No randomness without an explicit, seed-controlled source.
- Engine code must be replayable from an action log.

---

## Unity Lifecycle Discipline
- Awake: wiring and dependency validation only.
- Start: initial kick-off logic.
- Update: input polling or UI refresh only; no rule evaluation.
- Coroutines may wait, but must not contain game rules.

---

## Architecture Expectations
- UI must not directly mutate game state.
- Game engines/controllers should not reference Unity namespaces.
- MonoBehaviours should be thin, readable, and disposable.
- Prefer explicit composition over inheritance.

---

## Data & Configuration
- ScriptableObjects are for static configuration only (rulesets, constants).
- Do not store mutable runtime state in ScriptableObjects.
- Avoid premature data-driven abstractions.

---

## Quality Gates
- Behavior changes require either:
  - a deterministic repro sequence, or
  - a minimal test harness for the engine layer.
- Do not refactor unrelated files opportunistically.
- Optimize for clarity over cleverness.

---

## GitHub Tickets (Allowed)
- Agents may read/search existing GitHub Issues/PRs (“tickets”) for context, prior art, acceptance criteria, and to propose follow-up tickets.
- Prefer referencing existing tickets over re-specifying requirements from memory.

---

## Proprietary / Local-Only Dependencies

The Steamworks toolkit (and any other proprietary UPM package) is **local-only**: it exists on the local developer's machine and is never available to GitHub-hosted coding agents.

You can recognize you are a cloud agent if:
- There are direct references to proprietary toolkits or vendor packages that you cannot access.
- If anything inside vendor package folders seems to be missing when you try to reference it (e.g. `Packages/com.heathen.steamworks*` or similar).
Then if and only if you realize you are a cloud agent:
- Work only against project-owned interfaces and adapters (e.g. `ISteamworksProvider`, `ILobbyBackend`).
- Do not guess or invent vendor API signatures; doing so produces non-actionable PRs.

When you reach a capability gap as a cloud agent — a call you need to make that no project-owned interface yet exposes — **stop at the boundary** and emit an **Interface Change Request** in the PR body using the template below. Do not proceed past the boundary.

If you are NOT a cloud agent, and you CAN access the references and local proprietary toolkits, DO NOT avoid them. It is very unlikely you should modify them, but you definitely and routinely need to understand them and integrate with them.

### Interface Change Request template

~~~markdown
## Interface Change Request
**Reason:** <one sentence: why this new surface is needed>

### Proposed interface additions
```csharp
// Signatures only — no implementation
```

### Expected lifecycle / ownership rules
- Who creates / disposes the object?
- Thread-safety requirements?

### Error handling expectations
- What exceptions or error codes should be surfaced?
- Retry / fallback policy?

### Cloud agent completed
- <list of files / call sites already wired>

### Remaining for local developer / Unity integrator
- Implement the adapter behind the new interface
- Wire prefabs / scenes as needed
~~~