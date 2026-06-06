{-|
Module: Game.VoxelVerse.Query
Description: Shared query envelope for VoxelVerse requests.

This module is reserved for the common query-side structures used to ask for
VoxelVerse data.

Its job will be to define the shared query envelope used by the top-level API,
such as selectors, subset requests, projections, and other transport-level
query concepts.

Design notes:

- The query layer should be shared only at the envelope level.
- Ruleset-specific meaning should remain in the per-ruleset VoxelVerse modules.
- The shared query model should be strong enough to route and validate requests
  without pretending that all games expose the same semantic query language.
- Queries should support partial retrieval of information from an authoritative
  'GameState' without requiring full-state round trips.
- This module should avoid depending on 'Game.Core.Types' or concrete
  ruleset state. Query values are client-facing requests, not engine state
  payloads.
 -}
module Game.VoxelVerse.Query
    (
    )
where
