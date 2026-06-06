{-|
Module: App.VoxelVerse.Protocol
Description: Request and response envelope for the VoxelVerse top-level API.

This module is reserved for the transport-facing protocol used by the future
VoxelVerse entry point.

Its job will be to define the JSON-visible request and response shapes for
clients that interact with the engine through VoxelVerse queries rather than
through direct exchange of full 'GameState' values.

Design notes:

- The protocol layer should remain transport-focused and should not contain
  ruleset-specific logic.
- The protocol should be able to carry queries for partial or sparse views of
  the underlying game data.
- The protocol should not require clients to understand or submit the full
  authoritative 'GameState'.
- Any VoxelVerse response should be derivable from the authoritative
  'GameState', without introducing an additional mutable state model.
 -}
module App.VoxelVerse.Protocol
    (
    )
where
