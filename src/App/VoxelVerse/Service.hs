{-|
Module: App.VoxelVerse.Service
Description: Top-level service handler for the VoxelVerse API.

This module is reserved for the executable-facing service entry point that will
accept VoxelVerse requests and emit VoxelVerse responses.

Its job will be to:

- decode transport requests,
- forward them into the core VoxelVerse facade,
- encode responses, and
- stay free of ruleset-specific query interpretation.

Design notes:

- This layer should be an adapter only.
- It should not decide how a given ruleset maps a VoxelVerse query to a result.
- It should support a model where clients query subsets of game information
  instead of receiving the full 'GameState'.
- The authoritative source of truth remains the underlying 'GameState'.
 -}
module App.VoxelVerse.Service
    (
    )
where
