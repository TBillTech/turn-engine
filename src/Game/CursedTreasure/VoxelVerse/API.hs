{-|
Module: Game.CursedTreasure.VoxelVerse.API
Description: CursedTreasure-specific VoxelVerse projection and query handling.

This module is reserved for the CursedTreasure mapping between authoritative
'GameState' values and VoxelVerse-facing query responses.

Its job will be to:

- project CursedTreasure state into VoxelVerse-oriented structures,
- interpret shared VoxelVerse query envelopes in CursedTreasure-specific terms,
- enforce CursedTreasure visibility and redaction rules for query responses,
  and
- return sparse or partial VoxelVerse views derived from authoritative state.

Design notes:

- This module should own the meaning of VoxelVerse queries for CursedTreasure.
- It should not require the client to receive or submit the full authoritative
  state.
- It should avoid storing any additional VoxelVerse runtime state that cannot
  be regenerated or looked up from the current CursedTreasure 'GameState'.
- The authoritative source of truth remains the CursedTreasure game state.
- This module is expected to depend on both the CursedTreasure game-state
  types and the shared VoxelVerse query/response types.
- It should interpret shared VoxelVerse queries in CursedTreasure-specific
  terms rather than pushing CursedTreasure semantics back into the shared
  VoxelVerse modules.
 -}
module Game.CursedTreasure.VoxelVerse.API
    ( initialVoxelVerseState
    , initialVoxelVerseDelta
    , initialInteractionState
    , initialProjectionState
    , VoxelVerseContext
    , VoxelVersePlayerContext
    , InnerVoxelVersePlayerSession
    , InnerVoxelVerseSession
    , SessionM
    , computeNextGameState
    , applyToolM
    , createVoxelVerseView
    , GameState
    , CensoredGameState
    )
where

import Game.CursedTreasure.Types
import Game.CursedTreasure.VoxelVerse.Types
import Game.VoxelVerse.Types
import Game.Core.Primitives
import Control.Monad.Trans.RWS.Strict (RWST, runRWST)

initialVoxelVerseState :: PlayerId -> CensoredGameState -> VoxelVerseState
initialVoxelVerseState playerId gameState = undefined

initialVoxelVerseDelta :: CensoredGameState -> VoxelVerseDelta
initialVoxelVerseDelta gameState = undefined

initialInteractionState :: InteractionState
initialInteractionState = InteractionState

initialProjectionState :: ProjectionState
initialProjectionState = ProjectionState

type VoxelVerseContext = Context GameState
type VoxelVersePlayerContext = Context CensoredGameState
type InnerVoxelVersePlayerSession = VoxelVersePlayerSession VoxelVersePlayerContext InteractionState ProjectionState
type InnerVoxelVerseSession = VoxelVerseSession VoxelVerseContext InnerVoxelVersePlayerSession
type SessionStateType = SessionState InteractionState ProjectionState

type SessionM a = RWST VoxelVersePlayerContext VoxelVerseDelta SessionStateType (Either Text) a

computeNextGameState :: InnerVoxelVerseSession -> ToolApplication -> Either Text InnerVoxelVerseSession
computeNextGameState session tool = undefined

applyToolM :: ToolApplication -> SessionM ()
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
