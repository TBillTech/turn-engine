{-|
Module: Game.VoxelVerse.API
Description: Core facade for VoxelVerse generation and query dispatch.

This module is reserved for the core facade that will sit between the
transport-level VoxelVerse API and the ruleset-specific VoxelVerse adapters.

Its job will be to:

- accept authoritative 'GameState' values,
- dispatch by ruleset,
- ask the matching ruleset module to interpret VoxelVerse queries, and
- return shared VoxelVerse response structures.

Design notes:

- This should mirror the role that 'Game.Core.API' plays for the current
  ruleset-agnostic game API surface.
- This module should own dispatch, not game-specific interpretation.
- Each ruleset may have a distinct mapping from query envelope to sparse
  VoxelVerse response.
- The response must remain derivable from the supplied 'GameState', with no
  extra hidden runtime state stored in the VoxelVerse layer.
- This is the shared bridge layer that may depend on 'Game.Core.Types'.
- It should inspect the tagged 'Core.GameState' and delegate to the matching
  ruleset-specific VoxelVerse module.
- It should know about both the authoritative game-state side and the shared
  VoxelVerse query/response side, while leaving ruleset-specific query meaning
  to the delegated modules.
 -}
module Game.VoxelVerse.API
    (
    )
where

import qualified Game.Core.Types as Core
import qualified Game.Core.API as CoreAPI
import qualified Game.CursedTreasure.VoxelVerse.API as CursedTreasure
import qualified Game.FogOfBattle.VoxelVerse.API as FogOfBattle
import qualified Game.ArtOfWar.VoxelVerse.API as ArtOfWar
import qualified Game.RealEstate.VoxelVerse.API as RealEstate
import Game.VoxelVerse.Types
import Control.Monad.Trans.RWS.Strict (RWST, runRWST)


-- This API needs to simply pass through the concept of getGameSetupPlayers
getGameSetupPlayers :: [(Text, [Core.PlayerDescription])]
getGameSetupPlayers = CoreAPI.getGameSetupPlayers

-- But the createNewGame talks the VoxelVerse language (and also tracks the underlying GameState).
-- From the standpoint of downstream logic, the GameState is opaque, but needs to be tracked.
createNewGame :: [Core.PlayerDescription] -> Int
    -> Either Text (VoxelVerseSession, VoxelVerseView)
createNewGame players randomSeed = do
  createdGame <- CoreAPI.createNewGame players randomSeed
  session <- initializeSession createdGame
  pure (session, createVoxelVerseUpdate session)
  where
    initializeSession :: (Core.GameState, [Core.CensoredGameState]) -> Either Text VoxelVerseSession
    initializeSession (gameState, censoredStates) = do
      censoredState <- maybeToRight "VoxelVerse.createNewGame requires at least one player view" (viaNonEmpty head censoredStates)
      pure VoxelVerseSession
        { vvContext = VoxelVerseContext
          { previousCommittedState = Nothing
          , currentState = (gameState, censoredState)
          , planningState = Nothing
          }
        , vvModel = initialVoxelVerseState censoredState
        , vvModelDelta = initialVoxelVerseDelta censoredState
        , vvInteractionState = initialInteractionState censoredState
        , vvProjectionState = initialProjectionState censoredState
        }

-- enumerateActivePlayerOptions is now _entailed_ in the enabled property of the Voxels,
-- and so does _not_ appear on this API.

-- heuristicHint is now also _entailed_ in the enabled property of the Voxels by making this
-- architectural choice: we create a metaHeuristicTool ("hint" Tool), which is a meta tool 
-- that can be used to apply to another tool (except the metaHeuristicTool probably), which then 
-- creates a specialized heuristicTool, which adds a "heuristicScore" property to enabled options
-- for the underlying tool. The downstream logic simply needs to use this workflow with the
-- metaHeuristicTool to extract the AI bot "best" tool actions.

-- Summary can also be simple passthrough:
summary :: Core.GameState -> Text
summary = CoreAPI.summary

initialVoxelVerseState :: Core.CensoredGameState -> VoxelVerseState
initialVoxelVerseState gameState = undefined

initialVoxelVerseDelta :: Core.CensoredGameState -> VoxelVerseDelta
initialVoxelVerseDelta gameState = undefined

initialInteractionState :: Core.CensoredGameState -> VoxelVerseInteractionState
initialInteractionState gameState =
  case gameState of
    Core.CursedTreasureCensoredGameState _ ->
      CursedTreasureVoxelVerseInteractionState CursedTreasure.initialInteractionState
    Core.FogOfBattleCensoredGameState _ ->
      FogOfBattleVoxelVerseInteractionState FogOfBattle.initialInteractionState
    Core.ArtOfWarCensoredGameState _ ->
      ArtOfWarVoxelVerseInteractionState ArtOfWar.initialInteractionState
    Core.RealEstateCensoredGameState _ ->
      RealEstateVoxelVerseInteractionState RealEstate.initialInteractionState

initialProjectionState :: Core.CensoredGameState -> VoxelVerseProjectionState
initialProjectionState gameState =
  case gameState of
    Core.CursedTreasureCensoredGameState _ ->
      CursedTreasureVoxelVerseProjectionState CursedTreasure.initialProjectionState
    Core.FogOfBattleCensoredGameState _ ->
      FogOfBattleVoxelVerseProjectionState FogOfBattle.initialProjectionState
    Core.ArtOfWarCensoredGameState _ ->
      ArtOfWarVoxelVerseProjectionState ArtOfWar.initialProjectionState
    Core.RealEstateCensoredGameState _ ->
      RealEstateVoxelVerseProjectionState RealEstate.initialProjectionState

-- Generally speaking, the unrestricted Core.GameState needs to be preserved and kept for future
-- calls to the underlying API, and for correct GameState computations. It may also be necessary to
-- revert to a previously committed state, so we need a copy of a previous state for that purpose.
-- Otherwise, the logic at the VoxelVerse level should be consistently using a CensoredGameState. 
data VoxelVerseContext = VoxelVerseContext
    { previousCommittedState :: Maybe (Core.GameState, Core.CensoredGameState)
    , currentState :: (Core.GameState, Core.CensoredGameState)
    , planningState :: Maybe Core.CensoredGameState
    }

data VoxelVerseSession = VoxelVerseSession
    { vvContext :: VoxelVerseContext
    , vvModel :: VoxelVerseState
    , vvModelDelta :: VoxelVerseDelta
    , vvInteractionState :: VoxelVerseInteractionState
    , vvProjectionState :: VoxelVerseProjectionState
    }

data SessionState = SessionState
    {
      voxelVerseState :: VoxelVerseState
    , voxelVerseInteractionState :: VoxelVerseInteractionState
    , voxelVerseProjectionState :: VoxelVerseProjectionState
    }

type SessionM a = RWST VoxelVerseContext VoxelVerseDelta SessionState (Either Text) a

-- applyTool is the more generic and high level mutation of the GameState which _entails_ makeMove 
-- AND enumerateActivePlayerOptions AND hueristicHint. It even entails ViewPorts, since a ViewPort
-- turns out to be simply another kind of Tool Application.
applyTool :: VoxelVerseSession -> ToolApplication
  -> Either Text (VoxelVerseSession, VoxelVerseView)
applyTool session toolApplication =
    case runRWST (applyToolM toolApplication) session.vvContext sessionState of
        Left err -> Left err
        Right (newContext, nextState, deltas) ->
            Right $ createView
                ( VoxelVerseSession
                    { vvContext = newContext
                    , vvModel = nextState.voxelVerseState
                    , vvModelDelta = deltas
                    , vvInteractionState = nextState.voxelVerseInteractionState
                    , vvProjectionState = nextState.voxelVerseProjectionState
                    }
                )
  where
    sessionState = SessionState
      { voxelVerseState = session.vvModel
      , voxelVerseInteractionState = session.vvInteractionState
      , voxelVerseProjectionState = session.vvProjectionState
      }
    createView sess = (sess, createVoxelVerseUpdate sess)

createVoxelVerseUpdate :: VoxelVerseSession -> VoxelVerseView
createVoxelVerseUpdate sess = undefined

applyToolM :: ToolApplication -> SessionM VoxelVerseContext
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

