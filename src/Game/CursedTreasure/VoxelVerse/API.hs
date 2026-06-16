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
import qualified Data.Map.Strict as Map (lookup)

initialVoxelVerseState :: PlayerId -> CensoredGameState -> VoxelVerseState
initialVoxelVerseState playerId gameState = undefined

initialVoxelVerseDelta :: CensoredGameState -> VoxelVerseDelta
initialVoxelVerseDelta gameState = undefined

initialInteractionState :: InteractionState
initialInteractionState = InteractionState { toolOptions = mempty}

initialProjectionState :: ProjectionState
initialProjectionState = ProjectionState

type VoxelVerseContext = Context GameState
type VoxelVersePlayerContext = Context CensoredGameState
type InnerVoxelVersePlayerSession = VoxelVersePlayerSession VoxelVersePlayerContext InteractionState ProjectionState
type InnerVoxelVerseSession = VoxelVerseSession VoxelVerseContext InnerVoxelVersePlayerSession
type SessionStateType = SessionState InteractionState ProjectionState

type SessionM a = RWST VoxelVersePlayerContext VoxelVerseDelta SessionStateType (Either Text) a

toPlayerMove :: InnerVoxelVerseSession -> ToolApplication PropertySet -> Either Text PlayerMove
toPlayerMove = undefined

lookupToolPropertySet :: InnerVoxelVersePlayerSession -> ToolApplication PropertySetHandle
    -> Either Text (ToolApplication PropertySet)
lookupToolPropertySet session tool = maybeToRight eMsg updTool 
    where   toolContext = session.playerInteractionState.toolOptions
            eMsg = "Could not find tool handle " <> show tool.appliedTool <> " in previous Tool Options."
            updTool = do
                propertySet <- Map.lookup tool.appliedTool toolContext
                pure $ tool { appliedTool = propertySet }

isGameMove :: InnerVoxelVersePlayerSession -> ToolApplication PropertySet -> Bool
isGameMove session tool = not isFingerTool || notToolSelected selectedSets
    where   toolName = propertySetLookup "toolName" tool.appliedTool
            selectedSets = lookupStateVoxels (selectionLookupFn tool.toolLayer tool.appliedSelection) session.playerModel 
            isFingerTool = toolName == TextProperty "finger"
            notToolSelected [(_, voxel)] = NullProperty == propertySetLookup "toolName" voxel
            notToolSelected _ = False -- For Cursed Treasure, user can only select on item at a time.

makeMoveNextGameState :: InnerVoxelVerseSession -> ToolApplication PropertySet -> Either Text InnerVoxelVerseSession
makeMoveNextGameState session tool = undefined

updateInteractionState :: InnerVoxelVerseSession -> ToolApplication PropertySet -> Either Text InnerVoxelVerseSession
updateInteractionState session tool = undefined

getActivePlayerSession :: InnerVoxelVerseSession -> Either Text (PlayerId, InnerVoxelVersePlayerSession)
getActivePlayerSession session = (playerId, ) <$> maybeToRight eMsg mInnerSession
    where   playerId = session.vvContext.currentState.activePlayer
            eMsg = "Could not find player " <> show playerId <> " in players."
            mInnerSession = do
                let sessions = zip session.vvPlayerSessions session.vvContext.currentState.players
                fst . fst <$> uncons (filter ((playerId ==) . (.player.playerId) . snd) sessions)

computeNextGameState :: InnerVoxelVerseSession -> ToolApplication PropertySetHandle -> Either Text InnerVoxelVerseSession
computeNextGameState session toolHandle = do
    playerSession <- snd <$> getActivePlayerSession session
    tool <- lookupToolPropertySet playerSession toolHandle
    let     updateFn | isGameMove playerSession tool = makeMoveNextGameState
                     | otherwise = updateInteractionState
    updateFn session tool

applyToolM :: ToolApplication PropertySetHandle -> SessionM ()
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
