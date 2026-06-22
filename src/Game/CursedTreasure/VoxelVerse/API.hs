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
import Game.CursedTreasure.API
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
initialInteractionState = InteractionState { }

initialProjectionState :: ProjectionState
initialProjectionState = ProjectionState

type VoxelVerseContext = Context GameState
type VoxelVersePlayerContext = Context CensoredGameState
type InnerVoxelVersePlayerSession = VoxelVersePlayerSession VoxelVersePlayerContext InteractionState ProjectionState
type InnerVoxelVerseSession = VoxelVerseSession VoxelVerseContext InnerVoxelVersePlayerSession
type SessionStateType = SessionState InteractionState ProjectionState

type SessionM a = RWST VoxelVersePlayerContext VoxelVerseDelta SessionStateType (Either Text) a

data DecodedTool = DecodedTool
    { toolName :: VoxelPropertyValue
    , tool :: Voxel
    , location :: CubeCoordinate Int
    , selection :: [(CubeCoordinate Int, Voxel)]
}

fingerDecoder :: DecodedTool -> PlayerMove
fingerDecoder dTool = undefined

jeepDecoder :: DecodedTool -> PlayerMove
jeepDecoder dTool = undefined

clueDecoder :: DecodedTool -> PlayerMove
clueDecoder dTool = undefined

amuletDecoder :: DecodedTool -> PlayerMove
amuletDecoder dTool = undefined

removeMarkerDecoder :: DecodedTool -> PlayerMove
removeMarkerDecoder dTool = undefined

moveDecoders :: Map VoxelPropertyValue (DecodedTool -> PlayerMove)
moveDecoders = fromList
    [ (TextProperty "finger", fingerDecoder)
    , (TextProperty "jeep", jeepDecoder)
    , (TextProperty "clue", clueDecoder)
    , (TextProperty "amulet", amuletDecoder)
    , (TextProperty "removeMarker", removeMarkerDecoder)
    ]

toPlayerMove :: InnerVoxelVersePlayerSession -> ToolApplication -> Either Text PlayerMove
toPlayerMove session tool = toEitherMove mPlayerMove
    where   mTool = decodeTool session tool
            errorMsg = "Could not find toolName " <> show ((.toolName) <$> mTool) <> " in moveDecoders."
            mPlayerMove = do
                t <- mTool
                d <- Map.lookup t.toolName moveDecoders
                pure $ d t
            toEitherMove (Just (PlayerMoveError e)) = Left e
            toEitherMove (Just m) = Right m
            toEitherMove _ = Left errorMsg

decodeTool :: InnerVoxelVersePlayerSession -> ToolApplication -> Maybe DecodedTool
decodeTool session tool = decodedTool appliedToolList
    where   decodedTool [(loc, v)] = Just $ DecodedTool
                { toolName = propertySetLookup "toolName" v
                , tool = v
                , location = loc
                , selection = selectedSets }
            decodedTool _ = Nothing
            appliedToolAsSelection :: VoxelSelection
            appliedToolAsSelection = SparseSelection tool.toolLayer $ one tool.appliedTool
            appliedToolList = lookupStateVoxels (selectionLookupFn appliedToolAsSelection)
                session.playerModel
            selectedSets = lookupStateVoxels (selectionLookupFn tool.appliedSelection)
                session.playerModel

onlySelectedTool :: Maybe DecodedTool -> Maybe (CubeCoordinate Int, Voxel)
onlySelectedTool mTool = do
                t <- mTool
                let tools = filter ((NullProperty /=) . propertySetLookup "toolName" . snd) t.selection
                (f, rest) <- uncons tools
                if null rest then Just f else Nothing

isGameMove :: InnerVoxelVersePlayerSession -> ToolApplication -> Bool
isGameMove session tool = not isMetaAction
    where   mTool = decodeTool session tool
            isCancelSelected (_, voxel) = TrueProperty == propertySetLookup "cancel" voxel
            isCancel = any isCancelSelected $ maybe [] (.selection) mTool
            selectionError = maybe True ((1 /=) . length . (.selection)) mTool
            hasSelectedTool = isJust $ onlySelectedTool mTool
            isMetaAction = isCancel || hasSelectedTool || selectionError

makeMoveNextGameState :: InnerVoxelVerseSession -> ToolApplication -> Either Text InnerVoxelVerseSession
makeMoveNextGameState session tool = do
    playerSession <- getActivePlayerSession session
    move <- toPlayerMove (snd playerSession) tool
    let gameState = session.vvContext.currentState
        (newGameState, censoredGameStates) = makeMove gameState move
        playerOptions = enumerateActivePlayerOptions newGameState
        playerIds = map (.player.playerId) newGameState.players
        playerSessions = getZipList $ updGameState
            <$> ZipList session.vvPlayerSessions
            <*> ZipList censoredGameStates
            <*> ZipList playerIds
        newSession = session { vvContext = session.vvContext { previousCommittedState = Just session.vvContext.currentState
                                                             , currentState = newGameState }
                             , vvPlayerSessions = playerSessions }
        updGameState sess gS playerId = sess { playerContext = sess.playerContext { previousCommittedState = Just sess.playerContext.currentState
                                                                                  , currentState = gS } }
    pure newSession

fingerSockets :: InnerVoxelVersePlayerSession -> ToolApplication
fingerSockets = undefined

updatePlayerInteractionState :: PlayerId -> InnerVoxelVersePlayerSession -> ToolApplication
    -> Either Text InnerVoxelVersePlayerSession
updatePlayerInteractionState playerId session tool
    = Right $ session { playerInteractionState = newInteractionState }
    where   newInteractionState = session.playerInteractionState { currentToolChoices = newChoices }
            newChoices = fingerSockets session
            gameState = toGameState session.playerContext.currentState
            newSockets  | not isActive = []
                        | isGameMove session tool = []
                        | otherwise = sockets
            isActive = gameState.activePlayer == playerId
            sockets = []

updateInteractionState :: InnerVoxelVerseSession -> ToolApplication -> Either Text InnerVoxelVerseSession
updateInteractionState session tool = undefined

getActivePlayerSession :: InnerVoxelVerseSession -> Either Text (PlayerId, InnerVoxelVersePlayerSession)
getActivePlayerSession session = (playerId, ) <$> maybeToRight eMsg mInnerSession
    where   playerId = session.vvContext.currentState.activePlayer
            eMsg = "Could not find player " <> show playerId <> " in players."
            mInnerSession = do
                let sessions = zip session.vvPlayerSessions session.vvContext.currentState.players
                fst . fst <$> uncons (filter ((playerId ==) . (.player.playerId) . snd) sessions)

computeNextGameState :: InnerVoxelVerseSession -> ToolApplication -> Either Text InnerVoxelVerseSession
computeNextGameState session tool = do
    playerSession <- snd <$> getActivePlayerSession session
    let     updateFn | isGameMove playerSession tool = makeMoveNextGameState session tool
                     | otherwise = Right session
    updateFn

applyToolM :: ToolApplication -> SessionM ()
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
