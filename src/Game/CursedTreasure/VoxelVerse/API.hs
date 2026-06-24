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
import Data.Aeson (ToArgs)

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

pushButtonDecoder :: DecodedTool -> PlayerMove
pushButtonDecoder dTool = undefined

pushButtonEncoder :: InnerVoxelVersePlayerSession -> VoxelSelection
pushButtonEncoder session = undefined
    where   moves = session.playerInteractionState.enumeratedPlayerMoves


jeepDecoder :: DecodedTool -> PlayerMove
jeepDecoder dTool = undefined

jeepEncoder :: InnerVoxelVersePlayerSession -> VoxelSelection
jeepEncoder = undefined

clueDecoder :: DecodedTool -> PlayerMove
clueDecoder dTool = undefined

clueEncoder :: InnerVoxelVersePlayerSession -> VoxelSelection
clueEncoder = undefined

removeMarkerDecoder :: DecodedTool -> PlayerMove
removeMarkerDecoder dTool = undefined

removeMarkerEncoder :: InnerVoxelVersePlayerSession -> VoxelSelection
removeMarkerEncoder = undefined

pushButtonLabel :: VoxelPropertyValue
pushButtonLabel = TextProperty "pushButton"

moveJeepLabel :: VoxelPropertyValue
moveJeepLabel = TextProperty "moveJeep"

playClueLabel :: VoxelPropertyValue
playClueLabel = TextProperty "playClue"

removeMarkerLabel :: VoxelPropertyValue
removeMarkerLabel = TextProperty "removeMarker"

moveDecoders :: Map VoxelPropertyValue (DecodedTool -> PlayerMove)
moveDecoders = fromList
    [ (pushButtonLabel, pushButtonDecoder)
    , (moveJeepLabel, jeepDecoder)
    , (playClueLabel, clueDecoder)
    , (removeMarkerLabel, removeMarkerDecoder)
    ]

socketEncoders :: Map VoxelPropertyValue (InnerVoxelVersePlayerSession -> VoxelSelection)
socketEncoders = fromList
    [ (pushButtonLabel, pushButtonEncoder)
    , (moveJeepLabel, jeepEncoder)
    , (playClueLabel, clueEncoder)
    , (removeMarkerLabel, removeMarkerEncoder)
    ]

toPlayerMove :: ToolApplication -> InnerVoxelVersePlayerSession -> Either Text PlayerMove
toPlayerMove tool session = toEitherMove mPlayerMove
    where   mTool = decodeTool tool session
            errorMsg = "Could not find toolName " <> show ((.toolName) <$> mTool) <> " in moveDecoders."
            mPlayerMove = do
                t <- mTool
                d <- Map.lookup t.toolName moveDecoders
                pure $ d t
            toEitherMove (Just (PlayerMoveError e)) = Left e
            toEitherMove (Just m) = Right m
            toEitherMove _ = Left errorMsg

decodeTool :: ToolApplication -> InnerVoxelVersePlayerSession -> Maybe DecodedTool
decodeTool tool session = decodedTool appliedToolList
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

isGameMove :: ToolApplication -> InnerVoxelVersePlayerSession -> Bool
isGameMove tool session = not isMetaAction
    where   mTool = decodeTool tool session
            isCancelSelected (_, voxel) = TrueProperty == propertySetLookup "cancel" voxel
            isCancel = any isCancelSelected $ maybe [] (.selection) mTool
            selectionError = maybe True ((1 /=) . length . (.selection)) mTool
            hasSelectedTool = isJust $ onlySelectedTool mTool
            isMetaAction = isCancel || hasSelectedTool || selectionError

makeMoveNextGameState :: ToolApplication -> InnerVoxelVerseSession -> Either Text InnerVoxelVerseSession
makeMoveNextGameState tool session = do
    playerSession <- getActivePlayerSession session
    move <- toPlayerMove tool (snd playerSession)
    let gameState = session.vvContext.currentState
        (newGameState, censoredGameStates) = makeMove gameState move
        playerOptions playerId | playerId == gameState.activePlayer = enumerateActivePlayerOptions newGameState
                               | otherwise = []
        playerIds = map (.player.playerId) newGameState.players
        playerSessions = getZipList $ updGameState
            <$> ZipList session.vvPlayerSessions
            <*> ZipList censoredGameStates
            <*> ZipList playerIds
        newSession = session { vvContext = session.vvContext { previousCommittedState = Just session.vvContext.currentState
                                                             , currentState = newGameState }
                             , vvPlayerSessions = playerSessions }
        updGameState sess gS playerId = sess { playerContext = sess.playerContext { previousCommittedState = Just sess.playerContext.currentState
                                                                                  , currentState = gS }
                                             , playerInteractionState = sess.playerInteractionState
                                                { enumeratedPlayerMoves = playerOptions playerId } }
    pure newSession

toolLocation :: VoxelPropertyValue -> CubeCoordinate Int
toolLocation = undefined

pushButtonChoiceTool :: InnerVoxelVersePlayerSession -> ToolApplication
pushButtonChoiceTool session = ToolApplication 
    { toolLayer = 0
    , appliedTool = (toolLocation pushButtonLabel, NoVoxelSpecifier)
    , appliedSelection = pushButtonEncoder session
    }

noChoiceTool :: ToolApplication
noChoiceTool = ToolApplication
    { toolLayer = 0
    , appliedTool = (toolLocation pushButtonLabel, NoVoxelSpecifier)
    , appliedSelection = SparseSelection 0 mempty
    }

selectedChoiceTool :: Maybe (CubeCoordinate Int, Voxel) -> InnerVoxelVersePlayerSession -> ToolApplication
selectedChoiceTool Nothing session = pushButtonChoiceTool session
selectedChoiceTool (Just (_, selected)) session = ToolApplication 
    { toolLayer = 0
    , appliedTool = (toolLocation selectedName, NoVoxelSpecifier)
    , appliedSelection = encodeSelection session }
    where   selectedName = propertySetLookup "toolName" selected
            noSelection = SparseSelection 0 mempty
            encodeSelection = fromMaybe (const noSelection) $ Map.lookup selectedName socketEncoders

nextChoiceTool :: ToolApplication -> InnerVoxelVersePlayerSession -> ToolApplication
nextChoiceTool previous session 
    | notToolPicked = pushButtonChoiceTool session
    | otherwise = selectedChoiceTool selectedTool session
    where   mTool = decodeTool previous session
            isCancelSelected (_, voxel) = TrueProperty == propertySetLookup "cancel" voxel
            isCancel = any isCancelSelected $ maybe [] (.selection) mTool
            selectionError = maybe True ((1 /=) . length . (.selection)) mTool
            selectedTool = onlySelectedTool mTool
            notToolPicked = isCancel || isNothing selectedTool || selectionError

updatePlayerInteractionState :: ToolApplication -> PlayerId -> InnerVoxelVersePlayerSession
    -> Either Text InnerVoxelVersePlayerSession
updatePlayerInteractionState tool playerId session
    = Right $ session { playerInteractionState = newInteractionState }
    where   newInteractionState = session.playerInteractionState { currentToolChoices = newChoices }
            gameState = toGameState session.playerContext.currentState
            newChoices  | not isActive = noChoiceTool
                        | isGameMove tool session = pushButtonChoiceTool session
                        | otherwise = nextChoiceTool tool session
            isActive = gameState.activePlayer == playerId

updateInteractionStates :: ToolApplication -> InnerVoxelVerseSession -> Either Text InnerVoxelVerseSession
updateInteractionStates tool session = do
    let playerSessions = session.vvPlayerSessions
        playerIds = map (.player.playerId) session.vvContext.currentState.players
    updatedSessions <- zipWithM (updatePlayerInteractionState tool) playerIds playerSessions
    pure $ session { vvPlayerSessions = updatedSessions} 

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
    let     updated | isGameMove tool playerSession = makeMoveNextGameState tool session
                    | otherwise = Right session
    updated

applyToolM :: ToolApplication -> SessionM ()
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
