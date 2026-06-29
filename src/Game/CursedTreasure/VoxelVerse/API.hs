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
import qualified Data.Map.Strict as Map (lookup, fromList)
import Data.Aeson (ToArgs)
import System.Random.Stateful (globalStdGen)
import GHC.Base (TrName(TrNameD))

initialVoxelVerseState :: PlayerId -> CensoredGameState -> VoxelVerseState
initialVoxelVerseState playerId gameState = undefined

initialVoxelVerseDelta :: PlayerId -> CensoredGameState -> VoxelVerseDelta
initialVoxelVerseDelta playerId gameState = mempty

initialInteractionState :: PlayerId -> CensoredGameState -> InteractionState
initialInteractionState playerId gameState = undefined

initialProjectionState :: PlayerId -> CensoredGameState -> ProjectionState
initialProjectionState playerId gameState = ProjectionState

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

pushButtonLabel :: VoxelPropertyValue
pushButtonLabel = TextProperty "pushButton"

passTurnLabel :: VoxelPropertyValue
passTurnLabel = TextProperty "passTurn"

playClueLabel :: VoxelPropertyValue
playClueLabel = TextProperty "playClue"

moveJeepLabel :: VoxelPropertyValue
moveJeepLabel = TextProperty "moveJeep"

exchangeClueCardsLabel :: VoxelPropertyValue
exchangeClueCardsLabel = TextProperty "exchangeClueCards"

pickupAmuletLabel :: VoxelPropertyValue
pickupAmuletLabel = TextProperty "pickupAmulet"

incrMoveLabel :: VoxelPropertyValue
incrMoveLabel = TextProperty "useAmuletIncrMove"

removeMarkerLabel :: VoxelPropertyValue
removeMarkerLabel = TextProperty "removeMarker"

raiseTreasureLabel :: VoxelPropertyValue
raiseTreasureLabel = TextProperty "raiseTreasure"

treasurePassLabel :: VoxelPropertyValue
treasurePassLabel = TextProperty "treasurePass"

treasureTakeLabel :: VoxelPropertyValue
treasureTakeLabel = TextProperty "treasureTake"

treasureWardCurse :: VoxelPropertyValue
treasureWardCurse = TextProperty "treasureWardCurse"

treasureAcceptCurse :: VoxelPropertyValue
treasureAcceptCurse = TextProperty "treasureAcceptCurse"

toolNameLabel :: VoxelPropertyValue
toolNameLabel = TextProperty "toolName"

useAmuletLabel :: VoxelPropertyValue
useAmuletLabel = TextProperty "useAmulet"

raisingTreasureLabel :: VoxelPropertyValue
raisingTreasureLabel = TextProperty "raisingTreasure"

enabledLabel :: VoxelPropertyValue
enabledLabel = TextProperty "enabled"

cancelLabel :: VoxelPropertyValue
cancelLabel = TextProperty "cancel"

viewMoveNorthLabel :: VoxelPropertyValue
viewMoveNorthLabel = TextProperty "viewMoveNorth"

viewMoveEastLabel :: VoxelPropertyValue
viewMoveEastLabel = TextProperty "viewMoveEast"

viewMoveSouthLabel :: VoxelPropertyValue
viewMoveSouthLabel = TextProperty "viewMoveSouth"

viewMoveWestLabel :: VoxelPropertyValue
viewMoveWestLabel = TextProperty "viewMoveWest"

viewZoomInLabel :: VoxelPropertyValue
viewZoomInLabel = TextProperty "viewZoomIn"

viewZoomOutLabel :: VoxelPropertyValue
viewZoomOutLabel = TextProperty "viewZoomOut"

openMenuLabel :: VoxelPropertyValue
openMenuLabel = TextProperty "openMenu"

editTextLabel :: VoxelPropertyValue
editTextLabel = TextProperty "editText"

toolGlossary :: GlossaryList
toolGlossary = 
    [ (toolNameLabel
        , GlossaryEntry { glossaryDescription = "Property indicating what user/move/action tool corresponds to the Voxel or selected tool."
                        , glossaryRange = ListProperty [ pushButtonLabel
                                                    , cancelLabel
                                                    , passTurnLabel
                                                    , playClueLabel
                                                    , moveJeepLabel
                                                    , exchangeClueCardsLabel
                                                    , pickupAmuletLabel
                                                    , incrMoveLabel
                                                    , removeMarkerLabel
                                                    , raiseTreasureLabel
                                                    , treasurePassLabel
                                                    , treasureTakeLabel
                                                    , treasureWardCurse
                                                    , treasureAcceptCurse
                                                    , viewMoveNorthLabel
                                                    , viewMoveEastLabel
                                                    , viewMoveSouthLabel
                                                    , viewMoveWestLabel
                                                    , viewZoomInLabel
                                                    , viewZoomOutLabel
                                                    , openMenuLabel
                                                    , editTextLabel
                                        ]})
    , (useAmuletLabel
        , GlossaryEntry { glossaryDescription = "Whether the current tool will use up an Amulet"
                        , glossaryRange = TrueProperty }
        )
    , (raisingTreasureLabel
        , GlossaryEntry { glossaryDescription = "Property set during raising treasure mode"
                        , glossaryRange = TrueProperty })
    , (pushButtonLabel
        , GlossaryEntry { glossaryDescription = "Name of tool in Push Button mode, allowing user to take immediate actions OR possibly to select another tool"
                        , glossaryRange = pushButtonLabel})
    , (playClueLabel
        , GlossaryEntry { glossaryDescription = "Name of tool in Play clue mode, allowing user to play one of user's clues"
                        , glossaryRange = playClueLabel})
    , (moveJeepLabel
        , GlossaryEntry { glossaryDescription = "Name of tool in Move jeep mode, allowing user to select a next jeep location"
                        , glossaryRange = moveJeepLabel})
    , (removeMarkerLabel
        , GlossaryEntry { glossaryDescription = "Name of tool in Remove Marker mode, allowing user to select a marker to remove"
                        , glossaryRange = removeMarkerLabel})
    , (enabledLabel
        , GlossaryEntry { glossaryDescription = "All valid tool targets have this property set to true."
                        , glossaryRange = TrueProperty})
    ]

oceanLabel :: VoxelPropertyValue
oceanLabel = TextProperty "ocean"

riverLabel :: VoxelPropertyValue
riverLabel = TextProperty "river"

lagoonLabel :: VoxelPropertyValue
lagoonLabel = TextProperty "lagoon"

meadowLabel :: VoxelPropertyValue
meadowLabel = TextProperty "meadow"

jungleLabel :: VoxelPropertyValue
jungleLabel = TextProperty "jungle"

mountainLabel :: VoxelPropertyValue
mountainLabel = TextProperty "mountain"

beachLabel :: VoxelPropertyValue
beachLabel = TextProperty "beach"

terrainTypeLabel :: VoxelPropertyValue
terrainTypeLabel = TextProperty "terrainType"

jeepTokensLabel :: VoxelPropertyValue
jeepTokensLabel = TextProperty "jeepTokens"

markerTokensLabel :: VoxelPropertyValue
markerTokensLabel = TextProperty "markerTokens"

amuletTokenLabel :: VoxelPropertyValue
amuletTokenLabel = TextProperty "amuletToken"

hutTokenLabel :: VoxelPropertyValue
hutTokenLabel = TextProperty "hutToken"

statueTokenLabel :: VoxelPropertyValue
statueTokenLabel = TextProperty "statueToken"

palmTokenLabel :: VoxelPropertyValue
palmTokenLabel = TextProperty "palmToken"

largestFeatureLabel :: VoxelPropertyValue
largestFeatureLabel = TextProperty "isLargestFeature"

mapGlossary :: GlossaryList
mapGlossary =
    [ (terrainTypeLabel
        , GlossaryEntry { glossaryDescription = "Property specifying the terrain type of the voxel"
                        , glossaryRange = ListProperty [ oceanLabel
                                                       , riverLabel
                                                       , lagoonLabel
                                                       , meadowLabel
                                                       , jungleLabel
                                                       , mountainLabel
                                                       , beachLabel]})
    , (jeepTokensLabel
        , GlossaryEntry { glossaryDescription = "Property specifying list of Jeep Token colors on the voxel"
                        , glossaryRange = ListProperty $ map (TextProperty . show) allPlayerColors})
    , (markerTokensLabel
        , GlossaryEntry { glossaryDescription = "Property specifying list of Marker Token colors on the voxel"
                        , glossaryRange = ListProperty $ map (TextProperty . show) allClueColors})
    , (amuletTokenLabel
        , GlossaryEntry { glossaryDescription = "Property specifying that an amulet is on the voxel"
                        , glossaryRange = TrueProperty})
    , (hutTokenLabel
        , GlossaryEntry { glossaryDescription = "Property specifying that a hut is on the voxel"
                        , glossaryRange = TrueProperty})
    , (statueTokenLabel
        , GlossaryEntry { glossaryDescription = "Property specifying that a statue is on the voxel"
                        , glossaryRange = TrueProperty})
    , (palmTokenLabel
        , GlossaryEntry { glossaryDescription = "Property specifying that a plam tree is on the voxel"
                        , glossaryRange = TrueProperty})
    , (largestFeatureLabel
        , GlossaryEntry { glossaryDescription = "Property indicating that this voxel is part of a feature formation that is maximally sized per this feature type"
                        , glossaryRange = TrueProperty})
    ]

voxelGroupLabel = TextProperty "buttonGroup"

labeledGroupRange = ListProperty [TextProperty "some label"
                                 ,ListProperty [IntProperty 0, IntProperty 9999]]

groupTextSizeLabel = TextProperty "groupTextSize"

groupTextLabel = TextProperty "groupText"

isEditableLabel = TextProperty "isEditable"

isClickableLabel = TextProperty "isClickable"

groupBitmapNameLabel = TextProperty "groupBitmapName"

groupBitmapStateLabel = TextProperty "groupBitmapState"

hudGlossary :: GlossaryList
hudGlossary = 
    [ (voxelGroupLabel
        , GlossaryEntry { glossaryDescription = "Property indicating this voxel is part of the identified button group"
                        , glossaryRange = labeledGroupRange})
    , (groupTextSizeLabel
        , GlossaryEntry { glossaryDescription = "Property specifying the number of glyphs per voxel for text on a group"
                        , glossaryRange = ListProperty [IntProperty 1, IntProperty 3, IntProperty 12]})
    , (groupTextLabel
        , GlossaryEntry { glossaryDescription = "Property containing the text of the group"
                        , glossaryRange = TextProperty "any text"})
    , (groupBitmapNameLabel 
        , GlossaryEntry { glossaryDescription = "Property containing the name of the bitmap for the voxel group"
                        , glossaryRange = TextProperty "a bitmap name"})
    , (groupBitmapStateLabel
        , GlossaryEntry { glossaryDescription = "Property containing a text string for the bitmap display state for the group"
                        , glossaryRange = TextProperty "a state name"})
    , (isEditableLabel
        , GlossaryEntry { glossaryDescription = "Property is true if the group is editable"
                        , glossaryRange = TrueProperty})
    , (isClickableLabel
        , GlossaryEntry { glossaryDescription = "Property is true if the group is clickable/pressable"
                        , glossaryRange = TrueProperty})
    ]

glossary :: Glossary
glossary = Glossary $ Map.fromList $ map (first toText) 
    $ concat [toolGlossary, mapGlossary, hudGlossary]

pushButtonDecoder :: DecodedTool -> PlayerMove
pushButtonDecoder dTool = undefined

pushButtonEncoder :: SessionStateType -> VoxelSelection
pushButtonEncoder st = undefined
    where   moves = st.voxelVerseInteractionState.enumeratedPlayerMoves

jeepDecoder :: DecodedTool -> PlayerMove
jeepDecoder dTool = undefined

jeepEncoder :: SessionStateType -> VoxelSelection
jeepEncoder = undefined

clueDecoder :: DecodedTool -> PlayerMove
clueDecoder dTool = undefined

clueEncoder :: SessionStateType -> VoxelSelection
clueEncoder = undefined

removeMarkerDecoder :: DecodedTool -> PlayerMove
removeMarkerDecoder dTool = undefined

removeMarkerEncoder :: SessionStateType -> VoxelSelection
removeMarkerEncoder = undefined

moveDecoders :: Map VoxelPropertyValue (DecodedTool -> PlayerMove)
moveDecoders = fromList
    [ (pushButtonLabel, pushButtonDecoder)
    , (moveJeepLabel, jeepDecoder)
    , (playClueLabel, clueDecoder)
    , (removeMarkerLabel, removeMarkerDecoder)
    ]

socketEncoders :: Map VoxelPropertyValue (SessionStateType -> VoxelSelection)
socketEncoders = fromList
    [ (pushButtonLabel, pushButtonEncoder)
    , (moveJeepLabel, jeepEncoder)
    , (playClueLabel, clueEncoder)
    , (removeMarkerLabel, removeMarkerEncoder)
    ]

toolLocations :: Map VoxelPropertyValue (CubeCoordinate Int)
toolLocations = fromList
    [ (pushButtonLabel, undefined)
    , (moveJeepLabel, undefined)
    , (playClueLabel, undefined)
    , (removeMarkerLabel, undefined)
    ]

toPlayerMove :: ToolApplication -> VoxelVerseState -> Either Text PlayerMove
toPlayerMove tool st = toEitherMove mPlayerMove
    where   mTool = decodeTool tool st
            errorMsg = "Could not find toolName " <> show ((.toolName) <$> mTool) <> " in moveDecoders."
            mPlayerMove = do
                t <- mTool
                d <- Map.lookup t.toolName moveDecoders
                pure $ d t
            toEitherMove (Just (PlayerMoveError e)) = Left e
            toEitherMove (Just m) = Right m
            toEitherMove _ = Left errorMsg

decodeTool :: ToolApplication -> VoxelVerseState -> Maybe DecodedTool
decodeTool tool st = decodedTool appliedToolList
    where   decodedTool [(loc, v)] = Just $ DecodedTool
                { toolName = propertySetLookup "toolName" v
                , tool = v
                , location = loc
                , selection = selectedSets }
            decodedTool _ = Nothing
            appliedToolAsSelection :: VoxelSelection
            appliedToolAsSelection = SparseSelection tool.toolLayer $ one tool.appliedTool
            appliedToolList = lookupStateVoxels (selectionLookupFn appliedToolAsSelection)
                st
            selectedSets = lookupStateVoxels (selectionLookupFn tool.appliedSelection)
                st

onlySelectedTool :: Maybe DecodedTool -> Maybe (CubeCoordinate Int, Voxel)
onlySelectedTool mTool = do
                t <- mTool
                let tools = filter ((NullProperty /=) . propertySetLookup "toolName" . snd) t.selection
                (f, rest) <- uncons tools
                if null rest then Just f else Nothing

isGameMove :: ToolApplication -> VoxelVerseState -> Bool
isGameMove tool st = not isMetaAction
    where   mTool = decodeTool tool st
            isCancelSelected (_, voxel) = TrueProperty == propertySetLookup "cancel" voxel
            isCancel = any isCancelSelected $ maybe [] (.selection) mTool
            selectionError = maybe True ((1 /=) . length . (.selection)) mTool
            hasSelectedTool = isJust $ onlySelectedTool mTool
            isMetaAction = isCancel || hasSelectedTool || selectionError

makeMoveNextGameState :: ToolApplication -> InnerVoxelVerseSession -> Either Text InnerVoxelVerseSession
makeMoveNextGameState tool session = do
    playerSession <- getActivePlayerSession session
    move <- toPlayerMove tool (((.playerModel) . snd) playerSession)
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
toolLocation toolName = fromMaybe undefinedLocation $ Map.lookup toolName toolLocations
    where undefinedLocation = mkCubeCoordinate 0 0

pushButtonChoiceTool :: SessionStateType -> ToolApplication
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

selectedChoiceTool :: Maybe (CubeCoordinate Int, Voxel) -> SessionStateType -> ToolApplication
selectedChoiceTool Nothing session = pushButtonChoiceTool session
selectedChoiceTool (Just (_, selected)) session = ToolApplication 
    { toolLayer = 0
    , appliedTool = (toolLocation selectedName, NoVoxelSpecifier)
    , appliedSelection = encodeSelection session }
    where   selectedName = propertySetLookup "toolName" selected
            noSelection = SparseSelection 0 mempty
            encodeSelection = fromMaybe (const noSelection) $ Map.lookup selectedName socketEncoders

nextChoiceTool :: ToolApplication -> SessionStateType -> ToolApplication
nextChoiceTool previous st
    | notToolPicked = pushButtonChoiceTool st
    | otherwise = selectedChoiceTool selectedTool st
    where   mTool = decodeTool previous st.voxelVerseState
            isCancelSelected (_, voxel) = TrueProperty == propertySetLookup "cancel" voxel
            isCancel = any isCancelSelected $ maybe [] (.selection) mTool
            selectionError = maybe True ((1 /=) . length . (.selection)) mTool
            selectedTool = onlySelectedTool mTool
            notToolPicked = isCancel || isNothing selectedTool || selectionError

updatePlayerInteractionState :: ToolApplication -> VoxelVersePlayerContext -> SessionStateType
    -> Either Text SessionStateType
updatePlayerInteractionState tool ctx st
    = Right $ st { voxelVerseInteractionState = newInteractionState }
    where   playerId = st.voxelVerseState.thisPlayer
            newInteractionState = st.voxelVerseInteractionState { currentToolChoices = newChoices }
            gameState = toGameState ctx.currentState
            newChoices  | not isActive = noChoiceTool
                        | isGameMove tool st.voxelVerseState = pushButtonChoiceTool st
                        | otherwise = nextChoiceTool tool st
            isActive = gameState.activePlayer == playerId

getActivePlayerSession :: InnerVoxelVerseSession -> Either Text (PlayerId, InnerVoxelVersePlayerSession)
getActivePlayerSession session = (playerId, ) <$> maybeToRight eMsg mInnerSession
    where   playerId = session.vvContext.currentState.activePlayer
            eMsg = "Could not find player " <> show playerId <> " in players."
            mInnerSession = do
                let sessions = zip session.vvPlayerSessions session.vvContext.currentState.players
                fst . fst <$> uncons (filter ((playerId ==) . (.player.playerId) . snd) sessions)

computeNextGameState :: ToolApplication -> InnerVoxelVerseSession -> Either Text InnerVoxelVerseSession
computeNextGameState tool session = do
    playerSession <- snd <$> getActivePlayerSession session
    let     updated | isGameMove tool playerSession.playerModel = makeMoveNextGameState tool session
                    | otherwise = Right session
    updated

applyToolM :: ToolApplication -> SessionM ()
applyToolM tool = do
    -- lift (Left "delta computations not implemented yet")
    -- 1) Compose deltas
    -- 2) If in post move mode (isGameMove), impose deltas
    -- 3) Update interaction state, a.k.a. the current tool and sockets
    sessionState <- get
    ctx <- ask
    newSessionState <- lift (updatePlayerInteractionState tool ctx sessionState)
    put newSessionState


createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
