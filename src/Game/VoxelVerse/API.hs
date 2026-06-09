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
import qualified Game.CursedTreasure.VoxelVerse.Types as CursedTreasureVoxelVerse
import qualified Game.FogOfBattle.VoxelVerse.Types as FogOfBattleVoxelVerse
import qualified Game.ArtOfWar.VoxelVerse.Types as ArtOfWarVoxelVerse
import qualified Game.RealEstate.VoxelVerse.Types as RealEstateVoxelVerse
import qualified Game.CursedTreasure.Types as CursedTreasureTypes
import qualified Game.FogOfBattle.Types as FogOfBattleTypes
import qualified Game.ArtOfWar.Types as ArtOfWarTypes
import qualified Game.RealEstate.Types as RealEstateTypes
import qualified Game.Core.Primitives as Primitives
import Game.VoxelVerse.Types
import Control.Monad.Trans.RWS.Strict (RWST, runRWST, tell)
import qualified Data.Text as Text


-- This API needs to simply pass through the concept of getGameSetupPlayers
getGameSetupPlayers :: [(Text, [Core.PlayerDescription])]
getGameSetupPlayers = CoreAPI.getGameSetupPlayers

-- But the createNewGame talks the VoxelVerse language (and also tracks the underlying GameState).
-- From the standpoint of downstream logic, the GameState is opaque, but needs to be tracked.
createNewGame :: [Core.PlayerDescription] -> Int
    -> Either Text (CoreVoxelVerseSession, [VoxelVerseView])
createNewGame players randomSeed = do
  createdGame <- CoreAPI.createNewGame players randomSeed
  session <- initializeSession createdGame
  pure (session, map createVoxelVerseView session.vvPlayerSessions)
  where
    initializeSession :: (Core.GameState, [Core.CensoredGameState]) -> Either Text CoreVoxelVerseSession
    initializeSession (gameState, censoredStates) = do
      pure VoxelVerseSession
        { vvContext = Primitives.Context
          { previousCommittedState = Nothing
          , currentState = gameState
          , planningState = Nothing
          }
        , vvPlayerSessions = zipWith (curry initialPlayerSession) players censoredStates
        }
    initialPlayerSession (player, censoredState) = VoxelVersePlayerSession
        { playerContext = Primitives.Context
            { previousCommittedState = Nothing
            , currentState = censoredState
            , planningState = Nothing
            }
        , playerModel = initialVoxelVerseState player.playerId censoredState
        , playerModelDelta = initialVoxelVerseDelta censoredState
        , playerInteractionState = initialInteractionState censoredState
        , playerProjectionState = initialProjectionState censoredState
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

initialVoxelVerseState :: Primitives.PlayerId -> Core.CensoredGameState -> VoxelVerseState
initialVoxelVerseState playerId gameState =
    case gameState of
        Core.CursedTreasureCensoredGameState gS ->
            CursedTreasure.initialVoxelVerseState playerId gS
        Core.FogOfBattleCensoredGameState gS ->
            FogOfBattle.initialVoxelVerseState playerId gS
        Core.ArtOfWarCensoredGameState gS ->
            ArtOfWar.initialVoxelVerseState playerId gS
        Core.RealEstateCensoredGameState gS ->
            RealEstate.initialVoxelVerseState playerId gS

initialVoxelVerseDelta :: Core.CensoredGameState -> VoxelVerseDelta
initialVoxelVerseDelta gameState =
    case gameState of
        Core.CursedTreasureCensoredGameState gS ->
            CursedTreasure.initialVoxelVerseDelta gS
        Core.FogOfBattleCensoredGameState gS ->
            FogOfBattle.initialVoxelVerseDelta gS
        Core.ArtOfWarCensoredGameState gS ->
            ArtOfWar.initialVoxelVerseDelta gS
        Core.RealEstateCensoredGameState gS ->
            RealEstate.initialVoxelVerseDelta gS

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


-- computeNextGameState is strictly concerned with updating the GameState and CensoredGameState data 
-- structures within the Context structures of the session. This will usually do these things:
-- * prevousCommittedState = Just currentState
-- * currentState = makeMove (constructMoveFromTool appliedTool)
-- * planningState = Nothing
-- OR
-- * planningState = makeMove (constructMoveFromTool appliedTool)
computeNextGameState :: CoreVoxelVerseSession -> ToolApplication -> Either Text CoreVoxelVerseSession
computeNextGameState session appliedTool =
    case session.vvContext.currentState of
    Core.CursedTreasureGame _ -> do
      downSession <- convertSessionDown
        "CursedTreasure"
        toCursedTreasureGameState
        toCursedTreasureCensoredGameState
        toCursedTreasureInteractionState
        toCursedTreasureProjectionState
        session
      movedSession <- CursedTreasure.computeNextGameState downSession appliedTool
      pure $ convertSessionUp
        Core.CursedTreasureGame
        Core.CursedTreasureCensoredGameState
        CursedTreasureVoxelVerseInteractionState
        CursedTreasureVoxelVerseProjectionState
        movedSession
    Core.FogOfBattleGame _ -> do
      downSession <- convertSessionDown
        "FogOfBattle"
        toFogOfBattleGameState
        toFogOfBattleCensoredGameState
        toFogOfBattleInteractionState
        toFogOfBattleProjectionState
        session
      movedSession <- FogOfBattle.computeNextGameState downSession appliedTool
      pure $ convertSessionUp
        Core.FogOfBattleGame
        Core.FogOfBattleCensoredGameState
        FogOfBattleVoxelVerseInteractionState
        FogOfBattleVoxelVerseProjectionState
        movedSession
    Core.ArtOfWarGame _ -> do
      downSession <- convertSessionDown
        "ArtOfWar"
        toArtOfWarGameState
        toArtOfWarCensoredGameState
        toArtOfWarInteractionState
        toArtOfWarProjectionState
        session
      movedSession <- ArtOfWar.computeNextGameState downSession appliedTool
      pure $ convertSessionUp
        Core.ArtOfWarGame
        Core.ArtOfWarCensoredGameState
        ArtOfWarVoxelVerseInteractionState
        ArtOfWarVoxelVerseProjectionState
        movedSession
    Core.RealEstateGame _ -> do
      downSession <- convertSessionDown
        "RealEstate"
        toRealEstateGameState
        toRealEstateCensoredGameState
        toRealEstateInteractionState
        toRealEstateProjectionState
        session
      movedSession <- RealEstate.computeNextGameState downSession appliedTool
      pure $ convertSessionUp
        Core.RealEstateGame
        Core.RealEstateCensoredGameState
        RealEstateVoxelVerseInteractionState
        RealEstateVoxelVerseProjectionState
        movedSession

convertSessionDown
  :: Text
  -> (Core.GameState -> Maybe gameState)
  -> (Core.CensoredGameState -> Maybe censoredGameState)
  -> (VoxelVerseInteractionState -> Maybe interactionState)
  -> (VoxelVerseProjectionState -> Maybe projectionState)
  -> CoreVoxelVerseSession
  -> Either Text (VoxelVerseSession (Primitives.Context gameState) (VoxelVersePlayerSession (Primitives.Context censoredGameState) interactionState projectionState))
convertSessionDown rulesetName gameExtractor censoredExtractor interactionExtractor projectionExtractor coreSession = do
  nextContext <- convertContext
    ("Session game-state did not match " <> rulesetName <> " ruleset")
    gameExtractor
    coreSession.vvContext
  nextPlayerSessions <- traverse
    (convertPlayerSessionDown rulesetName censoredExtractor interactionExtractor projectionExtractor)
    coreSession.vvPlayerSessions
  pure VoxelVerseSession
    { vvContext = nextContext
    , vvPlayerSessions = nextPlayerSessions
    }

convertPlayerSessionDown
  :: Text
  -> (Core.CensoredGameState -> Maybe censoredGameState)
  -> (VoxelVerseInteractionState -> Maybe interactionState)
  -> (VoxelVerseProjectionState -> Maybe projectionState)
  -> CoreVoxelVersePlayerSession
  -> Either Text (VoxelVersePlayerSession (Primitives.Context censoredGameState) interactionState projectionState)
convertPlayerSessionDown rulesetName censoredExtractor interactionExtractor projectionExtractor playerSession = do
  nextPlayerContext <- convertContext
    ("Player censored-state did not match " <> rulesetName <> " ruleset")
    censoredExtractor
    playerSession.playerContext
  nextInteractionState <- maybeToRight
    ("Player interaction-state did not match " <> rulesetName <> " ruleset")
    (interactionExtractor playerSession.playerInteractionState)
  nextProjectionState <- maybeToRight
    ("Player projection-state did not match " <> rulesetName <> " ruleset")
    (projectionExtractor playerSession.playerProjectionState)
  pure VoxelVersePlayerSession
    { playerContext = nextPlayerContext
    , playerModel = playerSession.playerModel
    , playerModelDelta = playerSession.playerModelDelta
    , playerInteractionState = nextInteractionState
    , playerProjectionState = nextProjectionState
    }

convertContext
  :: Text
  -> (a -> Maybe b)
  -> Primitives.Context a
  -> Either Text (Primitives.Context b)
convertContext mismatchError extractor context = do
  nextCurrentState <- maybeToRight mismatchError (extractor context.currentState)
  nextPreviousCommittedState <- traverse (maybeToRight mismatchError . extractor)
    context.previousCommittedState
  nextPlanningState <- traverse (maybeToRight mismatchError . extractor)
    context.planningState
  pure Primitives.Context
    { previousCommittedState = nextPreviousCommittedState
    , currentState = nextCurrentState
    , planningState = nextPlanningState
    }

convertSessionUp
  :: (gameState -> Core.GameState)
  -> (censoredGameState -> Core.CensoredGameState)
  -> (interactionState -> VoxelVerseInteractionState)
  -> (projectionState -> VoxelVerseProjectionState)
  -> VoxelVerseSession (Primitives.Context gameState) (VoxelVersePlayerSession (Primitives.Context censoredGameState) interactionState projectionState)
  -> CoreVoxelVerseSession
convertSessionUp gameWrapper censoredWrapper interactionWrapper projectionWrapper innerSession =
  VoxelVerseSession
    { vvContext = fmap gameWrapper innerSession.vvContext
    , vvPlayerSessions = map wrapPlayerSession innerSession.vvPlayerSessions
    }
  where
  wrapPlayerSession playerSession =
    VoxelVersePlayerSession
      { playerContext = fmap censoredWrapper playerSession.playerContext
      , playerModel = playerSession.playerModel
      , playerModelDelta = playerSession.playerModelDelta
      , playerInteractionState = interactionWrapper playerSession.playerInteractionState
      , playerProjectionState = projectionWrapper playerSession.playerProjectionState
      }

convertSessionStateDown
  :: Text
  -> (VoxelVerseInteractionState -> Maybe interactionState)
  -> (VoxelVerseProjectionState -> Maybe projectionState)
  -> SessionStateType
  -> Either Text (SessionState interactionState projectionState)
convertSessionStateDown rulesetName interactionExtractor projectionExtractor sessionState = do
  nextInteractionState <- maybeToRight
    ("Session interaction-state did not match " <> rulesetName <> " ruleset")
    (interactionExtractor sessionState.voxelVerseInteractionState)
  nextProjectionState <- maybeToRight
    ("Session projection-state did not match " <> rulesetName <> " ruleset")
    (projectionExtractor sessionState.voxelVerseProjectionState)
  pure SessionState
    { voxelVerseState = sessionState.voxelVerseState
    , voxelVerseInteractionState = nextInteractionState
    , voxelVerseProjectionState = nextProjectionState
    }

convertSessionStateUp
  :: (interactionState -> VoxelVerseInteractionState)
  -> (projectionState -> VoxelVerseProjectionState)
  -> SessionState interactionState projectionState
  -> SessionStateType
convertSessionStateUp interactionWrapper projectionWrapper sessionState =
  SessionState
    { voxelVerseState = sessionState.voxelVerseState
    , voxelVerseInteractionState = interactionWrapper sessionState.voxelVerseInteractionState
    , voxelVerseProjectionState = projectionWrapper sessionState.voxelVerseProjectionState
    }

toCursedTreasureGameState :: Core.GameState -> Maybe CursedTreasureTypes.GameState
toCursedTreasureGameState gameState =
  case gameState of
    Core.CursedTreasureGame gS -> Just gS
    _ -> Nothing

toFogOfBattleGameState :: Core.GameState -> Maybe FogOfBattleTypes.GameState
toFogOfBattleGameState gameState =
  case gameState of
    Core.FogOfBattleGame gS -> Just gS
    _ -> Nothing

toArtOfWarGameState :: Core.GameState -> Maybe ArtOfWarTypes.GameState
toArtOfWarGameState gameState =
  case gameState of
    Core.ArtOfWarGame gS -> Just gS
    _ -> Nothing

toRealEstateGameState :: Core.GameState -> Maybe RealEstateTypes.GameState
toRealEstateGameState gameState =
  case gameState of
    Core.RealEstateGame gS -> Just gS
    _ -> Nothing

toCursedTreasureCensoredGameState :: Core.CensoredGameState -> Maybe CursedTreasureTypes.CensoredGameState
toCursedTreasureCensoredGameState gameState =
  case gameState of
    Core.CursedTreasureCensoredGameState gS -> Just gS
    _ -> Nothing

toFogOfBattleCensoredGameState :: Core.CensoredGameState -> Maybe FogOfBattleTypes.CensoredGameState
toFogOfBattleCensoredGameState gameState =
  case gameState of
    Core.FogOfBattleCensoredGameState gS -> Just gS
    _ -> Nothing

toArtOfWarCensoredGameState :: Core.CensoredGameState -> Maybe ArtOfWarTypes.CensoredGameState
toArtOfWarCensoredGameState gameState =
  case gameState of
    Core.ArtOfWarCensoredGameState gS -> Just gS
    _ -> Nothing

toRealEstateCensoredGameState :: Core.CensoredGameState -> Maybe RealEstateTypes.CensoredGameState
toRealEstateCensoredGameState gameState =
  case gameState of
    Core.RealEstateCensoredGameState gS -> Just gS
    _ -> Nothing

toCursedTreasureInteractionState :: VoxelVerseInteractionState -> Maybe CursedTreasureVoxelVerse.InteractionState
toCursedTreasureInteractionState interactionState =
  case interactionState of
    CursedTreasureVoxelVerseInteractionState s -> Just s
    _ -> Nothing

toFogOfBattleInteractionState :: VoxelVerseInteractionState -> Maybe FogOfBattleVoxelVerse.InteractionState
toFogOfBattleInteractionState interactionState =
  case interactionState of
    FogOfBattleVoxelVerseInteractionState s -> Just s
    _ -> Nothing

toArtOfWarInteractionState :: VoxelVerseInteractionState -> Maybe ArtOfWarVoxelVerse.InteractionState
toArtOfWarInteractionState interactionState =
  case interactionState of
    ArtOfWarVoxelVerseInteractionState s -> Just s
    _ -> Nothing

toRealEstateInteractionState :: VoxelVerseInteractionState -> Maybe RealEstateVoxelVerse.InteractionState
toRealEstateInteractionState interactionState =
  case interactionState of
    RealEstateVoxelVerseInteractionState s -> Just s
    _ -> Nothing

toCursedTreasureProjectionState :: VoxelVerseProjectionState -> Maybe CursedTreasureVoxelVerse.ProjectionState
toCursedTreasureProjectionState projectionState =
  case projectionState of
    CursedTreasureVoxelVerseProjectionState s -> Just s
    _ -> Nothing

toFogOfBattleProjectionState :: VoxelVerseProjectionState -> Maybe FogOfBattleVoxelVerse.ProjectionState
toFogOfBattleProjectionState projectionState =
  case projectionState of
    FogOfBattleVoxelVerseProjectionState s -> Just s
    _ -> Nothing

toArtOfWarProjectionState :: VoxelVerseProjectionState -> Maybe ArtOfWarVoxelVerse.ProjectionState
toArtOfWarProjectionState projectionState =
  case projectionState of
    ArtOfWarVoxelVerseProjectionState s -> Just s
    _ -> Nothing

toRealEstateProjectionState :: VoxelVerseProjectionState -> Maybe RealEstateVoxelVerse.ProjectionState
toRealEstateProjectionState projectionState =
  case projectionState of
    RealEstateVoxelVerseProjectionState s -> Just s
    _ -> Nothing

createVoxelVerseView :: CoreVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess =
  case sess.playerContext.currentState of
    Core.CursedTreasureCensoredGameState _ ->
      case convertPlayerSessionDown
        "CursedTreasure"
        toCursedTreasureCensoredGameState
        toCursedTreasureInteractionState
        toCursedTreasureProjectionState
        sess of
        Left err -> error err
        Right convertedSession -> CursedTreasure.createVoxelVerseView convertedSession
    Core.FogOfBattleCensoredGameState _ ->
      case convertPlayerSessionDown
        "FogOfBattle"
        toFogOfBattleCensoredGameState
        toFogOfBattleInteractionState
        toFogOfBattleProjectionState
        sess of
        Left err -> error err
        Right convertedSession -> FogOfBattle.createVoxelVerseView convertedSession
    Core.ArtOfWarCensoredGameState _ ->
      case convertPlayerSessionDown
        "ArtOfWar"
        toArtOfWarCensoredGameState
        toArtOfWarInteractionState
        toArtOfWarProjectionState
        sess of
        Left err -> error err
        Right convertedSession -> ArtOfWar.createVoxelVerseView convertedSession
    Core.RealEstateCensoredGameState _ ->
      case convertPlayerSessionDown
        "RealEstate"
        toRealEstateCensoredGameState
        toRealEstateInteractionState
        toRealEstateProjectionState
        sess of
        Left err -> error err
        Right convertedSession -> RealEstate.createVoxelVerseView convertedSession


type VoxelVerseContext = Primitives.Context Core.GameState
type VoxelVersePlayerContext = Primitives.Context Core.CensoredGameState
type CoreVoxelVersePlayerSession = VoxelVersePlayerSession VoxelVersePlayerContext VoxelVerseInteractionState VoxelVerseProjectionState
type CoreVoxelVerseSession = VoxelVerseSession VoxelVerseContext CoreVoxelVersePlayerSession
type SessionStateType = SessionState VoxelVerseInteractionState VoxelVerseProjectionState

type SessionM a = RWST VoxelVersePlayerContext VoxelVerseDelta SessionStateType (Either Text) a

-- applyTool is the more generic and high level mutation of the GameState which _entails_ makeMove 
-- AND enumerateActivePlayerOptions AND hueristicHint. It even entails ViewPorts, since a ViewPort
-- turns out to be simply another kind of Tool Application.
applyTool :: CoreVoxelVerseSession -> ToolApplication
  -> Either Text (CoreVoxelVerseSession, [VoxelVerseView])
applyTool session toolApplication = updSession eMovedSession $ updViews originalPlayerSessions
    where
        eMovedSession = computeNextGameState session toolApplication
        updSession (Left moveError) _ = Left moveError
        updSession _ (Left updError) = Left updError
        updSession (Right origSession) (Right views) = Right
            ( VoxelVerseSession
                { vvContext = origSession.vvContext
                , vvPlayerSessions = map fst views
                }
            , map snd views )
        updPlayerSession (c, s) = case runRWST (applyToolM toolApplication) c s of
            Left err -> Left err
            Right (_, nextState, deltas) ->
                Right
                    ( VoxelVersePlayerSession
                        { playerContext = c
                        , playerModel = nextState.voxelVerseState
                        , playerModelDelta = deltas
                        , playerInteractionState = nextState.voxelVerseInteractionState
                        , playerProjectionState = nextState.voxelVerseProjectionState
                        }
                    )
        originalPlayerSessions = either (const []) (.vvPlayerSessions) eMovedSession
        updViews s = collectEithersConcatErrors (map ((createView <$>) . updPlayerSession . toContextSession) s)
        toContextSession playerSession = (playerSession.playerContext,
            SessionState
            { voxelVerseState = playerSession.playerModel
            , voxelVerseInteractionState = playerSession.playerInteractionState
            , voxelVerseProjectionState = playerSession.playerProjectionState
            })
        createView sess = (sess, createVoxelVerseView sess)

        collectEithersConcatErrors :: [Either Text a] -> Either Text [a]
        collectEithersConcatErrors values =
            case partitionEithers values of
                ([], successes) -> Right successes
                (errors, _) -> Left (Text.intercalate "\n" errors)

applyToolM :: ToolApplication -> SessionM ()
applyToolM toolApplication = do
  coreContext <- ask
  sessionState <- get
  case coreContext.currentState of
    Core.CursedTreasureCensoredGameState _ ->
      dispatchRulesetApplyToolM
        "CursedTreasure"
        toCursedTreasureCensoredGameState
        toCursedTreasureInteractionState
        toCursedTreasureProjectionState
        CursedTreasureVoxelVerseInteractionState
        CursedTreasureVoxelVerseProjectionState
        CursedTreasure.applyToolM
        toolApplication
        coreContext
        sessionState
    Core.FogOfBattleCensoredGameState _ ->
      dispatchRulesetApplyToolM
        "FogOfBattle"
        toFogOfBattleCensoredGameState
        toFogOfBattleInteractionState
        toFogOfBattleProjectionState
        FogOfBattleVoxelVerseInteractionState
        FogOfBattleVoxelVerseProjectionState
        FogOfBattle.applyToolM
        toolApplication
        coreContext
        sessionState
    Core.ArtOfWarCensoredGameState _ ->
      dispatchRulesetApplyToolM
        "ArtOfWar"
        toArtOfWarCensoredGameState
        toArtOfWarInteractionState
        toArtOfWarProjectionState
        ArtOfWarVoxelVerseInteractionState
        ArtOfWarVoxelVerseProjectionState
        ArtOfWar.applyToolM
        toolApplication
        coreContext
        sessionState
    Core.RealEstateCensoredGameState _ ->
      dispatchRulesetApplyToolM
        "RealEstate"
        toRealEstateCensoredGameState
        toRealEstateInteractionState
        toRealEstateProjectionState
        RealEstateVoxelVerseInteractionState
        RealEstateVoxelVerseProjectionState
        RealEstate.applyToolM
        toolApplication
        coreContext
        sessionState
  where
  dispatchRulesetApplyToolM
    :: Text
    -> (Core.CensoredGameState -> Maybe rulesetCensoredState)
    -> (VoxelVerseInteractionState -> Maybe interactionState)
    -> (VoxelVerseProjectionState -> Maybe projectionState)
    -> (interactionState -> VoxelVerseInteractionState)
    -> (projectionState -> VoxelVerseProjectionState)
    -> (ToolApplication -> RWST (Primitives.Context rulesetCensoredState) VoxelVerseDelta (SessionState interactionState projectionState) (Either Text) ())
    -> ToolApplication
    -> VoxelVersePlayerContext
    -> SessionStateType
    -> SessionM ()
  dispatchRulesetApplyToolM rulesetName extractor interactionExtractor projectionExtractor interactionWrapper projectionWrapper rulesetApply tool coreContext sessionState =
    case convertContext
      ("Session censored-state did not match " <> rulesetName <> " ruleset")
      extractor
      coreContext of
      Left err -> lift (Left err)
      Right rulesetContext ->
        case convertSessionStateDown rulesetName interactionExtractor projectionExtractor sessionState of
          Left err -> lift (Left err)
          Right rulesetSessionState ->
            case runRWST (rulesetApply tool) rulesetContext rulesetSessionState of
              Left err -> lift (Left err)
              Right (_, nextState, deltas) -> do
                put (convertSessionStateUp interactionWrapper projectionWrapper nextState)
                tell deltas

