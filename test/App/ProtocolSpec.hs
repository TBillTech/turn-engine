module App.ProtocolSpec where

import Data.Aeson (FromJSON, ToJSON, Value, decodeStrict', eitherDecodeStrict', encode)
import qualified Data.ByteString.Char8 as StrictByteString
import qualified Game.Core.API as Core
import qualified Game.Core.Types as Core
import qualified Game.RealEstate.Types as RealEstate
import Test.Hspec

import App.Protocol

spec :: Spec
spec = do
    describe "ServiceRequest JSON examples" $ do
        it "round-trips getGameSetupPlayers" $
            assertExampleRoundTrip getGameSetupPlayersRequestJson $ \request ->
                request `shouldBe` GetGameSetupPlayers

        it "round-trips createNewGame" $
            assertExampleRoundTrip createNewGameRequestJson $ \request ->
                request `shouldBe` sampleCreateNewGameRequest

        it "round-trips enumerateActivePlayerOptions" $
            assertExampleRoundTrip enumerateOptionsRequestJson $ \request ->
                request `shouldBe` EnumerateActivePlayerOptions sampleGameState

        it "round-trips makeMove" $
            assertExampleRoundTrip makeMoveRequestJson $ \request ->
                request `shouldBe` MakeMove sampleGameState samplePlayerMove

        it "round-trips heuristicHint" $
            assertExampleRoundTrip heuristicHintRequestJson $ \request ->
                request `shouldBe` HeuristicHint 2 sampleGameState [samplePlayerMove]

        it "round-trips summary" $
            assertExampleRoundTrip summaryRequestJson $ \request ->
                request `shouldBe` Summary sampleGameState

    describe "ServiceResponse JSON examples" $ do
        it "round-trips gameSetupPlayers" $
            assertExampleRoundTrip gameSetupPlayersResponseJson $ \response ->
                response `shouldBe` GameSetupPlayers Core.getGameSetupPlayers

        it "round-trips newGameCreated success" $
            assertExampleRoundTrip newGameCreatedSuccessJson $ \response ->
                response `shouldBe` NewGameCreated (Right (sampleGameState, samplePlayerViews))

        it "round-trips newGameCreated error" $
            assertExampleRoundTrip newGameCreatedErrorJson $ \response ->
                response `shouldBe` NewGameCreated (Left "PlayerDescription list must be non-empty and all match the same ruleset.")

        it "round-trips activePlayerOptions" $
            assertExampleRoundTrip activePlayerOptionsResponseJson $ \response ->
                response `shouldBe` ActivePlayerOptions []

        it "round-trips moveApplied" $
            assertExampleRoundTrip moveAppliedResponseJson $ \response ->
                response `shouldBe` MoveApplied (sampleGameState, samplePlayerViews)

        it "round-trips hintGenerated" $
            assertExampleRoundTrip hintGeneratedResponseJson $ \response ->
                response `shouldBe` HintGenerated [(0, samplePlayerMove)]

        it "round-trips summaryGenerated" $
            assertExampleRoundTrip summaryGeneratedResponseJson $ \response ->
                response `shouldBe` SummaryGenerated "undefined"

        it "round-trips serviceError" $
            assertExampleRoundTrip serviceErrorResponseJson $ \response ->
                response `shouldBe` ServiceError "Invalid request JSON."

assertExampleRoundTrip
    :: (FromJSON a, ToJSON a, Show a)
    => StrictByteString.ByteString
    -> (a -> Expectation)
    -> Expectation
assertExampleRoundTrip exampleJson assertDecoded = do
    decodedValue <- case eitherDecodeStrict' exampleJson of
        Right value -> pure value
        Left err -> expectationFailure err >> error "unreachable"
    assertDecoded decodedValue
    StrictByteString.toStrict (encode decodedValue) `shouldBe` canonicalizeJson exampleJson

canonicalizeJson :: StrictByteString.ByteString -> StrictByteString.ByteString
canonicalizeJson exampleJson =
    case decodeStrict' exampleJson :: Maybe Value of
        Just value -> StrictByteString.toStrict (encode value)
        Nothing -> error "example JSON must be valid"

sampleCreateNewGameRequest :: ServiceRequest
sampleCreateNewGameRequest = CreateNewGame samplePlayers 12345

samplePlayers :: [Core.PlayerDescription]
samplePlayers =
    [ Core.RealEstatePlayerDescription (RealEstate.PlayerDescription "Planner 1")
    , Core.RealEstatePlayerDescription (RealEstate.PlayerDescription "Planner 2")
    ]

sampleGameState :: Core.GameState
sampleGameState =
    Core.RealEstateGame
        (RealEstate.GameState
            { players =
                [ RealEstate.PlayerDescription "Planner 1"
                , RealEstate.PlayerDescription "Planner 2"
                ]
            , turn = 0
            , gameOver = True
            })

samplePlayerViews :: [Core.CensoredGameState]
samplePlayerViews =
    [ Core.RealEstateCensoredGameState sampleCensoredState
    , Core.RealEstateCensoredGameState sampleCensoredState
    ]

sampleCensoredState :: RealEstate.CensoredGameState
sampleCensoredState =
    RealEstate.CensoredGameState
        (RealEstate.GameState
            { players =
                [ RealEstate.PlayerDescription "Planner 1"
                , RealEstate.PlayerDescription "Planner 2"
                ]
            , turn = 0
            , gameOver = True
            })

samplePlayerMove :: Core.PlayerMove
samplePlayerMove = Core.RealEstatePlayerMove RealEstate.NoOpMove

getGameSetupPlayersRequestJson :: StrictByteString.ByteString
getGameSetupPlayersRequestJson = "{\"request\":\"getGameSetupPlayers\"}"

createNewGameRequestJson :: StrictByteString.ByteString
createNewGameRequestJson = "{\"request\":\"createNewGame\",\"players\":[{\"tag\":\"RealEstatePlayerDescription\",\"contents\":{\"playerName\":\"Planner 1\"}},{\"tag\":\"RealEstatePlayerDescription\",\"contents\":{\"playerName\":\"Planner 2\"}}],\"randomSeed\":12345}"

enumerateOptionsRequestJson :: StrictByteString.ByteString
enumerateOptionsRequestJson = "{\"request\":\"enumerateActivePlayerOptions\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}}}"

makeMoveRequestJson :: StrictByteString.ByteString
makeMoveRequestJson = "{\"request\":\"makeMove\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},\"playerMove\":{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}}"

heuristicHintRequestJson :: StrictByteString.ByteString
heuristicHintRequestJson = "{\"request\":\"heuristicHint\",\"level\":2,\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},\"playerMoves\":[{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}]}"

summaryRequestJson :: StrictByteString.ByteString
summaryRequestJson = "{\"request\":\"summary\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}}}"

gameSetupPlayersResponseJson :: StrictByteString.ByteString
gameSetupPlayersResponseJson = "{\"response\":\"gameSetupPlayers\",\"rulesets\":[{\"name\":\"Cursed Treasure\",\"playerTemplates\":[{\"tag\":\"CursedTreasurePlayerDescription\",\"contents\":{\"playerId\":1,\"playerName\":\"Player PlayerId 1\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"}},{\"tag\":\"CursedTreasurePlayerDescription\",\"contents\":{\"playerId\":2,\"playerName\":\"Player PlayerId 2\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}},{\"tag\":\"CursedTreasurePlayerDescription\",\"contents\":{\"playerId\":3,\"playerName\":\"Player PlayerId 3\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Blue\"}},{\"tag\":\"CursedTreasurePlayerDescription\",\"contents\":{\"playerId\":4,\"playerName\":\"Player PlayerId 4\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Purple\"}}]},{\"name\":\"Fog Of Battle\",\"playerTemplates\":[]},{\"name\":\"Art Of War\",\"playerTemplates\":[]},{\"name\":\"Real Estate\",\"playerTemplates\":[]}]}"

newGameCreatedSuccessJson :: StrictByteString.ByteString
newGameCreatedSuccessJson = "{\"response\":\"newGameCreated\",\"status\":\"ok\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},\"playerViews\":[{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}}]}"

newGameCreatedErrorJson :: StrictByteString.ByteString
newGameCreatedErrorJson = "{\"response\":\"newGameCreated\",\"status\":\"error\",\"message\":\"PlayerDescription list must be non-empty and all match the same ruleset.\"}"

activePlayerOptionsResponseJson :: StrictByteString.ByteString
activePlayerOptionsResponseJson = "{\"response\":\"activePlayerOptions\",\"playerMoves\":[]}"

moveAppliedResponseJson :: StrictByteString.ByteString
moveAppliedResponseJson = "{\"response\":\"moveApplied\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},\"playerViews\":[{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}},{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerName\":\"Planner 1\"},{\"playerName\":\"Planner 2\"}],\"turn\":0,\"gameOver\":true}}]}"

hintGeneratedResponseJson :: StrictByteString.ByteString
hintGeneratedResponseJson = "{\"response\":\"hintGenerated\",\"hints\":[{\"score\":0,\"playerMove\":{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}}]}"

summaryGeneratedResponseJson :: StrictByteString.ByteString
summaryGeneratedResponseJson = "{\"response\":\"summaryGenerated\",\"summary\":\"undefined\"}"

serviceErrorResponseJson :: StrictByteString.ByteString
serviceErrorResponseJson = "{\"response\":\"serviceError\",\"message\":\"Invalid request JSON.\"}"