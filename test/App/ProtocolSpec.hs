module App.ProtocolSpec where

import Data.Aeson (FromJSON, ToJSON, Value, decodeStrict', eitherDecodeStrict', encode)
import qualified Data.ByteString as StrictByteString
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

        it "round-trips createNewGame with UTF-8 player names" $
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

        it "round-trips newGameCreated success with UTF-8 player names" $
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
    canonicalizeToValue (StrictByteString.toStrict (encode decodedValue)) `shouldBe` canonicalizeToValue exampleJson

canonicalizeToValue :: StrictByteString.ByteString -> Value
canonicalizeToValue exampleJson =
    case decodeStrict' exampleJson :: Maybe Value of
        Just value -> value
        Nothing -> error "example JSON must be valid"

sampleCreateNewGameRequest :: ServiceRequest
sampleCreateNewGameRequest = CreateNewGame samplePlayers 12345

samplePlayers :: [Core.PlayerDescription]
samplePlayers =
    [ Core.PlayerDescription
        { Core.playerRuleset = "Real Estate"
        , Core.playerId = fromMaybe (error "PlayerId 1 must be valid") (Core.mkPlayerId 1)
        , Core.playerName = "Jose Álvarez"
        , Core.playerAI = "Unassigned"
        , Core.playerColor = Core.Red
        }
    , Core.PlayerDescription
        { Core.playerRuleset = "Real Estate"
        , Core.playerId = fromMaybe (error "PlayerId 2 must be valid") (Core.mkPlayerId 2)
        , Core.playerName = "Miyu 星"
        , Core.playerAI = "Unassigned"
        , Core.playerColor = Core.Green
        }
    ]

sampleGameState :: Core.GameState
sampleGameState =
    Core.RealEstateGame
        (RealEstate.GameState
            { players = samplePlayers
            , turn = 0
            , activePlayer = sampleActivePlayer
            , latestMessage = ""
            , gameOver = True
            , seed = Core.mkSeedStream 0 0
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
            { players = samplePlayers
            , turn = 0
            , activePlayer = sampleActivePlayer
            , latestMessage = ""
            , gameOver = True
            , seed = Core.mkSeedStream 0 0
            })

sampleActivePlayer :: Core.PlayerId
sampleActivePlayer = fromMaybe (error "PlayerId 1 must be valid") (Core.mkPlayerId 1)

samplePlayerMove :: Core.PlayerMove
samplePlayerMove = Core.RealEstatePlayerMove RealEstate.NoOpMove

getGameSetupPlayersRequestJson :: StrictByteString.ByteString
getGameSetupPlayersRequestJson = utf8Json "{\"request\":\"getGameSetupPlayers\"}"

createNewGameRequestJson :: StrictByteString.ByteString
createNewGameRequestJson = utf8Json "{\"request\":\"createNewGame\",\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"randomSeed\":12345}"

enumerateOptionsRequestJson :: StrictByteString.ByteString
enumerateOptionsRequestJson = utf8Json "{\"request\":\"enumerateActivePlayerOptions\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}}}"

makeMoveRequestJson :: StrictByteString.ByteString
makeMoveRequestJson = utf8Json "{\"request\":\"makeMove\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},\"playerMove\":{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}}"

heuristicHintRequestJson :: StrictByteString.ByteString
heuristicHintRequestJson = utf8Json "{\"request\":\"heuristicHint\",\"level\":2,\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},\"playerMoves\":[{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}]}"

summaryRequestJson :: StrictByteString.ByteString
summaryRequestJson = utf8Json "{\"request\":\"summary\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}}}"

gameSetupPlayersResponseJson :: StrictByteString.ByteString
gameSetupPlayersResponseJson = utf8Json "{\"response\":\"gameSetupPlayers\",\"rulesets\":[{\"name\":\"Cursed Treasure\",\"playerTemplates\":[{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":1,\"playerName\":\"Player PlayerId 1\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":2,\"playerName\":\"Player PlayerId 2\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":3,\"playerName\":\"Player PlayerId 3\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Blue\"},{\"playerRuleset\":\"Cursed Treasure\",\"playerId\":4,\"playerName\":\"Player PlayerId 4\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Purple\"}]},{\"name\":\"Fog Of Battle\",\"playerTemplates\":[]},{\"name\":\"Art Of War\",\"playerTemplates\":[]},{\"name\":\"Real Estate\",\"playerTemplates\":[]}]}"

newGameCreatedSuccessJson :: StrictByteString.ByteString
newGameCreatedSuccessJson = utf8Json "{\"response\":\"newGameCreated\",\"status\":\"ok\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},\"playerViews\":[{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}}]}"

newGameCreatedErrorJson :: StrictByteString.ByteString
newGameCreatedErrorJson = utf8Json "{\"response\":\"newGameCreated\",\"status\":\"error\",\"message\":\"PlayerDescription list must be non-empty and all match the same ruleset.\"}"

activePlayerOptionsResponseJson :: StrictByteString.ByteString
activePlayerOptionsResponseJson = utf8Json "{\"response\":\"activePlayerOptions\",\"playerMoves\":[]}"

moveAppliedResponseJson :: StrictByteString.ByteString
moveAppliedResponseJson = utf8Json "{\"response\":\"moveApplied\",\"gameState\":{\"tag\":\"RealEstateGame\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},\"playerViews\":[{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}},{\"tag\":\"RealEstateCensoredGameState\",\"contents\":{\"players\":[{\"playerRuleset\":\"Real Estate\",\"playerId\":1,\"playerName\":\"Jose Álvarez\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Red\"},{\"playerRuleset\":\"Real Estate\",\"playerId\":2,\"playerName\":\"Miyu 星\",\"playerAI\":\"Unassigned\",\"playerColor\":\"Green\"}],\"turn\":0,\"activePlayer\":1,\"latestMessage\":\"\",\"gameOver\":true,\"seed\":[0,0]}}]}"

hintGeneratedResponseJson :: StrictByteString.ByteString
hintGeneratedResponseJson = utf8Json "{\"response\":\"hintGenerated\",\"hints\":[{\"score\":0,\"playerMove\":{\"tag\":\"RealEstatePlayerMove\",\"contents\":[]}}]}"

summaryGeneratedResponseJson :: StrictByteString.ByteString
summaryGeneratedResponseJson = utf8Json "{\"response\":\"summaryGenerated\",\"summary\":\"undefined\"}"

serviceErrorResponseJson :: StrictByteString.ByteString
serviceErrorResponseJson = utf8Json "{\"response\":\"serviceError\",\"message\":\"Invalid request JSON.\"}"

utf8Json :: Text -> StrictByteString.ByteString
utf8Json = encodeUtf8