module App.Protocol
    ( ServiceRequest (..)
    , ServiceResponse (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)

import qualified Game.Core.Types as Core

data ServiceRequest
    = GetGameSetupPlayers
    | CreateNewGame [Core.PlayerDescription] Int
    | EnumerateActivePlayerOptions Core.GameState
    | MakeMove Core.GameState Core.PlayerMove
    | HeuristicHint Int Core.GameState [Core.PlayerMove]
    | Summary Core.GameState
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ServiceResponse
    = GameSetupPlayers [(Text, [Core.PlayerDescription])]
    | NewGameCreated (Either Text (Core.GameState, [Core.CensoredGameState]))
    | ActivePlayerOptions [Core.PlayerMove]
    | MoveApplied (Core.GameState, [Core.CensoredGameState])
    | HintGenerated [(Int, Core.PlayerMove)]
    | SummaryGenerated Text
    | ServiceError Text
    deriving (Show, Eq, Generic, ToJSON)