module Game.FogOfBattle.VoxelVerse.API
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
    )
where

import Game.FogOfBattle.VoxelVerse.Types
import Game.FogOfBattle.Types
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

computeNextGameState :: InnerVoxelVerseSession -> ToolApplication PropertySetHandle -> Either Text InnerVoxelVerseSession
computeNextGameState session tool = undefined

applyToolM :: ToolApplication PropertySetHandle -> SessionM ()
applyToolM _toolApplication = do
    lift (Left "applyToolM not implemented yet")

createVoxelVerseView :: InnerVoxelVersePlayerSession -> VoxelVerseView
createVoxelVerseView sess = undefined
