{-|
Module: Game.VoxelVerse.Types
Description: Shared VoxelVerse data model.

This module is reserved for the shared data structures that represent the
VoxelVerse view exposed to clients.

Its job will be to define the stable output-side vocabulary used by the new
query API, including any common identifiers, containers, elements, subset
payloads, and metadata needed by clients.

Design notes:

- The VoxelVerse structures should be derived views over authoritative game
  state, not a second mutable state model.
- VoxelVerse responses will usually be sparse or partial representations of
  information that can be generated from a full 'GameState'.
- No VoxelVerse structure should become a hidden source of truth that stores
  gameplay state unavailable from, or inconsistent with, the underlying
  'GameState'.
- This module should not depend on 'Game.Core.Types' or any ruleset-specific
  game state type. It defines the shared VoxelVerse data model only.
 -}
module Game.VoxelVerse.Types
    (
        VoxelVerseState (..),
        VoxelVerseDelta,
        ToolApplication,
        VoxelVerseInteractionState (..),
        VoxelVerseProjectionState (..),
        VoxelVerseView,
        VoxelVerseSession (..),
        VoxelVersePlayerSession (..),
        SessionState (..)
    )
where

import qualified Game.ArtOfWar.VoxelVerse.Types as ArtOfWar
import qualified Game.CursedTreasure.VoxelVerse.Types as CursedTreasure
import qualified Game.FogOfBattle.VoxelVerse.Types as FogOfBattle
import qualified Game.RealEstate.VoxelVerse.Types as RealEstate
import qualified Game.Core.Primitives as Primitives
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data VoxelVerseInteractionState
    = CursedTreasureVoxelVerseInteractionState CursedTreasure.InteractionState
    | FogOfBattleVoxelVerseInteractionState FogOfBattle.InteractionState
    | ArtOfWarVoxelVerseInteractionState ArtOfWar.InteractionState
    | RealEstateVoxelVerseInteractionState RealEstate.InteractionState

data VoxelVerseProjectionState
    = CursedTreasureVoxelVerseProjectionState CursedTreasure.ProjectionState
    | FogOfBattleVoxelVerseProjectionState FogOfBattle.ProjectionState
    | ArtOfWarVoxelVerseProjectionState ArtOfWar.ProjectionState
    | RealEstateVoxelVerseProjectionState RealEstate.ProjectionState

-- **** Plain Voxel definition ****
-- We take it that a property name is a Text, and we define the property value as a 
-- limited set of options:
data VoxelPropertyValue = NullProperty
    | TrueProperty
    | IntProperty Int
    | DoubleProperty Double
    | TextProperty Text
    | ListProperty [VoxelPropertyValue]
    | KeyValueProperty PropertySet

-- Then the key-value labeled set of properties is nothing but a Text to VoxelPropertyValue map:
newtype PropertySet = PropertySet (Map Text VoxelPropertyValue)

propertySetUnion :: PropertySet -> PropertySet -> PropertySet
propertySetUnion (PropertySet left) (PropertySet right) = PropertySet (left <> right)

propertySetInsert :: Text -> VoxelPropertyValue -> PropertySet -> PropertySet
propertySetInsert key value (PropertySet properties) = PropertySet (Map.insert key value properties)

propertySetDifference :: PropertySet -> PropertySet -> PropertySet
propertySetDifference (PropertySet left) (PropertySet right) = PropertySet (Map.difference left right)

propertySetDelete :: Text -> PropertySet -> PropertySet
propertySetDelete key (PropertySet properties) = PropertySet (Map.delete key properties)

-- In Haskell, data structures are shared, so there will be no problem
-- keeping the PropertySet directly in the Voxel even multiplied by millions of references, 
-- When creating the view to be serialized, to resolve dereferencing, we will
-- remap the layers to contain handles which can be used to look up a PropertySet.
-- This does imply we should try to assign the same PropertySet to voxels over and over, not
-- rebuild PropertySets over and over, when possible. To accellerate searching for logically
-- identical Voxels, producing the view will require the PropertySetHashmap.

-- Notionally, a HexVoxel is defined like the following data structure:
-- data HexVoxel = HexVoxel {
--     q :: Int, -- Cube Coordinate q
--     r :: Int, -- Cube Coordinate r
--     b :: Int, -- The Bottom altitude of the hex in z
--     t :: Int, -- The Top altitude of the hex in z
--     enabled :: Bool, -- Whether the HexVoxel is enabled for the current Tool
--     -- And then each voxel has to contain an arbitrary set of additional properties
--     properties :: PropertySet
-- }
-- HOWEVER, "rendering" a view is obviously going to be walking the indexes, or presenting the voxels
-- sparsely like in the SparseHexVoxels defined below, so we don't need to track q and r within the
-- HexVoxel itself.  Moreover, the bottom altitude and the top altitude are conditionally needed for 
-- hexes NOT simply on the 0 z plane. Moreover, the HexVoxel being _enabled_ is ALSO a property that 
-- properly only belongs to some hex voxels, while most others can be disabled by default. Since enabled 
-- with value (TrueProperty) can be handled as a mutation of the PropertySet, it turns
-- out nothing remains in the HexVoxel data structure _except_ the properties:
-- type HexVoxel = PropertySet
-- And since there is nothing at this point distinguishing it from an arbitrary geometry Voxel:
type Voxel = PropertySet

-- **** VIEW Voxel Representation ****
-- We envision for this architecture that the number of HexVoxel objects could be grids of 
-- perhaps 3600 by 3600. Thus the HexVoxel _view_ should be lightweight versus properties, and we
-- should be able to compress it using property handles. So we define a property set handle
-- which is basically and notionally close to an index into an array of property sets:
type PropertySetHandle = Int

-- Now each Voxel in the _view_ (which gets serialized) should not literally contain a PropertySet 
-- because many terrain voxels or voxels part of a larger structure will have identical property sets. 
-- So we need another map to intern the property sets using the PropertySetHandle when constructing the view:
newtype PropertySets = PropertySets (Map PropertySetHandle PropertySet)

-- Morover, when in order to effciently compare a Voxel with others and to detect identical voxels,
-- we will need to map a property set hash back to candidate equal PropertySetHandles:
type PropertySetHash = Int
newtype PropertySetHashmap = PropertySetHashedHandles (Map PropertySetHash [PropertySetHandle])

-- **** PropertySet Construction ****
-- Now to build up useful PropertySets, we will wish to compose named property groups.
-- So for the convenience of PropertySet constructions we define:
type PropertyGroupName = Text
newtype PropertyGroups = PropertyGroups (Map PropertyGroupName PropertySet)

-- And so ultimately we construct compressed property sets by composing property concepts.
-- ConceptIs carries the idea that we specialize the concept to say "it is also this".
-- ConceptHas carries the idea that we specialize the concept to say "it also has this".
-- ConceptIsNot or HasNot carries the idea that we simplify the concept to remove specificity.
-- For example, if we wish to enable a Voxel for the current tool: ConceptIs enabled
-- Or if we wish to add an inventory: ConceptHas "storage" inventory
-- Or if we wish to remove the concept that it can be driven: ConceptIsNot driveable
data ConceptOperation concept = ConceptIs concept
    | ConceptHas Text concept
    | ConceptIsNot concept
    | ConceptHasNot Text concept

instance Functor ConceptOperation where
    fmap f (ConceptIs concept) = ConceptIs (f concept)
    fmap f (ConceptHas name concept) = ConceptHas name (f concept)
    fmap f (ConceptIsNot concept) = ConceptIsNot (f concept)
    fmap f (ConceptHasNot name concept) = ConceptHasNot name (f concept)

-- evaluateConceptOperations takes a list of ConceptOperation PropertySet, and applies the logical
-- operations as a foldr on an empty map.
evaluateConceptOperations :: [ConceptOperation PropertySet] -> PropertySet
evaluateConceptOperations = foldr foldfn (PropertySet Map.empty)
    where   foldfn (ConceptIs c) m = propertySetUnion m c
            foldfn (ConceptHas name (PropertySet c)) m
                | length c == 1 = propertySetInsert name (snd $ Map.elemAt 0 c) m
                | otherwise = propertySetInsert name (KeyValueProperty $ PropertySet c) m
            foldfn (ConceptIsNot c) m = propertySetDifference m c
            foldfn (ConceptHasNot name _) m = propertySetDelete name m

substituteConceptGroups :: PropertyGroups -> PropertyGroups -> [ConceptOperation Text]
    -> [ConceptOperation PropertySet]
substituteConceptGroups (PropertyGroups groups) (PropertyGroups newGroups) = map (fmap lookupGroup)
    where   lookupGroup name = fromMaybe (PropertySet Map.empty)
                (Map.lookup name groups <|> Map.lookup name newGroups)

newtype NamedPropertyOps concept = NamedPropertyOps [(Text, ConceptOperation concept)]

instance Semigroup (NamedPropertyOps concept) where
    NamedPropertyOps left <> NamedPropertyOps right = NamedPropertyOps 
        $ left ++ right

instance Monoid (NamedPropertyOps concept) where
    mempty = NamedPropertyOps mempty


-- One thing we notice about the property groups is that because they are "global", and identified
-- by the global PropertyGroupName, it is a monoid. Property groups could be "forgotten", but they
-- cannot be destroyed. Moreover, any newly created PropertyGroup will either be brand new or
-- exactly duplicate of another PropertyGroup.

instance Semigroup PropertyGroups where
    PropertyGroups left <> PropertyGroups right = PropertyGroups $ left <> right

instance Monoid PropertyGroups where
    mempty = PropertyGroups mempty

-- Naturally, "rendering" a view of the Voxel results in an array of Voxel objects 
-- with Cube Coordinate positions
type QRCoord = (Int, Int)
-- We may choose to have a sparse set:
type SparseHexVoxels = Map QRCoord Voxel

-- Or perhaps more likely a dense array
type FirstRowIndex = Int
type FirstColumnIndex = Int
type VoxelRow voxelType = (FirstRowIndex, [voxelType])
type VoxelBlock voxelType = (FirstColumnIndex, [VoxelRow voxelType])

-- But there is also a need to track changes from one game state to the next, which we notionally
-- expect to change a small fraction of the voxels. Moreover, we need to track voxel destruction,
-- creation, update, and movement. 

-- There are at least two passes over the Voxel Deltas. The VoxelOption is designed to support the 
-- initial VoxelMutaion as the result of a ordered composition. The monoid simply preserves the
-- ultimate from->to outcome. Obviously, either transitioning from or to nothing (NoVoxel) is an option.
-- NamedVoxel Text links to one of the NamedConceptOps which are also serially composed.
-- The NamedConceptOps serial composition is a composition at the property level, not the voxel level.
-- JustVoxel is an existing Voxel, either from previous passes, or not composed via NamedConceptOps. 
data VoxelOption = NoVoxel | NamedVoxel Text | JustVoxel Voxel
data VoxelMutation voxelType = VoxelMutation voxelType voxelType

instance Semigroup (VoxelMutation voxelType) where
    VoxelMutation l _ <> VoxelMutation _ r = VoxelMutation l r

instance Monoid (VoxelMutation VoxelOption) where
    mempty = VoxelMutation NoVoxel NoVoxel

instance Monoid (VoxelMutation PropertySetHandle) where
    mempty = VoxelMutation 0 0

-- We store the mutations sparsely:
newtype VoxelMutations voxelType = VoxelMutations (Map QRCoord (VoxelMutation voxelType))

instance Semigroup (VoxelMutations voxelType) where
    VoxelMutations left <> VoxelMutations right =
        VoxelMutations (Map.unionWith (<>) left right)

instance Monoid (VoxelMutations voxelType) where
    mempty = VoxelMutations mempty

-- We also want to track movements, but if we know where the voxel moves to, then the FromQRCoord is the
-- only content. Swapping Voxels is the same as two VoxelMovements, one the reverse of the other.
type FromQRCoord = QRCoord
-- We store the movements sparsely as well:
newtype VoxelMovements = VoxelMovements (Map QRCoord FromQRCoord)

instance Semigroup VoxelMovements where
    VoxelMovements bToC <> VoxelMovements aToB = VoxelMovements $ Map.union aToC (Map.map moveBToC aToB)
        where   moveBToC b = fromMaybe b $ Map.lookup b bToC
                found = Set.fromList $ Map.elems $ Map.filter (`Map.member` bToC) aToB
                aToC = Map.restrictKeys bToC $ Set.difference (Set.fromList (Map.keys bToC)) found

instance Monoid VoxelMovements where
    mempty = VoxelMovements mempty

-- Since our PropertySetHandles do NOT need to be 0 based for any particular reason, let us define:
-- Voxel == 0 as an empty hex, then for the sake of most likely GUI presentations, the VoxelBlock
-- serves as a fine presentation layer. We expect, furthermore, that the GUI will have other layers
-- of presentation taking precedence over all game layers, so we give the GUI all layer numbers above 0.
-- Moreover, we define presentation layer 0 as the HUD. Thus, higher numbers are always displayed first,
-- and we expect the GUI to hide layers when lower layers are being interacted with.
hudPresentationLayer :: Integer
hudPresentationLayer = 0

-- We expect all games to have at least two layers or more, with higher numbers being displayed on top
-- when visible. It is a sort of "logical" Z buffer. And so the "first" game layer is nominally at -1.
-- Usually, we imagine the Player Legend, Information, and Meta-Tool HUD will be layer 0. 
-- In addition, for every layer, we define a concept called a tool. The idea is that a user
-- _always_ has a currently active tool. Even when the player "has no tool" we imagine that the
-- player has the "hand" tool equipped. Matching this concept, we dub all voxels that can be 
-- manipulated by the tool to be valid sockets.  So for the currently equipped tool, the set of valid 
-- sockets is exactly the set of voxels with the property ("enabled", TrueProperty).
-- Now since the PropertySet is already a nicely expressive way to represent an arbitrary
-- complex of properties, and since the tools will almost always be a result of the "hand" tool picking
-- up a "tool" voxel in the GUI HUD, we will leverage the same data structure:
type SelectedTool = PropertySetHandle

-- Now a given game rule set should be sufficiently represented by a VoxelVerseState and a VoxelVerseDelta.
-- This _should_ be a complete description of the state of the voxels in each layer AND 
-- describe how the layer changed compared to the layer state from the previous GameState before 
-- the latest move.
-- Therefore, this VoxelLayerDelta contains a sequence of modifications to the voxels that resulted in the
-- current voxels, namely: 
-- * voxelMutateBefore occured first, which updated the voxel properties before any voxels moved,
-- * then voxelMove, which tracks where each moved voxel came from and went to
-- * and last voxelMutateAfter, which updated the voxel properties after any voxels moved (and/or mutated).  
data VoxelLayerDelta = VoxelLayerDelta {
    voxelMutateBefore :: VoxelMutations VoxelOption,
    voxelMove :: VoxelMovements,
    voxelMutateAfter :: VoxelMutations VoxelOption
}

instance Semigroup VoxelLayerDelta where
    left <> right = VoxelLayerDelta
        { voxelMutateBefore = left.voxelMutateBefore <> right.voxelMutateBefore
        , voxelMove = left.voxelMove <> right.voxelMove
        , voxelMutateAfter = left.voxelMutateAfter <> right.voxelMutateAfter
        }

instance Monoid VoxelLayerDelta where
    mempty :: VoxelLayerDelta
    mempty = VoxelLayerDelta
        { voxelMutateBefore = mempty
        , voxelMove = mempty
        , voxelMutateAfter = mempty
        }


data VoxelLayerState = VoxelLayerState {
    layerName :: Text,
    presentationLayer :: Int,
    voxels :: VoxelBlock Voxel
}

data VoxelVerseState = VoxelVerseState {
    propertyGroups :: PropertyGroups,
    voxelLayers :: [VoxelLayerState],
    thisPlayer :: Primitives.PlayerId
}

data VoxelVerseDelta = VoxelVerseDelta {
    newGroups :: PropertyGroups,
    namedPropertyOps :: NamedPropertyOps Text,
    voxelDeltas :: Map Int VoxelLayerDelta
}

instance Semigroup VoxelVerseDelta where
    left <> right = VoxelVerseDelta
        { newGroups = left.newGroups <> right.newGroups
        , namedPropertyOps = left.namedPropertyOps <> right.namedPropertyOps
        , voxelDeltas = Map.unionWith (<>) left.voxelDeltas right.voxelDeltas
        }

instance Monoid VoxelVerseDelta where
    mempty = VoxelVerseDelta
        { newGroups = mempty
        , namedPropertyOps = mempty
        , voxelDeltas = mempty
        }

-- For the user to actually apply a tool though, we need to define a selection of Voxels. There are 
-- arguments to be made for both sparse selections AND block selections, so support both: 
data ToolSelection = SparseToolSelection (Set QRCoord)
    | DenseToolSelection FirstColumnIndex [(FirstRowIndex, Int)]

-- To fully apply a tool then is captured in a ToolApplication:
data ToolApplication = ToolApplication {
    toolLayer :: Int,
    appliedTool :: SelectedTool,
    appliedSelection :: ToolSelection
}

-- It is also important for large Game Universes, to be able for downstream logic to specify an 
-- optional "orthographic" viewport to enable focusing, limiting, and even zooming in or out of the game world.
-- However, since every game could define view ports differently, and definitely define zooming
-- differently, we would naturally define the viewport as the result of applying a tool. This also enables
-- lenses and transparency layers if some games need those features.
-- Having said all that, our ViewPort data structure would be:
-- data ViewPort = ViewPort {
--     viewportLayer :: Int,
--     viewportTool :: SelectedTool,
--     viewportSelection :: ToolSelection
-- }
-- But that is just a ToolApplication, in the end, so we don't need a type to encode it.

-- Unlike for the VoxelVerse data structure, we don't need to segregate the monoidal patch type
-- part (the Deltas) from the state, there is just one view for the layer. It includes
-- in addition the isPreviewMode flag which tells the downstream logic whether the deltas applied
-- prior to the current view, or if they are building on the current view in preview mode.
-- NOTE: This may be suspect, but for now it is hard to imagine a game needing to wanting to both
-- display the previous turn transition animations AND ghost highlights for the preview mode.
-- In fact, if the user is viewing the preview mode and cancels it, and it snaps back to the 
-- current GameState, showing the prior turn animations again, that seems just fine.
data VoxelLayerView = VoxelLayerView {
    layerName :: Text,
    presentationLayer :: Int,
    voxels :: VoxelBlock PropertySetHandle,
    isPreviewMode :: Bool,
    voxelMutateBefore :: VoxelMutations PropertySetHandle,
    voxelMove :: VoxelMovements,
    voxelMutateAfter :: VoxelMutations PropertySetHandle
}

-- Therefore, with the interaction and projection state, after delegating to the special case Game rules 
-- createVoxelVerseView, the resulting VoxelVerseView will be generated and passed along to
-- the downstream logic. This resembles the VoxelVerse data structure in most ways, but is
-- organized for downstream use and potentially filtered by the createVoxelVerseView function.
data VoxelVerseView = VoxelVerseView 
    { viewingPlayer :: Primitives.PlayerId
    , propertyGroups :: PropertyGroups
    , propertySets :: PropertySets
    , toolApplication :: ToolApplication
    , voxelLayers :: [VoxelLayerView]
    } 

-- Generally speaking, the unrestricted Core.GameState needs to be preserved and kept for future
-- calls to the underlying API, and for correct GameState computations. It may also be necessary to
-- revert to a previously committed state, so we need a copy of a previous state for that purpose.
-- Otherwise, the logic at the VoxelVerse level should be consistently using a CensoredGameState. 
data VoxelVerseSession contextType playerType = VoxelVerseSession
    { vvContext :: contextType
    , vvPlayerSessions :: [playerType]
    }

data VoxelVersePlayerSession contextType interactionType projectionType = VoxelVersePlayerSession
    { playerContext :: contextType
    , playerModel :: VoxelVerseState
    , playerModelDelta :: VoxelVerseDelta
    , playerInteractionState :: interactionType
    , playerProjectionState :: projectionType
    }

data SessionState interactionType projectionType = SessionState
    { voxelVerseState :: VoxelVerseState
    , voxelVerseInteractionState :: interactionType
    , voxelVerseProjectionState :: projectionType
    }
