-- | Fork choice rule for 3SF-mini, aligned with leanSpec forkchoice/store.py.
-- Implements greedy heaviest observed subtree (GHOST) with interval-based ticking.
module Consensus.ForkChoice
  ( -- * Errors
    ForkChoiceError (..)
    -- * Store operations
  , initStore
  , onBlock
  , onAttestation
    -- * Head selection
  , getHead
  , getWeight
  , getAncestor
  , isDescendant
  ) where

import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing, Down (..))

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition (stateTransition)
import SSZ.Common (mkBytesN, unBytesN)
import SSZ.List (mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data ForkChoiceError
  = BlockSlotTooOld Slot Slot
  | BlockSlotInFuture Slot Slot
  | OrphanBlock
  | ParentStateNotFound
  | StateTransitionFailed String
  | AttestationSlotInFuture Slot Slot
  | AttestationTargetNotFound
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Store initialization
-- ---------------------------------------------------------------------------

-- | Initialize the store from genesis state, block, and config.
initStore :: BeaconState -> BeaconBlock -> Config -> Store
initStore genesisState genesisBlock cfg =
  let blockRoot = toRoot genesisBlock
      checkpoint = Checkpoint blockRoot 0
      emptyAttSigs = forceRight $ mkSszList @MAX_ATTESTATIONS []
  in  Store
    { stTime                  = 0
    , stConfig                = cfg
    , stJustifiedCheckpoint   = checkpoint
    , stFinalizedCheckpoint   = checkpoint
    , stHead                  = blockRoot
    , stSafeTarget            = checkpoint
    , stBlocks                = Map.singleton blockRoot genesisBlock
    , stBlockStates           = Map.singleton blockRoot genesisState
    , stLatestMessages        = Map.empty
    , stAttestationSignatures = emptyAttSigs
    }

-- | Get the current slot from the store (derived from time).
storeCurrentSlot :: Store -> Slot
storeCurrentSlot store = stTime store `div` 5  -- INTERVALS_PER_SLOT = 5

-- ---------------------------------------------------------------------------
-- Block processing
-- ---------------------------------------------------------------------------

-- | Process a new signed block into the store.
onBlock :: Store -> SignedBlock -> Either ForkChoiceError Store
onBlock store signedBlock = do
  let block = sbMessage signedBlock
      blockSlot = bbSlot block
      parentRoot = bbParentRoot block
      currentSlot = storeCurrentSlot store

  -- Slot bounds
  if blockSlot > currentSlot
    then Left (BlockSlotInFuture blockSlot currentSlot)
    else Right ()

  -- Block must not be older than finalized checkpoint
  if blockSlot < cpSlot (stFinalizedCheckpoint store)
    then Left (BlockSlotTooOld blockSlot (cpSlot (stFinalizedCheckpoint store)))
    else Right ()

  -- Parent must exist
  if not (Map.member parentRoot (stBlocks store))
    then Left OrphanBlock
    else Right ()

  -- Get parent state
  parentState <- case Map.lookup parentRoot (stBlockStates store) of
    Nothing -> Left ParentStateNotFound
    Just s  -> Right s

  -- Run state transition
  postState <- case stateTransition parentState signedBlock False of
    Left err -> Left (StateTransitionFailed (show err))
    Right s  -> Right s

  let blockRoot = toRoot block

  -- Update store with block and state
  let store1 = store
        { stBlocks      = Map.insert blockRoot block (stBlocks store)
        , stBlockStates = Map.insert blockRoot postState (stBlockStates store)
        }

  -- Update justified/finalized checkpoints from post-state
  let store2 = updateCheckpoints store1 postState

  -- Update head
  let newHead = computeHead store2
      store3 = store2 { stHead = newHead }

  Right store3

-- | Update store checkpoints if post-state has newer justified/finalized.
updateCheckpoints :: Store -> BeaconState -> Store
updateCheckpoints store postState =
  let newJust = bsLatestJustified postState
      newFin  = bsLatestFinalized postState
      store1 = if cpSlot newJust > cpSlot (stJustifiedCheckpoint store)
               then store { stJustifiedCheckpoint = newJust }
               else store
      store2 = if cpSlot newFin > cpSlot (stFinalizedCheckpoint store1)
               then store1 { stFinalizedCheckpoint = newFin }
               else store1
  in  store2

-- ---------------------------------------------------------------------------
-- Attestation processing
-- ---------------------------------------------------------------------------

-- | Process a gossip-time individual attestation.
onAttestation :: Store -> SignedAttestation -> Either ForkChoiceError Store
onAttestation store sa = do
  let ad = saData sa
      attSlot = adSlot ad
      vi = saValidatorIndex sa
      currentSlot = storeCurrentSlot store

  if attSlot > currentSlot
    then Left (AttestationSlotInFuture attSlot currentSlot)
    else Right ()

  -- Update latest message
  let headRoot = cpRoot (adHead ad)
      store1 = updateLatestMessage store vi attSlot headRoot

  Right store1

-- | Update a single validator's latest message if the new one is newer.
updateLatestMessage :: Store -> ValidatorIndex -> Slot -> Root -> Store
updateLatestMessage store vi slot root =
  let msg = LatestMessage slot root
      shouldUpdate = case Map.lookup vi (stLatestMessages store) of
        Nothing  -> True
        Just old -> slot > lmSlot old
  in  if shouldUpdate
        then store { stLatestMessages = Map.insert vi msg (stLatestMessages store) }
        else store

-- ---------------------------------------------------------------------------
-- Head selection
-- ---------------------------------------------------------------------------

-- | Deterministic head selection via greedy heaviest observed subtree.
getHead :: Store -> Root
getHead = stHead

-- | Compute head by walking from justified root.
computeHead :: Store -> Root
computeHead store =
  let startRoot = cpRoot (stJustifiedCheckpoint store)
  in  walkHead store startRoot

walkHead :: Store -> Root -> Root
walkHead store current =
  let children = getChildren store current
      finRoot = cpRoot (stFinalizedCheckpoint store)
      validChildren = filter (\r -> r == finRoot || isDescendant store finRoot r) children
  in  case validChildren of
        []  -> current
        cs  ->
          let weighted = map (\c -> (c, getWeight store c)) cs
              best = fst $ maximumBy (comparing snd <> comparing (Down . unBytesN . fst)) weighted
          in  walkHead store best

-- | Get immediate children of a block root.
getChildren :: Store -> Root -> [Root]
getChildren store parentRoot =
  [ root
  | (root, block) <- Map.toList (stBlocks store)
  , bbParentRoot block == parentRoot
  ]

-- | Compute the weight of a subtree rooted at the given block.
-- In leanSpec, all validators have equal weight (1 each).
getWeight :: Store -> Root -> Gwei
getWeight store root =
  let msgs = Map.toList (stLatestMessages store)
  in  sum [ 1
          | (_vi, lm) <- msgs
          , let msgRoot = lmRoot lm
          , msgRoot == root || isDescendant store root msgRoot
          ]

-- | Get ancestor of a block at a target slot.
getAncestor :: Store -> Root -> Slot -> Maybe Root
getAncestor store root targetSlot =
  case Map.lookup root (stBlocks store) of
    Nothing -> Nothing
    Just block
      | bbSlot block == targetSlot -> Just root
      | bbSlot block < targetSlot  -> Nothing
      | otherwise                  -> getAncestor store (bbParentRoot block) targetSlot

-- | Check if `descendant` is a descendant of `ancestor`.
isDescendant :: Store -> Root -> Root -> Bool
isDescendant store ancestor descendant
  | ancestor == descendant = True
  | otherwise =
      case Map.lookup descendant (stBlocks store) of
        Nothing    -> False
        Just block -> isDescendant store ancestor (bbParentRoot block)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"
