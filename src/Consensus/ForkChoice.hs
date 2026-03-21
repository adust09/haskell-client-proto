-- | Fork choice rule for 3SF-mini: greedy heaviest observed subtree (GHOST).
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
import Data.List (foldl', maximumBy)
import Data.Ord (comparing, Down (..))

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition
    ( expandAggregationBits
    , isActiveValidator
    , checkSlashingConditions
    , stateTransition
    )
import SSZ.Common (mkBytesN, unBytesN)
import SSZ.List (unSszList)
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

-- | Initialize the store from genesis state and block.
initStore :: BeaconState -> BeaconBlock -> Store
initStore genesisState genesisBlock =
  let blockRoot = toRoot genesisBlock
      checkpoint = Checkpoint 0 blockRoot
  in  Store
    { stJustifiedCheckpoint = checkpoint
    , stFinalizedCheckpoint = checkpoint
    , stBlocks              = Map.singleton blockRoot genesisBlock
    , stBlockStates         = Map.singleton blockRoot genesisState
    , stLatestMessages      = Map.empty
    , stCurrentSlot         = 0
    , stVoteHistory         = Map.empty
    }

-- ---------------------------------------------------------------------------
-- Block processing
-- ---------------------------------------------------------------------------

-- | Process a new signed block into the store.
onBlock :: Store -> SignedBeaconBlock -> Either ForkChoiceError Store
onBlock store signedBlock = do
  let block = sbbBlock signedBlock
      blockSlot = bbSlot block
      parentRoot = bbParentRoot block

  -- Slot bounds
  if blockSlot > stCurrentSlot store
    then Left (BlockSlotInFuture blockSlot (stCurrentSlot store))
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

  -- Extract latest messages from block's aggregated attestations
  let store3 = updateLatestMessagesFromBlock store2 block postState

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

-- | Update latest messages from aggregated attestations in a block.
updateLatestMessagesFromBlock :: Store -> BeaconBlock -> BeaconState -> Store
updateLatestMessagesFromBlock store block postState =
  let atts = unSszList (bbbAttestations (bbBody block))
      validators = bsValidators postState
  in  foldl' (\s saa ->
        let ad = saaData saa
            subnetId = saaSubnetId saa
            voterIndices = expandAggregationBits validators subnetId (saaAggregationBits saa)
            headRoot = adHeadRoot ad
            attSlot = adSlot ad
        in  foldl' (\s' vi ->
              updateLatestMessage s' vi attSlot headRoot
            ) s voterIndices
      ) store atts

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
-- Attestation processing
-- ---------------------------------------------------------------------------

-- | Process a gossip-time individual attestation.
onAttestation :: Store -> SignedAttestation -> Either ForkChoiceError Store
onAttestation store sa = do
  let ad = saData sa
      attSlot = adSlot ad
      vi = saValidatorIndex sa

  if attSlot > stCurrentSlot store
    then Left (AttestationSlotInFuture attSlot (stCurrentSlot store))
    else Right ()

  -- Update latest message
  let store1 = updateLatestMessage store vi attSlot (adHeadRoot ad)

  -- Append to vote history and check slashing
  let history = Map.findWithDefault [] vi (stVoteHistory store1)
      store2 = store1
        { stVoteHistory = Map.insert vi (ad : history) (stVoteHistory store1) }

  -- Check slashing (evidence-only, no state mutation)
  case checkSlashingConditions history ad of
    Left _evidence -> Right store2  -- log evidence but continue
    Right ()       -> Right store2

-- ---------------------------------------------------------------------------
-- Head selection
-- ---------------------------------------------------------------------------

-- | Deterministic head selection via greedy heaviest observed subtree.
getHead :: Store -> Root
getHead store =
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
getWeight :: Store -> Root -> Gwei
getWeight store root =
  let msgs = Map.toList (stLatestMessages store)
  in  sum [ getValidatorWeight store vi
          | (vi, lm) <- msgs
          , let msgRoot = lmRoot lm
          , msgRoot == root || isDescendant store root msgRoot
          ]

-- | Get effective balance of an active non-slashed validator.
getValidatorWeight :: Store -> ValidatorIndex -> Gwei
getValidatorWeight store vi =
  let justRoot = cpRoot (stJustifiedCheckpoint store)
  in  case Map.lookup justRoot (stBlockStates store) of
        Nothing -> 0
        Just bs ->
          let validators = unSszList (bsValidators bs)
              idx = fromIntegral vi
          in  if idx < length validators
                then let v = validators !! idx
                     in  if isActiveValidator v (bsSlot bs)
                           then vEffectiveBalance v
                           else 0
                else 0

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
