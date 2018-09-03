{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module instantiates the general framework in
-- 'Data.Acid.StateMachineTest' with an acid-state component that
-- implements a simple key-value store.
module KeyValueStateMachine (prop_sequential, prop_parallel) where

import           Codec.Serialise
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid.CBOR as AcidCBOR
import qualified Data.Acid.Local as Local
import           Data.Acid.StateMachineTest hiding (acidStateInterface)
import qualified Data.Map as Map
import           Data.Typeable
import           GHC.Generics
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Directory (removeDirectoryRecursive)


acidStateInterface :: (AcidCBOR.IsAcidic s, Serialise s, Typeable s, Show s)
                   => FilePath -> AcidStateInterface s
acidStateInterface fp =
    AcidStateInterface { openState            = AcidCBOR.openLocalStateFrom fp
                       , closeState           = AcidCBOR.closeAcidState
                       , checkpointState      = AcidCBOR.createCheckpoint
                       , checkpointCloseState = Local.createCheckpointAndClose
                       , resetState           = removeDirectoryRecursive fp
                                                  `catch` (\ (_ :: IOException) -> return ())
                       , statePath            = fp
                       }

deriving instance Generic Bomb
deriving instance Serialise Bomb


type Key = Int
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Generic, Eq, Show, Typeable, Serialise)

-- | Insert a key into the key-value store.
insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

-- | A slightly more complicated update transaction: reverse the value
-- at the given key, and return the resulting state.  Crucially, this
-- is non-idempotent, unlike 'insertKey'.
reverseKey :: Key -> Update KeyValue KeyValue
reverseKey key
    = do KeyValue m <- get
         let r = KeyValue (Map.adjust reverse key m)
         put r
         return r

-- | An update that may fail: reverse the value at the given key, or
-- fail if it is missing.
reverseKeyOrFail :: Key -> Bomb -> Update KeyValue ()
reverseKeyOrFail key _
    = do KeyValue m <- get
         case Map.lookup key m of
           Nothing  -> failUpdate "key not in map"
           Just val -> put (KeyValue (Map.insert key (reverse val) m))

-- | An update that attempts to put an undefined state.  This transaction should
-- simply fail and not modify the state.
breakState :: Update KeyValue ()
breakState = put (throw (TransactionError "broken state"))

-- | An update that puts a partially-defined state.  Unfortunately
-- acid-state does not handle this case gracefully, and will fail with
-- 'BlockedIndefinitelyOnMVar' (see #38).  Thus this update is not
-- included in 'keyValueCommands' below.
breakState2 :: Update KeyValue ()
breakState2 = put (KeyValue (Map.singleton 1 (throw (TransactionError "broken state"))))

-- | Look up a key from the store.
lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

-- | Look up a key from the store, or fail if it is missing.
lookupKeyOrFail :: Key -> Bomb -> Query KeyValue Value
lookupKeyOrFail key _
    = do KeyValue m <- ask
         maybe (failQuery "key not in map") return (Map.lookup key m)

-- | Query the current value of the state.  This is not used in the
-- generated commands, but is used for checking the state we get back
-- in 'prop_restore_old_state_1' etc.
askState :: Query KeyValue KeyValue
askState = ask

$(makeAcidic ''KeyValue ['insertKey, 'reverseKey, 'reverseKeyOrFail, 'breakState, 'breakState2, 'lookupKey, 'lookupKeyOrFail, 'askState])

deriving instance Generic InsertKey
deriving instance Generic ReverseKey
deriving instance Generic ReverseKeyOrFail
deriving instance Generic BreakState
deriving instance Generic LookupKey
deriving instance Generic LookupKeyOrFail

deriving instance Show InsertKey
deriving instance Show ReverseKey
deriving instance Show ReverseKeyOrFail
deriving instance Show BreakState
deriving instance Show LookupKey
deriving instance Show LookupKeyOrFail

instance NFData InsertKey
instance NFData ReverseKey
instance NFData ReverseKeyOrFail
instance NFData BreakState
instance NFData LookupKey
instance NFData LookupKeyOrFail

genKey :: Gen Key
genKey = Gen.int (Range.constant 1 10)

genValue :: Gen Value
genValue = Gen.string (Range.constant 0 10) Gen.alphaNum

keyValueCommands :: MonadIO m => [Command Gen m (Model KeyValue)]
keyValueCommands = [ acidUpdate        (InsertKey        <$> genKey <*> genValue)
                   , acidUpdate        (ReverseKey       <$> genKey)
                   , acidUpdateMayFail (ReverseKeyOrFail <$> genKey <*> genBomb)
                   , acidUpdateMayFail (pure BreakState)
                   , acidQuery         (LookupKey        <$> genKey)
                   , acidQueryMayFail  (LookupKeyOrFail  <$> genKey <*> genBomb)
                   ]

-- | Possible initial states; because of #20 we can currently only use
-- one of these when testing the properties.
initialStates :: [KeyValue]
initialStates = [ KeyValue Map.empty
                , KeyValue (Map.singleton 1 "foo")
                ]

prop_sequential :: Property
prop_sequential = acidStateSequentialProperty (acidStateInterface fp) (pure (head initialStates)) (Range.linear 1 10) keyValueCommands
  where
    fp = "state/KeyValueSequentialTest"

prop_parallel :: Property
prop_parallel = acidStateParallelProperty (acidStateInterface fp) (pure (head initialStates)) (Range.linear 1 10) (Range.linear 1 10) keyValueCommands
  where
    fp = "state/KeyValueParallelTest"
