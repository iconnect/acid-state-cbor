{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Codec.Serialise
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid (makeAcidic)
import           Data.Acid.CBOR
import           Data.Acid.Local (openLocalStateWithSerialiser, defaultSerialisationLayer, defaultStateDirectory)
import           Data.SafeCopy
import           Data.Typeable
import           GHC.Generics
import           System.Environment

-- Define our state type, with a phantom type parameter to tag it with
-- a serialisation backend.  This must be an instance of both
-- 'Serialise' and 'SafeCopy'.
data MultipleState s = MultipleState String
    deriving (Show, Typeable, Generic, Serialise)
$(deriveSafeCopy 0 'base ''MultipleState)


-- Tag types for the different backends, used to instantiate the
-- phantom type parameter of 'MultipleState'.
data CBORBackend = CBORBackend
data SafeCopyBackend = SafeCopyBackend

type CBORState     = MultipleState CBORBackend
type SafeCopyState = MultipleState SafeCopyBackend


-- The tag types must have 'Serialise' and 'SafeCopy' instances, even
-- though these are not used at runtime, because acid-state will end
-- up generating instances like
--
-- > instance Serialise s => Serialise (WriteState s)
-- > instance SafeCopy  s => SafeCopy  (WriteState s)
--
-- (it doesn't have a way to know that the constraints in the instance
-- contexts are redundant).

deriving instance Generic   CBORBackend
deriving instance Serialise CBORBackend
$(deriveSafeCopy 0 'base ''SafeCopyBackend)


-- The transaction we will execute over the state.

writeState :: String -> Update (MultipleState s) ()
writeState newValue
    = put (MultipleState newValue)

queryState :: Query (MultipleState s) String
queryState = do MultipleState string <- ask
                return string


-- The first makeAcidic call will generate WriteState and QueryState
-- data types, plus a bunch of instances including
--
-- > instance IsAcidic CBORState
--
-- (which requires FlexibleInstances and TypeSynonymInstances).

$(Data.Acid.CBOR.makeAcidic ''CBORState     ['writeState, 'queryState])


-- The second makeAcidic call will notice that WriteState/QueryState
-- have been defined and not re-define them, so it merely generates
-- the SafeCopy instances for the methods, plus
--
-- > instance IsAcidic SafeCopyState

$(Data.Acid.makeAcidic      ''SafeCopyState ['writeState, 'queryState])


main :: IO ()
main = do args <- getArgs
          main' args

main' :: [String] -> IO ()
main' args =
          case args of
            "cbor"    :args' -> withMyState @CBORBackend     serialiseSerialisationLayer (main'' args')
            "safecopy":args' -> withMyState @SafeCopyBackend defaultSerialisationLayer   (main'' args')
            _                -> error "first argument should be 'cbor' or 'safecopy'"

-- The following operations are polymorphic in the serialisation
-- backend.  The function above instantiates the 's' parameter
-- differently to choose the concrete backend type.
withMyState :: forall s a . (IsAcidic (MultipleState s), Typeable s)
            => SerialisationLayer (MultipleState s) -> (AcidState (MultipleState s) -> IO a) -> IO a
withMyState sl = bracket (openLocalStateWithSerialiser directory initialState sl) closeAcidState
   where
     initialState :: MultipleState s
     initialState = MultipleState "Hello world"
     directory    = defaultStateDirectory initialState

main'' :: (IsAcidic (MultipleState s), Typeable s) => [String] -> AcidState (MultipleState s) -> IO ()
main'' args acid = case args of
  []             -> do string <- query acid QueryState
                       putStrLn $ "The state is: " ++ string
  ["checkpoint"] -> do createCheckpoint acid
                       putStrLn "Checkpoint created"
  _              -> do update acid (WriteState (unwords args))
                       putStrLn "The state has been modified!"
