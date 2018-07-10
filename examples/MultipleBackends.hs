{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Codec.Serialise
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Acid.CBOR
import           Data.Acid.TemplateHaskell
import           Data.SafeCopy
import           Data.Typeable
import           GHC.Generics
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data MultipleState s = MultipleState String
    deriving (Show, Typeable, Generic, Serialise)
$(deriveSafeCopy 0 'base ''MultipleState)


data CBORBackend = CBORBackend
    deriving (Generic, Serialise)
type CBORState     = MultipleState CBORBackend


data SafeCopyBackend = SafeCopyBackend
$(deriveSafeCopy 0 'base ''SafeCopyBackend)
type SafeCopyState = MultipleState SafeCopyBackend


------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: String -> Update (MultipleState s) ()
writeState newValue
    = put (MultipleState newValue)

queryState :: Query (MultipleState s) String
queryState = do MultipleState string <- ask
                return string

$(makeEvents ['writeState, 'queryState])
$(Data.Acid.CBOR.makeAcidicWithoutEvents ''CBORState ['writeState, 'queryState])
$(Data.Acid.TemplateHaskell.makeAcidicWithoutEvents ''SafeCopyState ['writeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          main' args

main' :: [String] -> IO ()
main' args =
          case args of
            "cbor":args'     -> withMyState @CBORBackend (main'' args')
            "safecopy":args' -> withMyState @SafeCopyBackend (main'' args')
            _                -> error "first argument should be 'cbor' or 'safecopy'"

main'' :: (IsAcidic (MultipleState s), Typeable s) => [String] -> AcidState (MultipleState s) -> IO ()
main'' args acid = case args of
  []             -> do string <- query acid QueryState
                       putStrLn $ "The state is: " ++ string
  ["checkpoint"] -> do createCheckpoint acid
                       putStrLn "Checkpoint created"
  _              -> do update acid (WriteState (unwords args))
                       putStrLn "The state has been modified!"

withMyState :: (IsAcidic (MultipleState s), Typeable s) => (AcidState (MultipleState s) -> IO a) -> IO a
withMyState = bracket (openLocalState (MultipleState "Hello world")) closeAcidState
