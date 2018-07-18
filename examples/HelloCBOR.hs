{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Codec.Serialise
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid.CBOR
import           Data.Typeable
import           GHC.Generics
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data HelloCBORState a = HelloCBORState String a
    deriving (Show, Typeable, Generic)

instance Serialise a => Serialise (HelloCBORState a)

------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: String -> a -> Update (HelloCBORState a) ()
writeState newValue newValue'
    = put (HelloCBORState newValue newValue')

queryState :: Query (HelloCBORState a) String
queryState = do HelloCBORState string _ <- ask
                return string

$(makeAcidic ''HelloCBORState ['writeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          main' args

main' :: [String] -> IO ()
main' args = withMyState $ \ acid ->
          if null args
             then do string <- query acid QueryState
                     putStrLn $ "The state is: " ++ string
             else do update acid (WriteState (unwords args) ())
                     putStrLn "The state has been modified!"

withMyState :: (AcidState (HelloCBORState ()) -> IO a) -> IO a
withMyState = bracket (openLocalState (HelloCBORState "Hello world" ()))
                      closeAcidState
