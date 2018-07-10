{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module TestState
  ( TestState(..)
  , WriteState(..)
  , QueryState(..)
  ) where

import           Codec.Serialise
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid.CBOR
import           GHC.Generics
import qualified Test.Tasty.QuickCheck          as QC

data TestState a = TestState Int a
    deriving (Show, Generic)

instance Serialise a => Serialise (TestState a)

writeState :: Int -> a -> Update (TestState a) ()
writeState newValue newValue'
    = put (TestState newValue newValue')

queryState :: Query (TestState a) Int
queryState = do TestState string _ <- ask
                return string

$(makeAcidic ''TestState ['writeState, 'queryState])

deriving instance Eq a => Eq (WriteState a)
deriving instance Show a => Show (WriteState a)

instance QC.Arbitrary a => QC.Arbitrary (WriteState a) where
  arbitrary = WriteState <$> QC.arbitrary <*> QC.arbitrary
