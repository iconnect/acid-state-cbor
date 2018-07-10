{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           TestState

import           Data.Acid.Archive
import           Data.Acid.Common
import           Data.Acid.Core
import           Data.Acid.CBOR.Internal

import qualified Codec.Serialise            as CBOR
import qualified Codec.Serialise.Properties as CBOR
import qualified Data.ByteString.Lazy       as Lazy
import           Data.Coerce
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.QuickCheck      as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ Tasty.testGroup "serialiseIdentity"
    [ Tasty.testProperty "Checkpoint" (CBOR.serialiseIdentity :: Checkpoint () -> Bool)
    , Tasty.testProperty "Event"      (CBOR.serialiseIdentity :: WrappedEvent  -> Bool)
    , Tasty.testProperty "CBOREntry"  (CBOR.serialiseIdentity :: CBOREntry     -> Bool)
    , Tasty.testProperty "WriteState" (CBOR.serialiseIdentity :: WriteState () -> Bool)
    ]
  , Tasty.testGroup "flatTermIdentity"
    [ Tasty.testProperty "Checkpoint" (CBOR.flatTermIdentity :: Checkpoint () -> Bool)
    , Tasty.testProperty "Event"      (CBOR.flatTermIdentity :: WrappedEvent  -> Bool)
    , Tasty.testProperty "CBOREntry"  (CBOR.flatTermIdentity :: CBOREntry     -> Bool)
    , Tasty.testProperty "WriteState" (CBOR.flatTermIdentity :: WriteState () -> Bool)
    ]
  , Tasty.testGroup "hasValidFlatTerm"
    [ Tasty.testProperty "Checkpoint" (CBOR.hasValidFlatTerm :: Checkpoint () -> Bool)
    , Tasty.testProperty "Event"      (CBOR.hasValidFlatTerm :: WrappedEvent  -> Bool)
    , Tasty.testProperty "CBOREntry"  (CBOR.hasValidFlatTerm :: CBOREntry     -> Bool)
    , Tasty.testProperty "WriteState" (CBOR.hasValidFlatTerm :: WriteState () -> Bool)
    ]
  , Tasty.testProperty "Archiver round-trip" (archiverRoundTrip serialiseArchiver)
  ]

deriving instance Show s => Show (Checkpoint s)
deriving instance Eq s => Eq (Checkpoint s)

instance Tasty.Arbitrary s => Tasty.Arbitrary (Checkpoint s) where
  arbitrary = Checkpoint <$> Tasty.arbitrary <*> Tasty.arbitrary

instance CBOR.Serialise s => CBOR.Serialise (Checkpoint s) where
  encode = encodeCheckpoint
  decode = decodeCheckpoint


newtype WrappedEvent = WrappedEvent (Tagged Lazy.ByteString)
  deriving (Eq, Show)

instance Tasty.Arbitrary WrappedEvent where
  arbitrary = WrappedEvent <$> ((,) <$> (Lazy.pack <$> Tasty.arbitrary) <*> (Lazy.pack <$> Tasty.arbitrary))

instance CBOR.Serialise WrappedEvent where
  encode (WrappedEvent e) = encodeEvent e
  decode = WrappedEvent <$> decodeEvent


deriving instance Eq CBOREntry
deriving instance Show CBOREntry

instance Tasty.Arbitrary CBOREntry where
  arbitrary = CBOREntry . Lazy.pack <$> Tasty.arbitrary


-- | Test that concatenating multiple serialised lists of entries can
-- be deserialised into a flat list of entries
archiverRoundTrip :: Archiver -> [[CBOREntry]] -> Bool
archiverRoundTrip a xss0 =
    entriesToList (archiveRead a (mconcat (map (archiveWrite a) xss))) == concat xss
  where
    xss = coerce xss0 :: [[Entry]]
