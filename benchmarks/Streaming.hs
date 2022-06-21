{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           Codec.Serialise
import           Control.Monad
import           Control.Monad.Reader
import           Data.Acid as Acid
import qualified Data.Acid.CBOR as AcidCBOR
import qualified Data.Acid.CBOR.Internal as Internal
import qualified Data.Acid.Local as Internal
import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy
import           Data.Typeable
import           GHC.Generics
import           System.Directory
import           System.Environment

data StreamingCBOR = StreamingCBOR [Int]
    deriving (Show, Typeable, Generic)

instance Serialise StreamingCBOR

getLengthCBOR :: Query StreamingCBOR Int
getLengthCBOR = do StreamingCBOR xs <- ask
                   return $! length xs

$(AcidCBOR.makeAcidic ''StreamingCBOR ['getLengthCBOR])


data StreamingSafeCopy = StreamingSafeCopy [Int]
    deriving (Show, Typeable, Generic)

$(deriveSafeCopy 0 'base ''StreamingSafeCopy)

getLengthSafeCopy :: Query StreamingSafeCopy Int
getLengthSafeCopy = do StreamingSafeCopy xs <- ask
                       return $! length xs

$(Acid.makeAcidic ''StreamingSafeCopy ['getLengthSafeCopy])


{-
-- Serialising a CBOREntry containing a serialised Checkpoint [Int]
-- happens incrementally, and in constant space if we can GC the list
-- as we go (comment out the print)
main' :: IO ()
main' = do LBS.writeFile "foo" b
           print (length v)
  where
    c = Internal.Checkpoint 42 v
    v = [1..5000000] :: [Int]
    b = serialise (Internal.CBOREntry (Internal.serialiseCheckpoint c))
-}


main :: IO ()
main = do
    xs <- getArgs
    case xs of
      ["cbor",n]           -> do writeCBOR (read n)
                                 readCBOR
      ["cbor-write",n]     ->    writeCBOR (read n)
      ["cbor-read"]        ->    readCBOR

      ["safecopy",n]       -> do writeSafeCopy (read n)
                                 readSafeCopy
      ["safecopy-write",n] ->    writeSafeCopy (read n)
      ["safecopy-read"]    ->    readSafeCopy

      _ -> error "Streaming (cbor <n> | safecopy <n> | cbor-write <n> | cbor-read | safecopy-write <n> | safecopy-read)"

fpCBOR :: FilePath
fpCBOR = "state/Streaming-CBOR"

fpSafeCopy :: FilePath
fpSafeCopy = "state/Streaming-SafeCopy"

reset :: FilePath -> IO ()
reset fp = do exists <- doesDirectoryExist fp
              when exists $ removeDirectoryRecursive fp

writeCBOR :: Int -> IO ()
writeCBOR n = do reset fpCBOR
                 st <- AcidCBOR.openLocalStateFrom fpCBOR (StreamingCBOR [1..n])
                 x <- query st GetLengthCBOR
                 print x
                 createCheckpoint st
                 createCheckpoint st
                 createCheckpoint st
                 closeAcidState st

readCBOR :: IO ()
readCBOR = do st <- AcidCBOR.openLocalStateFrom fpCBOR (StreamingCBOR [])
              x <- query st GetLengthCBOR
              print x
              closeAcidState st

writeSafeCopy :: Int -> IO ()
writeSafeCopy n = do reset fpSafeCopy
                     st <- Acid.openLocalStateFrom fpSafeCopy (StreamingSafeCopy [1..n])
                     x <- query st GetLengthSafeCopy
                     print x
                     createCheckpoint st
                     createCheckpoint st
                     createCheckpoint st
                     closeAcidState st

readSafeCopy :: IO ()
readSafeCopy = do st <- Acid.openLocalStateFrom fpSafeCopy (StreamingSafeCopy [])
                  x <- query st GetLengthSafeCopy
                  print x
                  closeAcidState st


-- cabal new-build Streaming && dist-newstyle/build/x86_64-linux/ghc-8.4.3/acid-state-cbor-0.1.0.0/b/Streaming/build/Streaming/Streaming safecopy 5000000 +RTS -hy -s -xt  && /opt/ghc/8.4.3/bin/hp2ps -c Streaming.hp && ps2pdf Streaming.ps
