{-# LANGUAGE TemplateHaskell    #-}

module Data.Acid.CBOR.Internal where

import qualified Codec.Serialise     as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR
import           Control.Exception (displayException)
import           Control.Monad (unless)
import           Data.Acid.Archive as Archive (Archiver(..), Entries(..), Entry)
import           Data.Acid.Common (Checkpoint(..))
import           Data.Acid.Core (Serialiser(..), Tagged)
import           Data.Acid.CRC (crc16)
import           Data.Acid.TemplateHaskell (SerialiserSpec(..), mkCxtFromTyVars, analyseType, toStructName, allTyVarBndrNames, makeAcidicWithSerialiser, makeAcidicWithSerialiser')
import qualified Data.ByteString.Lazy as Lazy
import           Data.Monoid ((<>))
import           GHC.Generics (Generic)
import           Language.Haskell.TH (Q, Dec, Name, Type(..), instanceD, standaloneDerivD)

serialiseSerialiser :: CBOR.Serialise a => Serialiser a
serialiseSerialiser = Serialiser CBOR.serialise (either (Left . displayException) Right . CBOR.deserialiseOrFail)

checkpointSerialiseSerialiser :: CBOR.Serialise st => Serialiser (Checkpoint st)
checkpointSerialiseSerialiser = Serialiser serialiseCheckpoint deserialiseCheckpoint

serialiseCheckpoint :: CBOR.Serialise st => Checkpoint st -> Lazy.ByteString
serialiseCheckpoint = CBOR.toLazyByteString . encodeCheckpoint

encodeCheckpoint :: CBOR.Serialise st => Checkpoint st -> CBOR.Encoding
encodeCheckpoint (Checkpoint eid bs) =
  CBOR.encodeListLen 2 <> CBOR.encodeInt eid <> CBOR.encodeTag 24 <> CBOR.encode bs

deserialiseCheckpoint :: CBOR.Serialise st => Lazy.ByteString -> Either String (Checkpoint st)
deserialiseCheckpoint bs = case CBOR.deserialiseFromBytes decodeCheckpoint bs of
  Left err -> Left (displayException err)
  Right (_, c) -> Right c

decodeCheckpoint :: CBOR.Serialise st => CBOR.Decoder s (Checkpoint st)
decodeCheckpoint = CBOR.decodeListLen *> (Checkpoint <$> CBOR.decodeInt <*> (CBOR.decodeTag *> CBOR.decode))

eventSerialiseSerialiser :: Serialiser (Tagged Lazy.ByteString)
eventSerialiseSerialiser = Serialiser serialiseEvent deserialiseEvent

serialiseEvent :: Tagged Lazy.ByteString -> Lazy.ByteString
serialiseEvent = CBOR.toLazyByteString . encodeEvent

encodeEvent :: Tagged Lazy.ByteString -> CBOR.Encoding
encodeEvent (tag, bs) = CBOR.encodeListLen 2 <> CBOR.encodeBytes (Lazy.toStrict tag) <> CBOR.encode bs

deserialiseEvent :: Lazy.ByteString -> Either String (Tagged Lazy.ByteString)
deserialiseEvent bs = case CBOR.deserialiseFromBytes decodeEvent bs of
  Left err     -> Left (displayException err)
  Right (_, e) -> Right e

decodeEvent :: CBOR.Decoder s (Tagged Lazy.ByteString)
decodeEvent = CBOR.decodeListLen *> ((,) <$> (Lazy.fromStrict <$> CBOR.decodeBytes) <*> CBOR.decode)


serialiseArchiver :: Archiver
serialiseArchiver = Archiver (mconcat . map (CBOR.serialise . CBOREntry)) deserialiseEntries

-- AMG TODO: needs thought about better incremental support
deserialiseEntries :: Lazy.ByteString -> Entries
deserialiseEntries lbs
  | Lazy.null lbs = Archive.Done
  | otherwise     = case CBOR.deserialiseFromBytes CBOR.decode lbs of
                      Left err                -> Archive.Fail (displayException err)
                      Right (leftover, entry) -> Archive.Next (fromCBOREntry entry) (deserialiseEntries leftover)

newtype CBOREntry = CBOREntry { fromCBOREntry :: Entry }

instance CBOR.Serialise CBOREntry where
  encode (CBOREntry b) = CBOR.encodeListLen 2 <> CBOR.encodeTag 24 <> CBOR.encode b <> CBOR.encodeWord16 (crc16 b)
  decode = do
    _ <- CBOR.decodeListLen
    _ <- CBOR.decodeTag
    b <- CBOR.decode
    c <- CBOR.decodeWord16
    unless (crc16 b == c) $ fail "CRC mismatch"
    return (CBOREntry b)


makeSerialiseInstance :: Name -> Type -> Q [Dec]
makeSerialiseInstance eventName eventType
    = do let ty n = AppT (ConT n) (foldl AppT (ConT eventStructName) (map VarT (allTyVarBndrNames tyvars)))
         d1 <- instanceD (mkCxtFromTyVars [''CBOR.Serialise] tyvars context) (return (ty ''CBOR.Serialise)) []
         d2 <- standaloneDerivD (return []) (return (ty ''Generic))
         return [d1,d2]
    where eventStructName = toStructName eventName
          (tyvars, context, _args, _stateType, _resultType, _isUpdate) = analyseType eventName eventType

serialiserSpec :: SerialiserSpec
serialiserSpec =
    SerialiserSpec
        { predName                 = ''CBOR.Serialise
        , checkpointSerialiserName = 'checkpointSerialiseSerialiser
        , eventSerialiserName      = 'eventSerialiseSerialiser
        , methodSerialiserName     = 'serialiseSerialiser
        , archiverName             = 'serialiseArchiver
        , makeEventSerialiser      = makeSerialiseInstance
        }

makeAcidic :: Name -> [Name] -> Q [Dec]
makeAcidic = makeAcidicWithSerialiser serialiserSpec

makeAcidicWithoutEvents :: Name -> [Name] -> Q [Dec]
makeAcidicWithoutEvents = makeAcidicWithSerialiser' serialiserSpec
