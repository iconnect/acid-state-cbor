{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE BangPatterns       #-}

module Data.Acid.CBOR.Internal where

import qualified Codec.Serialise     as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR
import           Control.Exception (displayException)
import           Control.Monad (unless, replicateM)
import           Data.Acid.Common (IsAcidic)
import           Data.Acid.Abstract (AcidState)
import           Data.Acid.Archive as Archive (Archiver(..), Entries(..), Entry)
import           Data.Acid.Core (Serialiser(..), MethodSerialiser(..), MethodResult, Tagged)
import           Data.Acid.CRC.Incremental (crc16, crc16_incremental)
import           Data.Acid.Local (Checkpoint(..), SerialisationLayer(..), defaultStateDirectory, openLocalStateWithSerialiser, prepareLocalStateWithSerialiser)
import           Data.Acid.TemplateHaskell (SerialiserSpec(..), TypeAnalysis(..), mkCxtFromTyVars, analyseType, toStructName, allTyVarBndrNames, makeAcidicWithSerialiser)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (foldl1')
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import           Language.Haskell.TH (Q, Dec, DecQ, Name, Type(..), ExpQ, varE, appE, litE, conE, integerL, varP, conP, normalB, valD, funD, instanceD, clause, newName)


serialiseSerialisationLayer :: CBOR.Serialise st => SerialisationLayer st
serialiseSerialisationLayer =
  SerialisationLayer { checkpointSerialiser = checkpointSerialiseSerialiser
                     , eventSerialiser      = eventSerialiseSerialiser
                     , archiver             = serialiseArchiver
                     }

serialiseSerialiser :: CBOR.Serialise a => Serialiser a
serialiseSerialiser = Serialiser CBOR.serialise (either (Left . displayException) Right . CBOR.deserialiseOrFail)

serialiseMethodSerialiser :: (CBOR.Serialise a, CBOR.Serialise (MethodResult a)) => MethodSerialiser a
serialiseMethodSerialiser = MethodSerialiser serialiseSerialiser serialiseSerialiser

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
  encode (CBOREntry b) =
    let !(crc, b') = crc16_incremental b
    in CBOR.encodeListLen 2
         <> CBOR.encodeTag 24 <> CBOR.encode b'
         <> CBOR.encodeWord16 crc
  decode = do
    _ <- CBOR.decodeListLen
    _ <- CBOR.decodeTag
    b <- CBOR.decode
    c <- CBOR.decodeWord16
    unless (crc16 b == c) $ fail "CRC mismatch"
    return (CBOREntry b)


makeSerialiseInstance :: Name -> Type -> DecQ
makeSerialiseInstance eventName eventType
    = do let ty = AppT (ConT ''CBOR.Serialise) (foldl AppT (ConT eventStructName) (map VarT (allTyVarBndrNames tyvars)))
         vars <- replicateM (length argumentTypes) (newName "arg")
         let encodeBody = mappendE $ (varE 'CBOR.encodeListLen `appE` litE (integerL (fromIntegral (length vars + 1))))
                                   : (varE 'CBOR.encodeWord `appE` litE (integerL 0))
                                   : [varE 'CBOR.encode `appE` varE var | var <- vars]
             decodeBody = opE '(*>) (opE '(*>) (varE 'CBOR.decodeListLen) (varE 'CBOR.decodeWord))
                                    (applicativeE (conE eventStructName) (map (const (varE 'CBOR.decode)) argumentTypes))
         instanceD (mkCxtFromTyVars [''CBOR.Serialise] tyvars context) (return ty)
                 [ funD 'CBOR.encode [clause [conP eventStructName [varP var | var <- vars ]]
                                        (normalB encodeBody )
                                        [] ]
                 , valD (varP 'CBOR.decode) (normalB decodeBody) []
                 ]
    where TypeAnalysis { tyvars, context, argumentTypes } = analyseType eventName eventType
          eventStructName = toStructName eventName

-- | Construct an idiomatic expression (an expression in an
-- Applicative context), i.e.
--
-- > applicativeE ke []             = pure ke
-- > applicativeE ke [e1,e2,...,en] = ke <$> e1 <*> e2 ... <*> en
applicativeE :: ExpQ -> [ExpQ] -> ExpQ
applicativeE ke es0 =
    case es0 of
      []   -> varE 'pure `appE` ke
      e:es -> app' (opE '(<$>) ke e) es
  where
    app' e []      = e
    app' e (e':es) = app' (opE '(<*>) e e') es

opE :: Name -> ExpQ -> ExpQ -> ExpQ
opE n x y = varE n `appE` x `appE` y

mappendE :: [ExpQ] -> ExpQ
mappendE [] = varE 'mempty
mappendE es = foldl1' (opE 'mappend) es

serialiserSpec :: SerialiserSpec
serialiserSpec =
    SerialiserSpec
        { serialisationClassName = ''CBOR.Serialise
        , methodSerialiserName   = 'serialiseMethodSerialiser
        , makeEventSerialiser    = makeSerialiseInstance
        }

makeAcidic :: Name -> [Name] -> Q [Dec]
makeAcidic = makeAcidicWithSerialiser serialiserSpec


-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
openLocalState :: (Typeable st, IsAcidic st, CBOR.Serialise st)
              => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                             --   found.
              -> IO (AcidState st)
openLocalState initialState =
  openLocalStateFrom (defaultStateDirectory initialState) initialState

-- | Create an AcidState given an initial value.
--
--   This will create or resume a log found in the \"state\/[typeOf state]\/\" directory.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalState :: (Typeable st, IsAcidic st, CBOR.Serialise st)
                  => st                          -- ^ Initial state value. This value is only used if no checkpoint is
                                                 --   found.
                  -> IO (IO (AcidState st))
prepareLocalState initialState =
  prepareLocalStateFrom (defaultStateDirectory initialState) initialState

-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   Running two AcidState's from the same directory is an error
--   but will not result in dataloss.
openLocalStateFrom :: (IsAcidic st, CBOR.Serialise st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (AcidState st)
openLocalStateFrom directory initialState =
  openLocalStateWithSerialiser directory initialState serialiseSerialisationLayer

-- | Create an AcidState given a log directory and an initial value.
--
--   This will create or resume a log found in @directory@.
--   The most recent checkpoint will be loaded immediately but the AcidState will not be opened
--   until the returned function is executed.
prepareLocalStateFrom :: (IsAcidic st, CBOR.Serialise st)
                  => FilePath            -- ^ Location of the checkpoint and transaction files.
                  -> st                  -- ^ Initial state value. This value is only used if no checkpoint is
                                         --   found.
                  -> IO (IO (AcidState st))
prepareLocalStateFrom directory initialState =
  prepareLocalStateWithSerialiser directory initialState serialiseSerialisationLayer
