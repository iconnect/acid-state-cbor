{-# LANGUAGE TemplateHaskell    #-}

module Data.Acid.CBOR
    ( AcidState
    , openLocalState
    , openLocalStateFrom
    , closeAcidState
    , createCheckpoint
    , createArchive
    , update
    , query
    , EventResult
    , EventState
    , UpdateEvent
    , QueryEvent
    , Update
    , Query
    , IsAcidic
    , makeAcidic
    , liftQuery

    , SerialisationLayer
    , serialiseSerialisationLayer
    ) where

import           Data.Acid               ( AcidState
                                         , closeAcidState
                                         , createCheckpoint
                                         , createArchive
                                         , update
                                         , query
                                         , EventResult
                                         , EventState
                                         , UpdateEvent
                                         , QueryEvent
                                         , Update
                                         , Query
                                         , IsAcidic
                                         , liftQuery
                                         )
import           Data.Acid.Local         ( SerialisationLayer )
import           Data.Acid.CBOR.Internal ( openLocalState
                                         , openLocalStateFrom
                                         , makeAcidic
                                         , serialiseSerialisationLayer
                                         )
