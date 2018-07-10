{-# LANGUAGE TemplateHaskell    #-}

module Data.Acid.CBOR
  ( makeAcidic
  , makeAcidicWithoutEvents
  , makeEvents
  , module Data.Acid
  ) where

import           Data.Acid hiding (makeAcidic)
import           Data.Acid.TemplateHaskell (makeEvents)
import           Data.Acid.CBOR.Internal
