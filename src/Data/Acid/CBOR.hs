{-# LANGUAGE TemplateHaskell    #-}

module Data.Acid.CBOR
  ( makeAcidic
  , module Data.Acid
  ) where

import           Data.Acid hiding (makeAcidic)
import           Data.Acid.CBOR.Internal
