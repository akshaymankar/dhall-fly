{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Fly.Internal.DhallWithPrefix where

import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Casing
import Data.Text
import Dhall
import GHC.Generics

newtype FromDhallWithPrefix a = FromDhallWithPrefix a

instance (Generic a, GenericFromDhall (Rep a)) => FromDhall (FromDhallWithPrefix a) where
  autoWith opts =
    let modifier = pack . fieldLabelModifier (aesonPrefix snakeCase) . unpack
    in FromDhallWithPrefix <$> fmap GHC.Generics.to (evalState (genericAutoWith opts{fieldModifier = modifier}) 1)
