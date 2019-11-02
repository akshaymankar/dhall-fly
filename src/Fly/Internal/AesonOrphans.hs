{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fly.Internal.AesonOrphans where

import Data.Aeson
import Data.HashMap.Strict as M
import Data.Text

instance {-# OVERLAPPING #-} ToJSON (HashMap Text Text) where
  toJSON theMap =
    object (toPairs $ M.toList theMap)
    where toPairs []                = []
          toPairs ((key, value):xs) = (key .= value) : toPairs xs

instance {-# OVERLAPPING #-} ToJSON (HashMap Text (Maybe Text)) where
  toJSON theMap =
    object (toPairs $ M.toList theMap)
    where toPairs []                = []
          toPairs ((key, value):xs) = (key .= value) : toPairs xs


