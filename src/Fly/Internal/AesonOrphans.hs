{-# LANGUAGE FlexibleInstances #-}
module Fly.Internal.AesonOrphans where

import Data.Aeson
import Data.HashMap.Strict as M
import Data.Text

instance {-# OVERLAPPING #-} ToJSON (HashMap Text Text) where
  toJSON xs = object (toPairs $ M.toList xs)
              where toPairs []                = []
                    toPairs ((key, value):xs) = (key .= value) : toPairs xs

instance {-# OVERLAPPING #-} ToJSON (HashMap Text (Maybe Text)) where
  toJSON xs = object (toPairs $ M.toList xs)
              where toPairs []                = []
                    toPairs ((key, value):xs) = (key .= value) : toPairs xs


