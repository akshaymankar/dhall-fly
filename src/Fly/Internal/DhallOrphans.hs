{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fly.Internal.DhallOrphans where

import Data.Aeson
import Data.Scientific (fromFloatDigits)
import Dhall
import Dhall.Core
import Dhall.TH

import qualified Data.Vector as V

instance FromDhall Value where
  autoWith _ = Dhall.Type{..} where
    expected = $(staticDhallExpression "let Prelude = ./dhall-concourse/lib/prelude.dhall in Prelude.JSON.Type")
    extract (Lam _ (Const Dhall.Core.Type)
              (Lam _ _ x)) = extractJSONFromApps x
    extract x = extractJSONFromApps x
    extractJSONFromApps (App (Field (Var (V _ 0)) "bool") (BoolLit b)) = pure $ Data.Aeson.Bool b
    extractJSONFromApps (App (Field (Var (V _ 0)) "string") (TextLit (Chunks _ t))) = pure $ String t
    extractJSONFromApps (App (Field (Var (V _ 0)) "number") (DoubleLit n)) = pure $ Number $ fromFloatDigits $ getDhallDouble n
    extractJSONFromApps (App (Field (Var (V _ 0)) "object") o) = Object <$> Dhall.extract auto o
    extractJSONFromApps (App (Field (Var (V _ 0)) "array") a) = Array . V.fromList <$> Dhall.extract auto a
    extractJSONFromApps (Field (Var (V _ 0)) "null") = pure Null
    extractJSONFromApps t = typeError expected t

