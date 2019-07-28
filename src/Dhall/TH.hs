{-# LANGUAGE TemplateHaskell #-}
module Dhall.TH where

import Control.Monad
import Data.Data
import Data.Text
import Dhall
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

dhallExpr :: QuasiQuoter
dhallExpr = QuasiQuoter { quoteExp = compileDhall
                        , quotePat  = notHandled "patterns"
                        , quoteType = notHandled "types"
                        , quoteDec  = notHandled "declarations"
                        }
  where notHandled things = error $
          things ++ " are not handled by the dhall expression quasiquoter."

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'pack) <$> lift (unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

compileDhall :: String -> Q Exp
compileDhall =
  (runIO . inputExpr . pack) >=> liftDataWithText
