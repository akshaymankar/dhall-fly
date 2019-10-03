{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Yaml     (encode)
import Dhall         (auto, input)
import Dhall.JSON    (omitNull)
import Fly.Types     (Job)
import Fly.Yaml      (dhallToYaml)

import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text.IO          (getContents)

main :: IO ()
main = do
  stdin <- Data.Text.IO.getContents
  jobs <- (input auto stdin :: IO [Job])
  BC8.putStrLn $ encode $ omitNull $ dhallToYaml jobs
