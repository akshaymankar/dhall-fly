module Main where

import Data.Aeson.Yaml (encode)
import Dhall           (auto, input)
import Dhall.JSON      (omitNull)
import Fly.Types       (Job)
import Fly.Yaml        (dhallToYaml)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               (getContents)

main :: IO ()
main = do
  stdin <- Data.Text.IO.getContents
  jobs <- input auto stdin :: IO [Job]
  LBS.putStrLn $ encode $ omitNull $ dhallToYaml jobs
