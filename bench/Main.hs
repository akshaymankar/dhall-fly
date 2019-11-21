{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Fly.Yaml
import Fly.Types
import Dhall
import Data.Time.Clock

main :: IO ()
main = do
  exprStart <- getCurrentTime
  !expr <- inputExpr "./bench/data/simple.dhall"
  exprEnd <- getCurrentTime
  putStrLn $ "Reading simple expr took: " ++ show (diffUTCTime exprEnd exprStart)
  pipelineStart <- getCurrentTime
  !pipeline <- input auto "./bench/data/simple.dhall" :: IO [Job]
  pipelineEnd <- getCurrentTime
  putStrLn $ "Reading simple pipeline took: " ++ show (diffUTCTime pipelineEnd pipelineStart)
  toYamlStart <- getCurrentTime
  let !yaml = jobsToValue pipeline
  toYamlEnd <- getCurrentTime
  putStrLn $ "Translating to YAML took: " ++ show (diffUTCTime toYamlEnd toYamlStart)
