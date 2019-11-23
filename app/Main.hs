module Main where

import Data.Aeson          (Value)
import Data.Aeson.Yaml     (encode)
import Dhall               (auto, input, inputFile)
import Dhall.JSON          (omitNull)
import Fly.Types           (GroupedJob, Job)
import Fly.Yaml            (groupedJobsToValue, jobsToValue)
import Fly.Options

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               as T
import qualified Options.Applicative        as O

main :: IO ()
main = do
  opts <- O.execParser optsParserWithHelp
  yaml <- dhallTextToValue opts
  LBS.putStrLn $ encode $ omitNull yaml

dhallTextToValue :: Opts -> IO Value
dhallTextToValue (Opts Jobs (File f)) = do
  jobs <- inputFile auto f :: IO [Job]
  pure $ jobsToValue jobs
dhallTextToValue (Opts Jobs Stdin) = do
  jobs <- (input auto =<< T.getContents) :: IO [Job]
  pure $ jobsToValue jobs
dhallTextToValue (Opts GroupedJobs (File f)) = do
  groupedJobs <- inputFile auto f :: IO [GroupedJob]
  pure $ groupedJobsToValue groupedJobs
dhallTextToValue (Opts GroupedJobs Stdin) = do
  groupedJobs <- (input auto =<< T.getContents) :: IO [GroupedJob]
  pure $ groupedJobsToValue groupedJobs
