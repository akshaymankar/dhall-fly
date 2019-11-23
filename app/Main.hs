module Main where

import Control.Applicative ((<|>))
import Data.Aeson          (Value)
import Data.Aeson.Yaml     (encode)
import Dhall               (auto, input, inputFile)
import Dhall.JSON          (omitNull)
import Fly.Types           (GroupedJob, Job)
import Fly.Yaml            (groupedJobsToValue, jobsToValue)
import Options.Applicative ((<**>))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               as T
import qualified Options.Applicative        as O

data PipelineType = Jobs | GroupedJobs

data InputType = File FilePath | Stdin

data Opts = Opts { pipelineType :: PipelineType, inputType :: InputType }

inputTypeParser :: O.Parser InputType
inputTypeParser = (File <$> O.strOption (O.long "file"
                                         <> O.help "Optional, defaults to stdin"))
                  <|> pure Stdin

parsePipelineType :: String -> Maybe PipelineType
parsePipelineType "jobs"         = Just Jobs
parsePipelineType "grouped-jobs" = Just GroupedJobs
parsePipelineType _              = Nothing

pipelineTypeParser :: O.Parser PipelineType
pipelineTypeParser = O.option
                     (O.maybeReader parsePipelineType)
                     (O.long "pipeline-type"
                      <> O.help "'jobs' or 'grouped-jobs'"
                      <> O.value Jobs)

optsParser :: O.Parser Opts
optsParser = Opts <$> pipelineTypeParser <*> inputTypeParser

optsParserWithHelp :: O.ParserInfo Opts
optsParserWithHelp = O.info (optsParser <**> O.helper)
                     (O.fullDesc
                      <> O.progDesc "Translate dhall pipelines to yaml")

main :: IO ()
main = do
  opts <- O.execParser optsParserWithHelp
  yaml <- dhallTextToValue (pipelineType opts) (inputType opts)
  LBS.putStrLn $ encode $ omitNull yaml

dhallTextToValue :: PipelineType -> InputType -> IO Value
dhallTextToValue Jobs (File f) = do
  jobs <- inputFile auto f :: IO [Job]
  pure $ jobsToValue jobs
dhallTextToValue Jobs Stdin = do
  jobs <- (input auto =<< T.getContents) :: IO [Job]
  pure $ jobsToValue jobs
dhallTextToValue GroupedJobs (File f) = do
  groupedJobs <- inputFile auto f :: IO [GroupedJob]
  pure $ groupedJobsToValue groupedJobs
dhallTextToValue GroupedJobs Stdin = do
  groupedJobs <- (input auto =<< T.getContents) :: IO [GroupedJob]
  pure $ groupedJobsToValue groupedJobs
