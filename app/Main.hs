module Main where

import Control.Applicative ((<|>))
import Data.Aeson          (Value)
import Data.Aeson.Yaml     (encode)
import Data.Text           (Text)
import Dhall               (auto, input)
import Dhall.JSON          (omitNull)
import Fly.Types           (GroupedJob, Job)
import Fly.Yaml            (jobsToValue, groupedJobsToValue)
import Options.Applicative ((<**>))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               as T
import qualified Options.Applicative        as O

data PipelineType = Jobs | GroupedJobs

data InputType = File FilePath | Stdin

data Opts = Opts { inputType :: PipelineType, file :: InputType }

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
  textDhall <- case file opts of
                 Stdin     -> T.getContents
                 File path -> T.readFile path
  yaml <- dhallTextToValue (inputType opts) textDhall
  LBS.putStrLn $ encode $ omitNull yaml

dhallTextToValue :: PipelineType -> Text -> IO Value
dhallTextToValue Jobs textDhall = do
  jobs <- input auto textDhall :: IO [Job]
  pure $ jobsToValue jobs
dhallTextToValue GroupedJobs textDhall = do
  groupedJobs <- input auto textDhall :: IO [GroupedJob]
  pure $ groupedJobsToValue groupedJobs
