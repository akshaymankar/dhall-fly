module Fly.Options where

import Options.Applicative

data PipelineType = Jobs | GroupedJobs

data InputType = File FilePath | Stdin

data Opts = Opts { pipelineType :: PipelineType, inputType :: InputType }

inputTypeParser :: Parser InputType
inputTypeParser = (File
                    <$> strOption (long "file"
                                    <> help "Optional, defaults to stdin"))
                  <|> pure Stdin

parsePipelineType :: String -> Maybe PipelineType
parsePipelineType "jobs"         = Just Jobs
parsePipelineType "grouped-jobs" = Just GroupedJobs
parsePipelineType _              = Nothing

pipelineTypeParser :: Parser PipelineType
pipelineTypeParser = option
                     (maybeReader parsePipelineType)
                     (long "pipeline-type"
                      <> help "'jobs' or 'grouped-jobs'"
                      <> value Jobs)

optsParser :: Parser Opts
optsParser = Opts <$> pipelineTypeParser <*> inputTypeParser

optsParserWithHelp :: ParserInfo Opts
optsParserWithHelp = info (optsParser <**> helper)
                     (fullDesc
                      <> progDesc "Translate dhall pipelines to yaml")
