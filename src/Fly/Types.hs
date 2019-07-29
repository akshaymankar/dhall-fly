{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fly.Types where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.HashMap.Strict
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Dhall               (Natural)
import GHC.Generics

import qualified Data.HashMap.Strict as M
import qualified Dhall.Core
import qualified Dhall.JSON

data ResourceType = ResourceTypeInBuilt Text
                  | ResourceTypeCustom { rtcName   :: Text
                                       , rtcType   :: Text
                                       , rtcSource :: Maybe (HashMap Text Value)
                                       }
                    deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase){sumEncoding = UntaggedValue} ''ResourceType)

data Resource = Resource { resourceName         :: Text
                         , resourceType         :: ResourceType
                         , resourceSource       :: Maybe Value
                         , resourceVersion      :: Maybe Value
                         , resourceParams       :: Maybe Value
                         , resourceCheckEvery   :: Maybe Text
                         , resourceTags         :: Maybe [Text]
                         , resourceWebhookToken :: Maybe Text}
              deriving (Show, Generic, Eq)

instance ToJSON Resource where
  toJSON x = case genericToJSON (aesonPrefix snakeCase) x of
               Object m -> Object $ M.insert "type" (resourceTypeValue $ resourceType x) m
               v -> error ("Expected " ++ show v ++ "to be Object")
             where
               resourceTypeValue (ResourceTypeInBuilt t)    = String t
               resourceTypeValue (ResourceTypeCustom t _ _) = String t

data TaskRunConfig = TaskRunConfig { trcPath :: Text
                                   , trcArgs :: Maybe [Text]
                                   , trcDir  :: Maybe Text
                                   , trcUser :: Maybe Text
                                   }
                   deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskRunConfig)

data TaskImageResource = TaskImageResource { tirType    :: Text
                                           , tirSource  :: Value
                                           , tirParams  :: Maybe Value
                                           , tirVersion :: Maybe Value
                                           }
                       deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskImageResource)

data TaskInput = TaskInput { tiName     :: Text
                           , tiPath     :: Maybe Text
                           , tiOptional :: Maybe Bool
                           }
               deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskInput)

data TaskOutput = TaskOutput { toName :: Text, toPath :: Maybe Text }
                deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskOutput)

data TaskCache = TaskCache { taskcachePath :: Text}
               deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskCache)

data TaskConfig = TaskConfig { tcPlatform      :: Text
                             , tcRun           :: TaskRunConfig
                             , tcImageResource :: Maybe TaskImageResource
                             , tcRootfsURI     :: Maybe Text
                             , tcInputs        :: Maybe [TaskInput]
                             , tcOutputs       :: Maybe [TaskOutput]
                             , tcCaches        :: Maybe [TaskCache]
                             , tcParams        :: Maybe [(Text, Text)]
                             }
                deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskConfig)

data TaskSpec = TaskSpecFile Text | TaskSpecConfig TaskConfig
              deriving (Show, Generic, Eq)

instance ToJSON TaskSpec where
  toJSON (TaskSpecFile f)   = object [ "file" .= f ]
  toJSON (TaskSpecConfig c) = object [ "config" .= c ]

data GetVersion = GetVersionLatest
                | GetVersionEvery
                | GetVersionSpecific Value
                deriving (Show, Generic, Eq)

instance ToJSON GetVersion where
  toJSON GetVersionLatest       = String "latest"
  toJSON GetVersionEvery        = String "every"
  toJSON (GetVersionSpecific v) = v

data GetStep = GetStep { getName     :: Maybe Text
                       , getResource :: Resource
                       , getParams   :: Maybe Value
                       , getVersion  :: Maybe GetVersion
                       , getPassed   :: Maybe [Text]
                       , getTrigger  :: Maybe Bool
                       }
             deriving (Show, Generic, Eq)

instance ToJSON GetStep where
  toJSON GetStep{..} = object [ "get"      .= fromMaybe (resourceName getResource) getName
                              , "resource" .= toJSON ((resourceName getResource) <$ getName)
                              , "params"   .= getParams
                              , "version"  .= getVersion
                              , "passed"   .= getPassed
                              , "trigger"  .= getTrigger
                              ]

data PutStep = PutStep { putName      :: Maybe Text
                       , putResource  :: Resource
                       , putParams    :: Maybe Value
                       , putGetParams :: Maybe Value
                       }
             deriving (Show, Generic, Eq)

instance ToJSON PutStep where
  toJSON PutStep{..} = object [ "put"        .= fromMaybe (resourceName putResource) putName
                              , "resource"   .= toJSON ((resourceName putResource) <$ putName)
                              , "params"     .= putParams
                              , "get_params" .= putGetParams
                              ]

data TaskStep = TaskStep { taskTask          :: Text
                         , taskSpec          :: TaskSpec
                         , taskPrivileged    :: Maybe Bool
                         , taskParams        :: Maybe [(Text, Text)]
                         , taskImage         :: Maybe Text
                         , taskInputMapping  :: Maybe [(Text, Text)]
                         , taskOutputMapping :: Maybe [(Text, Text)]
                         }
              deriving (Show, Generic, Eq)

instance {-# OVERLAPPING #-} ToJSON [(Text, Text)] where
  toJSON xs = object (pairs xs)
              where pairs []                = []
                    pairs ((key, value):xs) = (key .= value) : pairs xs

instance ToJSON TaskStep where
  toJSON t@(TaskStep{..}) = case genericToJSON (aesonPrefix snakeCase) t of
                          Object o1 -> case toJSON taskSpec of
                                         Object o2 ->
                                           Object ( (M.delete "spec" o1) `M.union` o2)
                                         v -> error ("Expected " ++ show v ++ "to be Object")
                          v -> error ("Expected " ++ show v ++ "to be Object")

data Step = Get { stepGet :: GetStep, stepHooks :: StepHooks }
          | Put { stepPut :: PutStep, stepHooks :: StepHooks }
          | Task { stepTask :: TaskStep, stepHooks :: StepHooks }
          | Aggregate { aggregatedSteps :: [Step], stepHooks :: StepHooks }
          | Do { doSteps :: [Step], stepHooks :: StepHooks  }
          | Try { tryStep :: Step, stepHooks :: StepHooks  }
          deriving (Show, Generic, Eq)

mergeHooks :: ToJSON a => a -> StepHooks -> Value
mergeHooks step hooks = case toJSON step of
                          Object o1 -> case toJSON hooks of
                                         Object o2 -> Object (o1 `M.union` o2)
                                         v -> error ("Expected " ++ show v ++ "to be Object")
                          v -> error ("Expected " ++ show v ++ "to be Object")

instance ToJSON Step where
  toJSON (Get g h)           = mergeHooks g h
  toJSON (Put p h)           = mergeHooks p h
  toJSON (Task t h)          = mergeHooks t h
  toJSON (Aggregate steps h) = mergeHooks (object ["aggregate" .= steps]) h
  toJSON (Do steps h)        = mergeHooks (object ["do" .= steps]) h
  toJSON (Try step h)        = mergeHooks (object ["try" .= step]) h

data StepHooks = StepHooks { hookOnSuccess :: Maybe Step
                           , hookOnFailure :: Maybe Step
                           , hookOnAbort   :: Maybe Step
                           , hookEnsure    :: Maybe Step
                           }
               deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''StepHooks)

data Job = Job { jobName                 :: Text
               , jobPlan                 :: [Step]
               , jobSerial               :: Maybe Bool
               , jobBuildLogsToRetain    :: Maybe Natural
               , jobSerialGroups         :: Maybe [Text]
               , jobMaxInFlight          :: Maybe Natural
               , jobPublic               :: Maybe Bool
               , jobDisableManualTrigger :: Maybe Bool
               , jobInterruptible        :: Maybe Bool
               , jobOnSuccess            :: Maybe Step
               , jobOnFailure            :: Maybe Step
               , jobOnAbort              :: Maybe Step
               , jobEnsure               :: Maybe Step
               }
         deriving (Show, Generic, Eq)
$(deriveToJSON (aesonPrefix snakeCase) ''Job)
