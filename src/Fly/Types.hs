{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Fly.Types where

import Data.Aeson                   hiding (Result)
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.HashMap.Strict
import Data.Maybe                   (fromMaybe)
import Dhall
import Dhall.Core
import Dhall.TH
import Fly.Internal.AesonOrphans    ()
import Fly.Internal.DhallOrphans    ()
import Fly.Internal.DhallWithPrefix

import qualified Data.HashMap.Strict as M

data CustomResourceType = CustomResourceType { crtName   :: Text
                                             , crtType   :: Text
                                             , crtSource :: Maybe (HashMap Text Value)
                                             , crtPrivileged :: Maybe Bool
                                             , crtParams :: Maybe (HashMap Text Value)
                                             , crtCheckEvery :: Maybe Text
                                             , crtTags :: Maybe Text
                                             , crtUniqueVersionHistory :: Maybe Bool
                                             }
                    deriving (Show, Generic, Eq)
                    deriving FromDhall via FromDhallWithPrefix CustomResourceType

data ResourceType = ResourceTypeInBuilt Text
                  | ResourceTypeCustom CustomResourceType
                    deriving (Show, Generic, Eq)

instance FromDhall ResourceType where
  autoWith _ = Dhall.union ( ( ResourceTypeInBuilt <$> constructor "InBuilt" strictText)
                           <> ( ResourceTypeCustom <$> constructor "Custom" auto ))

data Resource = Resource { resourceName         :: Text
                         , resourceType         :: ResourceType
                         , resourceIcon         :: Maybe Text
                         , resourcePublic       :: Maybe Bool
                         , resourceSource       :: Maybe (HashMap Text Value)
                         , resourceVersion      :: Maybe (HashMap Text Text)
                         , resourceCheckEvery   :: Maybe Text
                         , resourceTags         :: Maybe [Text]
                         , resourceWebhookToken :: Maybe Text}
              deriving (Show, Generic, Eq)
              deriving FromDhall via FromDhallWithPrefix Resource

instance ToJSON Resource where
  toJSON x = case genericToJSON (aesonPrefix snakeCase) x of
               Object m -> Object $ M.insert "type" (resourceTypeValue $ resourceType x) m
               v -> error ("Expected " ++ show v ++ "to be Object")
             where
               resourceTypeValue (ResourceTypeInBuilt t) = String t
               resourceTypeValue (ResourceTypeCustom t)  = String $ crtName t

data TaskRunConfig = TaskRunConfig { trcPath :: Text
                                   , trcArgs :: Maybe [Text]
                                   , trcDir  :: Maybe Text
                                   , trcUser :: Maybe Text
                                   }
                   deriving (Show, Generic, Eq)
                   deriving FromDhall via FromDhallWithPrefix TaskRunConfig

data TaskImageResource = TaskImageResource { tirType    :: Text
                                           , tirSource  :: Maybe (HashMap Text Value)
                                           , tirParams  :: Maybe (HashMap Text Value)
                                           , tirVersion :: Maybe (HashMap Text Text)
                                           }
                       deriving (Show, Generic, Eq)
                       deriving FromDhall via FromDhallWithPrefix TaskImageResource

data TaskInput = TaskInput { tiName     :: Text
                           , tiPath     :: Maybe Text
                           , tiOptional :: Maybe Bool
                           }
               deriving (Show, Generic, Eq)
               deriving FromDhall via FromDhallWithPrefix TaskInput

data TaskOutput = TaskOutput { toName :: Text, toPath :: Maybe Text }
                deriving (Show, Generic, Eq)
                deriving FromDhall via FromDhallWithPrefix TaskOutput

newtype TaskCache = TaskCache { taskcachePath :: Text}
                  deriving (Show, Generic, Eq)
                  deriving FromDhall via FromDhallWithPrefix TaskCache

data TaskContainerLimits = TaskContainerLimits { tclCpu    :: Maybe Natural
                                               , tclMemory :: Maybe Natural}
                         deriving (Show, Generic, Eq)
                         deriving FromDhall via FromDhallWithPrefix TaskContainerLimits

data TaskConfig = TaskConfig { tcPlatform        :: Text
                             , tcRun             :: TaskRunConfig
                             , tcImageResource   :: Maybe TaskImageResource
                             , tcRootfsUri       :: Maybe Text
                             , tcInputs          :: Maybe [TaskInput]
                             , tcOutputs         :: Maybe [TaskOutput]
                             , tcCaches          :: Maybe [TaskCache]
                             , tcParams          :: Maybe (HashMap Text (Maybe Text))
                             , tcContainerLimits :: Maybe TaskContainerLimits
                             }
                deriving (Show, Generic, Eq)
                deriving FromDhall via FromDhallWithPrefix TaskConfig

data TaskSpec = TaskSpecFile Text | TaskSpecConfig TaskConfig
              deriving (Show, Generic, Eq)

instance FromDhall TaskSpec where
  autoWith _ = Dhall.union ((TaskSpecFile <$> constructor "File" strictText)
                           <> (TaskSpecConfig <$> constructor "Config" auto))

instance ToJSON TaskSpec where
  toJSON (TaskSpecFile f)   = object [ "file" .= f ]
  toJSON (TaskSpecConfig c) = object [ "config" .= c ]

data GetVersion = GetVersionLatest
                | GetVersionEvery
                | GetVersionSpecific (HashMap Text Text)
                deriving (Show, Generic, Eq)

instance ToJSON GetVersion where
  toJSON GetVersionLatest       = String "latest"
  toJSON GetVersionEvery        = String "every"
  toJSON (GetVersionSpecific v) = toJSON v

instance FromDhall GetVersion where
  autoWith _ = Dhall.union ((GetVersionLatest <$ constructor "Latest" strictText)
                           <> (GetVersionEvery <$ constructor "Every" strictText)
                           <> (GetVersionSpecific <$> constructor "SpecificVersion" auto))

data GetStep = GetStep { getGet      :: Maybe Text
                       , getResource :: Resource
                       , getParams   :: Maybe (HashMap Text Value)
                       , getVersion  :: Maybe GetVersion
                       , getPassed   :: Maybe [Text]
                       , getTrigger  :: Maybe Bool
                       , getTags     :: Maybe [Text]
                       , getTimeout  :: Maybe Text
                       , getAttempts :: Maybe Natural
                       }
             deriving (Show, Generic, Eq)
             deriving FromDhall via FromDhallWithPrefix GetStep

instance ToJSON GetStep where
  toJSON GetStep{..} = object [ "get"      .= fromMaybe (resourceName getResource) getGet
                              , "resource" .= toJSON (resourceName getResource <$ getGet)
                              , "params"   .= getParams
                              , "version"  .= getVersion
                              , "passed"   .= getPassed
                              , "trigger"  .= getTrigger
                              , "tags"     .= getTags
                              , "timeout"  .= getTimeout
                              , "attempts" .= getAttempts
                              ]

data PutStep = PutStep { putPut       :: Maybe Text
                       , putResource  :: Resource
                       , putParams    :: Maybe (HashMap Text Value)
                       , putGetParams :: Maybe (HashMap Text Value)
                       , putInputs    :: Maybe [Text]
                       , putTags      :: Maybe [Text]
                       , putTimeout   :: Maybe Text
                       , putAttempts  :: Maybe Natural
                       }
             deriving (Show, Generic, Eq)
             deriving FromDhall via FromDhallWithPrefix PutStep

instance ToJSON PutStep where
  toJSON PutStep{..} = object [ "put"        .= fromMaybe (resourceName putResource) putPut
                              , "resource"   .= toJSON (resourceName putResource <$ putPut)
                              , "params"     .= putParams
                              , "get_params" .= putGetParams
                              , "tags"       .= putTags
                              , "inputs"     .= putInputs
                              , "timeout"    .= putTimeout
                              , "attempts"   .= putAttempts
                              ]

data TaskStep = TaskStep { taskTask          :: Text
                         , taskConfig        :: TaskSpec
                         , taskPrivileged    :: Maybe Bool
                         , taskParams        :: Maybe (HashMap Text Text)
                         , taskImage         :: Maybe Text
                         , taskInputMapping  :: Maybe (HashMap Text Text)
                         , taskOutputMapping :: Maybe (HashMap Text Text)
                         , taskVars          :: Maybe (HashMap Text Value)
                         , taskTags          :: Maybe [Text]
                         , taskTimeout       :: Maybe Text
                         , taskAttempts      :: Maybe Natural
                         }
              deriving (Show, Generic, Eq)
              deriving FromDhall via FromDhallWithPrefix TaskStep

instance ToJSON TaskStep where
  toJSON t@TaskStep{..} =
    case genericToJSON (aesonPrefix snakeCase) t of
      Object o1 ->
        case toJSON taskConfig of
          Object o2 -> Object (M.delete "config" o1 `M.union` o2)
          v         -> error ("Expected " ++ show v ++ "to be Object")
      v -> error ("Expected " ++ show v ++ "to be Object")

data SetPipelineStep = SetPipelineStep { spSetPipeline :: Text
                                       , spFile        :: Text
                                       , spVars        :: Maybe (HashMap Text Value)
                                       , spVarFiles   :: Maybe [Text]
                                       , spTags        :: Maybe [Text]
                                       , spTimeout     :: Maybe Text
                                       , spAttempts    :: Maybe Natural
                                       }
                     deriving (Show, Generic, Eq)
                     deriving FromDhall via FromDhallWithPrefix SetPipelineStep

data InParallelStep = InParallelSteps {ipSteps  :: [Step]}
                    | InParallelStepConfig {ipConfig :: InParallelConfig}
                    deriving (Show, Generic, Eq)

inParallelSteps :: InParallelStep -> [Step]
inParallelSteps (InParallelSteps steps )                    = steps
inParallelSteps (InParallelStepConfig InParallelConfig{..}) = ipcSteps

instance ToJSON InParallelStep where
  toJSON (InParallelSteps steps)    = toJSON steps
  toJSON (InParallelStepConfig cfg) = toJSON cfg

instance FromDhall InParallelStep where
  autoWith _ = Dhall.union ((InParallelSteps <$> constructor "Steps" auto)
                           <> (InParallelStepConfig <$> constructor "Config" auto))

data InParallelConfig = InParallelConfig { ipcSteps    :: [Step]
                                         , ipcLimit    :: Maybe Natural
                                         , ipcFailFast :: Maybe Bool
                                         }
                      deriving (Show, Generic, Eq)
                      deriving FromDhall via FromDhallWithPrefix InParallelConfig

data Step = Get { stepGet :: GetStep, stepHooks :: StepHooks }
          | Put { stepPut :: PutStep, stepHooks :: StepHooks }
          | Task { stepTask :: TaskStep, stepHooks :: StepHooks }
          | Aggregate { aggregatedSteps :: [Step], stepHooks :: StepHooks }
          | SetPipeline { stepSetPipeline :: SetPipelineStep, stepHooks :: StepHooks }
          | InParallel { stepInParallel :: InParallelStep, stepHooks :: StepHooks }
          | Do { doSteps :: [Step], stepHooks :: StepHooks  }
          | Try { tryStep :: Step, stepHooks :: StepHooks  }
          deriving (Show, Generic, Eq)

instance FromDhall Step where
  autoWith _ = Dhall.Decoder{..} where
    expected = $(staticDhallExpression "./dhall-concourse/types/Step.dhall")
    extract (Lam _ _ --Step
             (Lam _ _ --Constructors
              c)) = extractStepFromConstructors c
    extract x = extractStepFromConstructors x -- While recursing, only the constructor applications are available
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "get") s) hooks)          = buildStep Get s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "put") s) hooks)          = buildStep Put s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "task") s) hooks)         = buildStep Task s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "set_pipeline") s) hooks) = buildStep SetPipeline s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "aggregate") s) hooks)    = buildStep Aggregate s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "do") s) hooks)           = buildStep Do s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "try") s) hooks)          = buildStep Try s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "in_parallel")
                                          (App (Field (Union _) "Steps") s)) hooks)      = buildStep (InParallel . InParallelSteps ) s hooks
    extractStepFromConstructors (App (App (Field (Var (V _ 0)) "in_parallel")
                                          (App (Field (Union _) "Config") s)) hooks)     = buildStep (InParallel . InParallelStepConfig ) s hooks
    extractStepFromConstructors t = typeError expected t
    buildStep f x y = f <$> Dhall.extract auto x <*> Dhall.extract auto y

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
  toJSON (SetPipeline s h)   = mergeHooks s h
  toJSON (Aggregate steps h) = mergeHooks (object ["aggregate" .= steps]) h
  toJSON (Do steps h)        = mergeHooks (object ["do" .= steps]) h
  toJSON (Try step h)        = mergeHooks (object ["try" .= step]) h
  toJSON (InParallel cfg h)  = mergeHooks (object ["in_parallel" .= cfg]) h

data StepHooks = StepHooks { hookOnSuccess :: Maybe Step
                           , hookOnFailure :: Maybe Step
                           , hookOnAbort   :: Maybe Step
                           , hookEnsure    :: Maybe Step
                           }
               deriving (Show, Generic, Eq)
               deriving FromDhall via FromDhallWithPrefix StepHooks

data JobBuildLogRetention = JobBuildLogRetention { jblrDays   :: Maybe Natural
                                                 , jblrBuilds :: Maybe Natural
                                                 }
                          deriving (Show, Generic, Eq)
                          deriving FromDhall via FromDhallWithPrefix JobBuildLogRetention

data Job = Job { jobName                 :: Text
               , jobOldName              :: Maybe Text
               , jobPlan                 :: [Step]
               , jobSerial               :: Maybe Bool
               , jobBuildLogRetention    :: Maybe JobBuildLogRetention
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
         deriving FromDhall via FromDhallWithPrefix Job

data GroupedJob = GroupedJob { gjJob :: Job, gjGroups :: [Text]}
                deriving (Show, Generic, Eq)
                deriving FromDhall via FromDhallWithPrefix GroupedJob

$(deriveToJSON (aesonPrefix snakeCase) ''CustomResourceType)
$(deriveToJSON (aesonPrefix snakeCase){sumEncoding = UntaggedValue} ''ResourceType)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskRunConfig)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskImageResource)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskInput)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskOutput)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskCache)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskConfig)
$(deriveToJSON (aesonPrefix snakeCase) ''SetPipelineStep)
$(deriveToJSON (aesonPrefix snakeCase) ''TaskContainerLimits)
$(deriveToJSON (aesonPrefix snakeCase) ''InParallelConfig)
$(deriveToJSON (aesonPrefix snakeCase) ''StepHooks)
$(deriveToJSON (aesonPrefix snakeCase) ''JobBuildLogRetention)
$(deriveToJSON (aesonPrefix snakeCase) ''Job)
