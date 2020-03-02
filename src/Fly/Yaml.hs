{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fly.Yaml where

import Data.Aeson
import Data.List  (nub)
import Data.Maybe (catMaybes)
import Data.Text  (Text)
import Fly.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Ordered as OM

jobsToValue :: [Job] -> Value
jobsToValue = Object . jobsToMap

groupedJobsToValue :: [GroupedJob] -> Value
groupedJobsToValue groupedJobs =
  let mapWithoutGroups = jobsToMap $ map gjJob groupedJobs
      groupsAsMap = groupedJobsToMap groupedJobs
      mkGroupValue (group, jobs) acc = object [ "name" .= group, "jobs" .= jobs ] : acc
      groupsAsValue = toJSON $ foldr mkGroupValue [] (OM.assocs groupsAsMap)
      groupsMap = HM.singleton "groups" groupsAsValue
  in Object $ HM.union mapWithoutGroups groupsMap

jobsToMap :: [Job] -> HM.HashMap Text Value
jobsToMap jobs =
  let resources = nub $ concatMap getResourcesFromJob jobs
      resourceTypes = nub $ customResourceTypes $ map Fly.Types.resourceType resources
  in HM.fromList [ ("resource_types", toJSON resourceTypes)
                 , ("resources", toJSON resources)
                 , ("jobs", toJSON jobs)
                 ]

groupedJobsToMap :: [GroupedJob] -> OM.OMap Text [Text]
groupedJobsToMap gjs =
  let toGroupJobsPair (GroupedJob j groups) = map (, [jobName j]) groups
      groupJobsPairs = concatMap toGroupJobsPair gjs
      foldFn pair = OM.unionWithL (\_ v1 v2 -> v1 ++ v2) (OM.fromList [pair])
  in foldr foldFn OM.empty groupJobsPairs

customResourceTypes :: [ResourceType] -> [ResourceType]
customResourceTypes [] = []
customResourceTypes (ResourceTypeInBuilt _ : rts) =  customResourceTypes rts
customResourceTypes (x : rts) =  x : customResourceTypes rts

getResourcesFromHooks :: StepHooks -> [Resource]
getResourcesFromHooks StepHooks{..} =
  concatMap getResourcesFromStep
  $ catMaybes [hookOnSuccess, hookOnFailure, hookOnAbort, hookEnsure]

getResourcesFromStep :: Step -> [Resource]
getResourcesFromStep s =
  stepResources s ++ getResourcesFromHooks (Fly.Types.stepHooks s)
  where
    stepResources (Get GetStep{..} _)   = [getResource]
    stepResources (Put PutStep{..} _)   = [putResource]
    stepResources (Task _ _)            = []
    stepResources (SetPipeline _ _)     = []
    stepResources (Aggregate steps _)   = getResourcesFromSteps steps
    stepResources (Do steps _)          = getResourcesFromSteps steps
    stepResources (Try step _)          = getResourcesFromStep step
    stepResources (InParallel inParallel _) =
       getResourcesFromSteps $ inParallelSteps inParallel

getResourcesFromSteps :: [Step] -> [Resource]
getResourcesFromSteps = concatMap getResourcesFromStep

getResourcesFromJob :: Job -> [Resource]
getResourcesFromJob Job{..} =
  getResourcesFromSteps
  $ jobPlan ++ catMaybes [ jobOnSuccess, jobOnFailure, jobOnAbort, jobEnsure ]
