{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fly.Yaml where

import Data.Aeson
import Data.Maybe (catMaybes)
import Fly.Types
import Data.List (nub)

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
  stepResources s ++ (getResourcesFromHooks $ Fly.Types.stepHooks s)
  where
    stepResources (Get (GetStep{..}) _) = [getResource]
    stepResources (Put (PutStep{..}) _) = [putResource]
    stepResources (Task _ _)            = []
    stepResources (Aggregate steps _)   = concatMap getResourcesFromStep steps
    stepResources (Do steps _)          = concatMap getResourcesFromStep steps
    stepResources (Try step _)          = getResourcesFromStep step
    stepResources (InParallel (InParallelSteps steps) _)   = concatMap getResourcesFromStep steps
    stepResources (InParallel (InParallelStepConfig(InParallelConfig{..})) _)   = concatMap getResourcesFromStep ipcSteps

getResourcesFromJob :: Job -> [Resource]
getResourcesFromJob Job{..} = concatMap getResourcesFromStep (jobPlan ++ catMaybes [ jobOnSuccess, jobOnFailure, jobOnAbort, jobEnsure ])

dhallToYaml :: [Job] -> Value
dhallToYaml jobs =
  let resources = nub $ concatMap getResourcesFromJob jobs
      resourceTypes = nub $ customResourceTypes $ map Fly.Types.resourceType resources
  in object [ "resource_types" .= resourceTypes
            , "resources" .= resources
            , "jobs" .= jobs
            ]
