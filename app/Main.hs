{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Dhall
import Fly.Types
import Fly.Interpret
import Data.Yaml
import Dhall.JSON
import Data.Maybe (catMaybes)
import Data.List (nub)

import qualified Data.Text.IO
import qualified Data.ByteString.Char8 as BC8

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
    stepResources (Task _ _) = []
    stepResources (Aggregate steps _) = concatMap getResourcesFromStep steps
    stepResources (Do steps _) = concatMap getResourcesFromStep steps
    stepResources (Try step _) = getResourcesFromStep step

getResourcesFromJob :: Job -> [Resource]
getResourcesFromJob Job{..} = concatMap getResourcesFromStep jobPlan

main :: IO ()
main = do
  stdin <- Data.Text.IO.getContents
  jobs <- input (list job) stdin
  let resources = nub $ concatMap getResourcesFromJob jobs
      resourceTypes = nub $ customResourceTypes $ map Fly.Types.resourceType resources
    in BC8.putStrLn $ encode $ omitNull $ object [ "resource_types" .= resourceTypes
                                                 , "resources" .= resources
                                                 , "jobs" .= jobs
                                                 ]
