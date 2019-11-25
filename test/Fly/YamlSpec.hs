{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Fly.YamlSpec where

import Test.Hspec

import Data.Aeson
import Fly.Types
import Fly.Yaml

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Fly.Yaml" $ do
    let gitResource = Resource { resourceName ="some-git"
                               , resourceType = ResourceTypeInBuilt "git"
                               , resourcePublic = Nothing
                               , resourceSource = Nothing
                               , resourceVersion = Nothing
                               , resourceIcon = Nothing
                               , resourceTags = Nothing
                               , resourceCheckEvery = Nothing
                               , resourceWebhookToken = Nothing
                               }

        slackResourceType = CustomResourceType { crtName = "slack"
                                               , crtType = "docker-image"
                                               , crtSource = Nothing
                                               , crtPrivileged = Nothing
                                               , crtParams = Nothing
                                               , crtCheckEvery = Nothing
                                               , crtTags = Nothing
                                               , crtUniqueVersionHistory = Nothing
                                               }
        stepSlack = Resource { resourceName ="step-slack"
                             , resourceType = ResourceTypeCustom slackResourceType
                             , resourcePublic = Nothing
                             , resourceSource = Nothing
                             , resourceVersion = Nothing
                             , resourceIcon = Nothing
                             , resourceTags = Nothing
                             , resourceCheckEvery = Nothing
                             , resourceWebhookToken = Nothing
                             }
        jobSlack = stepSlack { resourceName ="job-slack" }
        getGit = GetStep { getGet = Nothing
                         , getResource = gitResource
                         , getTags = Nothing
                         , getPassed = Nothing
                         , getTrigger = Nothing
                         , getTimeout = Nothing
                         , getAttempts = Nothing
                         , getParams = Nothing
                         , getVersion = Nothing
                         }
        putStepSlack = PutStep { putPut = Nothing
                               , putResource = stepSlack
                               , putTags = Nothing
                               , putTimeout = Nothing
                               , putAttempts = Nothing
                               , putParams = Nothing
                               , putGetParams = Nothing
                               , putInputs = Nothing
                               }
        putJobSlack = putStepSlack { putResource = jobSlack }
        emptyStepHooks = StepHooks Nothing Nothing Nothing Nothing
        hooks = emptyStepHooks{ hookOnFailure = Just (Put putStepSlack emptyStepHooks) }
        testJob = Job{ jobName = "test-job"
                     , jobPlan = [Get getGit hooks]
                     , jobOldName = Nothing
                     , jobSerial = Nothing
                     , jobBuildLogRetention = Nothing
                     , jobBuildLogsToRetain = Nothing
                     , jobSerialGroups = Nothing
                     , jobMaxInFlight = Nothing
                     , jobPublic = Nothing
                     , jobDisableManualTrigger = Nothing
                     , jobInterruptible = Nothing
                     , jobOnSuccess = Just (Put putJobSlack emptyStepHooks)
                     , jobOnFailure = Nothing
                     , jobOnAbort = Nothing
                     , jobEnsure = Nothing
                     }

    -- TODO: Make unordered assertions
    describe "jobsToValue (without groups)" $ do
      it "should translate jobs to Value as concourse expects it" $ do
        jobsToValue [testJob]
          `shouldBe` object [ "resources"      .= [gitResource, stepSlack, jobSlack]
                            , "resource_types" .= [slackResourceType]
                            , "jobs"           .= [testJob]
                            ]

    describe "groupedJobsToValue" $ do
      it "should translate grouped jobs to list of groups as concourse expects" $ do
        let groupedJob = GroupedJob testJob ["group1", "group2"]
        groupedJobsToValue [groupedJob]
          `shouldBe` object [ "resources"      .= [gitResource, stepSlack, jobSlack]
                            , "resource_types" .= [slackResourceType]
                            , "jobs"           .= [testJob]
                            , "groups"         .= toJSON [ object [ "name" .= "group2"
                                                                  , "jobs" .= [ jobName testJob ] ]
                                                         , object [ "name" .= "group1"
                                                                  , "jobs" .= [ jobName testJob ] ]
                                                         ]
                            ]
