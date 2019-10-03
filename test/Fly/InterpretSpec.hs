{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module Fly.InterpretSpec where

import Test.Hspec

import Data.Aeson
import Data.HashMap.Strict
import Dhall
import Fly.Types

spec :: Spec
spec = do
  describe "Fly.Interpret" $ do
    describe "Interpret ResourceType" $ do
      it "should interpret in built resource type" $ do
        resourceType <- input auto "let x = ./dhall-concourse/examples/kubo/resources/git-kubo-ci.dhall in x.type"
        resourceType `shouldBe` ResourceTypeInBuilt "git"
      it "should interpret custom resource type" $ do
        resourceType <- input auto "./dhall-concourse/examples/kubo/resource-types/bosh-deployment.dhall"
        resourceType `shouldBe` boshDeploymentResourceType

    describe "Interpret Resource" $ do
      it "should interpret a git resource" $ do
        resource <- input auto "./dhall-concourse/examples/kubo/resources/git-kubo-ci.dhall"
        resource `shouldBe` gitKuboCI

    describe "Interpret TaskConfig" $ do
      it "should interpret a File task" $ do
        task <- input auto "./dhall-concourse/defaults/TaskConfig.dhall"
        task `shouldBe` defaultTaskConfig

boshDeploymentResourceType = ResourceTypeCustom{..} where
  rtcName = "bosh-deployment"
  rtcType = "docker-image"
  rtcSource =  pure $ fromList [ ("repository", String "cloudfoundry/bosh-deployment-resource")
                               , ("tag", String "latest")
                               ]

gitKuboCI = Resource{..} where
  resourceName =  "git-kubo-ci"
  resourceType = ResourceTypeInBuilt "git"
  resourceCheckEvery = Nothing
  resourceParams = Nothing
  resourceSource = Just $ object [ "privateKey" .= "((git-ssh-key.private_key))"
                                 , "uri" .= "git@github.com:cloudfoundry-incubator/kubo-ci"
                                 , "branch" .= "master"
                                 ]
  resourceTags = Nothing
  resourceVersion = Nothing
  resourceWebhookToken = Nothing

defaultTaskRunConfig = TaskRunConfig{..} where
  trcPath = "CHANGEME"
  trcArgs = Nothing
  trcDir = Nothing
  trcUser = Nothing

defaultTaskConfig = TaskConfig{..} where
  tcPlatform = "linux"
  tcRun = defaultTaskRunConfig
  tcCaches = Nothing
  tcImageResource = Nothing
  tcInputs = Nothing
  tcOutputs = Nothing
  tcParams = Nothing
  tcRootfsURI = Nothing
