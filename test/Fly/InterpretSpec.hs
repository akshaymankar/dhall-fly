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
        resourceType <- input auto "./test/data/in-built-resource-type.dhall"
        resourceType `shouldBe` ResourceTypeInBuilt "git"

      it "should interpret custom resource type" $ do
        resourceType <- input auto "./test/data/custom-resource-type.dhall"
        resourceType `shouldBe` boshDeploymentResourceType

    describe "Interpret Resource" $ do
      it "should interpret a git resource" $ do
        resource <- input auto "./test/data/resource.dhall"
        resource `shouldBe` gitKuboCI

    describe "Interpret TaskConfig" $ do
      it "should interpret a File task" $ do
        task <- input auto "./dhall-concourse/defaults/TaskConfig.dhall"
        task `shouldBe` defaultTaskConfig

boshDeploymentResourceType = ResourceTypeCustom (CustomResourceType{..}) where
  crtName = "bosh-deployment"
  crtType = "docker-image"
  crtSource =  pure $ fromList [ ("repository", String "cloudfoundry/bosh-deployment-resource")
                               , ("tag", String "latest")
                               ]
  crtPrivileged = Nothing
  crtParams = Nothing
  crtCheckEvery = Nothing
  crtTags = Nothing
  crtUniqueVersionHistory = Nothing

gitKuboCI = Resource{..} where
  resourceName =  "git-kubo-ci"
  resourceType = ResourceTypeInBuilt "git"
  resourceCheckEvery = Nothing
  resourceParams = Nothing
  resourceSource = Just $ fromList [ "privateKey" .= "((git-ssh-key.private_key))"
                                   , "uri" .= "git@github.com:cloudfoundry-incubator/kubo-ci"
                                   , "branch" .= "master"
                                   ]
  resourceTags = Nothing
  resourceVersion = Nothing
  resourceWebhookToken = Nothing
  resourceIcon = Nothing
  resourcePublic = Nothing

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
  tcRootfsUri = Nothing
  tcContainerLimits = Nothing
