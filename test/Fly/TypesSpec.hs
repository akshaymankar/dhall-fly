{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module Fly.TypesSpec where

import Test.Hspec

import Data.Aeson
import Data.HashMap.Strict
import Dhall
import Fly.Types

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Fly.Types" $ do
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
        task <- input auto "./test/data/task-config.dhall"
        task `shouldBe` defaultTaskConfig

    describe "GetStep" $ do
      let testGet = GetStep{..} where
            getGet = Nothing
            getResource = gitKuboCI
            getParams = Nothing
            getVersion = Nothing
            getPassed = Nothing
            getTrigger = Nothing
            getTags = Nothing
            getTimeout = Nothing
            getAttempts = Nothing
      describe "Interpret" $ do
        it "should interpret a get step" $ do
          get <- input auto "./test/data/get.dhall"
          get `shouldBe` testGet
      describe "ToJSON" $ do
        it "should translate a get step" $ do
          toJSON testGet `shouldBe` object [ "passed"   .= Null
                                           , "get"      .= "git-kubo-ci"
                                           , "attempts" .= Null
                                           , "params"   .= Null
                                           , "version"  .= Null
                                           , "resource" .= Null
                                           , "trigger"  .= Null
                                           , "timeout"  .= Null
                                           , "tags"     .= Null ]

    describe "PutStep" $ do
      let testPut = PutStep{..} where
            putPut = Nothing
            putResource = gitKuboCI
            putParams = Nothing
            putGetParams = Nothing
            putInputs = Nothing
            putTags = Nothing
            putTimeout = Nothing
            putAttempts = Nothing
      describe "Interpret" $ do
        it "should interpret a put step" $ do
          put <- input auto "./test/data/put.dhall"
          put `shouldBe` testPut
      describe "ToJSON" $ do
        it "should translate a put step" $ do
          toJSON testPut `shouldBe` object [ "put"        .= "git-kubo-ci"
                                           , "attempts"   .= Null
                                           , "params"     .= Null
                                           , "get_params" .= Null
                                           , "resource"   .= Null
                                           , "timeout"    .= Null
                                           , "inputs"     .= Null
                                           , "tags"       .= Null ]

    describe "TaskStep" $ do
      let testTask = TaskStep{..} where
            taskTask = "test-task"
            taskConfig = TaskSpecFile "some-file"
            taskPrivileged = Nothing
            taskParams = Nothing
            taskImage = Nothing
            taskInputMapping = Nothing
            taskOutputMapping = Nothing
            taskVars = Nothing
            taskTags = Nothing
            taskTimeout = Nothing
            taskAttempts = Nothing

      describe "Interpret" $ do
        it "should interpret a task step" $ do
          task <- input auto "./test/data/task-step.dhall"
          task `shouldBe` testTask
      describe "ToJSON" $ do
        it "should translate a task step" $ do
          toJSON testTask `shouldBe` object [ "task"          .= "test-task"
                                            , "file"          .= "some-file"
                                            , "params"        .= Null
                                            , "privileged"    .= Null
                                            , "image"         .= Null
                                            , "input_mapping" .= Null
                                            , "output_mapping".= Null
                                            , "attempts"      .= Null
                                            , "timeout"       .= Null
                                            , "vars"          .= Null
                                            , "tags"          .= Null ]

boshDeploymentResourceType :: ResourceType
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

gitKuboCI :: Resource
gitKuboCI = Resource{..} where
  resourceName =  "git-kubo-ci"
  resourceType = ResourceTypeInBuilt "git"
  resourceCheckEvery = Nothing
  resourceSource = Just $ fromList [ "privateKey" .= "((git-ssh-key.private_key))"
                                   , "uri" .= "git@github.com:cloudfoundry-incubator/kubo-ci"
                                   , "branch" .= "master"
                                   ]
  resourceTags = Nothing
  resourceVersion = Nothing
  resourceWebhookToken = Nothing
  resourceIcon = Nothing
  resourcePublic = Nothing

defaultTaskRunConfig :: TaskRunConfig
defaultTaskRunConfig = TaskRunConfig{..} where
  trcPath = "true"
  trcArgs = Nothing
  trcDir = Nothing
  trcUser = Nothing

defaultTaskConfig :: TaskConfig
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
