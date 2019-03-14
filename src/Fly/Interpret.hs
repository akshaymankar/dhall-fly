{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Fly.Interpret where

import Data.Aeson          (Value (..))
import Data.HashMap.Strict as M
import Data.Scientific
import Data.Text           (Text)
import Dhall
import Dhall.Core          (Chunks (..), Expr (..), Var (..))
import Dhall.Map           (Map, fromList, lookup)
import Dhall.Parser        (Src)
import Dhall.TH
import Dhall.TypeCheck     (X)
import Fly.Types           hiding (autoHooks, getVersion, resourceType,
                            taskSpec)

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Vector   as V

import qualified Dhall.Core
import qualified Dhall.JSON

assocList :: Type Value
assocList = Type{..} where
  expected = [dhallExpr|List ./dhall-concourse/types/TextTextPair.dhall|]
  extract l@(ListLit _ _) = case Dhall.JSON.dhallToJSON (Dhall.JSON.convertToHomogeneousMaps c l) of
                              Left _  -> Nothing
                              Right v -> pure v
                            where c = Dhall.JSON.Conversion "mapKey" "mapValue"
  extract _ = Nothing

textPair :: Type (Text, Text)
textPair = Type{..} where
  expected = [dhallExpr|./dhall-concourse/types/TextTextPair.dhall|]
  extract (RecordLit m) = (,)
                       <$> extractFromMap "mapKey" auto
                       <*> extractFromMap "mapValue" auto
                       where extractFromMap = withType m
  extract _ = Nothing

instance Interpret Value where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/JSONObject.dhall|]
    extract (Lam _ _ -- JSON
             (Lam _ _ -- string
              (Lam _ _ -- number
               (Lam _ _ -- object
                (Lam _ _ -- array
                 (Lam _ _ -- bool
                  (Lam _ _ -- null
                   x))))))) = extractJSONFromApps x
    extract x = extractJSONFromApps x
    extractJSONFromApps (App (Var (V "string" _)) (TextLit (Chunks _ t))) = pure $ String t
    extractJSONFromApps (App (Var (V "number" _)) (DoubleLit n)) = pure $ Number $ fromFloatDigits n
    extractJSONFromApps (App (Var (V "object" _)) o) = Object <$> Dhall.extract auto o
    extractJSONFromApps (App (Var (V "array" _)) a) = Array . V.fromList <$> Dhall.extract auto a
    extractJSONFromApps (App (Var (V "bool" _)) b) = Data.Aeson.Bool <$> Dhall.extract auto b
    extractJSONFromApps (Var (V "null" _)) = pure Null
    extractJSONFromApps x = Nothing

instance Interpret (HashMap Text Value) where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/JSONObject.dhall|]
    extract (ListLit _ x) = M.fromList <$> (Prelude.sequence $ Prelude.map extractPair (F.toList x))
    extractPair (RecordLit m) = do
      key <- Dhall.extract auto =<< (Dhall.Map.lookup "mapKey" m)
      val <- Dhall.extract auto =<< (Dhall.Map.lookup "mapValue" m)
      Just (key, val)
    extractPair _ = Nothing

instance Interpret ResourceType where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/ResourceType.dhall|]
    extract (UnionLit "Custom" (RecordLit m) _) = extractCustomResourceType m
    extract (UnionLit "InBuilt" (TextLit (Chunks [] t)) _) = pure $ ResourceTypeInBuilt t
    extract _ = Nothing
    extractCustomResourceType m =
      ResourceTypeCustom
        <$> extractFromMap "name" auto
        <*> extractFromMap "type" auto
        <*> extractFromMap "source" auto
        where extractFromMap = withType m

instance Interpret Resource where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/Resource.dhall|]
    extract (RecordLit m) =
      Resource
      <$> extractFromMap "name"          auto
      <*> extractFromMap "type"          auto
      <*> extractFromMap "source"        (Dhall.maybe assocList)
      <*> extractFromMap "version"       (Dhall.maybe assocList)
      <*> extractFromMap "params"        (Dhall.maybe assocList)
      <*> extractFromMap "check_every"   auto
      <*> extractFromMap "tags"          (Dhall.maybe $ list auto)
      <*> extractFromMap "webhook_token" auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskRunConfig where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskRunConfig.dhall|]
    extract (RecordLit m) =
      TaskRunConfig
      <$> extractFromMap "path" auto
      <*> extractFromMap "args" (Dhall.maybe $ list auto)
      <*> extractFromMap "dir"  auto
      <*> extractFromMap "user" auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskImageResource where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/ImageResource.dhall|]
    extract (RecordLit m) =
      TaskImageResource
      <$> extractFromMap "type" auto
      <*> extractFromMap "source" assocList
      <*> extractFromMap "params"  (Dhall.maybe assocList)
      <*> extractFromMap "version" (Dhall.maybe assocList)
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskInput where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskInput.dhall|]
    extract (RecordLit m) =
      TaskInput
      <$> extractFromMap "name" auto
      <*> extractFromMap "path" auto
      <*> extractFromMap "optional"  auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskOutput where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskOutput.dhall|]
    extract (RecordLit m) =
      TaskOutput
      <$> extractFromMap "name" auto
      <*> extractFromMap "path" auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskCache where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskCache.dhall|]
    extract (RecordLit m) =
      TaskCache
      <$> extractFromMap "path" auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskConfig where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskConfig.dhall|]
    extract (RecordLit m) =
      TaskConfig
      <$> extractFromMap "platform" auto
      <*> extractFromMap "run" auto
      <*> extractFromMap "image_resource" auto
      <*> extractFromMap "rootfs_uri" auto
      <*> extractFromMap "inputs" (Dhall.maybe $ list auto)
      <*> extractFromMap "outputs" (Dhall.maybe $ list auto)
      <*> extractFromMap "caches" (Dhall.maybe $ list auto)
      <*> extractFromMap "params" (Dhall.maybe $ list textPair)
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret TaskSpec where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskSpec.dhall|]
    extract (UnionLit "Config" c _) = TaskSpecConfig <$> Dhall.extract auto c
    extract (UnionLit "File" (TextLit (Chunks [] t)) _) = pure $ TaskSpecFile t
    extract _ = Nothing

instance Interpret GetVersion where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/GetVersion.dhall|]
    extract (UnionLit "Latest" (TextLit _) _) = pure GetVersionLatest
    extract (UnionLit "Every" (TextLit _) _) = pure GetVersionEvery
    extract (UnionLit "SpecificVersion" l@(ListLit _ _) _) =
      case Dhall.JSON.dhallToJSON l of
          Left _  -> Nothing
          Right v -> pure $ GetVersionSpecific v

instance Interpret GetStep where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/GetStep.dhall|]
    extract (RecordLit m) =
      GetStep
      <$> extractFromMap "get" auto
      <*> extractFromMap "resource" auto
      <*> extractFromMap "params" (Dhall.maybe assocList)
      <*> extractFromMap "version" auto
      <*> extractFromMap "passed" auto
      <*> extractFromMap "trigger" auto
      where extractFromMap = withType m

instance Interpret PutStep where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/PutStep.dhall|]
    extract (RecordLit m) =
      PutStep
      <$> extractFromMap "put" auto
      <*> extractFromMap "resource" auto
      <*> extractFromMap "params" (Dhall.maybe assocList)
      <*> extractFromMap "get_params" (Dhall.maybe assocList)
      where extractFromMap = withType m

instance Interpret TaskStep where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/TaskStep.dhall|]
    extract (RecordLit m) =
      TaskStep
      <$> extractFromMap "task" auto
      <*> extractFromMap "config" auto
      <*> extractFromMap "privileged" auto
      <*> extractFromMap "params" (Dhall.maybe $ list textPair)
      <*> extractFromMap "image" auto
      <*> extractFromMap "input_mapping" (Dhall.maybe $ list textPair)
      <*> extractFromMap "output_mapping" (Dhall.maybe $ list textPair)
      where extractFromMap = withType m

instance Interpret StepHooks where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/StepHooks.dhall ./dhall-concourse/types/Step.dhall|]
    extract (RecordLit m) =
      StepHooks
      <$> extractFromMap "on_success" auto
      <*> extractFromMap "on_failure" auto
      <*> extractFromMap "on_abort" auto
      <*> extractFromMap "ensure" auto
      where extractFromMap = withType m
    extract _ = Nothing

instance Interpret Step where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/Step.dhall|]
    extract (Lam _ _ -- Step
             (Lam _ _ -- GetStep
              (Lam _ _ -- PutStep
               (Lam _ _ -- TaskStep
                (Lam _ _ -- AggregateStep
                 (Lam _ _ -- DoStep
                  (Lam _ _ -- TryStep
                   x))))))) = extractStepFromApps x
    extract x = extractStepFromApps x -- While recursing it loses all the `Lam`s
    extractStepFromApps (App (App (Var (V "GetStep" _)) s) hooks) = buildStep Get s hooks
    extractStepFromApps (App (App (Var (V "PutStep" _)) s) hooks) = buildStep Put s hooks
    extractStepFromApps (App (App (Var (V "TaskStep" _)) s) hooks) = buildStep Task s hooks
    extractStepFromApps (App (App (Var (V "AggregateStep" _)) s) hooks) = buildStep Aggregate s hooks
    extractStepFromApps (App (App (Var (V "DoStep" _)) s) hooks) = buildStep Do s hooks
    extractStepFromApps (App (App (Var (V "TryStep" _)) s) hooks) = buildStep Try s hooks
    extractStepFromApps _ = Nothing
    buildStep f x y = f <$> Dhall.extract auto x <*> Dhall.extract auto y

instance Interpret Job where
  autoWith _ = Type{..} where
    expected = [dhallExpr|./dhall-concourse/types/Job.dhall|]
    extract (RecordLit m) =
      Job
      <$> extractFromMap "name" auto
      <*> extractFromMap "plan" auto
      <*> extractFromMap "serial" auto
      <*> extractFromMap "build_logs_to_retain" auto
      <*> extractFromMap "serial_groups" auto
      <*> extractFromMap "max_in_flight" auto
      <*> extractFromMap "public" auto
      <*> extractFromMap "disable_manual_trigger" auto
      <*> extractFromMap "interruptible" auto
      <*> extractFromMap "on_success" auto
      <*> extractFromMap "on_failure" auto
      <*> extractFromMap "on_abort" auto
      <*> extractFromMap "ensure" auto
      where extractFromMap = withType m
    extract _ = Nothing

withType :: Map Text (Expr Src X) -> Text -> Type a -> Maybe a
withType m key t = Dhall.extract t =<< Dhall.Map.lookup key m
