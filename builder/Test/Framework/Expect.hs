module Test.Framework.Expect
  ( expectRight
  , expectPostPlan
  , expectIndexPlan
  ) where

import Modules.BuildPlan

expectRight :: String -> Either a b -> IO b
expectRight _ (Right value) = pure value
expectRight message (Left _) = error ("Assertion failed: " ++ message)

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"

expectIndexPlan :: BuildPlan -> IndexBuildPlan
expectIndexPlan (BuildIndexPlan plan) = plan
expectIndexPlan _ = error "expected BuildIndexPlan"
