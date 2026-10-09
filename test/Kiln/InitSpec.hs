module Kiln.InitSpec (spec) where

import Control.Monad (forM_)
import Kiln.Init (kilnInit)
import Kiln.TestUtil (withTempDir)
import System.Directory (doesFileExist, withCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Hspec

-- Sample paths at various depths of the "default" template, used to
-- check that the whole tree (not just the top level) gets copied.
defaultSampleFiles :: [FilePath]
defaultSampleFiles =
  [ "template/404.html"
  , "kiln-config.json"
  , "res/kiln.png"
  , "res/fonts/JetBrainsMono-Regular.ttf"
  , "template/component/navbar.html"
  , "res/post-imgs/Markdown_Formatting_Showcase/pipeline.svg"
  , "scripts/classic/anti-flash.js"
  ]

-- Likewise, a few sample paths from the "flow" template.
flowSampleFiles :: [FilePath]
flowSampleFiles =
  [ "kiln-config.json"
  , "template/index.html"
  , "src/2024-03-02-00.md"
  , "style/base.css"
  , "scripts/post-collapse.js"
  ]

spec :: Spec
spec = describe "kilnInit" $ do
  it "dies when the current directory is not empty" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "already-here.txt" "hi"
        kilnInit "default" `shouldThrow` (== ExitFailure 1)

  it "copies the whole \"default\" template" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        kilnInit "default"
        forM_ defaultSampleFiles $ \f -> doesFileExist f `shouldReturn` True

  it "copies the whole \"flow\" template" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        kilnInit "flow"
        forM_ flowSampleFiles $ \f -> doesFileExist f `shouldReturn` True

  it "dies on an unrecognized template name" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $
        kilnInit "nonexistent" `shouldThrow` (== ExitFailure 1)
