module Kiln.InitSpec (spec) where

import Control.Monad (forM_)
import Kiln.Init (kilnInit)
import Kiln.TestUtil (withTempDir)
import System.Directory (doesFileExist, withCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Hspec

-- Sample paths at various depths of the hello-kiln template, used to check
-- that the whole tree (not just the top level) gets copied.
sampleFiles :: [FilePath]
sampleFiles =
  [ "template/404.html"
  , "kiln-config.json"
  , "res/kiln.png"
  , "res/fonts/JetBrainsMono-Regular.ttf"
  , "template/component/navbar.html"
  , "res/post-imgs/Markdown_Formatting_Showcase/pipeline.svg"
  , "scripts/classic/anti-flash.js"
  ]

spec :: Spec
spec = describe "kilnInit" $ do
  it "dies when the current directory is not empty" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "already-here.txt" "hi"
        kilnInit `shouldThrow` (== ExitFailure 1)

  it "copies the whole hello-kiln template into an empty directory" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        kilnInit
        forM_ sampleFiles $ \f -> doesFileExist f `shouldReturn` True
