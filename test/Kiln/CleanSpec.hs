module Kiln.CleanSpec (spec) where

import Control.Monad (forM_)
import Kiln.Clean (kilnClean)
import Kiln.TestUtil (withTempDir)
import System.Directory
  ( createDirectoryIfMissing
  , doesPathExist
  , withCurrentDirectory
  )
import System.FilePath ((</>))
import Test.Hspec

configJson :: String
configJson =
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
  \\"out\":{\"post\":\"dist/post\",\"fonts-subset\":\"dist/fonts\",\"searchdb\":\"dist/searchdb.json\",\
  \\"index\":\"dist/index.html\",\"cache\":\"dist/.cache\"}},\"webroot\":\"/\",\"fonts\":{}}"

outPaths :: [FilePath]
outPaths =
  [ "dist/post"
  , "dist/fonts"
  , "dist/searchdb.json"
  , "dist/index.html"
  , "dist/.cache"
  ]

spec :: Spec
spec = describe "kilnClean" $ do
  it "removes every configured output file and directory" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJson
        createDirectoryIfMissing True "dist/post"
        writeFile ("dist" </> "post" </> "hello.html") "hi"
        createDirectoryIfMissing True "dist/fonts"
        writeFile "dist/searchdb.json" "{}"
        writeFile "dist/index.html" "<html></html>"
        createDirectoryIfMissing True "dist/.cache"

        kilnClean

        forM_ outPaths $ \p -> doesPathExist p `shouldReturn` False

  it "does nothing when the configured outputs do not exist yet" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJson
        kilnClean `shouldReturn` ()
