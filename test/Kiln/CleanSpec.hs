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
  "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
  \\"opt\":{\"webroot\":\"/\"},\
  \\"target\":{\"searchdb\":\"dist/searchdb.json\",\"cache-dir\":\"dist/.cache\",\
  \\"fonts\":{\"subset-dir\":\"dist/fonts\",\"sources\":[]},\
  \\"post\":{\"template\":\"post.html\",\"output-dir\":\"dist/post\"},\
  \\"pages\":[{\"template\":\"index.html\",\"output\":\"dist/index.html\"}]}}"

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

  it "doesn't try to remove a post output directory when \"post\" is null" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile
          "kiln-config.json"
          "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
          \\"opt\":{\"webroot\":\"/\"},\
          \\"target\":{\"searchdb\":\"dist/searchdb.json\",\"cache-dir\":\"dist/.cache\",\
          \\"fonts\":{\"subset-dir\":\"dist/fonts\",\"sources\":[]},\"post\":null,\"pages\":[]}}"
        kilnClean `shouldReturn` ()

  it "doesn't try to remove a fonts subset directory when \"fonts\" is null" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile
          "kiln-config.json"
          "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
          \\"opt\":{\"webroot\":\"/\"},\
          \\"target\":{\"searchdb\":\"dist/searchdb.json\",\"cache-dir\":\"dist/.cache\",\"fonts\":null,\
          \\"post\":{\"template\":\"post.html\",\"output-dir\":\"dist/post\"},\"pages\":[]}}"
        kilnClean `shouldReturn` ()
