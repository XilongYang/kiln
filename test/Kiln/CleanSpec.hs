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
  \\"fonts\":{\"subset-dir\":\"dist/fonts\",\"sources\":[\"res/JetBrainsMono-Regular.ttf\"]},\
  \\"post\":{\"template\":\"post.html\",\"output-dir\":\"dist/post\"},\
  \\"pages\":[{\"template\":\"index.html\",\"output\":\"dist/index.html\"}]}}"

-- | A config whose post pages land at the project root, beside the
-- sources and the config itself -- a reasonable thing to want, and the
-- shape that made a recursive clean catastrophic.
configJsonAtRoot :: String
configJsonAtRoot =
  "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
  \\"opt\":{\"webroot\":\"/\"},\
  \\"target\":{\"searchdb\":null,\"cache-dir\":\".cache\",\"fonts\":null,\
  \\"post\":{\"template\":\"post.html\",\"output-dir\":\".\"},\
  \\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

-- | Stand in for what a build recorded: one cache entry per post it
-- rendered, which is what tells `kilnClean` which output files are its.
recordBuilt :: FilePath -> [String] -> IO ()
recordBuilt cacheDir slugs = do
  createDirectoryIfMissing True (cacheDir </> "stage")
  forM_ slugs $ \slug -> writeFile (cacheDir </> "stage" </> slug ++ ".src") "snapshot"

spec :: Spec
spec = describe "kilnClean" $ do
  it "removes every output it recorded building, and the directories left empty" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJson
        recordBuilt "dist/.cache" ["hello"]
        createDirectoryIfMissing True "dist/post"
        writeFile "dist/post/hello.html" "hi"
        createDirectoryIfMissing True "dist/fonts"
        writeFile "dist/fonts/JetBrainsMono-Regular.woff2" "font"
        writeFile "dist/searchdb.json" "{}"
        writeFile "dist/index.html" "<html></html>"

        kilnClean

        forM_
          [ "dist/post/hello.html"
          , "dist/post"
          , "dist/fonts/JetBrainsMono-Regular.woff2"
          , "dist/fonts"
          , "dist/searchdb.json"
          , "dist/index.html"
          , "dist/.cache"
          ]
          $ \p -> doesPathExist p `shouldReturn` False

  it "takes a post's slug from its source file when the cache is already gone" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJson
        createDirectoryIfMissing True "src"
        writeFile "src/hello.md" "---\ntitle: T\ndate: 2024-01-01\n---\n"
        createDirectoryIfMissing True "dist/post"
        writeFile "dist/post/hello.html" "hi"

        kilnClean

        doesPathExist "dist/post/hello.html" `shouldReturn` False

  -- The whole point of deriving the file list from what kiln recorded:
  -- a post output directory may be shared, and used to be removed whole.
  it "leaves a file in the post output directory that it never built" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJson
        recordBuilt "dist/.cache" ["hello"]
        createDirectoryIfMissing True "dist/post"
        writeFile "dist/post/hello.html" "hi"
        writeFile "dist/post/hand-written.html" "mine"

        kilnClean

        doesPathExist "dist/post/hello.html" `shouldReturn` False
        doesPathExist "dist/post/hand-written.html" `shouldReturn` True
        doesPathExist "dist/post" `shouldReturn` True

  it "keeps the project when the post output directory is the project root" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile "kiln-config.json" configJsonAtRoot
        recordBuilt ".cache" ["hello"]
        createDirectoryIfMissing True "src"
        writeFile "src/hello.md" "---\ntitle: T\ndate: 2024-01-01\n---\n"
        createDirectoryIfMissing True "template"
        writeFile "template/post.html" "$body$"
        writeFile "hello.html" "built"
        writeFile "index.html" "built"

        kilnClean

        -- Its own outputs go...
        doesPathExist "hello.html" `shouldReturn` False
        doesPathExist "index.html" `shouldReturn` False
        doesPathExist ".cache" `shouldReturn` False
        -- ...and nothing else does.
        forM_ ["kiln-config.json", "src/hello.md", "template/post.html", "."] $ \p ->
          doesPathExist p `shouldReturn` True

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

  it "doesn't try to remove a search index when \"searchdb\" is null" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        writeFile
          "kiln-config.json"
          "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
          \\"opt\":{\"webroot\":\"/\"},\
          \\"target\":{\"searchdb\":null,\"cache-dir\":\"dist/.cache\",\"fonts\":null,\
          \\"post\":{\"template\":\"post.html\",\"output-dir\":\"dist/post\"},\"pages\":[]}}"
        kilnClean `shouldReturn` ()
