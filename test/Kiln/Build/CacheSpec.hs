module Kiln.Build.CacheSpec (spec) where

import Data.Time.Clock (addUTCTime)
import Kiln.Build.Cache
  ( cacheFile
  , componentsFingerprint
  , isFileFresh
  , isGlobalFresh
  , recordFile
  , recordGlobal
  )
import Kiln.TestUtil (withTempDir)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getModificationTime
  , removeFile
  , setModificationTime
  , withCurrentDirectory
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "isFileFresh / recordFile" $ do
    it "is stale when nothing has been recorded yet" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "hello"
          isFileFresh "cache/src.src" "src.txt" `shouldReturn` False

    it "is fresh right after recordFile, with nothing changed" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "hello"
          recordFile "cache/src.src" "src.txt"
          isFileFresh "cache/src.src" "src.txt" `shouldReturn` True

    it "is stale once the file's content actually changes" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "hello"
          recordFile "cache/src.src" "src.txt"
          writeFile "src.txt" "goodbye"
          isFileFresh "cache/src.src" "src.txt" `shouldReturn` False

    it "catches a same-size content edit via the hash fallback, since the mtime/size precheck alone can't" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "hello"
          recordFile "cache/src.src" "src.txt"
          writeFile "src.txt" "olleh" -- same length, different content; mtime still advances naturally
          isFileFresh "cache/src.src" "src.txt" `shouldReturn` False

    it "is still fresh when only mtime moves but the content (and size) don't" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "hello"
          recordFile "cache/src.src" "src.txt"
          mtime <- getModificationTime "src.txt"
          setModificationTime "src.txt" (addUTCTime 60 mtime)
          isFileFresh "cache/src.src" "src.txt" `shouldReturn` True

  describe "isGlobalFresh / recordGlobal" $ do
    it "is stale when nothing has been recorded yet" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $
          isGlobalFresh "cache/global" "fingerprint-a" `shouldReturn` False

    it "is fresh once the same fingerprint has been recorded" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          recordGlobal "cache/global" "fingerprint-a"
          isGlobalFresh "cache/global" "fingerprint-a" `shouldReturn` True

    it "is stale once the fingerprint changes" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          recordGlobal "cache/global" "fingerprint-a"
          isGlobalFresh "cache/global" "fingerprint-b" `shouldReturn` False

  describe "cacheFile" $
    it "copies the source file's contents to the cache path, creating parent directories" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "src.txt" "payload"
          cacheFile "cache/nested/dest.txt" "src.txt"
          doesFileExist "cache/nested/dest.txt" `shouldReturn` True
          contents <- readFile "cache/nested/dest.txt"
          contents `shouldBe` "payload"

  describe "componentsFingerprint" $ do
    it "is empty when there's no component directory" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $
          componentsFingerprint "." `shouldReturn` ""

    it "is the same whatever order the components were created in" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          createDirectoryIfMissing True "component"
          writeFile "component/b.html" "B"
          writeFile "component/a.html" "A"
          one <- componentsFingerprint "."
          writeFile "component/a.html" "A" -- rewritten, later mtime
          two <- componentsFingerprint "."
          two `shouldBe` one

    it "changes when a component's content changes" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          createDirectoryIfMissing True "component"
          writeFile "component/a.html" "A"
          earlier <- componentsFingerprint "."
          writeFile "component/a.html" "A!"
          later <- componentsFingerprint "."
          later `shouldNotBe` earlier

    -- Plain concatenation left the boundaries between files invisible,
    -- so this edit produced an identical fingerprint -- and every page
    -- kept its cache and its stale output, even though the two partials
    -- are spliced at different places in the template.
    it "distinguishes a block moved from one component's tail to the next's head" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          createDirectoryIfMissing True "component"
          writeFile "component/a.html" "ALPHA|MOVED"
          writeFile "component/b.html" "BETA"
          earlier <- componentsFingerprint "."
          writeFile "component/a.html" "ALPHA"
          writeFile "component/b.html" "|MOVEDBETA"
          later <- componentsFingerprint "."
          later `shouldNotBe` earlier

    it "distinguishes a component renamed without any content change" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          createDirectoryIfMissing True "component"
          writeFile "component/a.html" "SAME"
          earlier <- componentsFingerprint "."
          removeFile "component/a.html"
          writeFile "component/c.html" "SAME"
          later <- componentsFingerprint "."
          later `shouldNotBe` earlier
