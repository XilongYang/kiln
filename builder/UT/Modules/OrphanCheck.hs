module UT.Modules.OrphanCheck (suiteName, testCases) where

import Modules.Utils.OrphanCheck (findOrphanPosts)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for orphaned generated-post detection.
suiteName :: String
suiteName = "OrphanCheck"

testCases :: [TestCase]
testCases =
  [ mkTestCase "findOrphanPosts returns empty when all generated posts have sources" $ do
      let postFiles = ["a.html", "b.html", "readme.txt"]
      let srcFiles = ["a.md", "b.md", "draft.txt"]
      assertEq "all html pages should be matched by same-name markdown files"
        []
        (findOrphanPosts "posts" postFiles srcFiles)
  , mkTestCase "findOrphanPosts returns only html pages missing markdown source" $ do
      let postFiles = ["a.html", "b.html", "c.html"]
      let srcFiles = ["a.md", "c.md"]
      assertEq "only b.html should be reported as orphan"
        ["posts/b.html"]
        (findOrphanPosts "posts" postFiles srcFiles)
  , mkTestCase "findOrphanPosts ignores non-target extensions" $ do
      let postFiles = ["a.htm", "b.HTML", "c.html"]
      let srcFiles = ["c.markdown", "d.md"]
      assertEq "matching is strict: only .html and .md are considered"
        ["posts/c.html"]
        (findOrphanPosts "posts" postFiles srcFiles)
  ]
