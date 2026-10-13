module Kiln.Build.MarkdownSpec (spec) where

import Kiln.Build.Markdown (olify, rewriteLanguageMarks, splitAbstract, splitFrontmatter)
import Test.Hspec

-- | The attribute block `rewriteLanguageMarks` produces for a language
-- mark it accepts.
attrs :: String -> String
attrs mark = "``` {.language-" ++ mark ++ " .line-numbers .match-braces}"

spec :: Spec
spec = do
  describe "rewriteLanguageMarks" $ do
    it "rewrites a fence's bare language mark into pandoc attributes" $
      rewriteLanguageMarks "```haskell\nmain = pure ()\n```\n"
        `shouldBe` attrs "haskell" ++ "\nmain = pure ()\n```\n"

    it "leaves a fence with no language mark alone" $
      rewriteLanguageMarks "```\nplain\n```\n" `shouldBe` "```\nplain\n```\n"

    it "keeps a fence's indentation" $
      rewriteLanguageMarks "  ```js\n  x\n  ```\n"
        `shouldBe` "  " ++ attrs "js" ++ "\n  x\n  ```\n"

    it "accepts a language mark with the punctuation a class can carry" $
      rewriteLanguageMarks "```c++\nint main();\n```\n"
        `shouldBe` attrs "c++" ++ "\nint main();\n```\n"

    -- Splicing into an info string that already carried attributes
    -- produced a nested attribute block, which pandoc rendered as a
    -- paragraph of inline code -- losing the entire code block, with a
    -- zero exit status and no warning.
    it "leaves a fence whose info string already carries pandoc attributes alone" $
      rewriteLanguageMarks "```python {.numberLines}\nx = 1\n```\n"
        `shouldBe` "```python {.numberLines}\nx = 1\n```\n"

    it "leaves a multi-word info string alone" $
      rewriteLanguageMarks "```haskell ghci\nx\n```\n"
        `shouldBe` "```haskell ghci\nx\n```\n"

    -- A post about markdown quotes fences inside a longer fence. Those
    -- are the post's content, not markup for the builder to rewrite.
    it "leaves a fence quoted inside a longer fence alone" $
      rewriteLanguageMarks "````\n```python\nx = 1\n```\n````\n"
        `shouldBe` "````\n```python\nx = 1\n```\n````\n"

    it "rewrites a fence that follows a quoted one, having tracked the close" $
      rewriteLanguageMarks "````\n```js\n````\n\n```js\nreal\n```\n"
        `shouldBe` "````\n```js\n````\n\n" ++ attrs "js" ++ "\nreal\n```\n"

    it "leaves prose that merely mentions a fence alone" $
      rewriteLanguageMarks "a ```js inline mention\n"
        `shouldBe` "a ```js inline mention\n"

  describe "splitAbstract" $ do
    it "splits on the marker, keeping the frontmatter on the body" $
      splitAbstract "---\ntitle: T\n---\n\nintro\n\n<!--more-->\n\nbody\n"
        `shouldBe` (Just "intro", "---\ntitle: T\n---\n\n\nbody\n")

    it "returns no abstract when the post has no marker" $
      splitAbstract "---\ntitle: T\n---\n\nbody\n"
        `shouldBe` (Nothing, "---\ntitle: T\n---\n\nbody\n")

    it "splits a post with no frontmatter at all" $
      splitAbstract "intro\n\n<!--more-->\n\nbody\n"
        `shouldBe` (Just "intro", "\n\nbody\n")

    it "ignores surrounding whitespace on the marker line" $
      splitAbstract "intro\n\n  <!--more-->  \n\nbody\n"
        `shouldBe` (Just "intro", "\n\nbody\n")

    -- The marker is an instruction only in prose; inside a fence it is
    -- part of the HTML the post is showing its reader.
    it "ignores a marker inside a fenced code block" $
      splitAbstract "intro\n\n```html\n<!--more-->\n```\n\nbody\n"
        `shouldBe` (Nothing, "intro\n\n```html\n<!--more-->\n```\n\nbody\n")

    it "splits on a real marker that follows a quoted one" $
      splitAbstract "intro\n\n```html\n<!--more-->\n```\n\n<!--more-->\n\nbody\n"
        `shouldBe` (Just "intro\n\n```html\n<!--more-->\n```", "\n\nbody\n")

  describe "splitFrontmatter" $ do
    it "splits off a leading metadata block, delimiters included" $
      splitFrontmatter ["---", "title: T", "---", "", "body"]
        `shouldBe` (["---", "title: T", "---"], ["", "body"])

    it "returns no frontmatter when the document doesn't open with one" $
      splitFrontmatter ["body", "---", "more"]
        `shouldBe` ([], ["body", "---", "more"])

    it "returns no frontmatter when the block is never closed" $
      splitFrontmatter ["---", "title: T"]
        `shouldBe` ([], ["---", "title: T"])

  describe "olify" $ do
    it "rewrites every ul tag pair into an ol" $
      olify "<ul><li>a<ul><li>b</li></ul></li></ul>"
        `shouldBe` "<ol><li>a<ol><li>b</li></ol></li></ol>"

    it "leaves a fragment with no lists untouched" $
      olify "<p>nothing here</p>" `shouldBe` "<p>nothing here</p>"
