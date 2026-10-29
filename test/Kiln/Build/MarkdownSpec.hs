module Kiln.Build.MarkdownSpec (spec) where

import Kiln.Build.Markdown (absolutizeUrls, olify, rewriteLanguageMarks, splitAbstract, splitFrontmatter)
import Test.Hspec

-- | `absolutizeUrls` as a feed page calls it: against a site at
-- @https:\/\/example.com@ whose posts are published under @\/post\/@,
-- rewriting the body of the post at @\/post\/a.html@.
absolutize :: String -> String
absolutize = absolutizeUrls siteAbsolute (Just "https://example.com/post/a.html")

-- | A site-relative path as `Kiln.Build.Env.absoluteUrl` would resolve
-- it for that site.
siteAbsolute :: String -> String
siteAbsolute path = "https://example.com/" ++ dropWhile (== '/') path

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

  describe "absolutizeUrls" $ do
    it "joins a root-relative src onto the site's origin" $
      absolutize "<p><img src=\"/res/a.png\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/res/a.png\"></p>"

    it "joins a root-relative href onto the site's origin" $
      absolutize "<p><a href=\"/about.html\">about</a></p>"
        `shouldBe` "<p><a href=\"https://example.com/about.html\">about</a></p>"

    it "rewrites a video's poster alongside its src" $
      absolutize "<video src=\"/res/a.mp4\" poster=\"/res/a.png\"></video>"
        `shouldBe` "<video src=\"https://example.com/res/a.mp4\" poster=\"https://example.com/res/a.png\"></video>"

    it "resolves a document-relative url against the post's own page, dot segments and all" $
      absolutize "<p><img src=\"../res/a.png\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/res/a.png\"></p>"

    it "resolves a sibling document-relative url against the post's own directory" $
      absolutize "<p><img src=\"imgs/a.png\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/post/imgs/a.png\"></p>"

    -- Every footnote reference and back-link pandoc generates is one of
    -- these, and in a reader the page they'd resolve against is the
    -- reader's own.
    it "resolves a bare fragment against the post's own page" $
      absolutize "<p><a href=\"#fn1\">1</a></p>"
        `shouldBe` "<p><a href=\"https://example.com/post/a.html#fn1\">1</a></p>"

    it "leaves a url that already carries a scheme alone" $
      absolutize "<p><a href=\"https://pandoc.org\">pandoc</a> <a href=\"mailto:a@b.c\">mail</a></p>"
        `shouldBe` "<p><a href=\"https://pandoc.org\">pandoc</a> <a href=\"mailto:a@b.c\">mail</a></p>"

    it "leaves a protocol-relative url alone" $
      absolutize "<p><img src=\"//cdn.example.org/a.png\"></p>"
        `shouldBe` "<p><img src=\"//cdn.example.org/a.png\"></p>"

    it "leaves an empty url alone" $
      absolutize "<p><a href=\"\">nowhere</a></p>" `shouldBe` "<p><a href=\"\">nowhere</a></p>"

    it "rewrites a single-quoted attribute" $
      absolutize "<p><img src='/res/a.png'></p>"
        `shouldBe` "<p><img src='https://example.com/res/a.png'></p>"

    -- `href` is a suffix of plenty of attribute names that hold
    -- something else entirely; only a whole attribute name counts.
    it "leaves an attribute that merely ends in a url attribute's name alone" $
      absolutize "<p><a data-href=\"/res/a.png\">x</a></p>"
        `shouldBe` "<p><a data-href=\"/res/a.png\">x</a></p>"

    -- An alt or title beginning with a slash is prose, and rewriting it
    -- would corrupt text the reader displays.
    it "leaves an attribute that isn't a url alone, however its value starts" $
      absolutize "<p><img src=\"/res/a.png\" alt=\"/usr/bin/env\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/res/a.png\" alt=\"/usr/bin/env\"></p>"

    it "leaves text that merely looks like an attribute alone" $
      absolutize "<p>write it as src=\"/res/a.png\" to embed it</p>"
        `shouldBe` "<p>write it as src=\"/res/a.png\" to embed it</p>"

    -- A post *about* HTML is the one this would otherwise corrupt: in a
    -- code block the markup is already escaped, so there is no tag here
    -- at all -- just text that reads like one.
    it "leaves the HTML a post quotes in a code block alone" $
      absolutize "<pre><code>&lt;img src=\"/res/a.png\"&gt;</code></pre>"
        `shouldBe` "<pre><code>&lt;img src=\"/res/a.png\"&gt;</code></pre>"

    it "keeps a url's query and fragment" $
      absolutize "<p><a href=\"/search.html?q=a#top\">s</a></p>"
        `shouldBe` "<p><a href=\"https://example.com/search.html?q=a#top\">s</a></p>"

    it "never climbs out of the site's origin" $
      absolutize "<p><img src=\"/../../a.png\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/a.png\"></p>"

    it "leaves a body with no urls in it untouched" $
      absolutize "<p>nothing here</p>" `shouldBe` "<p>nothing here</p>"

    -- A site rendering no standalone post pages has no base to resolve
    -- these against; a link that still points where it always did beats
    -- a confidently wrong one.
    it "leaves relative urls alone for a post with no page of its own" $
      absolutizeUrls siteAbsolute Nothing "<p><a href=\"#fn1\">1</a><img src=\"a.png\"></p>"
        `shouldBe` "<p><a href=\"#fn1\">1</a><img src=\"a.png\"></p>"

    it "still absolutizes root-relative urls for a post with no page of its own" $
      absolutizeUrls siteAbsolute Nothing "<p><img src=\"/res/a.png\"></p>"
        `shouldBe` "<p><img src=\"https://example.com/res/a.png\"></p>"

    -- A site that declared no origin: there is nothing to absolutize
    -- against, and `Kiln.Build.Env.absoluteUrl` hands back what it got.
    it "leaves every url alone for a site with no declared origin" $
      absolutizeUrls id (Just "/post/a.html") "<p><img src=\"/res/a.png\"><a href=\"#fn1\">1</a></p>"
        `shouldBe` "<p><img src=\"/res/a.png\"><a href=\"/post/a.html#fn1\">1</a></p>"
