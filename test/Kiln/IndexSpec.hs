module Kiln.IndexSpec (spec) where

import Kiln.Index (PostEntry (..), renderPostsList)
import Test.Hspec

spec :: Spec
spec = describe "renderPostsList" $ do
  it "renders a single post in a single year" $
    renderPostsList
      "/"
      [PostEntry {postTitle = "Only Post", postDate = "2024-03-15", postSlug = "only-post"}]
      `shouldBe` unlines
        [ "<div class=\"post-wrapper\">"
        , "    <div class=\"post-year-wrapper\" style=\"display: block;\">"
        , "        <h3>2024</h3>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>03-15 <a href=\"/post/only-post.html\">Only Post</a></p>"
        , "        </div>"
        , "    </div>"
        , "</div>"
        ]

  it "sorts posts within a year newest first" $
    renderPostsList
      "/"
      [ PostEntry {postTitle = "Old", postDate = "2024-01-01", postSlug = "old"}
      , PostEntry {postTitle = "New", postDate = "2024-05-01", postSlug = "new"}
      ]
      `shouldBe` unlines
        [ "<div class=\"post-wrapper\">"
        , "    <div class=\"post-year-wrapper\" style=\"display: block;\">"
        , "        <h3>2024</h3>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>05-01 <a href=\"/post/new.html\">New</a></p>"
        , "        </div>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>01-01 <a href=\"/post/old.html\">Old</a></p>"
        , "        </div>"
        , "    </div>"
        , "</div>"
        ]

  it "sorts years newest first, each its own block" $
    renderPostsList
      "/"
      [ PostEntry {postTitle = "2023 Post", postDate = "2023-06-01", postSlug = "p2023"}
      , PostEntry {postTitle = "2024 Post", postDate = "2024-06-01", postSlug = "p2024"}
      ]
      `shouldBe` unlines
        [ "<div class=\"post-wrapper\">"
        , "    <div class=\"post-year-wrapper\" style=\"display: block;\">"
        , "        <h3>2024</h3>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>06-01 <a href=\"/post/p2024.html\">2024 Post</a></p>"
        , "        </div>"
        , "    </div>"
        , "    <div class=\"post-year-wrapper\" style=\"display: block;\">"
        , "        <h3>2023</h3>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>06-01 <a href=\"/post/p2023.html\">2023 Post</a></p>"
        , "        </div>"
        , "    </div>"
        , "</div>"
        ]

  it "renders nothing but the empty wrapper when there are no posts" $
    renderPostsList "/" [] `shouldBe` unlines ["<div class=\"post-wrapper\">", "</div>"]

  it "prefixes every post link with the webroot" $
    renderPostsList
      "/blog/"
      [PostEntry {postTitle = "Only Post", postDate = "2024-03-15", postSlug = "only-post"}]
      `shouldBe` unlines
        [ "<div class=\"post-wrapper\">"
        , "    <div class=\"post-year-wrapper\" style=\"display: block;\">"
        , "        <h3>2024</h3>"
        , "        <div class=\"post-wrapper\" style=\"display: block;\">"
        , "            <p>03-15 <a href=\"/blog/post/only-post.html\">Only Post</a></p>"
        , "        </div>"
        , "    </div>"
        , "</div>"
        ]
