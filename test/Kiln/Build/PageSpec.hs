module Kiln.Build.PageSpec (spec) where

import Kiln.Build.Env (BuildEnv (..))
import Kiln.Build.Page (PostListItem (..), absoluteItems, postListItems)
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (PostPageConfig (..))
import Kiln.TestUtil (testEnv)
import Test.Hspec

-- | A site rendering standalone post pages into @post/@, at `webroot`.
listAt :: String -> [PostEntry] -> [PostListItem]
listAt webroot = postListItems (envAt webroot)

envAt :: String -> BuildEnv
envAt webroot = testEnv webroot (Just (PostPageConfig "post.html" "post"))

-- | `envAt`, for a site that has also declared its own origin -- as a
-- site generating a feed must.
envWithOrigin :: String -> BuildEnv
envWithOrigin webroot = (envAt webroot) {envSiteUrl = "https://example.com/"}

entry :: String -> String -> String -> PostEntry
entry title date slug =
  PostEntry
    { postTitle = title
    , postDate = date
    , postSlug = slug
    , postTags = ""
    , postAbstract = Nothing
    , postContent = "<p>" ++ title ++ " body</p>"
    }

spec :: Spec
spec = do
  postListItemsSpec
  absoluteItemsSpec

postListItemsSpec :: Spec
postListItemsSpec = describe "postListItems" $ do
  it "marks a single post as both the first and last of its year" $
    case listAt "/" [entry "Only Post" "2024-03-15" "only-post"] of
      [item] -> do
        itemTitle item `shouldBe` "Only Post"
        itemMonthDay item `shouldBe` "03-15"
        itemUrl item `shouldBe` Just "/post/only-post.html"
        itemYear item `shouldBe` "2024"
        itemNewYear item `shouldBe` True
        itemLastOfYear item `shouldBe` True
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "sorts posts within a year newest first, marking only the boundaries" $
    case listAt "/" [entry "Old" "2024-01-01" "old", entry "New" "2024-05-01" "new"] of
      [newItem, oldItem] -> do
        itemTitle newItem `shouldBe` "New"
        itemNewYear newItem `shouldBe` True
        itemLastOfYear newItem `shouldBe` False
        itemTitle oldItem `shouldBe` "Old"
        itemNewYear oldItem `shouldBe` False
        itemLastOfYear oldItem `shouldBe` True
      items -> expectationFailure ("expected exactly two items, got " ++ show (length items))

  it "sorts years newest first, each its own newYear/lastOfYear block" $
    case listAt "/" [entry "2023 Post" "2023-06-01" "p2023", entry "2024 Post" "2024-06-01" "p2024"] of
      [newer, older] -> do
        itemYear newer `shouldBe` "2024"
        itemNewYear newer `shouldBe` True
        itemLastOfYear newer `shouldBe` True
        itemYear older `shouldBe` "2023"
        itemNewYear older `shouldBe` True
        itemLastOfYear older `shouldBe` True
      items -> expectationFailure ("expected exactly two items, got " ++ show (length items))

  it "renders nothing for an empty entry list" $
    listAt "/" [] `shouldBe` []

  it "prefixes every post's url with the webroot" $
    map itemUrl (listAt "/blog/" [entry "Only Post" "2024-03-15" "only-post"])
      `shouldBe` [Just "/blog/post/only-post.html"]

  it "passes an entry's abstract and content straight through" $
    case listAt "/" [(entry "Only Post" "2024-03-15" "only-post") {postAbstract = Just "<p>intro</p>"}] of
      [item] -> do
        itemAbstract item `shouldBe` Just "<p>intro</p>"
        itemContent item `shouldBe` "<p>Only Post body</p>"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "passes an entry's tags straight through, verbatim" $
    case listAt "/" [(entry "Only Post" "2024-03-15" "only-post") {postTags = "long,image"}] of
      [item] -> itemTags item `shouldBe` "long,image"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "defaults an entry with no tags to an empty string" $
    case listAt "/" [entry "Only Post" "2024-03-15" "only-post"] of
      [item] -> itemTags item `shouldBe` ""
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "has no url for a post on a site that renders no post pages" $
    map itemUrl (postListItems (testEnv "/" Nothing) [entry "Only Post" "2024-03-15" "only-post"])
      `shouldBe` [Nothing]

  -- A feed's own <updated> is the newest post's date, which a template
  -- can only pick out if the newest post says it is the newest.
  it "marks the newest post first and the oldest last, across years" $
    map (\i -> (itemTitle i, itemFirst i, itemLast i))
      (listAt "/" [entry "Old" "2023-01-01" "old", entry "Mid" "2024-01-01" "mid", entry "New" "2025-01-01" "new"])
      `shouldBe` [("New", True, False), ("Mid", False, False), ("Old", False, True)]

  it "marks a lone post both first and last" $
    map (\i -> (itemFirst i, itemLast i)) (listAt "/" [entry "Only" "2024-03-15" "only"])
      `shouldBe` [(True, True)]

-- | One post of a site whose body carries each kind of URL a rendered
-- post can: root-relative, document-relative, a bare fragment, and one
-- that is already absolute.
linkedEntry :: PostEntry
linkedEntry =
  (entry "Linked Post" "2024-03-15" "linked-post")
    { postAbstract = Just "<p><img src=\"/res/a.png\"></p>"
    , postContent =
        "<p><img src=\"/res/a.png\"><img src=\"imgs/b.png\">\
        \<a href=\"#fn1\">1</a><a href=\"https://pandoc.org\">pandoc</a></p>"
    }

absoluteItemsSpec :: Spec
absoluteItemsSpec = describe "absoluteItems" $ do
  it "absolutizes a post's own url, however the origin was spelled" $
    map itemUrl (absoluteItems (envWithOrigin "/blog/") (postListItems (envWithOrigin "/blog/") [linkedEntry]))
      `shouldBe` [Just "https://example.com/blog/post/linked-post.html"]

  it "absolutizes the urls inside a post's content against the post's own page" $
    case absoluteItems (envWithOrigin "/") (postListItems (envWithOrigin "/") [linkedEntry]) of
      [item] ->
        itemContent item
          `shouldBe` "<p><img src=\"https://example.com/res/a.png\">\
                     \<img src=\"https://example.com/post/imgs/b.png\">\
                     \<a href=\"https://example.com/post/linked-post.html#fn1\">1</a>\
                     \<a href=\"https://pandoc.org\">pandoc</a></p>"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "absolutizes the urls inside a post's abstract too" $
    case absoluteItems (envWithOrigin "/") (postListItems (envWithOrigin "/") [linkedEntry]) of
      [item] -> itemAbstract item `shouldBe` Just "<p><img src=\"https://example.com/res/a.png\"></p>"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "leaves a post with no page of its own without a url, content absolutized all the same" $
    let env = (testEnv "/" Nothing) {envSiteUrl = "https://example.com"}
     in case absoluteItems env (postListItems env [linkedEntry]) of
          [item] -> do
            itemUrl item `shouldBe` Nothing
            itemContent item `shouldSatisfy` elem "https://example.com/res/a.png" . attrValues
          items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  -- A site generating a feed without declaring its origin is
  -- misconfigured, and this can't invent the missing half: what it can
  -- still resolve (against the post's own page) it does, and what needs
  -- an origin it leaves exactly as relative as it found it.
  it "resolves only what it can for a site that declared no origin" $
    case absoluteItems (envAt "/") (postListItems (envAt "/") [linkedEntry]) of
      [item] -> do
        itemUrl item `shouldBe` Just "/post/linked-post.html"
        itemContent item
          `shouldBe` "<p><img src=\"/res/a.png\"><img src=\"/post/imgs/b.png\">\
                     \<a href=\"/post/linked-post.html#fn1\">1</a>\
                     \<a href=\"https://pandoc.org\">pandoc</a></p>"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

-- | The quoted attribute values of an HTML fragment, in order.
attrValues :: String -> [String]
attrValues html = case break (== '"') html of
  (_, '"' : rest) -> case break (== '"') rest of
    (value, '"' : more) -> value : attrValues more
    _                   -> []
  _ -> []
