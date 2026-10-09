module Kiln.Build.PageSpec (spec) where

import Kiln.Build.Page (PostListItem (..), postListItems)
import Kiln.Build.PostEntry (PostEntry (..))
import Test.Hspec

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
spec = describe "postListItems" $ do
  it "marks a single post as both the first and last of its year" $
    case postListItems "/" [entry "Only Post" "2024-03-15" "only-post"] of
      [item] -> do
        itemTitle item `shouldBe` "Only Post"
        itemMonthDay item `shouldBe` "03-15"
        itemUrl item `shouldBe` "/post/only-post.html"
        itemYear item `shouldBe` "2024"
        itemNewYear item `shouldBe` True
        itemLastOfYear item `shouldBe` True
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "sorts posts within a year newest first, marking only the boundaries" $
    case postListItems "/" [entry "Old" "2024-01-01" "old", entry "New" "2024-05-01" "new"] of
      [newItem, oldItem] -> do
        itemTitle newItem `shouldBe` "New"
        itemNewYear newItem `shouldBe` True
        itemLastOfYear newItem `shouldBe` False
        itemTitle oldItem `shouldBe` "Old"
        itemNewYear oldItem `shouldBe` False
        itemLastOfYear oldItem `shouldBe` True
      items -> expectationFailure ("expected exactly two items, got " ++ show (length items))

  it "sorts years newest first, each its own newYear/lastOfYear block" $
    case postListItems "/" [entry "2023 Post" "2023-06-01" "p2023", entry "2024 Post" "2024-06-01" "p2024"] of
      [newer, older] -> do
        itemYear newer `shouldBe` "2024"
        itemNewYear newer `shouldBe` True
        itemLastOfYear newer `shouldBe` True
        itemYear older `shouldBe` "2023"
        itemNewYear older `shouldBe` True
        itemLastOfYear older `shouldBe` True
      items -> expectationFailure ("expected exactly two items, got " ++ show (length items))

  it "renders nothing for an empty entry list" $
    postListItems "/" [] `shouldBe` []

  it "prefixes every post's url with the webroot" $
    map itemUrl (postListItems "/blog/" [entry "Only Post" "2024-03-15" "only-post"])
      `shouldBe` ["/blog/post/only-post.html"]

  it "passes an entry's abstract and content straight through" $
    case postListItems "/" [(entry "Only Post" "2024-03-15" "only-post") {postAbstract = Just "<p>intro</p>"}] of
      [item] -> do
        itemAbstract item `shouldBe` Just "<p>intro</p>"
        itemContent item `shouldBe` "<p>Only Post body</p>"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "passes an entry's tags straight through, verbatim" $
    case postListItems "/" [(entry "Only Post" "2024-03-15" "only-post") {postTags = "long,image"}] of
      [item] -> itemTags item `shouldBe` "long,image"
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))

  it "defaults an entry with no tags to an empty string" $
    case postListItems "/" [entry "Only Post" "2024-03-15" "only-post"] of
      [item] -> itemTags item `shouldBe` ""
      items -> expectationFailure ("expected exactly one item, got " ++ show (length items))
