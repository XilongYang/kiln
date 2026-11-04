module Kiln.Build.EnvSpec (spec) where

import Kiln.Build.Env (postOutputPath, postUrl, webrootPrefix)
import Kiln.Config (PostPageConfig (..))
import Kiln.TestUtil (testEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "postUrl" $ do
    it "links a post under the configured output directory" $
      postUrl (testEnv "/" (Just (PostPageConfig "post.html" "post"))) "slug"
        `shouldBe` Just "/post/slug.html"

    -- Assuming "post" instead would 404 every link on a site that
    -- named a different directory.
    it "follows the output directory wherever the config points it" $
      postUrl (testEnv "/" (Just (PostPageConfig "post.html" "articles"))) "slug"
        `shouldBe` Just "/articles/slug.html"

    it "contributes no path segment when posts live at the site root" $
      postUrl (testEnv "/" (Just (PostPageConfig "post.html" "."))) "slug"
        `shouldBe` Just "/slug.html"

    it "prefixes the webroot" $
      postUrl (testEnv "/blog/" (Just (PostPageConfig "post.html" "post"))) "slug"
        `shouldBe` Just "/blog/post/slug.html"

    -- Plain concatenation would produce "/blogpost/slug.html".
    it "joins a webroot spelled without a trailing slash" $
      postUrl (testEnv "/blog" (Just (PostPageConfig "post.html" "post"))) "slug"
        `shouldBe` Just "/blog/post/slug.html"

    it "has no url at all for a site that renders no post pages" $
      postUrl (testEnv "/" Nothing) "slug" `shouldBe` Nothing

  describe "postOutputPath" $ do
    it "writes a post's page into the configured directory" $
      postOutputPath (testEnv "/" (Just (PostPageConfig "post.html" "post"))) "slug"
        `shouldBe` Just "post/slug.html"

    it "has no output path for a site that renders no post pages" $
      postOutputPath (testEnv "/" Nothing) "slug" `shouldBe` Nothing

  describe "webrootPrefix" $ do
    it "normalizes every spelling of a webroot to one trailing slash" $
      map webrootPrefix ["/", "/blog", "/blog/", "/blog//"]
        `shouldBe` ["/", "/blog/", "/blog/", "/blog/"]

    it "leaves an empty webroot empty, so links stay relative" $
      webrootPrefix "" `shouldBe` ""
