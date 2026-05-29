module Test.UT.Modules.Builder (suiteName, testCases) where

import Modules.BuildPlan
import Modules.Builder
import Modules.Index.Item (IndexItem(..))
import Modules.SearchDB (SearchItem(..))
import Modules.Config
import Modules.Utils.Klb (parseKlb, renderKlb)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))
import Test.Framework.Asserts
import Test.Framework.Expect
import Test.Framework.Paths
import Test.Framework.TestSuite

-- Suite for builder integration-level execution helpers.
suiteName :: String
suiteName = "Builder"

testCases :: [TestCase]
testCases =
  [ testExecuteBuildPostPlan
  , testExecuteBuildIndexPlan
  ]

-- Confirms post build writes preprocess markdown and final html with expected transformations.
testExecuteBuildPostPlan :: TestCase
testExecuteBuildPostPlan =
  mkTestCase "executeBuildPlan runs post plan and writes html under .mock/post" $
    withCasePathsInSandbox suiteName "executeBuildPostPlan" ["src", "post", "temp", "template", "Src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
          postTemplatePath = templateFile casePaths "post.html"
          outputPath = tempFile casePaths "builder-ut-preprocessed.md"
          builtHtmlPath = tempFile casePaths "builder-ut-built.html"
          htmlPath = postFile casePaths "builder-ut-output.html"
          metaKlbPath = tempFile casePaths "builder-ut-meta.klb"
          searchItemPath = tempFile casePaths "builder-ut-search-item.klb"
          charsetPath = tempFile casePaths "builder-ut-charset.txt"
      writeFile (builderPath </> "builder-ut-stub.hs") "stub"
      copyFile parsePostFixturePath sourcePath
      copyFile postTemplateFixturePath postTemplatePath
      let basePlan = expectPostPlan (mkBuildPostPlan sourcePath)
      let plan =
            basePlan
              { planPreprocessedPath = outputPath
              , planBuiltHtmlPath = builtHtmlPath
              , planTargetHtmlPath = htmlPath
              , planPostTemplatePath = postTemplatePath
              , planPostMetaPath = metaKlbPath
              , planPostSearchItemPath = searchItemPath
              , planPostCharsetPath = charsetPath
              }
      createDirectoryIfMissing True (caseTempDir casePaths)
      executeBuildPlan (BuildPostPlan plan)
      exists <- doesFileExist outputPath
      assertTrue "executeBuildPlan should create preprocess markdown file for post plan" exists
      written <- readFile outputPath
      assertContains "written preprocess file should include front matter title" "title: Fixture Title" written
      assertContains "written preprocess file should include toc marker" "[[toc]]" written
      assertContains "written preprocess file should include rewritten C language fence"
        "``` {.language-C .line-numbers .match-braces}"
        written
      htmlExists <- doesFileExist htmlPath
      assertTrue "executeBuildPlan should create rendered html output for post plan" htmlExists
      html <- readFile htmlPath
      assertContains "rendered html should include a heading generated from markdown content" "<h2 id=\"sub-title2\">" html
      assertContains "rendered html should include wrapped abstract block from source" "<div class=\"abstract\">" html
      assertContains "rendered html should preserve rewritten code block classes"
        "<pre class=\"language-C line-numbers match-braces\">"
        html
      metaExists <- doesFileExist metaKlbPath
      searchItemExists <- doesFileExist searchItemPath
      charsetExists <- doesFileExist charsetPath
      assertTrue "executeBuildPlan should write post metadata artifact KLB" metaExists
      assertTrue "executeBuildPlan should write post search-item artifact KLB" searchItemExists
      assertTrue "executeBuildPlan should write post charset artifact" charsetExists
      metaKlb <- readFile metaKlbPath
      metaItems <- expectRight "post metadata artifact should parse as IndexItem KLB" (parseKlb metaKlb)
      assertEq "metadata artifact should contain one entry" 1 (length (metaItems :: [IndexItem]))
      let [metaItem] = (metaItems :: [IndexItem])
      assertEq "metadata artifact should keep post title"
        "Fixture Title"
        (itemTitle metaItem)
      searchItemKlb <- readFile searchItemPath
      searchItems <- expectRight "post search-item artifact should parse as SearchItem KLB" (parseKlb searchItemKlb)
      assertEq "search-item artifact should contain one entry" 1 (length (searchItems :: [SearchItem]))
      let [searchItem] = (searchItems :: [SearchItem])
      assertEq "search-item artifact should keep post title"
        "Fixture Title"
        (searchItemTitle searchItem)
      assertEq "search-item artifact should keep post url"
        "/post/parse-post-fixture.html"
        (searchItemUrl searchItem)
      assertTrue "search-item content rendered by pandoc should not be empty"
        (not (null (searchItemContent searchItem)))
      charset <- readFile charsetPath
      assertTrue "charset artifact should include title character F" ('F' `elem` charset)
      assertTrue "charset artifact should include heading character S" ('S' `elem` charset)

-- Confirms index build reads temp KLB items, renders template and writes target html file.
testExecuteBuildIndexPlan :: TestCase
testExecuteBuildIndexPlan =
  mkTestCase "executeBuildPlan renders index html from metadata artifacts and writes charset artifact" $
    withCasePathsInSandbox suiteName "executeBuildIndexPlan" ["post", "temp", "template", ".cache/artifacts/meta", ".cache/artifacts/charset", "Src"] $ \casePaths -> do
      let indexTemplatePath = templateFile casePaths "index.html"
          indexOutputPath = postFile casePaths "builder-ut-index.html"
          metaPath = metaArtifactsPath </> "index-source.klb"
          indexCharsetPath = charsetArtifactsPath </> "index.txt"
      writeFile (builderPath </> "builder-ut-stub.hs") "stub"
      writeFile indexTemplatePath "<html><body>$posts$</body></html>"
      klb <-
        case renderKlb [IndexItem "Index Title" "2026" "03" "22" "/post/index-title.html"] of
          Left e -> error ("Assertion failed: cannot render index-item KLB fixture: " ++ show e)
          Right text -> pure text
      writeFile metaPath klb
      let base = expectIndexPlan mkBuildIndexPlan
      let plan =
            base
              { planIndexHtmlPath = indexOutputPath
              , planIndexTemplatePath = indexTemplatePath
              }
      executeBuildPlan (BuildIndexPlan plan)
      exists <- doesFileExist indexOutputPath
      assertTrue "executeBuildPlan should create index output html file for index plan" exists
      html <- readFile indexOutputPath
      assertContains "index output should include year heading from items" "<h3>2026</h3>" html
      assertContains "index output should include item link title" "Index Title" html
      assertContains "index output should include item link url" "/post/index-title.html" html
      charsetExists <- doesFileExist indexCharsetPath
      assertTrue "index build should generate index charset artifact" charsetExists
      charset <- readFile indexCharsetPath
      assertTrue "index charset should include title character I" ('I' `elem` charset)
      assertTrue "index charset should include title character T" ('T' `elem` charset)
