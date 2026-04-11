module Test.Framework.Fixtures
  ( writeFakePyftsubsetSimple
  , writeFakePyftsubsetWithTrace
  , setupMainFixtureTree
  , setupFontSubsetFixtureTree
  ) where

import Modules.Config
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callProcess)

writeFakePyftsubsetSimple :: FilePath -> IO ()
writeFakePyftsubsetSimple scriptPath = do
  writeFile scriptPath $
    unlines
      [ "#!/bin/sh"
      , "for arg in \"$@\"; do"
      , "  case \"$arg\" in"
      , "    --output-file=*) out=\"${arg#--output-file=}\" ;;"
      , "  esac"
      , "done"
      , "[ -n \"$out\" ] && : > \"$out\""
      ]
  callProcess "chmod" ["+x", scriptPath]

writeFakePyftsubsetWithTrace :: FilePath -> IO ()
writeFakePyftsubsetWithTrace scriptPath = do
  writeFile scriptPath $
    unlines
      [ "#!/bin/sh"
      , "printf '%s\\n' \"$@\" > \"$PYFTSUBSET_TRACE\""
      , "for arg in \"$@\"; do"
      , "  case \"$arg\" in"
      , "    --output-file=*) out=\"${arg#--output-file=}\" ;;"
      , "  esac"
      , "done"
      , "[ -n \"$out\" ] && : > \"$out\""
      ]
  callProcess "chmod" ["+x", scriptPath]

setupMainFixtureTree :: IO ()
setupMainFixtureTree = do
  createDirectoryIfMissing True srcPath
  createDirectoryIfMissing True templateComponentPath
  createDirectoryIfMissing True fontPath
  writeFile (srcPath </> "fixture.md") fixturePostMarkdown
  writeFile templatePostPath fixturePostTemplate
  writeFile templateIndexPath fixtureIndexTemplate
  writeFile originFontFilePath "fake-otf"

setupFontSubsetFixtureTree :: IO ()
setupFontSubsetFixtureTree = do
  createDirectoryIfMissing True tempPath
  createDirectoryIfMissing True charsetArtifactsPath
  createDirectoryIfMissing True fontPath
  writeFile (charsetArtifactsPath </> "a.txt") "AAB"
  writeFile (charsetArtifactsPath </> "b.txt") "BCC"
  writeFile (charsetArtifactsPath </> "index.txt") "ZZA"
  writeFile originFontFilePath "fake-otf"

fixturePostMarkdown :: String
fixturePostMarkdown =
  unlines
    [ "---"
    , "title: Fixture Title"
    , "author: Fixture Author"
    , "date: 2026-03-25"
    , "---"
    , ""
    , "This is abstract."
    , ""
    , "## Section A"
    , ""
    , "Body line."
    ]

fixturePostTemplate :: String
fixturePostTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body><main>$body$</main></body></html>"
    ]

fixtureIndexTemplate :: String
fixtureIndexTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body>$posts$</body></html>"
    ]
