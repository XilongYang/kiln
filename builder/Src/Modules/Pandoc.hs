module Modules.Pandoc (runPandoc, renderMarkdownToPlaintext) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.Process (callProcess, readProcess)
import Modules.TypeAlias (Markdown)

-- ---[ Overview ]------------------------------------------------------------
-- | Thin wrapper around the pandoc CLI used by the build pipeline.
--
-- This module centralizes pandoc invocation and argument construction so call
-- sites keep a single integration contract.

-- ---[ Public API ]------------------------------------------------------------

-- | Converts markdown input to HTML via pandoc using the provided template.
--
-- The output directory is created before invoking pandoc.
-- HTML output uses the same argument profile as production builds.
runPandoc :: FilePath -> FilePath -> FilePath -> IO ()
runPandoc inputPath templatePath outputPath = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  callProcess "pandoc" (mkPandocArgs inputPath templatePath outputPath)

-- | Converts one markdown file into plaintext through pandoc.
--
-- This function only delegates conversion. Whitespace normalization and
-- escaping are handled downstream (KLB rendering / frontend consumer).
renderMarkdownToPlaintext :: Markdown -> IO String
renderMarkdownToPlaintext src = readProcess "pandoc"
    [ src
    , "-t", "plain"
    ]
    ""
-- ---[ Implementation Details ]-----------------------------------------------

-- | Builds deterministic pandoc arguments for production output.
--
-- Choices:
-- - no syntax highlight injection
-- - MathJax enabled
-- - wrap disabled for stable downstream processing
mkPandocArgs :: FilePath -> FilePath -> FilePath -> [String]
mkPandocArgs inputPath templatePath outputPath =
  [ inputPath
  , "-o", outputPath
  , "--template=" ++ templatePath
  , "--no-highlight"
  , "--mathjax"
  , "--wrap=none"
  ]
