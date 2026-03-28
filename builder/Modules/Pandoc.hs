module Modules.Pandoc (runPandoc) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.Process (callProcess)

-- ---[ Overview ]------------------------------------------------------------
-- | Thin wrapper around the pandoc CLI used by the build pipeline.
--
-- This module centralizes pandoc invocation and argument construction so call
-- sites keep a single integration contract.

-- ---[ Public API ]------------------------------------------------------------

-- | Converts markdown input to HTML via pandoc using the provided template.
--
-- The output directory is created before invoking pandoc.
runPandoc :: FilePath -> FilePath -> FilePath -> IO ()
runPandoc inputPath templatePath outputPath = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  callProcess "pandoc" (mkPandocArgs inputPath templatePath outputPath)

-- ---[ Implementation Details ]-----------------------------------------------

-- | Builds deterministic pandoc arguments for production output.
mkPandocArgs :: FilePath -> FilePath -> FilePath -> [String]
mkPandocArgs inputPath templatePath outputPath =
  [ inputPath
  , "-o", outputPath
  , "--template=" ++ templatePath
  , "--no-highlight"
  , "--mathjax"
  , "--wrap=none"
  ]
