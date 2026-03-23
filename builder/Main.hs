module Main where

import Modules.Config
import Modules.Template
import Modules.Utils.TempDir (withTempDir)
import Modules.Utils.OrphanCheck (checkOrphans)

import System.IO (writeFile)

-- Build entrypoint:
-- 1) recreate temp workspace;
-- 2) report generated post pages that no longer have source markdown;
-- 3) render templates into temp files for downstream steps.
main :: IO ()
main = withTempDir tempPath $ do
  checkOrphans

  templatePost <- genTemplate templatePostPath 
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- genTemplate templateIndexPath 
  writeFile renderedTemplateIndexPath templateIndex

