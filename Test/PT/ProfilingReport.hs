module Test.PT.ProfilingReport (generateProfilingHtmlReport) where

import Data.Char (isSpace, ord)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Numeric (showFFloat)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.Process (callProcess)

data CaseDef = CaseDef
  { cdKey :: String
  , cdTitle :: String
  } deriving (Show, Eq)

data CostEntry = CostEntry
  { ceName :: String
  , ceModule :: String
  , ceTimePct :: Double
  , ceAllocPct :: Double
  } deriving (Show, Eq)

data DetailEntry = DetailEntry
  { deName :: String
  , deModule :: String
  , deEntries :: Integer
  , deIndTimePct :: Double
  , deIndAllocPct :: Double
  , deInhTimePct :: Double
  , deInhAllocPct :: Double
  } deriving (Show, Eq)

data ProfReport = ProfReport
  { prTotalTimeSecs :: Double
  , prTotalAllocBytes :: Double
  , prDetails :: [DetailEntry]
  } deriving (Show, Eq)

data StageNode = StageNode
  { snName :: String
  , snTimePct :: Double
  , snAllocPct :: Double
  } deriving (Show, Eq)

data FnStat = FnStat
  { fsL1 :: String
  , fsL2 :: String
  , fsName :: String
  , fsModule :: String
  , fsCalls :: Integer
  , fsIndTimePct :: Double
  , fsIndAllocPct :: Double
  , fsInhTimePct :: Double
  , fsInhAllocPct :: Double
  } deriving (Show, Eq)

data CaseReport = CaseReport
  { crDef :: CaseDef
  , crProfile :: ProfReport
  , crL1 :: [StageNode]
  , crL2ByL1 :: Map.Map String [StageNode]
  , crFnStats :: [FnStat]
  } deriving (Show, Eq)

generateProfilingHtmlReport :: IO FilePath
generateProfilingHtmlReport = do
  repoRoot <- getCurrentDirectory
  let outDir = repoRoot </> ".cache" </> "PT" </> "profiling"
  createDirectoryIfMissing True outDir
  let exePath = outDir </> "profile-target"
  let buildLogPath = outDir </> "profiling-build.log"
  compileProfileTarget exePath buildLogPath

  caseReports <- mapM (runCaseProfile repoRoot outDir exePath) allCases
  let html = renderHtml caseReports
  let htmlPath = outDir </> "profiling-report.html"
  writeFile htmlPath html
  pure htmlPath

allCases :: [CaseDef]
allCases =
  [ CaseDef "500-normal" "500 normal posts (10 KiB each)"
  , CaseDef "500-one-changed" "500 normal posts, one tiny source changed"
  , CaseDef "5-huge" "5 huge posts (5 MiB each)"
  ]

runCaseProfile :: FilePath -> FilePath -> FilePath -> CaseDef -> IO CaseReport
runCaseProfile _repoRoot outDir exePath caseDef = do
  let key = cdKey caseDef
  let profBasePath = outDir </> ("profile-" ++ key)
  let profPath = profBasePath ++ ".prof"
  let runLogPath = outDir </> ("profiling-run-" ++ key ++ ".log")
  maybeRunWarmup outDir exePath caseDef
  cleanupIfExists profPath
  cleanupIfExists (profPath ++ ".prof")
  runProfileTarget exePath key profBasePath runLogPath
  prof <- parseProfReport profPath
  let costEntries = map detailAsCostEntry (filter hasVisibleCost (prDetails prof))
  let l1Nodes = aggregateL1 costEntries
  let l2ByL1 = aggregateL2 costEntries
  let fnStats = aggregateFnStats (prDetails prof)
  pure $
    CaseReport
      { crDef = caseDef
      , crProfile = prof
      , crL1 = l1Nodes
      , crL2ByL1 = l2ByL1
      , crFnStats = fnStats
      }

maybeRunWarmup :: FilePath -> FilePath -> CaseDef -> IO ()
maybeRunWarmup outDir exePath caseDef =
  if cdKey caseDef == "500-one-changed"
    then do
      let warmupLogPath = outDir </> "profiling-warmup-500-one-changed.log"
      runWarmupTarget exePath warmupLogPath
    else pure ()

runWarmupTarget :: FilePath -> FilePath -> IO ()
runWarmupTarget exePath logPath =
  callProcess "sh"
    [ "-c"
    , shellQuote exePath
        ++ " 500-one-changed-warmup > "
        ++ shellQuote logPath
        ++ " 2>&1"
    ]

compileProfileTarget :: FilePath -> FilePath -> IO ()
compileProfileTarget exePath logPath =
  let objDir = ".cache/PT/profiling/ghc-obj"
   in
  callProcess "sh"
    [ "-c"
    , "mkdir -p "
        ++ shellQuote objDir
        ++ " && ghc -O2 -prof -fprof-auto -rtsopts -fforce-recomp -hidir "
        ++ shellQuote objDir
        ++ " -odir "
        ++ shellQuote objDir
        ++ " -iSrc -i. Test/PT/ProfileTarget.hs -o "
        ++ shellQuote exePath
        ++ " > "
        ++ shellQuote logPath
        ++ " 2>&1"
    ]

runProfileTarget :: FilePath -> String -> FilePath -> FilePath -> IO ()
runProfileTarget exePath caseKey profBasePath logPath =
  callProcess "sh"
    [ "-c"
    , shellQuote exePath
        ++ " "
        ++ shellQuote caseKey
        ++ " +RTS -p -po"
        ++ shellQuote profBasePath
        ++ " -RTS > "
        ++ shellQuote logPath
        ++ " 2>&1"
    ]

parseProfReport :: FilePath -> IO ProfReport
parseProfReport path = do
  content <- readFile path
  let ls = lines content
  let totalTime = parseTotalTimeSecs ls
  let totalAlloc = parseTotalAllocBytes ls
  let details = parseDetailedEntries ls
  pure $
    ProfReport
      { prTotalTimeSecs = totalTime
      , prTotalAllocBytes = totalAlloc
      , prDetails = details
      }

parseTotalTimeSecs :: [String] -> Double
parseTotalTimeSecs ls =
  case firstJust (map parseLine ls) of
    Just x -> x
    Nothing -> 0
  where
    parseLine line =
      let ws = words (trimLeft line)
       in if take 2 ws == ["total", "time"] && length ws >= 4
            then readDouble (ws !! 3)
            else Nothing

parseTotalAllocBytes :: [String] -> Double
parseTotalAllocBytes ls =
  case firstJust (map parseLine ls) of
    Just x -> x
    Nothing -> 0
  where
    parseLine line =
      let ws = words (trimLeft line)
       in if take 2 ws == ["total", "alloc"] && length ws >= 4
            then readDouble (filter (/= ',') (ws !! 3))
            else Nothing

parseDetailedEntries :: [String] -> [DetailEntry]
parseDetailedEntries ls =
  case dropWhile (not . isIndividualHeader) ls of
    [] -> []
    (_:rest) ->
      case dropWhile (not . isDetailedColumnsHeader) rest of
        [] -> []
        (_:rows) -> mapMaybeDetail rows

isIndividualHeader :: String -> Bool
isIndividualHeader s =
  "individual" `List.isInfixOf` s &&
  "inherited" `List.isInfixOf` s

isDetailedColumnsHeader :: String -> Bool
isDetailedColumnsHeader s =
  "COST CENTRE" `List.isPrefixOf` trimLeft s &&
  "entries" `List.isInfixOf` s &&
  "%time" `List.isInfixOf` s &&
  "%alloc" `List.isInfixOf` s

mapMaybeDetail :: [String] -> [DetailEntry]
mapMaybeDetail = foldr collect []
  where
    collect line acc =
      case parseDetailLine line of
        Just x -> x : acc
        Nothing -> acc

parseDetailLine :: String -> Maybe DetailEntry
parseDetailLine raw =
  let ws = words raw
      n = length ws
   in if n < 9
        then Nothing
        else do
          entries <- readInteger (ws !! (n - 5))
          indTime <- readDouble (ws !! (n - 4))
          indAlloc <- readDouble (ws !! (n - 3))
          inhTime <- readDouble (ws !! (n - 2))
          inhAlloc <- readDouble (ws !! (n - 1))
          let name = ws !! 0
          let modName = ws !! 1
          Just $
            DetailEntry
              { deName = name
              , deModule = modName
              , deEntries = entries
              , deIndTimePct = indTime
              , deIndAllocPct = indAlloc
              , deInhTimePct = inhTime
              , deInhAllocPct = inhAlloc
              }

readDouble :: String -> Maybe Double
readDouble s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

readInteger :: String -> Maybe Integer
readInteger s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) =
  case x of
    Just _ -> x
    Nothing -> firstJust xs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

hasVisibleCost :: DetailEntry -> Bool
hasVisibleCost d = deIndTimePct d > 0 || deIndAllocPct d > 0

detailAsCostEntry :: DetailEntry -> CostEntry
detailAsCostEntry d =
  CostEntry
    { ceName = deName d
    , ceModule = deModule d
    , ceTimePct = deIndTimePct d
    , ceAllocPct = deIndAllocPct d
    }

aggregateL1 :: [CostEntry] -> [StageNode]
aggregateL1 entries =
  sortNodes $
    map toNode $
      Map.toList $
        foldr go Map.empty entries
  where
    go entry = Map.insertWith plus (l1Stage entry) (ceTimePct entry, ceAllocPct entry)
    plus (t1, a1) (t2, a2) = (t1 + t2, a1 + a2)
    toNode (k, (t, a)) = StageNode k t a

aggregateL2 :: [CostEntry] -> Map.Map String [StageNode]
aggregateL2 entries =
  Map.map sortNodes $
    Map.map (map toNode . Map.toList) byL1
  where
    byL1 = foldr go Map.empty entries
    go entry =
      let key1 = l1Stage entry
          key2 = l2Stage entry
          ins = Map.insertWith plus key2 (ceTimePct entry, ceAllocPct entry)
          plus (t1, a1) (t2, a2) = (t1 + t2, a1 + a2)
       in Map.insertWith (Map.unionWith plus) key1 (ins Map.empty)
    toNode (k, (t, a)) = StageNode k t a

aggregateFnStats :: [DetailEntry] -> [FnStat]
aggregateFnStats entries =
  List.sortBy (flip compare `on` fsIndTimePct) $
    filter hasVisibleFnStat $
      map snd $
        Map.toList $
          foldr go Map.empty entries
  where
    go e =
      let ce = detailAsCostEntry e
          key = (l1Stage ce, l2Stage ce, deName e, deModule e)
          val =
            FnStat
              { fsL1 = l1Stage ce
              , fsL2 = l2Stage ce
              , fsName = deName e
              , fsModule = deModule e
              , fsCalls = deEntries e
              , fsIndTimePct = deIndTimePct e
              , fsIndAllocPct = deIndAllocPct e
              , fsInhTimePct = deInhTimePct e
              , fsInhAllocPct = deInhAllocPct e
              }
       in Map.insertWith merge key val
    merge new old =
      FnStat
        { fsL1 = fsL1 old
        , fsL2 = fsL2 old
        , fsName = fsName old
        , fsModule = fsModule old
        , fsCalls = fsCalls old + fsCalls new
        , fsIndTimePct = fsIndTimePct old + fsIndTimePct new
        , fsIndAllocPct = fsIndAllocPct old + fsIndAllocPct new
        , fsInhTimePct = fsInhTimePct old + fsInhTimePct new
        , fsInhAllocPct = fsInhAllocPct old + fsInhAllocPct new
        }

hasVisibleFnStat :: FnStat -> Bool
hasVisibleFnStat s = fsIndTimePct s > 0 || fsIndAllocPct s > 0

sortNodes :: [StageNode] -> [StageNode]
sortNodes = List.sortBy (flip compare `on` snTimePct)

l1Stage :: CostEntry -> String
l1Stage e
  | ceModule e == "Modules.Post.Parse" = "Post Pipeline"
  | ceModule e == "Modules.Post.Preprocess" = "Post Pipeline"
  | ceModule e == "Modules.Pandoc" = "Post Pipeline"
  | ceModule e == "Modules.Toc" = "Post Pipeline"
  | ceModule e == "Modules.Builder" && "buildPostWithPlan" `List.isPrefixOf` ceName e = "Post Pipeline"
  | ceModule e == "Modules.Builder" && "writePost" `List.isPrefixOf` ceName e = "Post Pipeline"
  | ceModule e == "Modules.Builder" && ceName e == "writeCharset" = "Post Pipeline"
  | ceModule e == "Modules.Index.Render" = "Index Pipeline"
  | ceModule e == "Modules.Builder" && "buildIndexWithPlan" `List.isPrefixOf` ceName e = "Index Pipeline"
  | ceModule e == "Modules.FontSubset" = "Font Subset"
  | ceModule e == "Modules.SearchDB" = "SearchDB Aggregate"
  | ceModule e == "Test.PT.Modules.Performance" && "appendSearchItem" `List.isPrefixOf` ceName e = "SearchDB Aggregate"
  | ceModule e == "Main" && "appendSearchItem" `List.isPrefixOf` ceName e = "SearchDB Aggregate"
  | ceModule e == "Modules.BuildJudger" = "Hash & Build Judger"
  | ceModule e == "Modules.Utils.Files" && "hash" `List.isPrefixOf` ceName e = "Hash & Build Judger"
  | ceModule e == "Modules.Template" = "Template Expand"
  | otherwise = "Other"

l2Stage :: CostEntry -> String
l2Stage e
  | ceModule e == "Modules.Post.Parse" = "Parse"
  | ceModule e == "Modules.Post.Preprocess" = "Preprocess"
  | ceModule e == "Modules.Pandoc" && ceName e == "runPandoc" = "Pandoc HTML"
  | ceModule e == "Modules.Pandoc" && ceName e == "renderMarkdownToPlaintext" = "Pandoc Plaintext"
  | ceModule e == "Modules.Pandoc" = "Pandoc Other"
  | ceModule e == "Modules.Toc" = "TOC Inject"
  | ceModule e == "Modules.Builder" && ceName e == "writeCharset" = "Charset"
  | ceModule e == "Modules.Builder" && "writePostMeta" `List.isPrefixOf` ceName e = "Meta KLB"
  | ceModule e == "Modules.Builder" && "writePostSearchItem" `List.isPrefixOf` ceName e = "Search KLB"
  | ceModule e == "Modules.Builder" && "buildPostWithPlan" `List.isPrefixOf` ceName e = "Post Build Orchestration"
  | ceModule e == "Modules.Index.Render" = "Index Render"
  | ceModule e == "Modules.Builder" && "buildIndexWithPlan" `List.isPrefixOf` ceName e = "Index Build Orchestration"
  | ceModule e == "Modules.FontSubset" = "Font Subset"
  | ceModule e == "Modules.SearchDB" = "SearchDB"
  | ceModule e == "Test.PT.Modules.Performance" && "appendSearchItem" `List.isPrefixOf` ceName e = "Append Search Items"
  | ceModule e == "Modules.BuildJudger" = "Build Judger"
  | ceModule e == "Modules.Utils.Files" && "hash" `List.isPrefixOf` ceName e = "Hash IO"
  | ceModule e == "Modules.Template" = "Template Expand"
  | otherwise = "Other"

renderHtml :: [CaseReport] -> String
renderHtml caseReports =
  let casesJson = jsonArrayCaseReports caseReports
   in
  unlines
    [ "<!doctype html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "<meta charset=\"utf-8\" />"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
    , "<title>PT Profiling Stage Report</title>"
    , "<style>"
    , "body{font-family:IBM Plex Sans,PingFang SC,Noto Sans CJK SC,sans-serif;background:#f4f7fb;color:#122033;margin:0}"
    , ".wrap{width:min(1280px,96vw);margin:24px auto 40px}"
    , ".panel{background:#fff;border:1px solid #d9e2ef;border-radius:12px;padding:14px;margin-bottom:12px}"
    , "h1{font-size:28px;margin:0 0 8px}h2{font-size:18px;margin:0 0 8px}"
    , ".case-tabs{display:flex;gap:8px;flex-wrap:wrap;margin:8px 0 12px}"
    , ".btn{border:1px solid #b8c6d9;background:#fff;color:#122033;padding:6px 10px;border-radius:8px;cursor:pointer}"
    , ".btn.active{background:#0f305a;color:#fff;border-color:#0f305a}"
    , ".summary{display:grid;grid-template-columns:repeat(4,minmax(150px,1fr));gap:8px}"
    , ".kpi{background:#f7faff;border:1px solid #d9e2ef;border-radius:8px;padding:8px 10px}"
    , ".kpi .k{font-size:12px;color:#37506b}.kpi .v{font-size:16px;font-family:JetBrains Mono,monospace}"
    , ".toolbar{display:flex;gap:8px;flex-wrap:wrap;margin:8px 0 12px}"
    , ".pie-row{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px}"
    , ".pie-card{border:1px solid #d9e2ef;border-radius:10px;padding:10px}"
    , ".chart-title{font-weight:700;margin:2px 0 8px}"
    , ".pie-svg{width:100%;height:240px;display:block}"
    , ".pie-legend{display:grid;gap:6px;max-height:240px;overflow:auto;margin-top:6px}"
    , ".legend-item{display:flex;align-items:center;gap:8px;border:1px solid #d9e2ef;background:#fff;border-radius:8px;padding:5px 8px;font-size:12px;cursor:pointer}"
    , ".legend-item.passive{cursor:default}"
    , ".legend-item.active{border-color:#0f305a;background:#eef5ff}"
    , ".swatch{width:10px;height:10px;border-radius:2px;flex:0 0 10px}"
    , ".legend-name{flex:1;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}"
    , ".legend-val{font-family:JetBrains Mono,monospace}"
    , "table{width:100%;border-collapse:collapse;border:1px solid #d9e2ef;font-size:13px}"
    , "th,td{padding:8px;border-bottom:1px solid #d9e2ef;text-align:left}"
    , "th{background:#f7faff;position:sticky;top:0;z-index:1}"
    , ".fn-th-sort{cursor:pointer;user-select:none}"
    , ".table-wrap{overflow:auto;border-radius:10px}"
    , ".hint{font-size:13px;color:#37506b;margin-top:8px}"
    , "@media (max-width:1100px){.pie-row{grid-template-columns:1fr;}}"
    , "@media (max-width:960px){.summary{grid-template-columns:repeat(2,minmax(140px,1fr));}}"
    , "</style>"
    , "</head>"
    , "<body><div class=\"wrap\">"
    , "<h1>PT Profiling Case Report</h1>"
    , "<div class=\"panel\">"
    , "<h2>Case Selector</h2>"
    , "<div id=\"case-tabs\" class=\"case-tabs\"></div>"
    , "<div id=\"summary\" class=\"summary\"></div>"
    , "</div>"
    , "<div class=\"panel\">"
    , "<h2>Stage Charts</h2>"
    , "<div class=\"toolbar\"><button id=\"btn-time\" class=\"btn active\" type=\"button\">By Time %</button><button id=\"btn-alloc\" class=\"btn\" type=\"button\">By Alloc %</button></div>"
    , "<div class=\"pie-row\">"
    , "<div class=\"pie-card\"><div id=\"l1-title\" class=\"chart-title\"></div><svg id=\"l1-pie\" class=\"pie-svg\" viewBox=\"0 0 260 260\"></svg><div id=\"l1-legend\" class=\"pie-legend\"></div></div>"
    , "<div class=\"pie-card\"><div id=\"l2-title\" class=\"chart-title\"></div><svg id=\"l2-pie\" class=\"pie-svg\" viewBox=\"0 0 260 260\"></svg><div id=\"l2-legend\" class=\"pie-legend\"></div></div>"
    , "<div class=\"pie-card\"><div id=\"l3-title\" class=\"chart-title\"></div><svg id=\"l3-pie\" class=\"pie-svg\" viewBox=\"0 0 260 260\"></svg><div id=\"l3-legend\" class=\"pie-legend\"></div></div>"
    , "</div>"
    , "<div class=\"hint\">Interaction: click an L1 slice in the left chart to update the middle chart; click an L2 slice in the middle chart to update function details in the right chart.</div>"
    , "</div>"
    , "<div class=\"panel\">"
    , "<h2>Function Stats</h2>"
    , "<div class=\"toolbar\">"
    , "<select id=\"fn-l1\" class=\"btn\"></select>"
    , "<select id=\"fn-l2\" class=\"btn\"></select>"
    , "</div>"
    , "<div class=\"table-wrap\">"
    , "<table><thead><tr>"
    , "<th class=\"fn-th-sort\" data-sort-key=\"l1\" data-label=\"L1\">L1</th><th class=\"fn-th-sort\" data-sort-key=\"l2\" data-label=\"L2\">L2</th><th class=\"fn-th-sort\" data-sort-key=\"name\" data-label=\"Function\">Function</th><th class=\"fn-th-sort\" data-sort-key=\"module\" data-label=\"Module\">Module</th><th class=\"fn-th-sort\" data-sort-key=\"calls\" data-label=\"Calls\">Calls</th><th class=\"fn-th-sort\" data-sort-key=\"totalTime\" data-label=\"Total Time (ms)\">Total Time (ms)</th><th class=\"fn-th-sort\" data-sort-key=\"avgTime\" data-label=\"Avg Time (ms)\">Avg Time (ms)</th><th class=\"fn-th-sort\" data-sort-key=\"totalAlloc\" data-label=\"Total Alloc (B)\">Total Alloc (B)</th><th class=\"fn-th-sort\" data-sort-key=\"avgAlloc\" data-label=\"Avg Alloc (B)\">Avg Alloc (B)</th><th class=\"fn-th-sort\" data-sort-key=\"indTime\" data-label=\"Ind Time%\">Ind Time%</th><th class=\"fn-th-sort\" data-sort-key=\"indAlloc\" data-label=\"Ind Alloc%\">Ind Alloc%</th><th class=\"fn-th-sort\" data-sort-key=\"inhTime\" data-label=\"Inh Time%\">Inh Time%</th><th class=\"fn-th-sort\" data-sort-key=\"inhAlloc\" data-label=\"Inh Alloc%\">Inh Alloc%</th>"
    , "</tr></thead><tbody id=\"fn-body\"></tbody></table>"
    , "</div>"
    , "</div>"
    , "<script>"
    , "(function(){"
    , "const cases=" ++ casesJson ++ ";"
    , "let currentCase=cases.length?cases[0].id:'';"
    , "let metric='time';"
    , "let selectedL1='';"
    , "let selectedL2='';"
    , "const tabs=document.getElementById('case-tabs');"
    , "const summary=document.getElementById('summary');"
    , "const btnTime=document.getElementById('btn-time');"
    , "const btnAlloc=document.getElementById('btn-alloc');"
    , "const l1Title=document.getElementById('l1-title');"
    , "const l1Pie=document.getElementById('l1-pie');"
    , "const l1Legend=document.getElementById('l1-legend');"
    , "const l2Title=document.getElementById('l2-title');"
    , "const l2Pie=document.getElementById('l2-pie');"
    , "const l2Legend=document.getElementById('l2-legend');"
    , "const l3Title=document.getElementById('l3-title');"
    , "const l3Pie=document.getElementById('l3-pie');"
    , "const l3Legend=document.getElementById('l3-legend');"
    , "const fnL1=document.getElementById('fn-l1');"
    , "const fnL2=document.getElementById('fn-l2');"
    , "const fnBody=document.getElementById('fn-body');"
    , "const fnHeadCells=Array.from(document.querySelectorAll('th[data-sort-key]'));"
    , "let fnSortKey='totalTime';"
    , "let fnSortDir='desc';"
    , "function withCommaFixed(v,d){const n=Number(v);if(!Number.isFinite(n)){return '-';}return n.toLocaleString('en-US',{minimumFractionDigits:d,maximumFractionDigits:d});}"
    , "function numPct(v){if(v===null){return '-';}return withCommaFixed(Math.round(v*10)/10,1)+'%';}"
    , "function numMs(v){if(v===null){return '-';}return withCommaFixed(Math.round(v*1000)/1000,3);}"
    , "function numInt(v){if(v===null){return '-';}return Math.round(v).toLocaleString('en-US');}"
    , "function getCase(){return cases.find(c=>c.id===currentCase)||cases[0]||null;}"
    , "const PIE_COLORS=['#1f6dcf','#2ea1ff','#0c8f67','#f29f05','#d94841','#6f42c1','#008b8b','#ba5d07','#6b7280','#b91c1c','#10b981','#0ea5e9'];"
    , "function metricVal(item){return metric==='time'?item.time:item.alloc;}"
    , "function sortByMetric(items){return items.slice().sort((a,b)=>metricVal(b)-metricVal(a));}"
    , "function pathArc(cx,cy,r,a0,a1){"
    , "  const x0=cx+r*Math.cos(a0),y0=cy+r*Math.sin(a0);"
    , "  const x1=cx+r*Math.cos(a1),y1=cy+r*Math.sin(a1);"
    , "  const large=(a1-a0)>Math.PI?1:0;"
    , "  return 'M'+cx+' '+cy+' L'+x0+' '+y0+' A'+r+' '+r+' 0 '+large+' 1 '+x1+' '+y1+' Z';"
    , "}"
    , "function aggregateTop(items,valKey,limit){"
    , "  const rows=items.filter(x=>x[valKey]>0).sort((a,b)=>b[valKey]-a[valKey]);"
    , "  if(rows.length<=limit)return rows;"
    , "  const top=rows.slice(0,limit);"
    , "  const rest=rows.slice(limit).reduce((s,x)=>s+x[valKey],0);"
    , "  return top.concat([{name:'Other',value:rest}]);"
    , "}"
    , "function makePieData(raw,byMetric){"
    , "  const rows=raw.map(x=>({name:x.name,value:byMetric(x)})).filter(x=>x.value>0).sort((a,b)=>b.value-a.value);"
    , "  if(!rows.length)return [];"
    , "  return aggregateTop(rows,'value',10);"
    , "}"
    , "function drawPie(svg,legend,title,data,selectedName,onSelect,clickable){"
    , "  svg.innerHTML='';"
    , "  legend.innerHTML='';"
    , "  const total=data.reduce((s,x)=>s+x.value,0);"
    , "  if(total<=0){legend.innerHTML='<div class=\"legend-item passive\"><span class=\"legend-name\">No Data</span></div>';return;}"
    , "  const cx=130,cy=130,r=96;"
    , "  let a=-Math.PI/2;"
    , "  data.forEach((row,i)=>{"
    , "    const ratio=row.value/total;"
    , "    const b=a+ratio*Math.PI*2;"
    , "    const color=PIE_COLORS[i%PIE_COLORS.length];"
    , "    let node='';"
    , "    if(ratio>=0.9999){node='<circle cx=\"'+cx+'\" cy=\"'+cy+'\" r=\"'+r+'\" fill=\"'+color+'\"></circle>';}else{node='<path d=\"'+pathArc(cx,cy,r,a,b)+'\" fill=\"'+color+'\"></path>';}"
    , "    svg.insertAdjacentHTML('beforeend',node);"
    , "    const active=(selectedName&&selectedName===row.name);"
    , "    const cls='legend-item'+(clickable?(active?' active':''):' passive');"
    , "    const val=numPct(row.value);"
    , "    legend.insertAdjacentHTML('beforeend','<button type=\"button\" class=\"'+cls+'\"><span class=\"swatch\" style=\"background:'+color+'\"></span><span class=\"legend-name\" title=\"'+esc(row.name)+'\">'+esc(row.name)+'</span><span class=\"legend-val\">'+val+'</span></button>');"
    , "    a=b;"
    , "  });"
    , "  if(clickable){Array.from(legend.children).forEach((el,i)=>{el.onclick=function(){onSelect(data[i].name);};});}"
    , "}"
    , "function esc(s){return String(s).replace(/[&<>\"']/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c];});}"
    , "function renderTabs(){"
    , "  tabs.innerHTML='';"
    , "  for(const c of cases){"
    , "    const b=document.createElement('button');"
    , "    b.type='button';"
    , "    b.className='btn'+(c.id===currentCase?' active':'');"
    , "    b.textContent=c.title;"
    , "    b.onclick=function(){currentCase=c.id;selectedL1='';renderAll();};"
    , "    tabs.appendChild(b);"
    , "  }"
    , "}"
    , "function renderSummary(){"
    , "  const c=getCase(); if(!c){summary.innerHTML='';return;}"
    , "  summary.innerHTML=''+"
    , "    '<div class=\"kpi\"><div class=\"k\">Case</div><div class=\"v\">'+esc(c.title)+'</div></div>'+"
    , "    '<div class=\"kpi\"><div class=\"k\">Total Time</div><div class=\"v\">'+numMs(c.totalTimeMs)+' ms</div></div>'+"
    , "    '<div class=\"kpi\"><div class=\"k\">Total Alloc</div><div class=\"v\">'+numInt(c.totalAllocBytes)+' B</div></div>'+"
    , "    '<div class=\"kpi\"><div class=\"k\">Functions</div><div class=\"v\">'+numInt(c.fnStats.length)+'</div></div>';"
    , "}"
    , "function renderStageCharts(){"
    , "  const c=getCase(); if(!c){return;}"
    , "  const l1Rows=sortByMetric(c.l1);"
    , "  if(!selectedL1||!l1Rows.some(x=>x.name===selectedL1)){selectedL1=l1Rows[0]?l1Rows[0].name:'';}"
    , "  const l2Rows=sortByMetric(c.l2ByL1[selectedL1]||[]);"
    , "  if(!selectedL2||!l2Rows.some(x=>x.name===selectedL2)){selectedL2=l2Rows[0]?l2Rows[0].name:'';}"
    , "  const fnRows=c.fnStats.filter(x=>x.l1===selectedL1&&x.l2===selectedL2);"
    , "  const l1Data=makePieData(l1Rows,metricVal);"
    , "  const l2Data=makePieData(l2Rows,metricVal);"
    , "  const fnData=makePieData(fnRows,function(x){return metric==='time'?x.indTimePct:x.indAllocPct;});"
    , "  l1Title.textContent='L1 ('+(metric==='time'?'time%':'alloc%')+')';"
    , "  l2Title.textContent='L2 of '+(selectedL1||'N/A')+' ('+(metric==='time'?'time%':'alloc%')+')';"
    , "  l3Title.textContent='Functions of '+(selectedL2||'N/A')+' ('+(metric==='time'?'time%':'alloc%')+')';"
    , "  drawPie(l1Pie,l1Legend,l1Title,l1Data,selectedL1,function(name){selectedL1=name;selectedL2='';renderAll();},true);"
    , "  drawPie(l2Pie,l2Legend,l2Title,l2Data,selectedL2,function(name){selectedL2=name;renderAll();},true);"
    , "  drawPie(l3Pie,l3Legend,l3Title,fnData,'',function(){},false);"
    , "}"
    , "function renderFnFilter(){"
    , "  const c=getCase(); if(!c){fnL1.innerHTML='';fnL2.innerHTML='';return;}"
    , "  const prevL1=fnL1.value || 'ALL';"
    , "  const prevL2=fnL2.value || 'ALL';"
    , "  const l1Names=Array.from(new Set(c.fnStats.map(x=>x.l1))).sort();"
    , "  fnL1.innerHTML='<option value=\"ALL\">All L1</option>'+l1Names.map(n=>'<option value=\"'+esc(n)+'\">'+esc(n)+'</option>').join('');"
    , "  fnL1.value=l1Names.includes(prevL1)?prevL1:'ALL';"
    , "  const currentL1=fnL1.value||'ALL';"
    , "  const l2Source=currentL1==='ALL'?c.fnStats:c.fnStats.filter(r=>r.l1===currentL1);"
    , "  const l2Names=Array.from(new Set(l2Source.map(x=>x.l2))).sort();"
    , "  fnL2.innerHTML='<option value=\"ALL\">All L2</option>'+l2Names.map(n=>'<option value=\"'+esc(n)+'\">'+esc(n)+'</option>').join('');"
    , "  fnL2.value=l2Names.includes(prevL2)?prevL2:'ALL';"
    , "}"
    , "function pickSortKey(row,sortKey){"
    , "  if(sortKey==='l1')return row.l1;"
    , "  if(sortKey==='l2')return row.l2;"
    , "  if(sortKey==='name')return row.name;"
    , "  if(sortKey==='module')return row.module;"
    , "  if(sortKey==='calls')return row.calls;"
    , "  if(sortKey==='avgTime')return row.avgTimeMs;"
    , "  if(sortKey==='totalAlloc')return row.totalAllocBytes;"
    , "  if(sortKey==='avgAlloc')return row.avgAllocBytes;"
    , "  if(sortKey==='indTime')return row.indTimePct;"
    , "  if(sortKey==='indAlloc')return row.indAllocPct;"
    , "  if(sortKey==='inhTime')return row.inhTimePct;"
    , "  if(sortKey==='inhAlloc')return row.inhAllocPct;"
    , "  return row.totalTimeMs;"
    , "}"
    , "function cmpValues(a,b){"
    , "  if(a===null||a===undefined){return (b===null||b===undefined)?0:1;}"
    , "  if(b===null||b===undefined){return -1;}"
    , "  const ta=typeof a,tb=typeof b;"
    , "  if(ta==='number'&&tb==='number'){return a-b;}"
    , "  return String(a).localeCompare(String(b));"
    , "}"
    , "function updateFnSortHeaders(){"
    , "  fnHeadCells.forEach(function(th){"
    , "    const key=th.getAttribute('data-sort-key')||'';"
    , "    const label=th.getAttribute('data-label')||th.textContent||'';"
    , "    if(key===fnSortKey){"
    , "      th.textContent=label+' '+(fnSortDir==='asc'?'↑':'↓');"
    , "      th.setAttribute('aria-sort',fnSortDir==='asc'?'ascending':'descending');"
    , "    }else{"
    , "      th.textContent=label;"
    , "      th.setAttribute('aria-sort','none');"
    , "    }"
    , "  });"
    , "}"
    , "function renderFnTable(){"
    , "  const c=getCase(); if(!c){fnBody.innerHTML='';return;}"
    , "  const l1=fnL1.value||'ALL';"
    , "  const l2=fnL2.value||'ALL';"
    , "  let rows=c.fnStats.slice();"
    , "  if(l1!=='ALL'){rows=rows.filter(r=>r.l1===l1);}"
    , "  if(l2!=='ALL'){rows=rows.filter(r=>r.l2===l2);}"
    , "  rows.sort((a,b)=>{"
    , "    const lhs=pickSortKey(a,fnSortKey);"
    , "    const rhs=pickSortKey(b,fnSortKey);"
    , "    const d=cmpValues(lhs,rhs);"
    , "    return fnSortDir==='asc'?d:-d;"
    , "  });"
    , "  updateFnSortHeaders();"
    , "  fnBody.innerHTML=rows.map(r=>'<tr>'+"
    , "    '<td>'+esc(r.l1)+'</td>'+"
    , "    '<td>'+esc(r.l2)+'</td>'+"
    , "    '<td>'+esc(r.name)+'</td>'+"
    , "    '<td>'+esc(r.module)+'</td>'+"
    , "    '<td>'+numInt(r.calls)+'</td>'+"
    , "    '<td>'+numMs(r.totalTimeMs)+'</td>'+"
    , "    '<td>'+numMs(r.avgTimeMs)+'</td>'+"
    , "    '<td>'+numInt(r.totalAllocBytes)+'</td>'+"
    , "    '<td>'+numInt(r.avgAllocBytes)+'</td>'+"
    , "    '<td>'+numPct(r.indTimePct)+'</td>'+"
    , "    '<td>'+numPct(r.indAllocPct)+'</td>'+"
    , "    '<td>'+numPct(r.inhTimePct)+'</td>'+"
    , "    '<td>'+numPct(r.inhAllocPct)+'</td>'+"
    , "  '</tr>').join('');"
    , "}"
    , "function renderAll(){"
    , "  btnTime.classList.toggle('active',metric==='time');"
    , "  btnAlloc.classList.toggle('active',metric==='alloc');"
    , "  renderTabs();"
    , "  renderSummary();"
    , "  renderStageCharts();"
    , "  renderFnFilter();"
    , "  renderFnTable();"
    , "}"
    , "btnTime.onclick=function(){metric='time';renderAll();};"
    , "btnAlloc.onclick=function(){metric='alloc';renderAll();};"
    , "fnHeadCells.forEach(function(th){"
    , "  th.onclick=function(){"
    , "    const key=th.getAttribute('data-sort-key')||'totalTime';"
    , "    if(fnSortKey===key){fnSortDir=(fnSortDir==='desc'?'asc':'desc');}else{fnSortKey=key;fnSortDir='desc';}"
    , "    renderFnTable();"
    , "  };"
    , "});"
    , "fnL1.onchange=function(){renderFnFilter();renderFnTable();};"
    , "fnL2.onchange=renderFnTable;"
    , "renderAll();"
    , "})();"
    , "</script>"
    , "</div></body></html>"
    ]

jsonArrayCaseReports :: [CaseReport] -> String
jsonArrayCaseReports xs = "[" ++ List.intercalate "," (map jsonCaseReport xs) ++ "]"

jsonCaseReport :: CaseReport -> String
jsonCaseReport cr =
  let prof = crProfile cr
      totalTimeMs = prTotalTimeSecs prof * 1000
   in
  "{"
    ++ "\"id\":" ++ jsonString (cdKey (crDef cr)) ++ ","
    ++ "\"title\":" ++ jsonString (cdTitle (crDef cr)) ++ ","
    ++ "\"totalTimeMs\":" ++ showNum totalTimeMs ++ ","
    ++ "\"totalAllocBytes\":" ++ showNum (prTotalAllocBytes prof) ++ ","
    ++ "\"l1\":" ++ jsonArrayStageNodes (crL1 cr) ++ ","
    ++ "\"l2ByL1\":" ++ jsonObjectNodeMap (crL2ByL1 cr) ++ ","
    ++ "\"fnStats\":" ++ jsonArrayFnStats totalTimeMs (prTotalAllocBytes prof) (crFnStats cr)
    ++ "}"

jsonArrayStageNodes :: [StageNode] -> String
jsonArrayStageNodes nodes = "[" ++ List.intercalate "," (map jsonStageNode nodes) ++ "]"

jsonObjectNodeMap :: Map.Map String [StageNode] -> String
jsonObjectNodeMap m =
  "{" ++ List.intercalate "," (map renderPair (Map.toList m)) ++ "}"
  where
    renderPair (k, nodes) = jsonString k ++ ":" ++ jsonArrayStageNodes nodes

jsonStageNode :: StageNode -> String
jsonStageNode n =
  "{"
    ++ "\"name\":" ++ jsonString (snName n) ++ ","
    ++ "\"time\":" ++ showNum (snTimePct n) ++ ","
    ++ "\"alloc\":" ++ showNum (snAllocPct n)
    ++ "}"

jsonArrayFnStats :: Double -> Double -> [FnStat] -> String
jsonArrayFnStats totalTimeMs totalAllocBytes stats =
  "[" ++ List.intercalate "," (map render stats) ++ "]"
  where
    render s =
      let callsD = fromIntegral (fsCalls s)
          totalFnTimeMs = totalTimeMs * fsIndTimePct s / 100
          totalFnAllocBytes = totalAllocBytes * fsIndAllocPct s / 100
          avgFnTimeMs = if fsCalls s > 0 then Just (totalFnTimeMs / callsD) else Nothing
          avgFnAllocBytes = if fsCalls s > 0 then Just (totalFnAllocBytes / callsD) else Nothing
       in
      "{"
        ++ "\"l1\":" ++ jsonString (fsL1 s) ++ ","
        ++ "\"l2\":" ++ jsonString (fsL2 s) ++ ","
        ++ "\"name\":" ++ jsonString (fsName s) ++ ","
        ++ "\"module\":" ++ jsonString (fsModule s) ++ ","
        ++ "\"calls\":" ++ show (fsCalls s) ++ ","
        ++ "\"indTimePct\":" ++ showNum (fsIndTimePct s) ++ ","
        ++ "\"indAllocPct\":" ++ showNum (fsIndAllocPct s) ++ ","
        ++ "\"inhTimePct\":" ++ showNum (fsInhTimePct s) ++ ","
        ++ "\"inhAllocPct\":" ++ showNum (fsInhAllocPct s) ++ ","
        ++ "\"totalTimeMs\":" ++ showNum totalFnTimeMs ++ ","
        ++ "\"avgTimeMs\":" ++ jsonMaybeNum avgFnTimeMs ++ ","
        ++ "\"totalAllocBytes\":" ++ showNum totalFnAllocBytes ++ ","
        ++ "\"avgAllocBytes\":" ++ jsonMaybeNum avgFnAllocBytes
        ++ "}"

showNum :: Double -> String
showNum x = showFFloat (Just 6) x ""

jsonMaybeNum :: Maybe Double -> String
jsonMaybeNum Nothing = "null"
jsonMaybeNum (Just x) = showNum x

jsonString :: String -> String
jsonString s = "\"" ++ concatMap escapeChar s ++ "\""

escapeChar :: Char -> String
escapeChar c
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | c == '\n' = "\\n"
  | c == '\r' = "\\r"
  | c == '\t' = "\\t"
  | ord c < 0x20 = "\\u" ++ hex4 (ord c)
  | otherwise = [c]

hex4 :: Int -> String
hex4 x =
  let hex = "0123456789abcdef"
      nibble n = [hex !! n]
      d3 = (x `div` 4096) `mod` 16
      d2 = (x `div` 256) `mod` 16
      d1 = (x `div` 16) `mod` 16
      d0 = x `mod` 16
   in nibble d3 ++ nibble d2 ++ nibble d1 ++ nibble d0

cleanupIfExists :: FilePath -> IO ()
cleanupIfExists p = do
  exists <- doesFileExist p
  if exists then removeFile p else pure ()

shellQuote :: String -> String
shellQuote s = "'" ++ concatMap escape s ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]
