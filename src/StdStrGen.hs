{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module StdStrGen    ( genStr
                    , genStd
                    )
                    where

-- Public imports
import System.Directory
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Builder
{-import Text.Pandoc.Generic-}
import Text.Blaze.Renderer.String
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Binary.UTF8.Generic as BS8
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Set as S
-- private imports
import ProgramOptions
import StrStdTypes
import ZephyrJson
import JiraTypes
import IssueHierarchy
import DescriptionParser
import DocxCustom

import Debug.Trace

genStr :: Options -> IO ()
genStr opts = do
    createDirectoryIfMissing True $ dropFileName . optDocxFile $ opts
    putStrLn "Reading str source "
    strSrc :: StrSrc <- liftM read $ readFile (optStrStdFile opts)
    putStrLn "Reading issue hierarchy"
    hierarchy :: IssueHierarchy <- liftM read $ readFile (optHierarchyFile opts)
    putStrLn "Generating pandoc"
    cd <- getCurrentDirectory
    let bfn = dropExtension . optDocxFile $ opts
        -- _docOptions ::  FilePath -> WriterOptions
        docOptions = def {writerUserDataDir = Just cd}
        docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}
        pandoc = Pandoc docMeta (_strToDoc strSrc hierarchy)
    putStrLn "Generating native"
    writeFile (bfn ++ "_Native.txt") $ writeNative docOptions pandoc
    putStrLn "Generating markdown"
    BS.writeFile (bfn ++ "_MarkDown.txt") $ BS8.fromString $ writeMarkdown docOptions pandoc
    putStrLn "Generating html"
    BS.writeFile (bfn ++ "_HTML.html") $ BS8.fromString (renderMarkup $ writeHtml docOptions pandoc)
    putStrLn "Generating docx"
    d <- writeDocxCustom docOptions pandoc
    BS.writeFile (optDocxFile opts) d 
    return ()

_statusToText :: TestStatus -> String
_statusToText  = _s 
    where
        _s TestPass = "Passed"
        _s TestFail = "Failed"
        _s TestWip  = "In Progress"
        _s TestBlocked = "Blocked"
        _s TestUnexecuted = "Unexecuted"
        _s NullTestStatus = fail "Should never be here"

(<|) :: (b -> a) -> b -> a
infixl 8 <|
f <| a = f a

_strToDoc :: StrSrc -> IssueHierarchy -> [Block]
_strToDoc StrSrc{..} hierarchy = toList $   
                                header 1 . text <| "Requirements Accepted" <> _strRequirementsAccepted strTests 
                             <> header 1 . text <| "Test Result Summary" <> _strTestCaseSummary strTests 
                             <> header 1 . text <| "Tests" <> _strTestCases 2 strTests
                             <> header 1 . text <| "Traceability" <> _strRequirementTraceability strTests
                             <> _hierarchyExerpt hierarchy strTests

_strRequirementsAccepted :: [StrTestSrc] -> Blocks
_strRequirementsAccepted ts = if null rs 
        then para . text <| "No requirements were accepted as implemented." 
        else  para . text <| "The following requirements have been verified and accepted as implemented:" <> simpleTable [para . text <| "Requirement ID"
                                                                                                                         ,para . text <| "Requirement Title"
                                                                                                                         ,para . text <| "Test ID"
                                                                                                                         ,para . text <| "Test Title"
                                                                                                                         ] rs
    where
        rs :: [[Blocks]]
        rs = filter (not . null) . map snd . sortBy cmp . concatMap closedStories $ ts
        closedStories :: StrTestSrc -> [(String, [Blocks])]
        closedStories t = map cmb . filter ((==) "Closed" . jsiStatus) . strStories $ t
            where 
                cmb :: JsIssue -> (String, [Blocks])
                cmb i = (jsiKey i, inf i ++ (inf . strIssue $ t))
        inf :: JsIssue -> [Blocks]
        inf i = [para . text . jsiKey $ i, para . text . jsiSummary $ i]
        cmp :: (String, [Blocks]) -> (String, [Blocks]) -> Ordering
        cmp l r = compare (_extractIdFromKey . fst $ l) (_extractIdFromKey . fst $ r)


_strTestCases :: Int -> [StrTestSrc] -> Blocks
_strTestCases h ts = foldl (<>) (fromList []) . map toDoc . _orderTests $ ts
    where
        toDoc :: StrTestSrc -> Blocks
        toDoc t = hdr <> stat <> cmnt <> desc  <> stepTbl
            where
                ti = strIssue t 
                tr = strResult t
                ss = strSteps t
                hdr = header h $ text . jsiKey <| ti <> str " - " <> text . jsiSummary <| ti
                stat = header (h+1) . text <| "Status" <> para . text . _statusToText . status <| tr
                cmnt = header (h+1) . text <| "Comment" <> para . text. (\s-> if s =="" then "No comments." else s) . comment <| tr
                desc = header (h+1) . text <| "Description" 
                        <> fromList . normHdrs . parseDescription 0 . filter (/= '\r') . fromMaybe "" . jsiDescription <| ti
                normHdrs :: [Block] -> [Block]
                normHdrs bs = topDown modHdr bs 
                    where
                        minHdr = minimum . queryWith getHdr $ bs
                        getHdr (Header i _ _) = [i]
                        getHdr _ = []
                        offHdr = (h + 2) - minHdr
                        modHdr (Header i a l) = Header (offHdr + i) a l
                        modHdr a = a
                stepTbl = header (h+1) . text <| "Steps" <>  
                          simpleTable [para . text $ "#"
                                      ,para . text $ "Description"
                                      ,para . text $ "Data"
                                      ,para . text $ "Expected"
                                      ,para . text $ "Status"
                                      ,para . text $ "Comment"] steps
                steps :: [[Blocks]]
                steps = map step (zip ss [1..])
                    where 
                        step s = [para . text . show . snd $ s
                                 ,para . text . stepInfoDesc . fst . fst $ s
                                 ,para . text . stepInfoData . fst . fst $ s
                                 ,para . text . stepInfoExpect . fst . fst $ s
                                 ,para . text . _statusToText . stepResStatus . snd . fst $ s
                                 ,para . text . stepResComment . snd . fst $ s]

_strTestCaseSummary :: [StrTestSrc] -> Blocks
_strTestCaseSummary ts = if null rs 
        then para . text <| "No tests !!!." 
        else para . text <| "The following table provides a summary of the test results." 
                         <> simpleTable [para . text <| "Test ID", para . text <| "Test Title", para . text <| "Status"] rs
    where
        rs :: [[Blocks]]
        rs = filter (not . null) . map smry . _orderTests $ ts
        smry t = [para . text . jsiKey . strIssue $ t
                ,para . text . jsiSummary . strIssue $ t
                ,para . text . _statusToText . status . strResult $ t]

_strRequirementTraceability :: TestDesc a => [a] -> Blocks
_strRequirementTraceability ts  = table (text "") [(AlignLeft, 0.5/2),(AlignLeft, 0.5/2),(AlignLeft, 0.5/2),(AlignLeft, 0.5/2)] 
                                              [para . text $ "Requirement ID"
                                              ,para . text $ "Requirement Title"
                                              ,para . text $ "Test ID"
                                              ,para . text $ "Test Title"] rs
    where 
        rs = map toR . collapse . sortBy csk . extract $ ts
        -- order by storyKey
        csk :: (String, String, String, String) -> (String, String, String, String) -> Ordering
        csk (sortKeyL, _, _, _) (sortKeyR, _, _, _) = compare (_extractIdFromKey sortKeyL) (_extractIdFromKey sortKeyR)
        -- remove duplicated
        collapse [] = []
        collapse (a:[]) = [a]
        collapse (a1:a2:as) 
            | a1 == a2      = a1 : collapse as
            | otherwise     = a1 : collapse (a2:as)
        -- turn it into rows
        toR (storyKey, storySummary, testKey, testSummary) = [para . text $ storyKey
                                                             ,para . text $ storySummary 
                                                             ,para . text $ testKey 
                                                             ,para . text $ testSummary 
                                                             ]
        -- ts to  (storyKey, storySummary, testKey, testSummary)
        extract :: TestDesc a => [a] -> [(String, String, String, String)] 
        extract ts' = concatMap ex ts'
            where 
                ex t = map (ex' (tstIssue t)) (tstStories t)
                ex' ti si = (jsiKey si, jsiSummary si, jsiKey ti, jsiSummary ti)


_hierarchyExerpt :: TestDesc a => IssueHierarchy -> [a] -> Blocks
_hierarchyExerpt is' tsts' = header 1 . text <| "Appendix A - SSS Excerpt" <>  expndChild 2  (filt is' tsts')
    where
        expndChild :: Int -> IssueHierarchy -> Blocks
        expndChild l IssueHierarchyRoot {ihChildren = cs} = fromList . concatMap (toList . expndChild l) $ cs
        expndChild l issue = hdr <> cnt <> rest 
            where
                hdr = header l (text . ihKey <| issue <> str " : " <> text . ihSummary <| issue) -- Header nullAttr [Str $ ihKey issue, Str ":", Space, Str $ ihSummary issue] :: Block
                cnt = fromList . parseDescription l . filter (/= '\r') . ihDescription $ issue
                rest = fromList . concatMap (toList . expndChild (l+1)) $ ihChildren issue
                ihKey (IssueHierarchyRoot _) = ""
                ihKey h = jsiKey . ihIssue $ h
                ihSummary (IssueHierarchyRoot _) = ""
                ihSummary h = jsiSummary . ihIssue $ h
                ihDescription (IssueHierarchyRoot _) = ""
                ihDescription h = fromMaybe "" . jsiDescription . ihIssue $ h

        filt is tsts = fromJust . reduce $ is
            where
                incSet = S.fromList . concatMap (map jsiKey . tstStories) $ tsts
                reduce :: IssueHierarchy -> Maybe IssueHierarchy
                reduce IssueHierarchy {ihIssue = i, ihChildren = cs}
                    | S.member (jsiKey i) incSet = Just (IssueHierarchy i cs')
                    | null cs' = Nothing
                    | otherwise = Just (IssueHierarchy i cs')
                    where cs' = filtCs cs 
                reduce IssueHierarchyRoot {ihChildren = cs} = Just IssueHierarchyRoot{ihChildren = filtCs cs}
                filtCs = map fromJust . filter isJust . map reduce




genStd :: Options -> IO ()
genStd opts = do
    createDirectoryIfMissing True $ dropFileName . optDocxFile $ opts
    putStrLn "Reading std source "
    stdSrc :: StdSrc <- liftM read $ readFile (optStrStdFile opts)
    putStrLn "Reading issue hierarchy"
    hierarchy :: IssueHierarchy <- liftM read $ readFile (optHierarchyFile opts)
    putStrLn "Generating pandoc"
    cd <- getCurrentDirectory
    let bfn = dropExtension . optDocxFile $ opts
        -- _docOptions ::  FilePath -> WriterOptions
        docOptions = def {writerUserDataDir = Just cd}
        docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}
        pandoc = Pandoc docMeta (_stdToDoc stdSrc hierarchy)
    putStrLn "Generating native"
    writeFile (bfn ++ "_Native.txt") $ writeNative docOptions pandoc
    putStrLn "Generating markdown"
    BS.writeFile (bfn ++ "_MarkDown.txt") $ BS8.fromString $ writeMarkdown docOptions pandoc
    putStrLn "Generating html"
    BS.writeFile (bfn ++ "_HTML.html") $ BS8.fromString (renderMarkup $ writeHtml docOptions pandoc)
    putStrLn "Generating docx"
    d <- writeDocxCustom docOptions pandoc
    BS.writeFile (optDocxFile opts) d 
    return ()

_stdToDoc :: StdSrc -> IssueHierarchy -> [Block]
_stdToDoc StdSrc{..} hierarchy = toList $ header 1 . text <| "Tests" <> _stdTestCases 2 stdTests
                             <> header 1 . text <| "Traceability" <> _strRequirementTraceability stdTests
                             <> _hierarchyExerpt hierarchy stdTests 



_stdTestCases :: Int -> [StdTestSrc] -> Blocks
_stdTestCases h ts = foldl (<>) (fromList []) . map toDoc . _orderTests $ ts
    where
        toDoc :: StdTestSrc -> Blocks
        toDoc t = hdr <> desc  <> stepTbl
            where
                ti = stdIssue t 
                ss = stdSteps t
                hdr = header h $ text . jsiKey <| ti <> str " - " <> text . jsiSummary <| ti
                desc = header (h+1) . text <| "Description" 
                        <> fromList . normHdrs . parseDescription 0 . filter (/= '\r') . fromMaybe "" . jsiDescription <| ti
                normHdrs :: [Block] -> [Block]
                normHdrs bs = topDown modHdr bs 
                    where
                        minHdr = minimum . queryWith getHdr $ bs
                        getHdr (Header i _ _) = [i]
                        getHdr _ = []
                        offHdr = (h + 2) - minHdr
                        modHdr (Header i a l) = Header (offHdr + i) a l
                        modHdr a = a
                stepTbl = header (h+1) . text <| "Steps" <> 
                          simpleTable [para . text $ "#"
                                      ,para . text $ "Description"
                                      ,para . text $ "Data"
                                      ,para . text $ "Expected"
                                      ] steps
                steps :: [[Blocks]]
                steps = map step (zip ss [1..])
                    where 
                        step s = [para . text . show . snd $ s
                                 ,para . text . stepInfoDesc . fst $ s
                                 ,para . text . stepInfoData . fst $ s
                                 ,para . text . stepInfoExpect . fst $ s
                                 ]


_extractIdFromKey :: String -> Int
_extractIdFromKey  = read . tail . dropWhile ('-'/=)  

_orderTests :: TestDesc a => [a] -> [a]
_orderTests  = sortBy cmp 
    where 
        cmp l r = compare (_extractIdFromKey . jsiKey . tstIssue $ l) (_extractIdFromKey . jsiKey . tstIssue $ r)
