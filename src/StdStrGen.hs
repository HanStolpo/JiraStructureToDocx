{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module StdStrGen    ( genStr
                    )
                    where

-- Public imports
import System.Directory
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Generic
import Text.Blaze.Renderer.String
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Binary.UTF8.Generic as BS8
import Control.Monad
import Data.Maybe
import Data.List
-- private imports
import ProgramOptions
import StrStdTypes
import ZephyrJson
import JiraTypes
import DescriptionParser



genStr :: Options -> IO ()
genStr opts = do
    createDirectoryIfMissing True $ dropFileName . optDocxFile $ opts
    putStrLn "Reading str source "
    strSrc :: StrSrc <- liftM read $ readFile (optStrStdFile opts)
    putStrLn "Generating pandoc"
    cd <- getCurrentDirectory
    let bfn = dropExtension . optDocxFile $ opts
        -- _docOptions ::  FilePath -> WriterOptions
        docOptions = def {writerUserDataDir = Just cd}
        docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}
        pandoc = Pandoc docMeta . _strToDoc $ strSrc
    putStrLn "Generating native"
    writeFile (bfn ++ "_Native.txt") $ writeNative docOptions pandoc
    putStrLn "Generating markdown"
    BS.writeFile (bfn ++ "_MarkDown.txt") $ BS8.fromString $ writeMarkdown def pandoc
    putStrLn "Generating html"
    BS.writeFile (bfn ++ "_HTML.html") $ BS8.fromString (renderMarkup $ writeHtml docOptions pandoc)
    putStrLn "Generating docx"
    d <- writeDocx docOptions pandoc
    BS.writeFile (optDocxFile opts) d 
    return ()

_statusToText :: TestStatus -> String
_statusToText  = _s 
    where
        _s TestPass = "Passed"
        _s TestFail = "Failed"
        _s TestWip  = "In Progress"
        _s TestBlocked = "Blocked"
        _s TestUnexecuted = "Not Executed"
        _s NullTestStatus = fail "Should never be here"

(<|) :: (b -> a) -> b -> a
infixl 8 <|
f <| a = f a

_strToDoc :: StrSrc -> [Block]
_strToDoc StrSrc{..} = toList $ header 1 . text <| "Requirements Accepted" <>  _strRequirementsAccepted strTests 
                             <> header 1 . text <| "Test Case Summary" <> _strTestCaseSummary strTests 
                             <> header 1 . text <| "Test Cases" <> _strTestCases 2 strTests
                             <> header 1 . text <| "Traceability" <> _strRequirementTraceability strTests

_strRequirementsAccepted :: [StrTestSrc] -> Blocks
_strRequirementsAccepted ts = if null rs 
        then para . text <| "No issues have been accepted as implemented as part of this test cycle." 
        else para . text <| "The following requirements have been accepted as implemented:" <> simpleTable [para . text <| "Requirement ID", para . text <| "Test ID"] rs
    where
        rs :: [[Blocks]]
        rs = filter (not . null) . concatMap closedStories $ ts
        closedStories :: StrTestSrc -> [[Blocks]]
        closedStories t = map (flip (:) [para . text . jsiKey . strIssue $ t] . para . text . jsiKey) . filter ((==) "Closed" . jsiKey) . strStories $ t


_strTestCases :: Int -> [StrTestSrc] -> Blocks
_strTestCases h ts = foldl (<>) (fromList []) . map toDoc $ ts
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
                stepTbl = simpleTable [para . text $ "Description"
                                      ,para . text $ "Data"
                                      ,para . text $ "Expected"
                                      ,para . text $ "Status"
                                      ,para . text $ "Comment"] steps
                steps :: [[Blocks]]
                steps = map step ss
                    where 
                        step s = [para . text . stepInfoDesc . fst $ s
                                 ,para . text . stepInfoData . fst $ s
                                 ,para . text . stepInfoExpect . fst $ s
                                 ,para . text . _statusToText . stepResStatus . snd $ s
                                 ,para . text . stepResComment . snd $ s]

_strTestCaseSummary :: [StrTestSrc] -> Blocks
_strTestCaseSummary ts = if null rs 
        then para . text <| "No tests !!!." 
        else para . text <| "The following table provides a summary of the test results." 
                         <> simpleTable [para . text <| "Test ID", para . text <| "Summary", para . text <| "Status"] rs
    where
        rs :: [[Blocks]]
        rs = filter (not . null) . map sum $ ts
        sum t = [para . text . jsiKey . strIssue $ t
                ,para . text . jsiSummary . strIssue $ t
                ,para . text . _statusToText . status . strResult $ t]

_strRequirementTraceability :: [StrTestSrc] -> Blocks
_strRequirementTraceability ts  = table (text "") [(AlignLeft, 0.5/2),(AlignLeft, 0.5/2),(AlignLeft, 0.5/2),(AlignLeft, 0.5/2)] 
                                              [para . text $ "Requirement ID"
                                              ,para . text $ "Requirement Summary"
                                              ,para . text $ "Test ID"
                                              ,para . text $ "Test Summary"] rs
    where 
        rs = map toR . collapse . sortBy pred . extract $ ts
        -- order by storyKey
        pred :: (String, String, String, String) -> (String, String, String, String) -> Ordering
        pred (sortKeyL, _, _, _) (sortKeyR, _, _, _) = compare sortKeyL sortKeyR
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
        extract :: [StrTestSrc] -> [(String, String, String, String)] 
        extract ts = concatMap ex ts
            where 
                ex StrTestSrc{..} = map (ex' strIssue) strStories
                ex' ti si = (jsiKey si, jsiSummary si, jsiKey ti, jsiSummary ti)
