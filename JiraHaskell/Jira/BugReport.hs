{-# LANGUAGE OverloadedStrings
  , RecordWildCards
  #-}


module Jira.BugReport (generateBugReport) where 

import Data.Functor
import qualified Data.Char as C
import Data.Tree
import Data.Function
import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Format
import Text.Pandoc
import Text.Pandoc.Builder
import Text.CSV
import System.FilePath
import System.Directory
import System.Locale
import Network.HTTP.Conduit
import Network (withSocketsDo)
-- local
import Pandoc.Reader.Jira
import Jira.JiraTypes
import Jira.Utility
import Jira.Query as Q
import Jira.ProgramOptions

newtype BugInfo = BugInfo (Issue, Bool, String, String) deriving (Eq, Show)

prepareBugs :: [Issue] -> [Either String BugInfo]
prepareBugs = map verifyBugDescription

verifyBugDescription :: Issue -> Either String BugInfo
verifyBugDescription i = (\rs -> BugInfo (i, resolved, fst rs, snd rs)) <$> descH
    where
        desc = maybe (Left "No description") Right . issueDescription $ i
        prsdDesc = (parseDescription 0 <$> desc) >>= (\a -> if null a 
                                                             then Left "Failed to parse description" 
                                                             else Right a)
        descH = (unflattenPandoc <$> prsdDesc) >>= extractReportResolution
        resolved = let st = map C.toLower . issueStatus $ i in st == "closed" || st == "resolved"


summarizeText :: Int -> String -> String
summarizeText l s = if length s > l then take (max (l-3) 0) s ++ "..." else s

extractReportResolution :: Tree Block -> Either String (String, String)
extractReportResolution h = case h of
    Node { rootLabel = Null
            , subForest = ( Node {rootLabel = Header 4 _ [Str"Report"], subForest = a}
                        : Node {rootLabel = Header 4 _ [Str"Resolution"], subForest = b}
                        : _
                        )
            } -> Right (toTxt a, toTxt b)
    _ -> Left "Error the description format is incorrect. At a minimum it must have a header level 4 'Report' followed by a header level 4 'Resolution'"
    where toTxt = writePlain def . Pandoc nullMeta . concatMap flatten 



issueTimeToDateTxt :: Maybe IssueTime -> String
issueTimeToDateTxt = takeWhile (/='T') . maybe "" show

isResolved :: BugInfo -> Bool
isResolved (BugInfo (_, b, _, _)) = b

createdDate :: BugInfo -> Maybe IssueTime
createdDate (BugInfo (i, _, _, _)) = issueCreateDate i

resolvedDate :: BugInfo -> Maybe IssueTime
resolvedDate (BugInfo (i, _, _, _)) = issueResolvedDate i

issueKeyParts :: BugInfo -> (String, Int)
issueKeyParts (BugInfo (i, _, _, _)) = (pfx, ki)
    where
        pfx = takeWhile (/='-') . issueKey $ i
        kis = drop 1 . dropWhile (/='-') . issueKey $ i
        ki = if null kis then -1 else read kis




invalidBugToRecord :: (Issue, Either String BugInfo) -> Record
invalidBugToRecord (i, Left e) = [issueKey i, issueSummary i, maybe "" show . issueAssignee $ i, maybe "" show . issueReporter $ i, e]
invalidBugToRecord (_, Right _) = error "invalidBugToRecord"

makeReport :: UTCTime -> [BugInfo] -> [Block]
makeReport ct bs = toList (overView <> outStandingIssue <> resolvedIssue <> issueDetails)
    where
        outStandingIssue =  header 1 (str "Summary of Outstanding Issues") <> simpleTable hdgO (map oRow . sortBy (compare `on` Down . createdDate) . filter (not . isResolved) $ bs)
        resolvedIssue = header 1 (str "Summary of Resolved Issues") <> simpleTable hdgR (map rRow . sortBy (compare `on` Down . resolvedDate) . filter isResolved $ bs)
        issueDetails = header 1 (str "Issue Details") <> (mconcat . map toDt . sortBy (compare `on` issueKeyParts) $ bs)
        hdgO = init hdgR
        hdgR = map para [str "ID", str "Title", str "Report", str "Resolution", str "Report Date", str "Resolved Date"]
        oRow  = init . rRow
        rRow (BugInfo (i, _, rep, res)) = map para  [ str . issueKey $ i
                                                    , text . summarizeText 60 . issueSummary $ i
                                                    , text . summarizeText 60 $ rep
                                                    , text . summarizeText 60 $ res
                                                    , str . issueTimeToDateTxt . issueCreateDate $ i
                                                    , str . issueTimeToDateTxt . issueResolvedDate $ i
                                                    ]
        toDt :: BugInfo -> Blocks 
        toDt (BugInfo (i, _, _, _)) = header 2 (text (issueKey i ++ " - " ++ issueSummary i)) <> (fromList . correctHdrLevels 3 . parseDescription 0 . maybe "" id . issueDescription $ i)
        overView = header 1 (str "Overview") 
                <> (para . text $ 
                "This report provides a summary of issues that have not been resolved yet in section 2 (Summary of Outstanding Issues) and " ++
                "a summary of issues that have been resolved in section 3 (Summary of Resolved Issues). The summary sections do not list the " ++
                "detailed descriptions of the issues, those are provided in section 4 (Issue Details). ") 
                <> (para . str . ("This report was generated on " ++) $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) ct)


generateBugReport :: Options -> IO ()
generateBugReport opt' = do
    opt <- validate opt'
    bugs <- Q.query opt
    ct <- getCurrentTime
    let
        output = fromJust . optFileOutput $ opt
        imld = issueImageLocalizeInfo (dropExtension output ++ "_images") bugs
        imgs = map (\(_, uri, ph) -> (uri, imgLink ph)) imld
        -- fixPh p =  map (\c -> if c == '\\' then '/' else c) . makeRelative (takeDirectory output) $ p
        fixPh p = makeRelative (takeDirectory output) $ p
        mapImgRep = M.fromList . map (\(o, _, ph) -> (o,ph{imgLink = fixPh . imgLink $ ph})) $ imld
        repImg :: ImageLink -> ImageLink
        repImg il@ImageLink{imgLink = o} = fromMaybe il . M.lookup o $ mapImgRep
        repIssueImgs i@Issue{issueDescription = Just d} = i{issueDescription = Just . replaceImageLinks repImg $ d}
        repIssueImgs i = i
        bugsPrepped = prepareBugs . map repIssueImgs $ bugs
        invalidBugs = filter (isLeft . snd) . zip bugs $ bugsPrepped 
        invalidBugsCsv = ["ID", "Title", "assignee", "reporter", "error"] : map invalidBugToRecord invalidBugs 
        validBugs = either (const []) id . sequence . filter isRight $ bugsPrepped
    createDirectoryIfMissing True . dropFileName $ output
    writeFile (dropExtension output ++ "_problematic_issues.csv") . printCSV $ invalidBugsCsv
    let report = makeReport ct validBugs
    writeFile output . writeMarkdown (def {writerColumns = 120, writerWrapText = True} ) . Pandoc nullMeta $ report
    writeFile (dropExtension output ++ ".html") . writeHtmlString (def {writerColumns = 120, writerWrapText = True} ) . Pandoc nullMeta $ report
    withSocketsDo . withManager $ \m -> do
        fetchImagesLocal m (B.pack . fromMaybe "" . optUsr $ opt) (B.pack . fromMaybe "" . optPwd $ opt) (S.toList . S.fromList $ imgs)
