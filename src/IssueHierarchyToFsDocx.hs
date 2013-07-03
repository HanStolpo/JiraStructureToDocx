{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables, NamedFieldPuns, RecordWildCards#-}

module IssueHierarchyToFsDocx (genFsDoc) where

import Data.Maybe
import Data.Monoid
import Data.List
import Data.Function
import Text.CSV
import qualified Data.Map as M
import Control.Monad.Error
import Control.Applicative
import System.Directory
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Generic
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Binary.UTF8.Generic as BS8
import qualified Data.Aeson as AS
import Text.Blaze.Renderer.String
import System.FilePath
-- Local imports
import IssueHierarchy
import JiraTypes
import DescriptionParser
import ProgramOptions
import DocxCustom
 
genFsDoc :: Options -> IO ()
genFsDoc opts = do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optDocxFile $ opts
    cd <- getCurrentDirectory
    putStrLn "Reading FS issue hierarchy"
    Just fsH :: Maybe IssueHierarchy <- liftM AS.decode . BS.readFile . optHierarchyFile $ opts
    putStrLn "Reading SSS issue hierarchy"
    Just ssH :: Maybe IssueHierarchy <- liftM AS.decode . BS.readFile . fromJust . optHierarchySssFile $ opts
    putStrLn "Generating pandoc"
    let pandoc = replaceImages $ Pandoc docMeta $ hierarchyToDoc (ihChildren fsH) (ihChildren ssH)
    {-let bfn = dropExtension . optDocxFile $ opts-}
    {-putStrLn "Generating native"-}
    {-writeFile (bfn ++ "_Native.txt") $ writeNative (docOptions cd) pandoc-}
    {-putStrLn "Generating markdown"-}
    {-BS.writeFile (bfn ++ "_MarkDown.txt") $ BS8.fromString $ writeMarkdown def pandoc-}
    {-putStrLn "Generating html"-}
    {-BS.writeFile (bfn ++ "_HTML.html") $ BS8.fromString (renderMarkup $ writeHtml (docOptions cd) pandoc)-}
    putStrLn "Generating docx"
    d <- writeDocxCustom (docOptions cd) pandoc
    BS.writeFile (optDocxFile opts) d
    writeFile ((dropExtension . optDocxFile $ opts) ++ "_TraceFsToSss.csv") . printCSV $ trmFsToSss (ihChildren fsH) (ihChildren ssH)
    writeFile ((dropExtension . optDocxFile $ opts) ++ "_TraceSssToFs.csv") . printCSV $ trmSssToFs (ihChildren fsH) (ihChildren ssH)
    putStrLn "Done"
    return ()

replaceImages :: Pandoc -> Pandoc
replaceImages = bottomUp modImg
    where
        modImg (Image _ _) = Strong . toList . text $ "(Image not displayed refer to FS)"
        modImg x = x

docOptions ::  FilePath -> WriterOptions
docOptions cd = def {writerUserDataDir = Just cd
                    }
docMeta ::  Meta
docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}

matchNoTraceLink :: IssueLink -> Bool
matchNoTraceLink (Inward s _ _) = s == "not traceable because"
matchNoTraceLink (Outward _ _ _) = False

matchSrcLink :: IssueLink -> Bool
matchSrcLink (Inward s _ _) = s == "is source of" || s == "relates to"
matchSrcLink (Outward s _ _) = s == "relates to"

getLinkId :: IssueLink -> Int
getLinkId (Inward _ i _) = i
getLinkId (Outward _ i _) = i

issueKeyToInt :: String -> Int
issueKeyToInt = read . drop 1 . dropWhile ('-'/=) 

{-hierarchyToDoc :: [IssueHierarchy] -> [IssueHierarchy] -> [Block]-}
{-hierarchyToDoc fs sss = toList . foldl mappend mempty . map (expndChild 1) $ fs-}
    {-where-}
        {-sssMap :: M.Map Int Issue-}
        {-sssMap = M.fromList . concatMap ext $ sss-}
            {-where-}
                {-ext :: IssueHierarchy -> [(Int, Issue)]-}
                {-ext ih = ((issueId . ihIssue $ ih, ihIssue ih) : (concatMap ext . ihChildren $ ih))-}
        {-expndChild :: Int -> IssueHierarchy -> Blocks-}
        {-expndChild l issue@IssueHierarchyRoot{} = error "not expecting an IssueHierarchyRoot"-}
        {-expndChild l issue@IssueHierarchy{..} = hdr <> cnt <> rest-}
            {-where-}
                {-hdr = header l . text $ issueKey ihIssue ++ ": " ++ issueSummary ihIssue-}
                {-cnt = src <> dsc <> trc-}
                {-src = case issueSources ihIssue of -}
                            {-Nothing -> mempty -}
                            {-Just s -> (header (max 7 (l+1)) . text $ "Source Location in FS") <> (para . text $ s)-}
                {-dsc = case issueDescription ihIssue of -}
                            {-Nothing -> mempty -}
                            {-Just s -> (header (max 7 (l+1)) . text $ "Content from FS") <> (fromList $ parseDescription (max 8 (l+2)) $ filter (/= '\r') $ s)-}
                {-trc = case sortBy (compare `on` issueKeyToInt . issueKey) . map fromJust . filter isJust . map (flip M.lookup sssMap) . map getLinkId . filter matchSrcLink . issueLinks $ ihIssue of-}
                    {-[] -> mempty-}
                    {-ls -> (header (max 7 (l+1)) . text $ "Traceability FS to SSS") <> (simpleTable [para . text $ "Issue Key", para . text $ "Issue Summary"] . map keySummary $ ls)-}
                {-keySummary i = [para . text . issueKey $ i, para . text . issueSummary $ i]-}
                {-rest = foldl mappend mempty .  map (expndChild (l+1)) $ ihChildren -}

noTrace :: [String] -> Maybe Blocks
noTrace ss = para . text . getTxt <$> (find fm . sortBy fo $ ss)
    where
    noTraceText = [("LxReqNa", "N.A.")
                  ,("LxReqImplicit", "N.A. Implicit")
                  ,("LxReqBaseline", "N.A. Baseline")
                  ,("LxReqBackgrounSquadron", "N.A. Squadron background")
                  ,("LxReqBackgrounTmh", "N.A. THM background")
                  ]
    noTraceOrd = (map fst . reverse $ noTraceText) `zip` ([1..]::[Int])
    maxOrd = length noTraceOrd
    fo = compare `on` fromMaybe maxOrd . flip lookup noTraceOrd
    fm = isJust . flip lookup noTraceOrd
    getTxt = fromMaybe " " . flip lookup noTraceText



hierarchyToDoc :: [IssueHierarchy] -> [IssueHierarchy] -> [Block]
hierarchyToDoc fs sss = toList . foldl mappend mempty . map (expndChild 1) $ fs
    where
        sssMap :: M.Map Int Issue
        sssMap = M.fromList . concatMap ext $ sss
            where
                ext :: IssueHierarchy -> [(Int, Issue)]
                ext ih = ((issueId . ihIssue $ ih, ihIssue ih) : (concatMap ext . ihChildren $ ih))
        expndChild :: Int -> IssueHierarchy -> Blocks
        expndChild l issue@IssueHierarchyRoot{} = error "not expecting an IssueHierarchyRoot"
        expndChild l issue@IssueHierarchy{..} = hdr <> cnt <> rest
            where
                hdr = header l . text $ issueKey ihIssue ++ ": " ++ issueSummary ihIssue
                cnt = table mempty [(AlignLeft, 0.1), (AlignLeft, 0.6), (AlignLeft, 0.5)] [para . text $ "Source", para . text $ "Content", para . text $ "Traceability"] [[ src, dsc, fromMaybe trc . noTrace . issueLabels $ ihIssue]]
                -- cnt = simpleTable [src, dsc, trc] [ ]
                src = case issueSources ihIssue of 
                            Nothing -> para . text $ "None"
                            Just s -> (para . text $ s)
                dsc = case issueDescription ihIssue of 
                            Nothing -> para . text $ "None"
                            Just s ->  (fromList $ parseDescription (max 8 (l+2)) . filter (/= '\r') $ s)
                trc = case sortBy (compare `on` issueKeyToInt . issueKey) . map fromJust . filter isJust . map (flip M.lookup sssMap) . map getLinkId . filter matchSrcLink . issueLinks $ ihIssue of
                    [] -> para . text $ "None"
                    ls ->  foldl mappend mempty . map keySummary $ ls
                keySummary i = para $ (text . issueKey $ i) <> (text ": ") <> (text . issueSummary $ i)
                rest = foldl mappend mempty .  map (expndChild (l+1)) $ ihChildren 

trm :: [IssueHierarchy] -> [IssueHierarchy] -> CSV
trm fs sss =  concatMap (expndChild 1) $ fs
    where
        sssMap :: M.Map Int Issue
        sssMap = M.fromList . concatMap ext $ sss
            where
                ext :: IssueHierarchy -> [(Int, Issue)]
                ext ih = ((issueId . ihIssue $ ih, ihIssue ih) : (concatMap ext . ihChildren $ ih))
        expndChild :: Int -> IssueHierarchy -> [Record]
        expndChild l issue@IssueHierarchyRoot{} = error "not expecting an IssueHierarchyRoot"
        expndChild l issue@IssueHierarchy{..} = case sortBy (compare `on` issueKeyToInt .  issueKey) . map fromJust . filter isJust . map (flip M.lookup sssMap) . map getLinkId . filter matchSrcLink . issueLinks $ ihIssue of
                    [] -> rest
                    ls -> (map row ls) ++ rest
            where
                row i = [issueKey ihIssue, issueSummary ihIssue, fromMaybe " " . issueSources $ ihIssue , issueKey i, issueSummary i]
                rest = concatMap (expndChild (l+1)) $ ihChildren


trmFsToSss :: [IssueHierarchy] -> [IssueHierarchy] -> CSV
trmFsToSss fs sss = concatMap (sortBy cmpC4) . groupBy grpC1 . sortBy cmpC1 $ trm fs sss
    where
        grpC1 = (==) `on` issueKeyToInt . head
        cmpC1 = compare `on` issueKeyToInt . head
        cmpC4 = compare `on` issueKeyToInt . head . drop 3

trmSssToFs :: [IssueHierarchy] -> [IssueHierarchy] -> CSV
trmSssToFs fs sss = concatMap (sortBy cmpC3) . groupBy grpC1 . sortBy cmpC1 . map f $ trm fs sss
    where
        f [fsKey, fsSum, fsSrc, ssKey, ssSum] = [ssKey, ssSum, fsKey, fsSum, fsSrc]
        grpC1 = (==) `on` issueKeyToInt . head
        cmpC1 = compare `on` issueKeyToInt . head
        cmpC3 = compare `on` issueKeyToInt . head . drop 2

main :: IO ()
main = do
    let opts = optionsDefault {optDocxFile = "../Output/FsTrace.docx", optHierarchySssFile = Just "../Output/LynxSssIssueHierarchy.txt", optHierarchyFile = "../Output/LynxFsHierarchy.txt"}
    genFsDoc opts
