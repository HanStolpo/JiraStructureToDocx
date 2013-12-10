{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables#-}

module IssueHierarchyToDocx (genDoc) where

import Data.Maybe
import Control.Monad.Error
import System.Directory
import Text.Pandoc
import Text.Pandoc.Builder hiding ((<>))
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as BS8
import qualified Data.Aeson as AS
import qualified Data.Yaml as YAML
import qualified Data.Map as M
import Text.Blaze.Renderer.String
import System.FilePath
import Safe
-- Local imports
import IssueHierarchy
import JiraTypes
import DescriptionParser
import ProgramOptions
import DocxCustom
 
genDoc :: Options -> IO ()
genDoc opts = do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optDocxFile $ opts
    cd <- getCurrentDirectory
    putStrLn "Reading issue hierarchy"
    Just hierarchy :: Maybe IssueHierarchy <- liftM YAML.decode $ B.readFile (optHierarchyFile opts)
    putStrLn "Generating pandoc"
    let pandoc = Pandoc docMeta $ concatMap (hierarchyToDoc idIssue cntPfx) (ihChildren hierarchy)
        bfn = dropExtension . optDocxFile $ opts
        idIssue = case optUseLinkedIssueAsId opts of
                        Nothing     -> issueKey
                        Just lnm    -> idFromLink lnm
        cntPfx = case optUseLinkedIssueAsId opts of
                        Nothing     -> const []
                        Just _      -> realIssueKey
    putStrLn "Generating native"
    writeFile (bfn ++ "_Native.txt") $ writeNative (docOptions cd) pandoc
    putStrLn "Generating markdown"
    BS.writeFile (bfn ++ "_MarkDown.txt") $ BS8.fromString $ writeMarkdown def pandoc
    putStrLn "Generating html"
    BS.writeFile (bfn ++ "_HTML.html") $ BS8.fromString (renderMarkup $ writeHtml (docOptions cd) pandoc)
    putStrLn "Generating docx"
    d <- writeDocxCustom (docOptions cd) pandoc
    BS.writeFile (optDocxFile opts) d
    return ()

docOptions ::  FilePath -> WriterOptions
docOptions cd = def {writerUserDataDir = Just cd
                    }
docMeta ::  Meta
docMeta = Meta M.empty


hierarchyToDoc :: (Issue -> String) -> (Issue -> [Block]) -> IssueHierarchy -> [Block]
hierarchyToDoc idIssue cntPfx = expndChild 1
    where
        expndChild :: Int -> IssueHierarchy -> [Block]
        expndChild l issue = (hdr : cnt) ++ rest 
            where
                hdr = Header l nullAttr [Str $ ihKey issue, Str ":", Space, Str $ ihSummary issue] :: Block
                cnt = ihCntPfx issue ++ (parseDescription l $ filter (/= '\r') $ ihDescription issue :: [Block])
                rest = concatMap (expndChild (l+1)) $ ihChildren issue :: [Block]
                ihKey (IssueHierarchyRoot _) = ""
                ihKey h = idIssue . ihIssue $ h
                ihSummary (IssueHierarchyRoot _) = ""
                ihSummary h = issueSummary . ihIssue $ h
                ihDescription (IssueHierarchyRoot _) = ""
                ihDescription h = fromMaybe "" . issueDescription . ihIssue $ h
                ihCntPfx (IssueHierarchyRoot _) = []
                ihCntPfx h = cntPfx . ihIssue $ h


idFromLink :: String -> Issue -> String
idFromLink lnm i = headNote n . map issueLinkKey . filter f . issueLinks $ i
    where
        f = (lnm==) . issueLinkDescription
        n = "hierarchyToDoc - no issue link of description " ++ show lnm ++ "present in " ++ issueKey i

realIssueKey :: Issue -> [Block]
realIssueKey i = toList . para . strong . superscript . str $ "JIRA ID : " ++ issueKey i
