{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables#-}

module IssueHierarchyToDocx (genDoc) where

import Data.Maybe
import Control.Monad.Error
import System.Directory
import Text.Pandoc
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as BS8
import qualified Data.Aeson as AS
import qualified Data.Yaml as YAML
import Text.Blaze.Renderer.String
import System.FilePath
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
    let pandoc = Pandoc docMeta $ concatMap hierarchyToDoc (ihChildren hierarchy)
    let bfn = dropExtension . optDocxFile $ opts
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
docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}


hierarchyToDoc :: IssueHierarchy -> [Block]
hierarchyToDoc = expndChild 1
    where
        expndChild :: Int -> IssueHierarchy -> [Block]
        expndChild l issue = (hdr : cnt) ++ rest 
            where
                hdr = Header l nullAttr [Str $ ihKey issue, Str ":", Space, Str $ ihSummary issue] :: Block
                cnt = parseDescription l $ filter (/= '\r') $ ihDescription issue :: [Block]
                rest = concatMap (expndChild (l+1)) $ ihChildren issue :: [Block]
                ihKey (IssueHierarchyRoot _) = ""
                ihKey h = issueKey . ihIssue $ h
                ihSummary (IssueHierarchyRoot _) = ""
                ihSummary h = issueSummary . ihIssue $ h
                ihDescription (IssueHierarchyRoot _) = ""
                ihDescription h = fromMaybe "" . issueDescription . ihIssue $ h

