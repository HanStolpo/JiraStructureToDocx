{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables, RecordWildCards#-}

module IssueHierarchyToDocx (genDoc, hierarchyToDoc, idFromLink) where

import Data.Maybe
import Control.Monad.Error
import System.Directory
import Text.Pandoc
import Text.Pandoc.Builder hiding ((<>))
import Text.Parsec.Char
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Combinator
import Control.Applicative hiding (many)
--import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as BS8
--import qualified Data.Aeson as AS
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
    hierarchy <- either (\e -> error $ "failure reading hierarchy file [" ++ optHierarchyFile opts ++ "] reason being : " ++ e) 
                        id . YAML.decodeEither <$> B.readFile (optHierarchyFile opts)
    putStrLn "Generating pandoc"
    let pandoc = Pandoc docMeta $ concatMap (hierarchyToDoc idIssue cntPfx modDesc) (ihChildren hierarchy)
        bfn = dropExtension . optDocxFile $ opts
        (idIssue, cntPfx, modDesc) = case optUseLinkedIssueAsId opts of
                        Nothing     -> (issueKey, const [], id)
                        Just lnm    -> (idFromLink lnm, realIssueKey, replaceLinks lnm hierarchy)
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


hierarchyToDoc :: (Issue -> String) -> (Issue -> [Block]) -> (String -> String) -> IssueHierarchy -> [Block]
hierarchyToDoc idIssue cntPfx modDesc = expndChild 1
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
                ihDescription h = modDesc . fromMaybe "" . issueDescription . ihIssue $ h
                ihCntPfx (IssueHierarchyRoot _) = []
                ihCntPfx h = cntPfx . ihIssue $ h


idFromLink :: String -> Issue -> String
idFromLink lnm i = headNote n . map issueLinkKey . filter f . issueLinks $ i
    where
        f = (lnm==) . issueLinkDescription
        n = "hierarchyToDoc - no issue link of description " ++ show lnm ++ "present in " ++ issueKey i

realIssueKey :: Issue -> [Block]
realIssueKey i = toList . para . strong . superscript . str $ "JIRA ID : " ++ issueKey i


linkMap :: String -> IssueHierarchy -> M.Map String String
linkMap lnm (IssueHierarchyRoot cs) = M.unions . map (linkMap lnm) $ cs
linkMap lnm (IssueHierarchy i@(Issue {..}) cs) = M.insert issueKey (idFromLink lnm i) . M.unions . map (linkMap lnm) $ cs

issueKeyPfx :: M.Map String String -> String
issueKeyPfx m = foldl chk (ext . head $ ks) ks
    where
        ks = M.keys m
        ext s = either (error . ("ext "++). show) id $ parse p s s
        p = many1 $ choice [letter, char '-']
        chk s n | s == ext n = s
                | otherwise = error "inconsistent issue key in remapping [" ++ s ++ "/=" ++ n ++ "]"


replaceLinks :: String -> IssueHierarchy -> (String -> String)
replaceLinks lnm hierarchy = f
    where
        m       = linkMap lnm hierarchy
        ikPfx   = issueKeyPfx m
        f       = prsRepl m ikPfx

prsRepl :: M.Map String String -> String -> String -> String
prsRepl m ikPfx s = either (error . ("prsRepl "++) . show) concat (parse p s s)
    where
        p = many . choice $ [lnk, txt]
        txt = manyTill anyChar (lookAhead (void lnk <|> eof)) >>= \txt' -> if null txt' then fail "" else return txt'
        lnk = try $ fromJustNote "error renaming issue link" . (`M.lookup` m) <$> ((++) <$> string ikPfx <*> many1 digit)
