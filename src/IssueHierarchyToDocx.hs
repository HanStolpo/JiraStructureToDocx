-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Typeable
import Data.Data
import qualified Data.Char as C
import Control.Monad.Error
import GHC.Generics
import Control.Exception
import Debug.Trace
import Text.PrettyPrint.GenericPretty as GP
import System.Directory
import Text.Pandoc
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Binary.UTF8.Generic as BS8
import System.Directory
import Text.Blaze.Renderer.String
-- Local imports
import IssueHierarchy
import DescriptionParser
 
main = do
    cd <- getCurrentDirectory
    hierarchy :: IssueHierarchy <- liftM read $ readFile (cd ++ "/IssueHierarchy.txt") 
    let pandoc = Pandoc docMeta $ (concat $ map hierarchyToDoc $ ihChildren hierarchy)
    writeFile (cd ++ "/IssueHierarchyDoc_Native.txt") $ writeNative (docOptions cd) pandoc
    BS.writeFile (cd ++ "/IssueHierarchyDoc_MarkDown.txt") $ BS8.fromString $ writeMarkdown def pandoc
    BS.writeFile (cd ++ "/IssueHierarchyDoc_HTML.html") $ BS8.fromString $ (renderMarkup $ writeHtml (docOptions cd) pandoc)
    doc <- writeDocx (docOptions cd) pandoc
    BS.writeFile (cd ++ "/IssueHierarchy.docx") doc 
    return ()

docOptions cd = def {writerUserDataDir = Just cd
                    }
docMeta = Meta {docTitle = [], docAuthors = [], docDate = []}


hierarchyToDoc :: IssueHierarchy -> [Block]
hierarchyToDoc ih = expndChild 1 ih 
    where
        expndChild :: Int -> IssueHierarchy -> [Block]
        expndChild l issue = (hdr : cnt) ++ rest 
            where
                hdr = Header l nullAttr [Str $ ihKey issue, Str ":", Space, Str $ ihSummary issue] :: Block
                cnt = parseDescription l $ filter (\c -> c /= '\r') $ ihDescription issue :: [Block]
                rest = concat . (map (expndChild (l+1))) $ ihChildren issue :: [Block]

