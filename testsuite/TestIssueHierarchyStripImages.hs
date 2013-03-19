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
import System.Directory
-- Local imports
import IssueHierarchy
import DescriptionParser
import ImageStripper
import qualified Data.Map as M
 
main = do
    cd <- getCurrentDirectory
    hierarchy :: IssueHierarchy <- liftM read $ readFile (cd ++ "/IssueHierarchy.txt") 
    let images = extractImages hierarchy
   -- forM (M.keys images) $ putStrLn . show
    let hierarchy' = replaceImagesUri images hierarchy
    writeFile (cd ++ "/IssueHierarchyReplaced.txt") $ GP.pretty hierarchy'
    return ()
