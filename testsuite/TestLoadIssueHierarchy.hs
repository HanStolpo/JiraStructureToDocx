-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Char
import Data.Typeable
import Data.Data
import Control.Monad.Error
import GHC.Generics
import Control.Exception
import Control.Applicative
import Debug.Trace
import Text.PrettyPrint.GenericPretty as GP
import IssueHierarchy
import System.Directory

 
main = do
    cd <- getCurrentDirectory
    let ph = cd ++ "/IssueHierarchy.txt"
    s <- readFile ph
    let hierarchy = read s :: IssueHierarchy
    GP.pp hierarchy
