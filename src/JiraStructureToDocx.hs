{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}
import Data.List
import System.Directory
import System.Environment
import System.Exit(exitSuccess)
-- local files
import JiraStructureToIssueHierarchy
import IssueHierarchyToDocx
import ProgramOptions
 
main :: IO Int
main = do
    opt <- processCmdArgs
    case optOperation opt of
        FetchOnly -> fetchHierarchy opt
        GenDocOnly -> genDoc opt
    exitSuccess



