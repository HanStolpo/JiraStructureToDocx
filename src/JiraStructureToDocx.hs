{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}
-- RUN_GHC_COMMAND_ARGS = --gen-doc-fs --document-file=../Output/FsTrace.docx --sss-hierarchy-file=../Output/LynxSssIssueHierarchy.yaml --hierarchy-file=../Output/LynxFsHierarchy.txt

import System.Exit(exitSuccess)
import Text.PrettyPrint.GenericPretty as GP
-- local files
import JiraStructureToIssueHierarchy
import IssueHierarchyToDocx
import IssueHierarchyToFsDocx
import ProgramOptions
import StdStrFetch
import StdStrGen
 
main :: IO Int
main = do
    opts <- processCmdArgs
    let optsText = "\nOptions are=\n" ++ GP.pretty (opts{optPwd = Just ""})
    case optOperation opts of
        FetchOnly -> putStrLn ("Fetching issue hierarchy" ++ optsText)  >> fetchHierarchy opts
        GenDocOnly -> putStrLn ("Generating document" ++ optsText) >> genDoc opts
        FetchStr -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStrSrc opts
        FetchStd -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStdSrc opts
        GenDocStr -> putStrLn ("Generating STR" ++ optsText)  >> genStr opts
        GenDocStd -> putStrLn ("Generating STD" ++ optsText)  >> genStd opts
        GenFs -> putStrLn ("Generating FS" ++ optsText)  >> genFsDoc opts
    exitSuccess


