{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}
-- RUN_GHC_COMMAND_ARGS = --fetch-only --url=http://srv1.za.5dt.com:8090 --sid=101 --hierarchy-file=../bin/LynxSssIssueHierarchy.txt

import System.Exit(exitSuccess)
import Text.PrettyPrint.GenericPretty as GP
-- local files
import JiraStructureToIssueHierarchy
import IssueHierarchyToDocx
import ProgramOptions
 
main :: IO Int
main = do
    opts <- processCmdArgs
    let optsText = "\nOptions are=\n" ++ GP.pretty (opts{optPwd = Just ""})
    case optOperation opts of
        FetchOnly -> putStrLn ("Fetching issue hierarchy" ++ optsText)  >> fetchHierarchy opts
        GenDocOnly -> putStrLn ("Generating document" ++ optsText) >> genDoc opts
    exitSuccess



