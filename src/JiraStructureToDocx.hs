{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}
-- RUN_GHC_COMMAND_ARGS = --fetch-str --url=http://srv1.za.5dt.com:8090 --cycle-name=Sprint\ 2\ Demonstrations --str-std-file=../Outputs/str.txt
-- xUN_GHC_COMMAND_ARGS = --help

import System.Exit(exitSuccess)
import Text.PrettyPrint.GenericPretty as GP
-- local files
import JiraStructureToIssueHierarchy
import IssueHierarchyToDocx
import ProgramOptions
import StdStrFetch
 
main :: IO Int
main = do
    opts <- processCmdArgs
    let optsText = "\nOptions are=\n" ++ GP.pretty (opts{optPwd = Just ""})
    case optOperation opts of
        FetchOnly -> putStrLn ("Fetching issue hierarchy" ++ optsText)  >> fetchHierarchy opts
        GenDocOnly -> putStrLn ("Generating document" ++ optsText) >> genDoc opts
        FetchStr -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStrSrc opts
        FetchStd -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStdSrc opts
        GenDocStr -> fail "not implemented yet"
        GenDocStd -> fail "not implemented yet"
    exitSuccess


