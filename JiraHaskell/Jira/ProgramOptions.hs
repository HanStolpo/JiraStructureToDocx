{-# LANGUAGE DeriveDataTypeable, RecordWildCards, DeriveGeneric #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite
-- RUN_GHC_COMMAND_ARGS = --gen-doc-only --hierarchy-file=../bin/IssueHierarchy.txt

module Jira.ProgramOptions   ( Operation(..)
                        , Options(..)
                        , optionsDefault
                        , processCmdArgs
                        , validatePwd
                        , validateUrs
                        ) where
 
import System.Console.CmdArgs.Implicit
import System.Directory
import System.IO
import Control.Monad
import Safe
import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import Data.Maybe

data Operation =  FetchOnly         --- Fetch the structure only and don't generate the document
                | GenDocOnly        --- Generate the document only
                | FetchStr          --- Fetch the STR
                | FetchStd          --- Fethc the STD
                | GenDocStr         --- Generate the STR document
                | GenDocStd         --- Generate the STD document
                | GenFs             --- Generate the FS traceability
                deriving (Show, Data, Typeable, Generic)
-- Make Operation pretty printable
instance Out Operation

data Options = Options  { optOperation          :: Operation
                        , optUsr                :: Maybe String
                        , optPwd                :: Maybe String
                        , optBaseUrl            :: Maybe String
                        , optStructureId        :: Maybe Int
                        , optHierarchyFile      :: String
                        , optHierarchySssFile   :: Maybe String
                        , optDocxFile           :: String
                        , optProjectId          :: Maybe Int
                        , optCycleName          :: Maybe String
                        , optStrStdFile         :: String
                        , optQueryString        :: Maybe String
                        , optStrStdIssuesFile   :: String
                        , optUseLinkedIssueAsId :: Maybe String
                        } deriving (Show, Data, Typeable, Generic)
-- Make Options pretty printable
instance Out Options

optionsDefault :: Options
optionsDefault = Options { optOperation             = FetchOnly
                         , optUsr                   = Nothing
                         , optPwd                   = Nothing
                         , optBaseUrl               = Nothing
                         , optStructureId           = Nothing
                         , optHierarchyFile         = "StructureHierarchy.yaml"
                         , optHierarchySssFile      = Nothing
                         , optDocxFile              = "Structure.docx"
                         , optProjectId             = Nothing
                         , optCycleName             = Nothing
                         , optStrStdFile            = "StrStd.txt"
                         , optQueryString           = Nothing
                         , optStrStdIssuesFile      = "StrStdIssues.txt"
                         , optUseLinkedIssueAsId    = Nothing
                         }

options :: Options
options = Options { optOperation        = enum  [ FetchOnly     &= explicit &= name "fetch-only"    &= help "Only fetch the structure hierarchy from JIRA"
                                                , GenDocOnly    &= explicit &= name "gen-doc-only"  &= help "Only generate the document given a structure hierarchy previously fetched from JIRA"
                                                , FetchStr      &= explicit &= name "fetch-str"     &= help "Fetch the data related to the STR"
                                                , FetchStd      &= explicit &= name "fetch-std"     &= help "Fetch the data related to the STD"
                                                , GenDocStr     &= explicit &= name "gen-doc-str"   &= help "Generate the STR given the data previously fetched"
                                                , GenDocStd     &= explicit &= name "gen-doc-std"   &= help "Generate the STD given the data previously fetched"
                                                , GenFs         &= explicit &= name "gen-doc-fs"    &= help "Generate the FS traceability to the SSS"
                                                ]
                  , optUsr                  = def  &= explicit &= name "usr"                        &= help "user name"
                  , optPwd                  = def  &= explicit &= name "pwd"                        &= help "password"
                  , optBaseUrl              = def  &= explicit &= name "url"                        &= help "the base URL to the JIRA instance"
                  , optStructureId          = def  &= explicit &= name "sid"                        &= help "the ID of the JIRA structure"
                  , optHierarchyFile        = defH &= explicit &= name "hierarchy-file"             &= help "the name of the hierarchy file that will be used" &= opt defH
                  , optHierarchySssFile     = def  &= explicit &= name "sss-hierarchy-file"         &= help "when generating the FS traceability then the SSS hierarchy needs to be supplied too"
                  , optDocxFile             = defD &= explicit &= name "document-file"              &= help "the name of the document file that will be generated" &= opt defD
                  , optProjectId            = def  &= explicit &= name "projid"                     &= help "the ID of the JIRA project"
                  , optCycleName            = def  &= explicit &= name "cycle-name"                 &= help "the name of the Zephyr test cycle"
                  , optStrStdFile           = defS &= explicit &= name "str-std-file"               &= help "the name of the file contains or will contain the STR STD data" &= opt defS 
                  , optQueryString          = def  &= explicit &= name "query"                      &= help "optional jira query to filter document structure generation"
                  , optStrStdIssuesFile     = defS &= explicit &= name "str-std-issues-file"        &= help "the set of issue linked to test that are being tested (generated when STD is generated used by STR)" &= opt defS 
                  , optUseLinkedIssueAsId   = def  &= explicit &= name "use-linked-issue-as-id"     &= help "use ID of the issue linked through the named relationship for the issue ID instead of its actual ID" 
                  } &= program "JiraStructureToDocX"
            where
                defH = optHierarchyFile optionsDefault
                defD = optDocxFile optionsDefault
                defS = optStrStdFile optionsDefault

processCmdArgs :: IO Options
processCmdArgs =  cmdArgs options >>= validate

validate :: Options -> IO Options

validate opts@(Options {optOperation =  GenDocOnly, ..}) = do
    ph <- validateFile optHierarchyFile
    pd <- canonicalizePath optDocxFile
    return opts {optHierarchyFile = ph, optDocxFile = pd}
    
validate opts@(Options {optOperation =  FetchOnly, ..}) = do
    url <- validateUrl optBaseUrl
    usr <- validateUrs optUsr
    pwd <- validatePwd optPwd
    ph <- canonicalizePath optHierarchyFile
    return opts { optBaseUrl = Just url
                , optUsr = Just usr
                , optPwd = Just pwd
                , optHierarchyFile = ph
                , optStructureId = Just $ fromJustNote "Structure Id is required" optStructureId
                }

validate opts@(Options {optOperation =  FetchStr}) = _validateFetchStrStd True opts

validate opts@(Options {optOperation =  FetchStd}) = _validateFetchStrStd False opts

validate opts@(Options {optOperation =  GenDocStr}) = _validateGenStrStd opts

validate opts@(Options {optOperation =  GenDocStd}) = _validateGenStrStd opts

validate opts@(Options {optOperation =  GenFs, ..}) = do
    ph <- validateFile optHierarchyFile
    _ <- case optHierarchySssFile of
            Just h -> validateFile h
            Nothing -> fail "For FS expecting a hierarchy file contianing the SSS"
    pd <- canonicalizePath optDocxFile
    return opts {optHierarchyFile = ph, optDocxFile = pd}

_validateFetchStrStd :: Bool -> Options -> IO Options
_validateFetchStrStd isStr opts@(Options{..}) = do
    url <- validateUrl optBaseUrl
    usr <- validateUrs optUsr
    pwd <- validatePwd optPwd
    p <- canonicalizePath optStrStdFile
    p2 <- if isStr 
                then validateFile optStrStdIssuesFile
                else canonicalizePath optStrStdIssuesFile
    when (isNothing optCycleName) (fail "expecting cycle name")
    when (isNothing optProjectId) (fail "expecting a project Id")
    return opts { optBaseUrl = Just url
                , optUsr = Just usr
                , optPwd = Just pwd
                , optStrStdFile = p
                , optStrStdIssuesFile = p2
                }

_validateGenStrStd :: Options -> IO Options
_validateGenStrStd opts@(Options{..}) = do
    pd <- canonicalizePath optDocxFile
    p <- validateFile optStrStdFile
    p2 <- validateFile optStrStdIssuesFile
    ph <- validateFile optHierarchyFile
    return opts {optStrStdFile = p, optDocxFile = pd, optHierarchyFile = ph, optStrStdIssuesFile = p2}

validateFile :: String -> IO String
validateFile ph =  do
    ph' <- canonicalizePath ph
    doesFileExist ph' >>= flip unless (fail $ "File does not exits :" ++ ph)
    return ph'

validateUrl :: Maybe String -> IO String
validateUrl (Just s) = return s
validateUrl Nothing = fail "Expecting a base URL to be specified"

validateUrs :: Maybe String -> IO String
validateUrs (Just s) = return s
validateUrs Nothing =  putStrLn "Please enter a user name:" >> getLine

validatePwd :: Maybe String -> IO String
validatePwd (Just s) = return s
validatePwd Nothing =  do 
    echo <- hGetEcho stdin
    hSetEcho stdin False 
    s <- putStrLn "Please enter a password:" >> getLine
    hSetEcho stdin echo 
    return s


{-main = do -}
    {-o <- processCmdArgs-}
    {-print o-}
    {-return ()-}
