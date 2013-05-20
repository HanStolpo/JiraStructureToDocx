{-# LANGUAGE DeriveDataTypeable, RecordWildCards, DeriveGeneric #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite
-- RUN_GHC_COMMAND_ARGS = --gen-doc-only --hierarchy-file=../bin/IssueHierarchy.txt

module ProgramOptions   ( Operation(..)
                        , Options(..)
                        , processCmdArgs
                        ) where
 
import System.Console.CmdArgs.Implicit
import System.Directory
import System.IO
import Control.Monad(unless)
import Safe
import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP

data Operation =  FetchOnly         --- Fetch the structure only and don't generate the document
                | GenDocOnly        --- Generate the document only
                deriving (Show, Data, Typeable, Generic)
-- Make Operation pretty printable
instance Out Operation

data Options = Options  { optOperation      :: Operation
                        , optUsr            :: Maybe String
                        , optPwd            :: Maybe String
                        , optBaseUrl        :: Maybe String
                        , optStructureId    :: Maybe Int
                        , optHierarchyFile  :: String
                        , optDocxFile       :: String
                        } deriving (Show, Data, Typeable, Generic)
-- Make Options pretty printable
instance Out Options

options :: Options
options = Options { optOperation        = enum  [ FetchOnly     &= explicit &= name "fetch-only"    &= help "Only fetch the structure hierarchy from JIRA"
                                                , GenDocOnly    &= explicit &= name "gen-doc-only"  &= help "Only generate the document given a structure hierarchy previously fetched from JIRA"]
                  , optUsr              = def  &= explicit &= name "usr"            &= help "user name"
                  , optPwd              = def  &= explicit &= name "pwd"            &= help "password"
                  , optBaseUrl          = def  &= explicit &= name "url"            &= help "the base URL to the JIRA instance"
                  , optStructureId      = def  &= explicit &= name "sid"            &= help "the ID of the JIRA structure"
                  , optHierarchyFile    = defH &= explicit &= name "hierarchy-file" &= help "the name of the hierarchy file that will be used" &= opt defH
                  , optDocxFile         = defD &= explicit &= name "document-file"  &= help "the name of the document file that will be generated" &= opt defD
                  } &= program "JiraStructureToDocX"
            where
                defH = "StructureHierarchy.txt"
                defD = "Structure.docx"

processCmdArgs :: IO Options
processCmdArgs =  cmdArgs options >>= validate

validate :: Options -> IO Options

validate opts@(Options {optOperation =  GenDocOnly, ..}) = do
    ph <- validateHierarchyFile optHierarchyFile
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

validateHierarchyFile :: String -> IO String
validateHierarchyFile ph =  do
    ph' <- canonicalizePath ph
    doesFileExist ph' >>= flip unless (fail $ "Hierarchy file does not exits :" ++ ph)
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
