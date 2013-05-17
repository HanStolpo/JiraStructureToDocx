{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite
-- RUN_GHC_COMMAND_ARGS = --fetch-only --url=http://srv1.za.5dt.com:8090/

module ProgramOptions   ( Operation(..)
                        , Options(..)
                        , processCmdArgs
                        ) where
 
import System.Console.CmdArgs.Implicit
import System.Directory
import System.FilePath
import System.IO
import Data.Data

data Operation =  FetchOnly         --- Fetch the structure only and don't generate the document
                | GenDocOnly        --- Generate the document only
                deriving (Show, Data, Typeable)

data Options = Options  { optOperation      :: Operation
                        , optUsr            :: Maybe String
                        , optPwd            :: Maybe String
                        , optBaseUrl        :: Maybe String
                        , optHierarchyFile  :: String
                        , optDocxFile       :: String
                        } deriving (Show, Data, Typeable)

defH = "StructureHierarchy.txt"
defD = "Structure.docx"
options = Options { optOperation        = enum  [ FetchOnly     &= explicit &= name "fetch-only"    &= help "Only fetch the structure hierarchy from JIRA"
                                                , GenDocOnly    &= explicit &= name "gen-doc-only"  &= help "Only generate the document given a structure hierarchy previously fetched from JIRA"]
                  , optUsr              = def  &= explicit &= name "usr"            &= help "user name"
                  , optPwd              = def  &= explicit &= name "pwd"            &= help "password"
                  , optBaseUrl          = def  &= explicit &= name "url"            &= help "the base URL to the JIRA instance"
                  , optHierarchyFile    = defH &= explicit &= name "hierarchy-file" &= help "the name of the hierarchy file that will be used" &= opt defH
                  , optDocxFile         = defD &= explicit &= name "document-file"  &= help "the name of the document file that will be generated" &= opt defD
                  } &= program "JiraStructureToDocX"

processCmdArgs :: IO Options
processCmdArgs =  cmdArgs options >>= validate

validate :: Options -> IO Options

validate opt@(Options {optOperation =  GenDocOnly, ..}) = do
    ph <- validateHierarchyFile optHierarchyFile
    pd <- canonicalizePath optDocxFile
    return opt {optHierarchyFile = ph, optDocxFile = pd}
    
validate opt@(Options {optOperation =  FetchOnly, ..}) = do
    url <- validateUrl optBaseUrl
    usr <- validateUrs optUsr
    pwd <- validatePwd optPwd
    return opt {optBaseUrl = Just url, optUsr = Just usr, optPwd = Just pwd}

validateHierarchyFile :: String -> IO String
validateHierarchyFile ph =  canonicalizePath ph >>= doesFileExist >>= (\b -> if b then return ph else fail $ "Hierarchy file does not exits :" ++ ph)

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


main = do 
    o <- processCmdArgs
    print o
    return ()
