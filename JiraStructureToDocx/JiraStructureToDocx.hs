-- {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}

import System.Exit(exitSuccess)
import Text.PrettyPrint.GenericPretty as GP
import GHC.IO.Encoding
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Control.Monad
import Data.Functor
import Data.Maybe
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BS
-- local files
import Jira.JiraStructureToIssueHierarchy
import Jira.IssueHierarchyToDocx
import Jira.IssueHierarchyToFsDocx
import Jira.ProgramOptions
import Jira.StdStrFetch
import Jira.StdStrGen
import Jira.BugReport
import Pandoc.Writer.HackedDocx
 
main :: IO Int
main = do
    opts <- processCmdArgs
    setLocaleEncoding utf8
    let optsText = "\nOptions are=\n" ++ GP.pretty (opts{optPwd = Just ""})
    case optOperation opts of
        FetchOnly -> putStrLn ("Fetching issue hierarchy" ++ optsText)  >> fetchHierarchy opts
        GenDocOnly -> putStrLn ("Generating document" ++ optsText) >> genDoc opts
        FetchStr -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStrSrc opts
        FetchStd -> putStrLn ("Fetching STR" ++ optsText)  >> fetchStdSrc opts
        GenDocStr -> putStrLn ("Generating STR" ++ optsText)  >> genStr opts
        GenDocStd -> putStrLn ("Generating STD" ++ optsText)  >> genStd opts
        GenFs -> putStrLn ("Generating FS" ++ optsText)  >> genFsDoc opts
        GenBugReport -> putStrLn ("Generating a bug report " ++ optsText) >> generateBugReport opts
        GenDocX -> putStrLn ("Reading in a Pandoc Markdown document and generating a Docx document " ++ optsText) >> genDocx opts
    exitSuccess


genDocx :: Options -> IO ()
genDocx opts = do
    let ropt = def{readerExtensions = pandocExtensions, readerParseRaw = True}
        output = fromJust . optFileOutput $ opts
        input = fromJust . optFileInput $ opts
    (p', ws) <- readMarkdownWithWarnings ropt <$> readFile input
    unless (null ws) (putStrLn "Following warnings were detected while reading the document" >> mapM_ putStrLn ws)
    absImgPathPfx <- canonicalizePath  . takeDirectory $ input
    p <- makePandocImagesAbsolute absImgPathPfx p'
    putStrLn absImgPathPfx 
    let wopt = case optPandocDataDir opts of
                    Just ph -> def {writerUserDataDir = Just ph}
                    Nothing -> def {writerUserDataDir = takeDirectory <$> optFileInput opts}
    writeDocxCustom wopt p >>= BS.writeFile output
    return ()


makePandocImagesAbsolute :: String -> Pandoc -> IO Pandoc
makePandocImagesAbsolute pathPfx = walkM (modeImageLink pathPfx)


modeImageLink :: String -> Inline -> IO Inline
modeImageLink pathPfx img@(Image altTxt (relPath, title)) 
        | isValid relPath && isRelative relPath = do
            fullPath <- canonicalizePath (pathPfx </> relPath)
            ok  <- doesFileExist fullPath
            return (if ok then Image altTxt (fullPath, title) else img)
        | otherwise = return img
modeImageLink _ a = return a
