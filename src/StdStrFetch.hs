{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, DeriveGeneric, RankNTypes #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module StdStrFetch  (fetchStdSrc
                    ,fetchStrSrc
                    ,fetchCycleIdFromName
                    ,fetchCycleSchedules
                    ) where
import GHC.Exts

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 ()
import Data.ByteString.Char8 as B (pack)

import Data.Maybe
import Data.List
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Applicative
import Network.Socket(withSocketsDo)
import Control.Monad.IO.Class(liftIO)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(dropFileName)
import Text.PrettyPrint.GenericPretty as GP
import Data.Char(toLower)
-- local files
import ProgramOptions
import JiraTypes     as J
import ZephyrJson    as Z
import StrStdTypes
import ImageStripper
import JiraStructureToIssueHierarchy


type Query_ a b = forall m. (MonadBaseControl IO m, MonadResource m) =>
                         Options
                        -> Manager
                        -> a
                        -> m (Either String b)

_fromEither :: Monad m => Either String b -> m b
_fromEither = either fail return 

_makeReq :: (MonadBaseControl IO m, MonadResource m) => Options -> String -> Request m
_makeReq opts url'' = (applyBasicAuth usr pwd url) {responseTimeout = Just (30 * 1000000)}
        where
            usr = pack . fromJust . optUsr $ opts
            pwd = pack . fromJust . optPwd $ opts 
            url' = (++ url'') . fromJust . optBaseUrl $ opts 
            url = fromJust . parseUrl $ url'


fetchCycleIdFromName :: Query_ String Int 
fetchCycleIdFromName opts man n = do
    let pid = fromJust . optProjectId $ opts
        url =  "/rest/zephyr/latest/cycle/statusCount?pid=" ++ show pid ++ "&vid=-1&offset=0"
        req = _makeReq opts url
        findCycle rsp = liftM (find ((n==) . cycleName)) rsp >>= maybe (fail "Cycle not found") (return . cycleId) 
    findCycle . decodeProjectCyclesResponse . responseBody <$> httpLbs req man


fetchCycleSchedules :: Query_ Int [Schedule] 
fetchCycleSchedules opts man cid = Right <$> iter 0
    where 
        iter :: forall m. (MonadBaseControl IO m, MonadResource m) => Int -> m [Schedule]
        iter cnt = do
            let url =  "/rest/zephyr/latest/schedule?cycleId=" ++ show cid ++ "&action=expand&offset=" ++ show cnt ++ "&sorter=ID:DESC"
                req = _makeReq opts url
            (scs, t) <- _fromEither =<< decodeCycleResponse . responseBody <$> httpLbs req man
            if cnt < t 
                then (scs ++) <$> iter (length scs + cnt)
                else return scs
                

fetchTestStepInfos :: Query_ Int [TestStepInfo]
fetchTestStepInfos opts man iid = do
    let url =  "/rest/zephyr/latest/teststep/" ++ show iid 
        req = _makeReq opts url
    decodeTestStepsResponse . responseBody <$> httpLbs req man

fetchTestStepResults :: Query_ Int [TestStepResult]
fetchTestStepResults opts man sid = do
    let url =  "/rest/zephyr/latest/stepresult?scheduleId=" ++ show sid ++ "&expand=executionStatus"  
        req = _makeReq opts url
    decodeTestStepResultsResponse . responseBody <$> httpLbs req man

fetchTestIssueById :: Query_ Int Issue
fetchTestIssueById opts man' iid = do
    -- repeated use of same connection is resulting the following bug (InvalidStatusLine "" when reusing a connection #117) in HTTP Conduit
    man <- liftIO $ newManager def 
    let url =  "/rest/api/latest/issue/" ++ show iid
        req = _makeReq opts url
    decodeIssueResponse . responseBody <$> httpLbs req man


fetchStdTestSrc :: Query_ Schedule StdTestSrc
fetchStdTestSrc opts man sc = do
    let iid = Z.issueId sc
    issue <- _fromEither =<< fetchTestIssueById opts man iid
    let storyIds = _getTestsIssueIds issue
    stories <- forM storyIds (\i -> _fromEither =<< fetchTestIssueById opts man i)
    steps <- _fromEither =<< fetchTestStepInfos opts man iid
    return $ Right StdTestSrc{stdIssue = issue, stdStories = stories, stdSteps = steps}

_fetchStdSrc :: Query_ String StdSrc
_fetchStdSrc opts man cn = do
    cid <- _fromEither =<< fetchCycleIdFromName opts man cn 
    scs <- _fromEither =<< fetchCycleSchedules opts man cid
    Right . StdSrc <$> forM scs (_fromEither <=< fetchStdTestSrc opts man)

_extractOpenStdIssues :: [StdTestSrc] -> S.Set String
_extractOpenStdIssues ts = S.fromList . map J.issueKey . concatMap f $ ts
    where
        f = filter (("Closed"/=) . issueStatus) . stdStories

fetchStdSrc :: Options -> IO ()
fetchStdSrc opts = withSocketsDo $ runResourceT $ do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optStrStdFile $ opts
    manager <- liftIO $ newManager def
    std' <- _fromEither =<< _fetchStdSrc opts manager (fromJust . optCycleName $ opts)
    liftIO $ putStrLn "Localising images"
    let images = extractImagesFromTests (stdTests std')
    imagesLoc <- localizeImages manager (pack . fromJust . optUsr $ opts) (pack . fromJust . optPwd $ opts) (fromJust . optBaseUrl $ opts) (dropFileName . optStrStdFile $ opts) images
    let ts = replaceImagesUriInTests imagesLoc (stdTests std')
    liftIO $ putStrLn "Writing out test data"
    liftIO $ writeFile (optStrStdFile opts) $ GP.pretty std'{stdTests = ts}
    liftIO $ putStrLn "Writing out test issues"
    liftIO $ writeFile (optStrStdIssuesFile opts) $ GP.pretty (S.toList $ _extractOpenStdIssues ts)
    return ()

_getTestsIssueIds :: Issue -> [Int]
_getTestsIssueIds Issue{issueLinks = ls} = map getId . filter onlyTest $ ls
    where
        getId (Outward _ i _) = i
        getId (Inward _ i _) = i
        onlyTest (Outward d _ _) = ("tests" == ) . map toLower $ d
        onlyTest (Inward d _ _) = ("tests" == ) . map toLower $ d

fetchStrTestSrc :: Query_ Schedule StrTestSrc
fetchStrTestSrc opts man sc = do
    let iid = Z.issueId sc
        sid = scheduleId sc
    issue <- _fromEither =<< fetchTestIssueById opts man iid
    let storyIds = _getTestsIssueIds issue
    stories <- forM storyIds (\i -> _fromEither =<< fetchTestIssueById opts man i)
    stepInfos <- _fromEither =<< fetchTestStepInfos opts man iid
    stepResults <- _fromEither =<< fetchTestStepResults opts man sid
    {-let steps = filter chkTpl $ zip (sortWith stepInfoId stepInfos)(sortWith stepResInfoId stepResults)-}
        {-chkTpl (i,r) = stepInfoId i == stepResInfoId r-}
    let steps = gobble (sortWith stepInfoId stepInfos)(sortWith stepResInfoId stepResults)
        gobble [] _ = []
        gobble is [] = map defJoin is
        gobble (i:is) (r:rs) 
            | stepInfoId i == stepResInfoId r = (i,r) : gobble is rs
            | otherwise = defJoin i : gobble is (r:rs)
        defJoin i = (i, TestStepResult{stepResId = 0, stepResInfoId = stepInfoId i, stepResStatus = TestUnexecuted, stepResComment = ""})
    return $ Right StrTestSrc{strIssue = issue, strStories = stories, strResult = sc, strSteps = sortWith (stepInfoOrdId . fst) steps}

_fetchStrSrc :: Query_ String StrSrc
_fetchStrSrc opts man cn = do
    cid <- _fromEither =<< fetchCycleIdFromName opts man cn 
    scs <- _fromEither =<< fetchCycleSchedules opts man cid
    Right . StrSrc <$> forM scs (_fromEither <=< fetchStrTestSrc opts man)

fetchStrSrc :: Options -> IO ()
fetchStrSrc opts = withSocketsDo $ runResourceT $ do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optStrStdFile $ opts
    manager <- liftIO $ newManager def
    let cn = (fromJust . optCycleName $ opts)
    str' <- _fromEither =<< _fetchStrSrc opts manager cn
    liftIO $ putStrLn "Localising images"
    let images = extractImagesFromTests (strTests str')
    imagesLoc <- localizeImages manager (pack . fromJust . optUsr $ opts) (pack . fromJust . optPwd $ opts) (fromJust . optBaseUrl $ opts) (dropFileName . optStrStdFile $ opts) images
    let ts = replaceImagesUriInTests imagesLoc (strTests str')
    liftIO $ writeFile (optStrStdFile opts) $ GP.pretty str'{strTests = ts}
    return ()
