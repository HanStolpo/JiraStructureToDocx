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
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Applicative
-- local files
import ProgramOptions
import JiraTypes
import ZephyrJson
import StrStdTypes
import Network.Socket(withSocketsDo)
import Control.Monad.IO.Class(liftIO)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(dropFileName)
import Text.PrettyPrint.GenericPretty as GP


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

fetchTestIssueById :: Query_ Int JsIssue
fetchTestIssueById opts man iid = do
    let url =  "/rest/api/latest/issue/" ++ show iid
        req = _makeReq opts url
    decodeJsIssueResponse . responseBody <$> httpLbs req man


fetchStdTestSrc :: Query_ Schedule StdTestSrc
fetchStdTestSrc opts man sc = do
    let iid = issueId sc
    issue <- _fromEither =<< fetchTestIssueById opts man iid
    steps <- _fromEither =<< fetchTestStepInfos opts man iid
    return $ Right StdTestSrc{stdIssue = issue, stdSteps = steps}

_fetchStdSrc :: Query_ String StdSrc
_fetchStdSrc opts man cn = do
    cid <- _fromEither =<< fetchCycleIdFromName opts man cn 
    scs <- _fromEither =<< fetchCycleSchedules opts man cid
    Right . StdSrc <$> forM scs (_fromEither <=< fetchStdTestSrc opts man)

fetchStdSrc :: Options -> IO ()
fetchStdSrc opts = withSocketsDo $ runResourceT $ do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optStrStdFile $ opts
    manager <- liftIO $ newManager def
    std' <- _fromEither =<< _fetchStdSrc opts manager (fromJust . optCycleName $ opts)
    liftIO $ writeFile (optStrStdFile opts) $ GP.pretty std'
    return ()

fetchStrTestSrc :: Query_ Schedule StrTestSrc
fetchStrTestSrc opts man sc = do
    let iid = issueId sc
        sid = scheduleId sc
    issue <- _fromEither =<< fetchTestIssueById opts man iid
    stepInfos <- _fromEither =<< fetchTestStepInfos opts man iid
    stepResults <- _fromEither =<< fetchTestStepResults opts man sid
    let steps = filter chkTpl $ zip (sortWith stepInfoId stepInfos)(sortWith stepResInfoId stepResults)
        chkTpl (i,r) = stepInfoId i == stepResInfoId r
    return $ Right StrTestSrc{strIssue = issue, strResult = sc, strSteps = steps}

_fetchStrSrc :: Query_ String StrSrc
_fetchStrSrc opts man cn = do
    cid <- _fromEither =<< fetchCycleIdFromName opts man cn 
    scs <- _fromEither =<< fetchCycleSchedules opts man cid
    Right . StrSrc <$> forM scs (_fromEither <=< fetchStrTestSrc opts man)

fetchStrSrc :: Options -> IO ()
fetchStrSrc opts = withSocketsDo $ runResourceT $ do
    liftIO $ print opts
    liftIO $ createDirectoryIfMissing True $ dropFileName . optStrStdFile $ opts
    manager <- liftIO $ newManager def
    let cn = (fromJust . optCycleName $ opts)
    str' <- _fromEither =<< _fetchStrSrc opts manager cn
    liftIO $ writeFile (optStrStdFile opts) $ GP.pretty str' 
    return ()
