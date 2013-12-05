{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, QuasiQuotes
    , DeriveGeneric, RankNTypes, OverloadedStrings, NamedFieldPuns, RecordWildCards#-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module Query  (query
              ,addLabel
              ,removeLabel
              ,linkIssues
              ,createIssue
              ,fillStructure
              ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.ByteString.Char8 as B (pack)

import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Applicative
import Network.Socket(withSocketsDo)
-- import Debug.Trace
import Text.RawString.QQ
import Control.Monad.IO.Class
-- local files
import ProgramOptions
import JiraTypes
import IssueHierarchy


data QueryRes_ = QueryRes_  { total      :: Int
                            , issues     :: [Issue]
                            } deriving (Show, Generic)

instance FromJSON QueryRes_ where
    parseJSON (Object v) = do
        _total <- v .: "total"
        _issues <- v .: "issues"
        return $ QueryRes_ _total (map jsiGetIssue _issues)
    parseJSON a = typeMismatch "Expecting JSON object for JsIssue" a


-- Run a query defined by optQueryString and return the list of issues
query :: Options -> IO [Issue]
query opts = case optQueryString opts of
    Nothing -> return []
    Just qs -> withSocketsDo $ withManager (_query 0)
        where 
            usr = pack . fromJust . optUsr $ opts
            pwd = pack . fromJust . optPwd $ opts
            baseUrl = fromJust . optBaseUrl $ opts  
            _query :: Int -> Manager -> ResourceT IO [Issue]
            _query fetched manager = do
                resBody <- liftM  responseBody $ httpLbs req manager
                case _decode resBody of 
                    Left  e     -> fail $ "Error decoding response body : " ++ e
                    Right qr    -> let  is = issues qr
                                        totalFetched = length is + fetched
                                        in if totalFetched >= total qr then return is  else (is++) <$> _query totalFetched manager
                where 
                    url = baseUrl ++ "/rest/api/2/search?jql=" ++ qs ++ "&startAt=" ++ show (fetched) ++ "&fields=summary,description,attachment,issuelinks,status,labels,customfield_10900,customfield_10003" 
                    req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
                    req = req' {responseTimeout = Just 30000000 }
                    _decode :: ByteString -> Either String QueryRes_
                    _decode = eitherDecode

-- Add a label to an issue
addLabel :: Options     -- connection options
         -> String      -- the label to add
         -> Issue       -- the issue to which the label will be added
         -> IO Status   -- exception on failure
addLabel opts l i = withSocketsDo $ withManager $ \manager -> do
    let usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ "/rest/api/2/issue/" ++ issueKey i
        req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
        req  = req' {method = methodPut, requestHeaders = modH . requestHeaders $ req', requestBody = bdy, responseTimeout = Just 30000000}
        modH hs = (hContentType, "application/json"):hs
        bdy = RequestBodyBS . pack $ "{\"update\":{\"labels\":[{\"add\":\"" ++ l ++ "\"}]}}"
    liftM  responseStatus $ httpLbs req manager
    

-- remove a label from an issue
removeLabel :: Options  -- connection options
         -> String      -- the label to remove
         -> Issue       -- the issue to which the label will be removeed
         -> IO Status   -- exception on failure
removeLabel opts l i = withSocketsDo $ withManager $ \manager -> do
    let usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ "/rest/api/2/issue/" ++ issueKey i
        req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
        req  = req' {method = methodPut, requestHeaders = modH . requestHeaders $ req', requestBody = bdy, responseTimeout = Just 30000000}
        modH hs = (hContentType, "application/json"):hs
        bdy = RequestBodyBS . pack $ "{\"update\":{\"labels\":[{\"remove\":\"" ++ l ++ "\"}]}}"
    liftM  responseStatus $ httpLbs req manager


-- Link two issues
linkIssues  :: Options     -- connection options
            -> String      -- link type
            -> Issue       -- the inward issue of the link relationship (associated with outward description)
            -> Issue       -- the outward issue of the link relationship (associated with inward description)
            -> IO Status   -- exception on failure
linkIssues opts t inI outI = withSocketsDo $ withManager $ \manager -> do
    let usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ "/rest/api/2/issueLink"
        req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
        req  = req' {method = methodPost, requestHeaders = modH . requestHeaders $ req', requestBody = bdy, responseTimeout = Just 30000000}
        modH hs = (hContentType, "application/json"):hs
        bdy = RequestBodyBS . pack $ [r| {
            "type": {
                "name": "|] ++ t ++ [r|"
            },
            "inwardIssue": {
                "key": "|] ++ issueKey inI ++ [r|"
            },
            "outwardIssue": {
                "key": "|] ++ issueKey outI ++ [r|"
            }
        }|]
    liftM  responseStatus $ httpLbs req manager


newtype CreateRespId = CreateRespId {getCreateRespId :: (String, String)}
instance FromJSON CreateRespId where
    parseJSON (Object v) = CreateRespId <$> ((,) <$> v .: "id" <*> v .: "key")
    parseJSON a = typeMismatch "Expecting JSON object for create issue response" a

newtype Id_ = Id_ String
instance ToJSON Id_ where toJSON (Id_ s) = object ["id" .= s]

newtype CreateIssue_ = CreateIssue_ (Id_, Id_, Issue) 
instance ToJSON CreateIssue_ where
    toJSON (CreateIssue_ (projId, issueType, Issue {..})) 
                        = object $  ["project" .= projId, "issuetype" .= issueType]
                        ++ ["summary" .= issueSummary] 
                        ++ fromMaybe [] ((\d->["description" .= d]) <$> issueDescription)
                        ++ ["labels" .= issueLabels]
                        ++ fromMaybe [] ((\p->["customfield_10003" .= p]) <$> issueStoryPoints)

-- create an issue
createIssue :: Options     -- connection options
            -> String      -- project ID
            -> String      -- Issue type
            -> Issue       -- data for the issue
            -> IO (String, String)   -- the ID of the created issue exception on failure
createIssue opts projId issueType i = withSocketsDo $ withManager $ \manager -> do
    let usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ "/rest/api/2/issue"
        req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
        req  = req' {method = methodPost, requestHeaders = modH . requestHeaders $ req', requestBody = bdy, responseTimeout = Just 30000000}
        modH hs = (hContentType, "application/json"):hs
        bdy = RequestBodyBS . pack $ bdy'
        bdy' =  [r| 
                    {
                        "fields": |] ++ unpack (encode (CreateIssue_ (Id_ projId, Id_ issueType, i))) ++ [r|
                    }
                |]
    {-liftIO $ putStrLn bdy'-}
    liftM (getCreateRespId . fromJust . decode . responseBody) $ httpLbs req manager


fillStructure :: Options        -- connection options
              -> String         -- structure ID
              -> IssueHierarchy -- the issue hierarchy to fill the structure (only the issueId fields are used)
              -> IO Status
--fillStructure = undefined
fillStructure _ _ (IssueHierarchy _ _) = error "fillStructure must be called with the root of the hierarchy"
fillStructure opts sId h@(IssueHierarchyRoot _) = withSocketsDo $ withManager $ \manager -> do
    let usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ "/rest/structure/1.0/structure/" ++ sId ++"/forest"
        req' = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
        req  = req' {method = methodPost, requestHeaders = modH . requestHeaders $ req', requestBody = bdy, responseTimeout = Just 30000000}
        modH hs = (hContentType, "application/json"):hs
        bdy = RequestBodyBS . pack $ _fillStuctureJSON h
    -- liftIO $ putStrLn (_fillStuctureJSON h)
    liftM  responseStatus $ httpLbs req manager


_fillStuctureJSON :: IssueHierarchy -> String
_fillStuctureJSON (IssueHierarchy _ _) = error "_fillStuctureJSON must be called with the root of the hierarchy"
_fillStuctureJSON (IssueHierarchyRoot cs) = 
        [r| {
                "base":0,
                "root":0,
                "actions": [ 
                |] ++ (fst . foldl addI ("",(0,0)) $ cs) ++ [r|
                ]
            }
        |]
        where
            sep [] = ""
            sep _ = ",\n"
            addI _ (IssueHierarchyRoot _) = error "_fillStuctureJSON - can never be here"
            addI (s,(u,a)) (IssueHierarchy i cs') = (s', (u,iid))
                where 
                    iid = issueId i
                    s' = s ++ sep s ++
                        [r|
                            {
                                "action":"add",
                                "issue": |] ++ (show . issueId $ i) ++ [r|,
                                "under": |] ++ (show u) ++ [r|,
                                "after": |] ++ (show a) ++ [r|
                            }
                        |] ++ sep scs ++ scs
                    scs = fst . foldl addI ("", (issueId i, 0)) $ cs'

