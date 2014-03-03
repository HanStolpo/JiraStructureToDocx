{-# LANGUAGE FlexibleContexts, QuasiQuotes, DeriveGeneric, RankNTypes, OverloadedStrings, NamedFieldPuns, RecordWildCards#-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module Query  (query
              ,addLabel
              ,removeLabel
              ,linkIssues
              ,createIssue
              ,fillStructure
              ,updateIssueDescription
              ,makeReq
              ,makeReqJson
              ,fetchIssueById
              ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.ByteString.Char8 as B (pack)
import Text.PrettyPrint.GenericPretty

--import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Applicative
import Network.Socket(withSocketsDo)
import Safe
-- import Debug.Trace
import Text.RawString.QQ
{-import Control.Monad.IO.Class-}
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

makeReq ::  Options -> String -> Request m
makeReq opts urlSuf =  (applyBasicAuth usr pwd . fromJust . parseUrl $ url){responseTimeout = Just 30000000}
    where
        usr = pack . fromJust . optUsr $ opts
        pwd = pack . fromJust . optPwd $ opts
        baseUrl = fromJust . optBaseUrl $ opts  
        url = baseUrl ++ urlSuf

makeReqJson :: Options -> String -> String -> Request m
makeReqJson opts urlSuf bdy = req {requestHeaders = modH . requestHeaders $ req, requestBody = RequestBodyBS . pack $ bdy}
    where
        req = makeReq opts urlSuf
        modH hs = (hContentType, "application/json"):hs


-- Run a query defined by optQueryString and return the list of issues
query :: Options -> IO [Issue]
query opts = case optQueryString opts of
    Nothing -> return []
    Just qs -> withSocketsDo $ withManager (_query 0)
        where 
            _query :: Int -> Manager -> ResourceT IO [Issue]
            _query fetched manager = do
                resBody <- liftM  responseBody $ httpLbs req manager
                case _decode resBody of 
                    Left  e     -> fail $ "Error decoding response body : " ++ e
                    Right qr    -> let  is = issues qr
                                        totalFetched = length is + fetched
                                        in if totalFetched >= total qr then return is  else (is++) <$> _query totalFetched manager
                where 
                    url = "/rest/api/2/search?jql=" ++ qs ++ "&startAt=" ++ show fetched ++ "&fields=summary,description,attachment,issuelinks,status,labels,customfield_10900,customfield_10003" 
                    req = makeReq opts url
                    _decode :: ByteString -> Either String QueryRes_
                    _decode = eitherDecode

fetchIssueById :: Options -> Int -> IO Issue
fetchIssueById opts iid = withSocketsDo $ withManager $ \manager -> do
    let url =  "/rest/api/latest/issue/" ++ show iid ++ "/?fields=summary,description,attachment,issuelinks,status,labels,customfield_10900,customfield_10003"
        req = makeReq opts url
        err e = error $ "fetchIssueById - error fetching issue id = " ++ show iid ++ " with options = " ++ pretty opts ++ " reason = " ++ e
    either err id . decodeIssueResponse . responseBody <$> httpLbs req manager


-- Add a label to an issue
addLabel :: Options     -- connection options
         -> String      -- the label to add
         -> Issue       -- the issue to which the label will be added
         -> IO Status   -- exception on failure
addLabel opts l i = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/api/2/issue/" ++ issueKey i
        req = (makeReqJson opts url bdy){method = methodPut}
        bdy = "{\"update\":{\"labels\":[{\"add\":\"" ++ l ++ "\"}]}}"
    liftM  responseStatus $ httpLbs req manager
    

-- remove a label from an issue
removeLabel :: Options  -- connection options
         -> String      -- the label to remove
         -> Issue       -- the issue to which the label will be removeed
         -> IO Status   -- exception on failure
removeLabel opts l i = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/api/2/issue/" ++ issueKey i
        req = (makeReqJson opts url bdy){method = methodPut}
        bdy = "{\"update\":{\"labels\":[{\"remove\":\"" ++ l ++ "\"}]}}"
    liftM  responseStatus $ httpLbs req manager


-- Link two issues
linkIssues  :: Options     -- connection options
            -> String      -- link type
            -> Issue       -- the inward issue of the link relationship (associated with outward description)
            -> Issue       -- the outward issue of the link relationship (associated with inward description)
            -> IO Status   -- exception on failure
linkIssues opts t inI outI = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/api/2/issueLink"
        req = (makeReqJson opts url bdy){method = methodPost}
        bdy = [r| {
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
            -> IO (String, String)   -- the ID of the created issue exception on failure (issueId, issueKey)
createIssue opts projId issueType i = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/api/2/issue"
        req = (makeReqJson opts url bdy){method = methodPost}
        bdy =  [r| 
                    {
                        "fields": |] ++ unpack (encode (CreateIssue_ (Id_ projId, Id_ issueType, i))) ++ [r|
                    }
                |]
    liftM (getCreateRespId . fromJust . decode . responseBody) $ httpLbs req manager


fillStructure :: Options        -- connection options
              -> String         -- structure ID
              -> IssueHierarchy -- the issue hierarchy to fill the structure (only the issueId fields are used)
              -> IO Status
--fillStructure = undefined
fillStructure _ _ (IssueHierarchy _ _) = error "fillStructure must be called with the root of the hierarchy"
fillStructure opts sId h@(IssueHierarchyRoot _) = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/structure/1.0/structure/" ++ sId ++"/forest"
        req = (makeReqJson opts url bdy){method = methodPost}
        bdy = _fillStuctureJSON h
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
                                "under": |] ++ show u ++ [r|,
                                "after": |] ++ show a ++ [r|
                            }
                        |] ++ sep scs ++ scs
                    scs = fst . foldl addI ("", (issueId i, 0)) $ cs'


newtype UpdateIssueDescription_ = UpdateIssueDescription_ Issue
instance ToJSON UpdateIssueDescription_ where
    toJSON (UpdateIssueDescription_ Issue {..}) = object ["update" .= update]
            where
                update = object ["description" .= [setD]]
                setD = object ["set" .= fromJustDef "" issueDescription]

-- update the issue's description in jira
updateIssueDescription  :: Options     -- connection options
                        -> Issue       -- the issue who's description will be updated in jira
                        -> IO Status   -- exception on failure
updateIssueDescription opts i = withSocketsDo $ withManager $ \manager -> do
    let url = "/rest/api/2/issue/" ++ issueKey i
        req = (makeReqJson opts url bdy){method = methodPut}
        bdy =  unpack . encode . UpdateIssueDescription_ $ i
    {-liftIO $ print req-}
    {-liftIO $ print bdy-}
    liftM  responseStatus $ httpLbs req manager
