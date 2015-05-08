{-# LANGUAGE OverloadedStrings
  , DeriveDataTypeable
  , FlexibleContexts
  , DeriveGeneric
  , DisambiguateRecordFields
  , NamedFieldPuns
  , RecordWildCards
  , QuasiQuotes
  , TemplateHaskell
  , TypeSynonymInstances 
  , FlexibleInstances 
  , ScopedTypeVariables
  , RecordWildCards
  #-}
module Jira.JiraTypes   ( Attachment(..)
                        , Issue(..)
                        , defIssue
                        , IssueLink(..)
                        , issueLinkDescription
                        , issueLinkKey
                        , issueLinkId
                        , IssueTime(..)
                        , decodeIssue
                        , decodeIssueResponse
                        , jiraTypesTestGroup
                        , parseIssueFromJiraResponseObject
                        , jiraIssueResponseFieldsSpec
                        ) where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import qualified Text.PrettyPrint as PP
import qualified Data.Aeson as AS
import qualified Data.Aeson.Types as AS (typeMismatch, Parser)
import qualified Data.Yaml as YAML
import qualified Data.Text as T
import Data.Strings
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Text.RawString.QQ
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Data.ByteString.Lazy.Char8 (ByteString, unpack)     -- only import string instances for overloaded strings
--import qualified Data.ByteString as BS
import Control.Applicative((<$>), (<*>), (<|>))
import Control.Monad
import Data.Default
import Debug.Trace
import Control.DeepSeq
import Jira.Utility

------------------------------------------
-- embed templated test case handling
jiraTypesTestGroup ::  Test.Framework.Test
jiraTypesTestGroup = $(testGroupGenerator)


-------------------------------------------
-- Attachments
-------------------------------------------
data Attachment = Attachment
    {
        attMimeType :: String,      -- The issue attachment mime type
        attUri  :: String,          -- The issue content URI
        attFileName :: String       -- The issue file name
    }
    deriving (Eq, Show, Read, Generic)
-- Make attachment pretty printable
instance Out Attachment
instance AS.ToJSON Attachment 
instance AS.FromJSON Attachment 

instance NFData Attachment where
    rnf Attachment { .. } =  rnf attMimeType 
                            `seq` rnf attUri
                            `seq` rnf attFileName
                            `seq` ()
-- proxy type to handle parsing jira response
newtype JiraRspAttachment = JiraRspAttachment {fromJiraRspAttachment :: Attachment} 
instance AS.FromJSON JiraRspAttachment where
    parseJSON (AS.Object v) = liftM JiraRspAttachment (Attachment 
                                                    <$> v AS..: "mimeType"   -- attMimeType
                                                    <*> v AS..: "content"    -- attUri
                                                    <*> v AS..: "filename"   -- attFileName
                                                  )
    parseJSON v = AS.typeMismatch "object" v



---------------------------------
-- Issue links
----------------------------------

data IssueLink = Outward String Int String | Inward String Int String -- Direction Description IssueId IssueKey
                 deriving (Eq, Show, Read, Generic)
instance Out IssueLink
instance AS.ToJSON IssueLink 
instance AS.FromJSON IssueLink 

instance NFData IssueLink where
    rnf (Outward d i k) =  rnf d `seq` rnf i `seq` rnf k `seq` ()
    rnf (Inward d i k) =  rnf d `seq` rnf i `seq` rnf k `seq` ()

issueLinkDescription :: IssueLink -> String
issueLinkDescription (Outward d _ _) = d
issueLinkDescription (Inward d _ _) = d

issueLinkKey :: IssueLink -> String
issueLinkKey (Outward _ _ k) = k
issueLinkKey (Inward _ _ k) = k

issueLinkId :: IssueLink -> Int
issueLinkId (Outward _ i _) = i
issueLinkId (Inward _ i _) = i

-- decode jira response for issue link as either an inward or an outward link using proxy type
newtype JiraRspIssueLink = JiraRspIssueLink {fromJiraRspIssueLink :: IssueLink} 
instance AS.FromJSON JiraRspIssueLink where
    parseJSON (AS.Object v) = 
        let mKey = (AS..:"key")
            mId = (AS..:"id")
            mInwardIssue = v AS..: "inwardIssue" 
            mType = v AS..: "type"
            mInwardLink = Inward  <$> (mType >>= (AS..:"inward")) 
                                  <*> (read <$> (mInwardIssue >>= mId)) 
                                  <*> (mInwardIssue >>= mKey)
            mOutwardIssue = v AS..: "outwardIssue" 
            mOutwardLink = Outward <$> (mType >>= (AS..:"outward")) 
                                   <*> (read <$> (mOutwardIssue >>= mId)) 
                                   <*> (mOutwardIssue >>= mKey)
        in liftM JiraRspIssueLink (mOutwardLink <|> mInwardLink)
    parseJSON v = AS.typeMismatch "object" v


---------------------------------------------------------------------------
-- Define a type 
newtype IssueTime = IssueTime {issueTimeToZonedTime :: ZonedTime}
-- custom Show instance to allow Issue to be part of Show
instance Show IssueTime where show  = show . issueTimeToString
-- custom Read instance to allow Issue to be part of Read
instance Read IssueTime where 
    readsPrec _ s = 
        case readsPrec 0 s of
            [] -> []
            (s', rs):_ -> case parseIssueTime s' of
                Nothing -> []
                Just t -> [(t, rs)]
-- custom Eq instance to allow Issue to be part of Eq
instance Eq IssueTime where (IssueTime _l) == (IssueTime _r) = zonedTimeToUTC _l == zonedTimeToUTC _r
-- ordering
instance Ord IssueTime where compare (IssueTime _l) (IssueTime _r) = compare (zonedTimeToUTC _l) (zonedTimeToUTC _r)
-- auto derive NFData instance from ZonedTime to allow Issue to be part of NFData
instance NFData IssueTime
-- Out instance for generic pretty
instance Out IssueTime where
    doc t = PP.text ("(fromJust . parseIssueTime $ " ++ show t ++ ")")
    docPrec _ = doc

-- The format specifier that defines how times are shown or read
issueTimeFmt :: String
issueTimeFmt = iso8601DateFormat (Just "%H:%M:%S%Q%z")
-- The local that defines how times are shown or read
issueTimeLocale :: TimeLocale
issueTimeLocale = defaultTimeLocale

-- Converts an IssueTime to a String
issueTimeToString :: IssueTime -> String
issueTimeToString = formatTime issueTimeLocale issueTimeFmt . issueTimeToZonedTime

-- Converts a String to an IssueTime possibly failing
parseIssueTime :: String -> Maybe IssueTime
parseIssueTime = fmap IssueTime . parseTime issueTimeLocale issueTimeFmt

-- Parse IssueTime from JSON or YAML
instance AS.FromJSON IssueTime where
    parseJSON v@(AS.String s) = case parseIssueTime (T.unpack s) of
                                  Just t -> return t
                                  Nothing -> AS.typeMismatch "expecting string representing date in SQL / ISO8601 format " v
    parseJSON v = AS.typeMismatch "object" v

-- Convert IssueTime to JSON or YAML
instance AS.ToJSON IssueTime where
    toJSON  = AS.String . T.pack . issueTimeToString
        

---------------------------------------------------------------------------
-- 

data Issue = Issue 
              { issueId               :: Int                -- the integer ID for he issue
              , issueKey              :: String             -- the string key for the issue
              , issueType             :: String             -- type of the isse, new if missing 'Story'
              , issueSummary          :: String             -- the issue's summary
              , issueDescription      :: Maybe String       -- the issue's description if it was present
              , issueCreateDate       :: Maybe IssueTime    -- the create date, new so Maybe (technically all issues should have this value)
              , issueStatus           :: String             -- the status of the issue
              , issueResolvedDate     :: Maybe IssueTime    -- the date the issue was last resovled if any
              , issueAttachments      :: [Attachment]       -- list of possibly associated attachements
              , issueLinks            :: [IssueLink]        -- list of possible links to other issues (descries the link)
              , issueLabels           :: [String]           -- the list of labels associated with this issue
              , issueReporter         :: Maybe String       -- the reporter, new so Maybe (technically all issues should have this value)
              , issueAssignee         :: Maybe String       -- the issue's assignee if any
              , issueStoryPoints      :: Maybe Int          -- the story points associated with the issue if any
              , issueSources          :: Maybe String       -- the sources field associated with the story if it has any
              } deriving (Eq, Show, Read, Generic)
instance Out Issue
instance Default Issue where
    def = defIssue

defIssue :: Issue
defIssue = Issue { issueId              = def 
                 , issueKey             = def 
                 , issueType            = def 
                 , issueSummary         = def 
                 , issueDescription     = def 
                 , issueCreateDate      = def 
                 , issueStatus          = def 
                 , issueResolvedDate    = def 
                 , issueAttachments     = def 
                 , issueLinks           = def 
                 , issueLabels          = def 
                 , issueReporter        = def 
                 , issueAssignee        = def 
                 , issueStoryPoints     = def 
                 , issueSources         = def 
                 } 

instance NFData Issue where
    rnf Issue { .. } =    rnf issueId 
                    `seq` rnf issueKey
                    `seq` rnf issueType
                    `seq` rnf issueSummary
                    `seq` rnf issueDescription
                    `seq` rnf issueCreateDate
                    `seq` rnf issueStatus
                    `seq` rnf issueResolvedDate
                    `seq` rnf issueAttachments
                    `seq` rnf issueLinks
                    `seq` rnf issueLabels
                    `seq` rnf issueReporter
                    `seq` rnf issueAssignee
                    `seq` rnf issueStoryPoints
                    `seq` rnf issueSources
                    `seq` ()

-- manually define to and from JSON instances for backwards compatability when adding new fields
instance AS.FromJSON Issue where
    parseJSON (AS.Object v) = 
              Issue <$> v AS..:  "issueId"
                    <*> v AS..:  "issueKey"
                    <*> v AS..:? "issueType" AS..!= "Story" -- key is allowed to be missing "Story" then
                    <*> v AS..:  "issueSummary"
                    <*> v AS..:? "issueDescription" -- key is allowed to be missing "Nothing" then
                    <*> v AS..:? "issueCreateDate"  -- key is allowed to be missing "Nothing" then
                    <*> v AS..:  "issueStatus"
                    <*> v AS..:? "issueResolvedDate"  -- key is allowed to be missin "Nothing" then
                    <*> v AS..:? "issueAttachments" AS..!= [] -- key is allowed to be missing "[]" then
                    <*> v AS..:? "issueLinks" AS..!= []       -- key is allowed to be missing "[]" then
                    <*> v AS..:? "issueLabels" AS..!= []      -- key is allowed to be missing "[]" then
                    <*> v AS..:? "issueReporter"     -- key is allowed to be missing "Nothing" then
                    <*> v AS..:? "issueAssignee"     -- key is allowed to be missing "Nothing" then
                    <*> v AS..:? "issueStoryPoints"  -- key is allowed to be missing "Nothing" then 
                    <*> v AS..:? "issueSources"      -- key is allowed to be missing "Nothing" then
    parseJSON a = AS.typeMismatch "Expecting JSON object for JiraIssue" a


instance AS.ToJSON Issue where
    toJSON Issue{..} = AS.object 
                        [ "issueId" AS..= issueId
                        , "issueKey" AS..= issueKey
                        , "issueType" AS..= issueType
                        , "issueSummary" AS..= issueSummary
                        , "issueDescription" AS..= issueDescription
                        , "issueCreateDate" AS..= issueCreateDate
                        , "issueStatus" AS..= issueStatus
                        , "issueResolveDate" AS..= issueResolvedDate
                        , "issueAttachments" AS..= issueAttachments
                        , "issueLinks" AS..= issueLinks
                        , "issueLabels" AS..= issueLabels
                        , "issueReporter" AS..= issueReporter
                        , "issueAssignee" AS..= issueAssignee
                        , "issueStoryPoints" AS..= issueStoryPoints
                        , "issueSources" AS..= issueSources
                        ]

-- The specifications of the required fields to be returned when quering jira for an issue
jiraIssueResponseFieldsSpec :: String
jiraIssueResponseFieldsSpec =   "issuetype," ++
                                "summary," ++
                                "description," ++
                                "created," ++ 
                                "status," ++
                                "resolutiondate," ++ 
                                "labels," ++
                                "issuelinks," ++ 
                                "attachment," ++
                                "reporter," ++
                                "assignee," ++
                                "customfield_10003," ++    -- story points field
                                "customfield_10900"        -- sources field 
-- Parse Jira jason response using a proxy type
newtype JiraRspIssue = JiraRspIssue {fromJiraRspIssue :: Issue} deriving (Show, Read, Generic)
instance AS.FromJSON JiraRspIssue where
    parseJSON (AS.Object v) = do
        fs <- v AS..: "fields"
                    -- issueId
        i <- Issue <$> (read <$> v AS..: "id")
                    -- issueKey
                    <*> v AS..: "key"
                    -- issueType
                    <*> (fs AS..: "issuetype" >>= (AS..: "name"))
                    -- issueSummary
                    <*> fs AS..: "summary"
                    -- issueDescription
                    <*> fs AS..:? "description" 
                    -- issueCreateDate
                    <*> fs AS..:? "created"
                    -- issueStatus
                    <*> (fs AS..: "status" >>= (AS..: "name"))
                    -- issueResolvedDate
                    <*> fs AS..:? "resolutiondate"
                    -- issueAttachments
                    <*> ((map fromJiraRspAttachment) <$> fs AS..:? "attachment" AS..!= [])
                    -- issueLinks
                    <*> ((map fromJiraRspIssueLink) <$> fs AS..:? "issuelinks" AS..!= [])
                    -- issueLabels
                    <*> fs AS..:? "labels" AS..!= []
                    -- issueReporter
                    <*> ((fs AS..: "reporter" >>= (AS..:"name")) <|> return Nothing)
                    -- issueAssignee
                    <*> ((fs AS..: "assignee" >>= (AS..:"name")) <|> return Nothing)
                    -- issueStoryPoints
                    <*> fs AS..:? "customfield_10003" AS..!= Nothing 
                    -- issueSources
                    <*> fs AS..:? "customfield_10900" AS..!= Nothing 
        return $ JiraRspIssue i 
    parseJSON a = AS.typeMismatch "object" a

parseIssueFromJiraResponseObject :: AS.Value -> AS.Parser Issue
parseIssueFromJiraResponseObject v = fromJiraRspIssue <$> AS.parseJSON v

decodeIssue :: Monad m => ByteString -> m Issue
decodeIssue = return . fromJiraRspIssue <=< decodeJsIssue

decodeJsIssue :: Monad m => ByteString -> m JiraRspIssue
decodeJsIssue s = case AS.eitherDecode s of
                        Left e -> fail $ "Error decoding JASON for issue : " ++ e ++ "\n" ++ unpack s
                        Right f -> return f

decodeIssueResponse :: ByteString  -> Either String Issue
decodeIssueResponse = fmap fromJiraRspIssue . decodeJsIssue

case_decodeIssueResponse :: Assertion
case_decodeIssueResponse = Right expected @=? decodeIssueResponse s
    where
        expected = defIssue {issueId = 15846
                            ,issueKey = "LYNX-853"
                            ,issueSummary = "summaryBlah"
                            ,issueDescription = Just "descriptionBlah"
                            ,issueStatus = "Resolved"
                            ,issueLinks = [Inward "relates to" 15824 "LYNX-831", Outward "tests" 15498 "LYNX-619"]
                            ,issueLabels = ["LxIpe"]
                            }
        s = [r|{"expand": "renderedFields,names,schema,transitions,operations,editmeta,changelog",
                "id": "15846",
                "self": "http://jira.server.com/rest/api/latest/issue/15846",
                "key": "LYNX-853",
                "fields": {"summary": "summaryBlah",
                            "progress": {"progress": 0, "total": 0}, "timetracking": {},
                            "issuetype": {"self": "http://jira.server.com/rest/api/2/issuetype/14",
                                        "id": "14",
                                        "description": "This Issue Type is used to create Zephyr Test within Jira.",
                                        "iconUrl": "http://jira.server.com/download/resources/com.thed.zephyr.je/images/icons/ico_zephyr_issuetype.png",
                                        "name": "Test", "subtask": false},
                            "votes": {"self": "http://jira.server.com/rest/api/2/issue/LYNX-853/votes",
                                    "votes": 0, "hasVoted": false},
                            "customfield_11100": null, "resolutiondate": null,
                            "timespent": null,
                            "reporter": {"self": "http://jira.server.com/rest/api/2/user?username=user.name",
                                        "name": "user.name", "emailAddress": "",
                                        "avatarUrls": {"16x16": "http://jira.server.com/secure/useravatar?size=small&avatarId=10116",
                                                        "48x48": "http://jira.server.com/secure/useravatar?avatarId=10116"},
                                        "displayName": "User Name", "active": true},
                            "aggregatetimeoriginalestimate": null,
                            "updated": "2013-04-17T13:33:57.000+0200",
                            "created": "2013-03-22T11:47:10.000+0200",
                            "description": "descriptionBlah",
                            "customfield_11001": null, "customfield_11000": null,
                            "customfield_10003": null,
                            "issuelinks": [{"id": "13657",
                                            "self": "http://jira.server.com/rest/api/2/issueLink/13657",
                                            "type": {"id": "10003", "name": "Relates", "inward": "relates to",
                                                    "outward": "relates to",
                                                    "self": "http://jira.server.com/rest/api/2/issueLinkType/10003"},
                                            "inwardIssue": {"id": "15824", "key": "LYNX-831",
                                                            "self": "http://jira.server.com/rest/api/2/issue/15824",
                                                            "fields": {"summary": "summaryBlah2",
                                                                    "status": {"self": "http://jira.server.com/rest/api/2/status/5",
                                                                                "description": "A resolution has been taken, and it is awaiting verification by reporter. From here issues are either reopened, or are closed.",
                                                                                "iconUrl": "http://jira.server.com/images/icons/status_resolved.gif",
                                                                                "name": "Resolved", "id": "5"},
                                                                    "issuetype": {"self": "http://jira.server.com/rest/api/2/issuetype/3",
                                                                                    "id": "3",
                                                                                    "description": "A task that needs to be done.",
                                                                                    "iconUrl": "http://jira.server.com/images/icons/task.gif",
                                                                                    "name": "Task",
                                                                                    "subtask": false}}}},
                                        {"id": "13957",
                                            "self": "http://jira.server.com/rest/api/2/issueLink/13957",
                                            "type": {"id": "10501", "name": "Tests", "inward": "is tested by",
                                                    "outward": "tests",
                                                    "self": "http://jira.server.com/rest/api/2/issueLinkType/10501"},
                                            "outwardIssue": {"id": "15498", "key": "LYNX-619",
                                                            "self": "http://jira.server.com/rest/api/2/issue/15498",
                                                            "fields": {"summary": "summaryBlah3",
                                                                        "status": {"self": "http://jira.server.com/rest/api/2/status/5",
                                                                                "description": "A resolution has been taken, and it is awaiting verification by reporter. From here issues are either reopened, or are closed.",
                                                                                "iconUrl": "http://jira.server.com/images/icons/status_resolved.gif",
                                                                                "name": "Resolved", "id": "5"},
                                                                        "issuetype": {"self": "http://jira.server.com/rest/api/2/issuetype/8",
                                                                                    "id": "8",
                                                                                    "description": "A user story",
                                                                                    "iconUrl": "http://jira.server.com/images/icons/ico_story.png",
                                                                                    "name": "Story",
                                                                                    "subtask": false}}}}],
                            "watches": {"self": "http://jira.server.com/rest/api/2/issue/LYNX-853/watchers",
                                        "watchCount": 1, "isWatching": false},
                            "worklog": {"startAt": 0, "maxResults": 0, "total": 0,
                                        "worklogs": []},
                            "customfield_10000": null, "customfield_10101": null,
                            "customfield_10600": "4144", "subtasks": [],
                            "customfield_10100": null,
                            "status": {"self": "http://jira.server.com/rest/api/2/status/5",
                                    "description": "A resolution has been taken, and it is awaiting verification by reporter. From here issues are either reopened, or are closed.",
                                    "iconUrl": "http://jira.server.com/images/icons/status_resolved.gif",
                                    "name": "Resolved", "id": "5"},
                            "labels": ["LxIpe"], "workratio": -1,
                            "assignee": {"self": "http://jira.server.com/rest/api/2/user?username=user.name",
                                        "name": "user.name", "emailAddress": "",
                                        "avatarUrls": {"16x16": "http://jira.server.com/secure/useravatar?size=small&avatarId=10116",
                                                        "48x48": "http://jira.server.com/secure/useravatar?avatarId=10116"},
                                        "displayName": "User Name", "active": true},
                            "attachment": [], "customfield_10500": "3561",
                            "aggregatetimeestimate": null,
                            "project": {"self": "http://jira.server.com/rest/api/2/project/LYNX",
                                        "id": "10500", "key": "LYNX", "name": "LYNX",
                                        "avatarUrls": {"16x16": "http://jira.server.com/secure/projectavatar?size=small&pid=10500&avatarId=10009",
                                                    "48x48": "http://jira.server.com/secure/projectavatar?pid=10500&avatarId=10009"}},
                            "customfield_10700": null, "customfield_11200": "0.0",
                            "timeestimate": null,
                            "aggregateprogress": {"progress": 0, "total": 0},
                            "lastViewed": "2013-05-21T17:01:31.988+0200",
                            "customfield_10800": null,
                            "comment": {"startAt": 0, "maxResults": 1, "total": 1,
                                        "comments": [{"self": "http://jira.server.com/rest/api/2/issue/15846/comment/14206",
                                                    "id": "14206",
                                                    "author": {"self": "http://jira.server.com/rest/api/2/user?username=user.name",
                                                                "name": "user.name", "emailAddress": "",
                                                                "avatarUrls": {"16x16": "http://jira.server.com/secure/useravatar?size=small&avatarId=10116",
                                                                                "48x48": "http://jira.server.com/secure/useravatar?avatarId=10116"},
                                                                "displayName": "User Name", "active": true},
                                                    "body": "Passed\u000a",
                                                    "updateAuthor": {"self": "http://jira.server.com/rest/api/2/user?username=user.name",
                                                                    "name": "user.name", "emailAddress": "",
                                                                    "avatarUrls": {"16x16": "http://jira.server.com/secure/useravatar?size=small&avatarId=10116",
                                                                                    "48x48": "http://jira.server.com/secure/useravatar?avatarId=10116"},
                                                                    "displayName": "User Name",
                                                                    "active": true},
                                                    "created": "2013-04-17T13:32:54.000+0200",
                                                    "updated": "2013-04-17T13:32:54.000+0200"}]},
                            "timeoriginalestimate": null, "customfield_10801": null,
                            "aggregatetimespent": null}}
        |]



case_serializeIssue :: Assertion
case_serializeIssue = Just i @=? (AS.decode . {-(\s -> trace ("\n****\n" ++ unpack s ++ "\n****\n") s) .-}  prettyJson . AS.encode $ i)
    where 
        i = defIssue
            {
                issueId = 4,
                issueKey = "issueKey",
                issueSummary = "issueSummary",
                issueDescription = Just "issueDescription\nhi there, i say \n, what are",
                issueStatus = "issueStatus",
                issueAttachments = [Attachment {
                                    attMimeType = "attMimeType",
                                    attUri = "attUri",
                                    attFileName = "attFileName"
                                    }],
                issueLinks = [Outward "Blah" 1 "Blah", Inward "Blahs" 2 "Blahs"]
            }

case_serializeIssueYaml :: Assertion
case_serializeIssueYaml = Just i @=? (YAML.decode . (\s -> trace ("\n****\n" ++ toString s ++ "\n****\n") s) .  YAML.encode $ i)
    where 
        i = defIssue
            {
                issueId = 4,
                issueKey = "issueKey",
                issueSummary = "issueSummary",
                issueDescription = Just "issueDescription\nhi there, i say \n, what are",
                issueStatus = "issueStatus",
                issueAttachments = [Attachment {
                                    attMimeType = "attMimeType",
                                    attUri = "attUri",
                                    attFileName = "attFileName"
                                    }],
                issueLinks = [Outward "Blah" 1 "Blah", Inward "Blahs" 2 "Blahs"]
            }

-- Note missing maybe is fine but empty strings are not
case_deserializeIssue :: Assertion
case_deserializeIssue = Right i @=? AS.eitherDecode s
    where
        i = defIssue
            {
                issueId = 4,
                issueKey = "issueKey",
                issueSummary = "issueSummary",
                issueDescription = Nothing, -- Just "issueDescription",
                issueStatus = "issueStatus",
                issueAttachments = [Attachment {
                                    attMimeType = "attMimeType",
                                    attUri = "attUri",
                                    attFileName = "attFileName"
                                    }],
                issueLinks = [] -- [(Outward "Blah" 1 "Blah"), (Inward "Blahs" 2 "Blahs")]
            }
        s = [r|
                {"issueDescription":null,
                "issueAttachments":
                [
                {"attUri":"attUri",
                    "attFileName":"attFileName",
                    "attMimeType":"attMimeType"
                }
                ],
                "issueStoryPoints":null,
                "issueSources":null,
                "issueLabels":
                [
                ],
                "issueId":4,
                "issueKey":"issueKey",
                "issueSummary":"issueSummary",
                "issueStatus":"issueStatus",
                "issueLinks":
                [
                ]
                }
            |]
-------------------------------------------------
-- Debug main
------------------------------------------------
-- main :: IO ()
-- main = defaultMain [jiraTypesTestGroup]
