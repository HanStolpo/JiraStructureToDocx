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
module JiraTypes   ( Attachment(..)
                    , Issue(..)
                    , defIssue
                    , IssueLink(..)
                    , issueLinkDescription
                    , issueLinkKey
                    , issueLinkId
                    , decodeIssue
                    , decodeIssueResponse
                    , jiraTypesTestGroup
                    , JsIssue(..)
                    ) where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import qualified Data.Aeson as AS
import qualified Data.Aeson.Types as AS (typeMismatch)
import qualified Data.Yaml as YAML
import Data.Strings
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
import Utility

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
newtype JsAttachment = JsAttachment {jsiGetAttachment :: Attachment} deriving (Eq, Show, Read, Generic)
-- Make the attachment JSON deserializable
instance AS.FromJSON JsAttachment where
    parseJSON (AS.Object v) = do
        mimeType <- v AS..: "mimeType"
        fileName <- v AS..: "filename"
        content <- v AS..: "content"
        return $ JsAttachment Attachment {attMimeType = mimeType, attFileName = fileName, attUri = content}
    parseJSON a = AS.typeMismatch "Expecting JSON object for Attachment" a



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

-- proxy type to handle parsing jira response
newtype JsIssueLink = JsIssueLink {jsiGetIssueLink :: IssueLink} deriving (Eq, Show, Read, Generic)

-- decode link type from jira response
data IssueLinkType_ = IssueLinkType_{_inwardDesc :: String, _outwardDesc :: String}
instance AS.FromJSON IssueLinkType_ where
    parseJSON (AS.Object v) = IssueLinkType_ <$> v AS..: "inward" <*> v AS..: "outward"
    parseJSON a = AS.typeMismatch "Expecting JSON object for IssueLinkType_" a

-- decode link data from jira response
data IssueLinkData_ = IssueLinkData_{_linkedIssueId :: Int, _linkedIssueKey :: String}
instance AS.FromJSON IssueLinkData_ where
    parseJSON (AS.Object v) = IssueLinkData_ <$> fmap read (v AS..: "id") <*> v AS..: "key"
    parseJSON a = AS.typeMismatch "Expecting JSON object for IssueLinkData_" a

-- decode an inward link from jira response
_makeInwardLink :: IssueLinkData_ -> IssueLinkType_ -> IssueLink
_makeInwardLink d t = Inward (_inwardDesc t) (_linkedIssueId d) (_linkedIssueKey d)

-- decode an outward link from jira response
_makeOutwardLink :: IssueLinkData_ -> IssueLinkType_ -> IssueLink
_makeOutwardLink d t = Outward (_outwardDesc t) (_linkedIssueId d) (_linkedIssueKey d)

-- decode jira response for issue link as either an inward or an outward link
instance AS.FromJSON JsIssueLink where
    parseJSON (AS.Object v) = JsIssueLink <$> (_makeInwardLink <$> v AS..: "inwardIssue" <*> v AS..: "type") 
                            <|> 
                             JsIssueLink <$> (_makeOutwardLink <$> v AS..: "outwardIssue" <*> v AS..: "type")
    parseJSON a = AS.typeMismatch "Expecting JSON object for  IssueLink" a
        

---------------------------------------------------------------------------
-- Class to represent the JASON of an issue

data Issue = Issue
    {
        issueId             :: Int,          -- The issue identifier
        issueKey            :: String,       -- The issue key
        issueSummary        :: String,       -- The summary field 
        issueDescription    :: Maybe String, -- The optional description field
        issueStatus         :: String,
        issueAttachments    :: [Attachment], -- The list of optional attachments
        issueLinks          :: [IssueLink],
        issueLabels         :: [String],
        issueStoryPoints    :: Maybe Int,
        issueSources        :: Maybe String
    } 
    deriving (Eq, Show, Read, Generic)
instance Out Issue
instance AS.ToJSON Issue 
instance AS.FromJSON Issue 
instance Default Issue where
    def = defIssue

defIssue :: Issue
defIssue = Issue 0 "" "" Nothing "" [] [] [] Nothing Nothing

instance NFData Issue where
    rnf Issue { .. } =  rnf issueId 
                `seq` rnf issueKey 
                `seq` rnf issueSummary
                `seq` rnf issueDescription
                `seq` rnf issueStatus
                `seq` rnf issueAttachments
                `seq` rnf issueLinks
                `seq` rnf issueLabels
                `seq` rnf issueStoryPoints
                `seq` rnf issueSources
                `seq` ()

-- proxy type for decoding jira response
newtype JsIssue = JsIssue {jsiGetIssue :: Issue} deriving (Eq, Show, Read, Generic)

-- Specify JSON parser for JsIssue 
instance AS.FromJSON JsIssue where
    parseJSON (AS.Object v) = do
        ident <- read <$> v AS..: "id"
        key <- v AS..: "key"
        fields <- v AS..: "fields"
        summary <- fields AS..: "summary"
        description <- fields AS..:? "description"
        statusObj <- fields AS..: "status"
        status <- statusObj AS..: "name"
        attachments <- fields AS..:? "attachment" AS..!= []
        issueLinks <- fields AS..:? "issuelinks" AS..!= []
        labels :: [String] <- fields AS..:? "labels" AS..!= []
        storyPoints :: Maybe Int <- fields AS..:? "customfield_10003" AS..!= Nothing
        sources :: Maybe String <- fields AS..:? "customfield_10900" AS..!= Nothing
        return $ JsIssue (Issue ident key summary description status (map jsiGetAttachment attachments) (map jsiGetIssueLink issueLinks) labels storyPoints sources)
    parseJSON a = AS.typeMismatch "Expecting JSON object for JsIssue" a

decodeIssue :: Monad m => ByteString -> m Issue
decodeIssue = return . jsiGetIssue <=< decodeJsIssue

decodeJsIssue :: Monad m => ByteString -> m JsIssue
decodeJsIssue s = case AS.eitherDecode s of
                        Left e -> fail $ "Error decoding JASON for issue : " ++ e ++ "\n" ++ unpack s
                        Right f -> return f

decodeIssueResponse :: ByteString  -> Either String Issue
decodeIssueResponse = fmap jsiGetIssue . decodeJsIssue

-- decodeJsIssueResponse :: ByteString  -> Either String JsIssue
-- decodeJsIssueResponse = AS.eitherDecode 

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
