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
  #-}
module JiraTypes   ( Attachment(..)
                    , JsIssue(..)
                    , decodeJsIssue
                    , decodeJsIssueResponse
                    , jiraTypesTestGroup
                    ) where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import qualified Data.Aeson as AS
import qualified Data.Aeson.Types as AS (typeMismatch)
import Text.RawString.QQ
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Data.ByteString.Lazy.Char8 (ByteString)     -- only import string instances for overloaded strings
import Control.Applicative((<$>))

jiraTypesTestGroup ::  Test.Framework.Test
jiraTypesTestGroup = $(testGroupGenerator)

data Attachment = Attachment
    {
        attMimeType :: String,      -- The issue attachment mime type
        attUri  :: String,          -- The issue content URI
        attFileName :: String       -- The issue file name
    }
    deriving (Eq, Show, Read, Generic)
-- Make attachment pretty printable
instance Out Attachment
-- Make the attachment JSON deserializable
instance AS.FromJSON Attachment where
    parseJSON (AS.Object v) = do
        mimeType <- v AS..: "mimeType"
        fileName <- v AS..: "filename"
        content <- v AS..: "content"
        return Attachment {attMimeType = mimeType, attFileName = fileName, attUri = content}
    parseJSON a = AS.typeMismatch "Expecting JSON object for Attachment" a

---------------------------------------------------------------------------
-- Class to represent the JASON of an issue

data JsIssue = JsIssue
    {
        jsiId  :: Int,                  -- The issue identifier
        jsiKey :: String,               -- The issue key
        jsiSummary :: String,           -- The summary field 
        jsiDescription :: Maybe String, -- The optional description field
        jsiAttachments :: [Attachment]  -- The list of optional attachments
    } 
    deriving (Eq, Show, Read, Generic)
instance Out JsIssue

-- Specify JSON parser for JsIssue 
instance AS.FromJSON JsIssue where
    parseJSON (AS.Object v) = do
        ident <- read <$> v AS..: "id"
        key <- v AS..: "key"
        fields <- v AS..: "fields"
        summary <- fields AS..: "summary"
        description <- fields AS..:? "description"
        attachments <- fields AS..:? "attachment" AS..!= []
        return $ JsIssue ident key summary description attachments
    parseJSON a = AS.typeMismatch "Expecting JSON object for JsIssue" a

decodeJsIssue :: Monad m => ByteString -> m JsIssue
decodeJsIssue s = case AS.eitherDecode s of
                        Left e -> fail $ "Error decoding JASON for issue : " ++ e 
                        Right f -> return f

decodeJsIssueResponse :: ByteString  -> Either String JsIssue
decodeJsIssueResponse = AS.eitherDecode 

case_decodeJsIssueResponse :: Assertion
case_decodeJsIssueResponse = Right expected @=? decodeJsIssueResponse s
    where
        expected = JsIssue 15846 "LYNX-853" "summaryBlah" (Just "descriptionBlah") []
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
-------------------------------------------------
-- Debug main
------------------------------------------------
{-main :: IO ()-}
{-main = defaultMain [jiraTypesTestGroup]-}
