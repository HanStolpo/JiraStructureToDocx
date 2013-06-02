{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
module IssueHierarchy where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import JiraTypes


data IssueHierarchy = IssueHierarchyRoot {ihChildren :: [IssueHierarchy]} 
                    | IssueHierarchy {ihIssue :: JsIssue, ihChildren :: [IssueHierarchy]}
    deriving (Show, Read, Generic)
instance Out IssueHierarchy

{-ihKey :: IssueHierarchy -> String-}
{-ihKey (IssueHierarchy i _) = jsiKey i-}

{-ihSummary :: IssueHierarchy -> String-}
{-ihSummary (IssueHierarchy i _) = jsiSummary i-}

{-ihDescription :: IssueHierarchy -> String-}
{-ihDescription (IssueHierarchy i _) = fromMaybe "" . jsiDescription $ i-}

{-ihAttachment :: IssueHierarchy -> [Attachment]-}
{-ihAttachment (IssueHierarchy i _) = jsiAttachments i-}
