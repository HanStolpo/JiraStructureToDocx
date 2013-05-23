{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
module IssueHierarchy where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import JiraTypes


data IssueHierarchy = IssueHierarchy
    {
        ihKey :: String,
        ihSummary :: String,
        ihDescription :: String,
        ihAttachment :: [Attachment],
        ihChildren :: [IssueHierarchy]
    }
    deriving (Show, Read, Generic)
instance Out IssueHierarchy
