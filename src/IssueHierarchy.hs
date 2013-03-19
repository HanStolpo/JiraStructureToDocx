 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
module IssueHierarchy where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import qualified Data.Aeson as AS

data Attachment = Attachment
    {
        attMimeType :: String,      -- The issue attachment mime type
        attUri  :: String,          -- The issue content URI
        attFileName :: String       -- The issue file name
    }
    deriving (Show, Read, Generic)
-- Make attachment pretty printable
instance Out Attachment
-- Make the attachment JSON deserializable
instance AS.FromJSON Attachment where
    parseJSON (AS.Object v) = do
        mimeType <- v AS..: "mimeType"
        fileName <- v AS..: "filename"
        content <- v AS..: "content"
        return $ Attachment {attMimeType = mimeType, attFileName = fileName, attUri = content}

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
