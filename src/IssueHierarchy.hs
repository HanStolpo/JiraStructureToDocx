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
module IssueHierarchy   ( IssueHierarchy(..)
                        , issueHierarchyTestGroup
                        ) where

import GHC.Generics
import Data.Default
import Text.PrettyPrint.GenericPretty as GP
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import qualified Data.Aeson as AS
-- import Text.RawString.QQ

import Data.ByteString.Lazy.Char8 (unpack)     -- only import string instances for overloaded strings
import Debug.Trace

import JiraTypes
import Utility

------------------------------------------
-- embed templated test case handling
issueHierarchyTestGroup ::  Test.Framework.Test
issueHierarchyTestGroup = $(testGroupGenerator)

data IssueHierarchy = IssueHierarchyRoot {ihChildren :: [IssueHierarchy]} 
                    | IssueHierarchy {ihIssue :: Issue, ihChildren :: [IssueHierarchy]}
    deriving (Show, Eq, Read, Generic)
instance Out IssueHierarchy
instance AS.ToJSON IssueHierarchy 
instance AS.FromJSON IssueHierarchy 

case_serializeHierarchy :: Assertion
case_serializeHierarchy = Just h @=? (AS.decode . (\s -> trace ("\n" ++ unpack s ++ "\n") s) . prettyJson . AS.encode $ h)
    where 
        h = IssueHierarchyRoot
            {
                ihChildren = 
                [
                    IssueHierarchy 
                    {
                        ihIssue = defIssue 
                        {
                            issueId = 4,
                            issueKey = "issueKey",
                            issueSummary = "issueSummary",
                            issueDescription = Just "issueDescription",
                            issueStatus = "issueStatus",
                            issueAttachments = [Attachment {
                                                attMimeType = "attMimeType",
                                                attUri = "attUri",
                                                attFileName = "attFileName"
                                                }],
                            issueLinks = [Outward "Blah" 1 "Blah", Inward "Blahs" 2 "Blahs"]
                        },
                        ihChildren = def
                    },
                    IssueHierarchy 
                    {
                        ihIssue = defIssue
                        {
                            issueId = 5,
                            issueKey = "2issueKey",
                            issueSummary = "2eissueSummary",
                            issueDescription = Just "2issueDescription",
                            issueStatus = "2issueStatus",
                            issueAttachments = [Attachment {
                                                attMimeType = "2attMimeType",
                                                attUri = "2attUri",
                                                attFileName = "2attFileName"
                                                }],
                            issueLinks = [Outward "2Blah" 1 "2Blah", Inward "2Blahs" 2 "2Blahs"]
                        },
                        ihChildren = def
                    }
                ]
            }
-- -------------------------------------------------
-- -- Debug main
-- ------------------------------------------------
-- main :: IO ()
-- main = defaultMain [issueHierarchyTestGroup]
