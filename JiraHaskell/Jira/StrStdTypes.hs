{-# LANGUAGE DeriveGeneric #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module Jira.StrStdTypes  where
 
import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
-- internal
import Jira.JiraTypes
import Jira.ZephyrJson


newtype StdSrc = StdSrc
        {
            stdTests :: [StdTestSrc]
        } deriving (Show, Read, Generic)

instance Out StdSrc

class TestDesc a where
    tstIssue    :: a -> Issue
    tstModifyIssue :: a -> Issue -> a
    tstStories  :: a -> [Issue]


data StdTestSrc = StdTestSrc 
        {
            stdIssue :: Issue,
            stdStories  :: [Issue],
            stdSteps :: [TestStepInfo]
        } deriving (Show, Read, Generic)

instance Out StdTestSrc

instance TestDesc StdTestSrc where
    tstIssue = stdIssue
    tstStories = stdStories
    tstModifyIssue t i = t{stdIssue = i}



newtype StrSrc = StrSrc
        {
            strTests :: [StrTestSrc]
        } deriving (Show, Read, Generic)

instance Out StrSrc


data StrTestSrc = StrTestSrc 
        {
            strIssue    :: Issue,
            strStories  :: [Issue],
            strResult   :: Execution,
            strSteps    :: [(TestStepInfo, TestStepResult)]
        } deriving (Show, Read, Generic)

instance Out StrTestSrc

instance TestDesc StrTestSrc where
    tstIssue = strIssue
    tstStories = strStories
    tstModifyIssue t i = t{strIssue = i}
