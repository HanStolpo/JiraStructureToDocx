{-# LANGUAGE DeriveDataTypeable, RecordWildCards, DeriveGeneric #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module StrStdTypes  where
 
import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
-- internal
import JiraTypes
import ZephyrJson


newtype StdSrc = StdSrc
        {
            stdTests :: [StdTestSrc]
        } deriving (Show, Read, Generic)

instance Out StdSrc

class TestDesc a where
    tstIssue    :: a -> JsIssue
    tstStories  :: a -> [JsIssue]


data StdTestSrc = StdTestSrc 
        {
            stdIssue :: JsIssue,
            stdStories  :: [JsIssue],
            stdSteps :: [TestStepInfo]
        } deriving (Show, Read, Generic)

instance Out StdTestSrc

instance TestDesc StdTestSrc where
    tstIssue = stdIssue
    tstStories = stdStories




newtype StrSrc = StrSrc
        {
            strTests :: [StrTestSrc]
        } deriving (Show, Read, Generic)

instance Out StrSrc


data StrTestSrc = StrTestSrc 
        {
            strIssue    :: JsIssue,
            strStories  :: [JsIssue],
            strResult   :: Schedule,
            strSteps    :: [(TestStepInfo, TestStepResult)]
        } deriving (Show, Read, Generic)

instance Out StrTestSrc

instance TestDesc StrTestSrc where
    tstIssue = strIssue
    tstStories = strStories
