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


data StdTestSrc = StdTestSrc 
        {
            stdIssue :: JsIssue,
            stdSteps :: [TestStepInfo]
        } deriving (Show, Read, Generic)

instance Out StdTestSrc





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
