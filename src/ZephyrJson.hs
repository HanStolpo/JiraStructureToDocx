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
module ZephyrJson   ( TestStatus(..)
                    , TestStepInfo(..)
                    , TestStepResult(..)
                    , Schedule(..)
                    , decodeCycleResponse
                    , decodeTestStepsResponse
                    , decodeTestStepResultsResponse
                    , zephyrJsonTestGroup
                    ) where

import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import qualified Data.Aeson as AS
import Text.RawString.QQ
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Data.ByteString.Lazy.Char8 (ByteString)     -- only import string instances for overloaded strings
import qualified Data.Map as M
import Data.Maybe(fromMaybe, fromJust)
import Control.Applicative((<$>), (<|>), (<*>))
import Data.List(find)

_errObjExpected :: String -> a
_errObjExpected s = error $ "Expecting JSON object for " ++ s

zephyrJsonTestGroup ::  Test.Framework.Test
zephyrJsonTestGroup = $(testGroupGenerator)

-----------------------------------
-- StatusDef_
-----------------------------------
-- Internal type used to decode the status definition
data StatusDef_ = StatusDef_
    {
        _ident :: Int, 
        _desc  :: String,
        _name  :: String
    }
    deriving (Eq, Show, Read, Generic)
-- Make pretty printable
instance Out StatusDef_
-- Make JSON deserializable
instance AS.FromJSON StatusDef_ where
    parseJSON (AS.Object v) = do
        _ident' <- v AS..: "id"
        _desc' <- v AS..: "desc" <|> v AS..: "description"
        _name' <- v AS..: "name"
        return StatusDef_ {_ident = _ident', _desc = _desc', _name = _name'}
    parseJSON _ = _errObjExpected  "StatusDef_"


case_decodeStatusDef ::  Assertion
case_decodeStatusDef = expected @=? AS.decode s
    where
        expected = Just StatusDef_ {_ident = 1, _desc = "blah blah", _name="blah"}
        s = [r|{"id": 1, "color": "#75B000", "desc": "blah blah", "name": "blah"}|]


-----------------------------------
-- TestStatus
-----------------------------------
-- Status of a test or test step
data TestStatus = TestPass
                | TestFail
                | TestWip
                | TestBlocked
                | TestUnexecuted
                | NullTestStatus
    deriving (Eq, Show, Read, Generic)
instance Out TestStatus

_testStatusFromDef :: StatusDef_ -> TestStatus
_testStatusFromDef StatusDef_{_ident = 1, ..} = TestPass
_testStatusFromDef StatusDef_{_ident = 2, ..} = TestFail
_testStatusFromDef StatusDef_{_ident = 3, ..} = TestWip
_testStatusFromDef StatusDef_{_ident = 4, ..} = TestBlocked
_testStatusFromDef StatusDef_{_ident = -1, ..} = TestUnexecuted
_testStatusFromDef StatusDef_{..} = error $ "unknown status identifier " ++ show _ident


-----------------------------------
-- Schedule
-----------------------------------
data Schedule = Schedule
    {
        cycleName :: String,
        cycleId :: Int,
        scheduleId :: Int,
        issueKey :: String,
        issueId :: Int,
        summary :: String,
        executedOn :: String,
        executedBy :: String,
        status :: TestStatus,
        comment :: String
    }deriving (Eq, Show, Read, Generic)
instance Out Schedule

data ScheduleDef_ = ScheduleDef_ {executionStatus :: String, schedule :: Schedule} deriving (Eq, Show, Read, Generic)
instance Out ScheduleDef_
instance AS.FromJSON ScheduleDef_ where
    parseJSON (AS.Object v) = do
        cycleName' <- v AS..: "cycleName"
        cycleId' <- v AS..: "cycleID"
        scheduleId' <- v AS..: "scheduleID"
        issueKey' <- v AS..: "issueKey"
        issueId' <- v AS..: "issueID"
        summary' <- v AS..: "summary"
        executedOn' <- v AS..: "executedOn"
        executedBy' <- v AS..: "executedByDisplay"
        executionStatus' <- v AS..: "executionStatus"
        comment' <- v AS..: "comment"
        return ScheduleDef_ {executionStatus = executionStatus', schedule = Schedule {  cycleName =cycleName',
                                                                                        cycleId =cycleId',
                                                                                        scheduleId =scheduleId',
                                                                                        issueKey =issueKey',
                                                                                        issueId =issueId',
                                                                                        status = NullTestStatus,
                                                                                        summary =summary',
                                                                                        executedOn =executedOn',
                                                                                        executedBy =executedBy',
                                                                                        comment =comment' }}
    parseJSON _ = _errObjExpected "ScheduleDef_"

case_decodeScheduleDef ::  Assertion
case_decodeScheduleDef = Right expected @=? AS.eitherDecode s
    where
        expected =  ScheduleDef_ {executionStatus = "1", schedule = Schedule {cycleName = "cycleNameBlah" ,
                                                                            cycleId = 2,
                                                                            scheduleId = 75,
                                                                            issueKey = "LYNX-1002",
                                                                            issueId = 16101,
                                                                            status = NullTestStatus,
                                                                            summary = "summaryBlah",
                                                                            executedOn = "02/May/13 12:14 PM",
                                                                            executedBy = "executedByDisplayBlah",
                                                                            comment = "commentBlah" }}
        s = [r|{"summary": "summaryBlah",
                "executedOn": "02/May/13 12:14 PM",
                "cycleName": "cycleNameBlah", 
                "cycleID": 2,
                "label": "LxSensor", 
                "issueId": 16101,
                "versionName": "Unscheduled", 
                "issueID": 16101,
                "executedByDisplay": "executedByDisplayBlah", 
                "executionStatus": "1",
                "projectID": 10500, 
                "htmlComment": "",
                "executedBy": "handre.stolp", 
                "component": "", 
                "versionID": -1,
                "issueKey": "LYNX-1002", 
                "scheduleID": 75, 
                "comment": "commentBlah"}
            |]


-----------------------------------
-- Cycle_
-----------------------------------
type StatusMap_ = M.Map String StatusDef_
instance Out StatusMap_ where
    doc  = GP.docList . M.toList
    docPrec _  = GP.docList . M.toList

data Cycle_ = Cycle_ {statusDefs :: StatusMap_, schedules :: [ScheduleDef_]} deriving (Eq, Show, Read, Generic)
instance Out Cycle_

instance AS.FromJSON Cycle_ where
    parseJSON (AS.Object v) = do
        schedules' <- v AS..: "schedules"
        statusDefs' <- v AS..: "status"
        return Cycle_ {schedules = schedules', statusDefs = statusDefs'}
    parseJSON _ = _errObjExpected "Cycle_"

case_decodeCycleSchedules ::  Assertion
case_decodeCycleSchedules = Right expected @=? AS.eitherDecode s
    where
        expected =  Cycle_ 
                    { statusDefs = M.fromList [("1", StatusDef_ {_ident = 1, _desc = "blah", _name="PASS"}), ("2", StatusDef_ {_ident = 2, _desc = "blah", _name="FAIL"})],
                      schedules = [ScheduleDef_ {executionStatus = "1", schedule = Schedule { cycleName = "cycleNameBlah1" ,
                                                                                            cycleId = 2,
                                                                                            scheduleId = 75,
                                                                                            issueKey = "LYNX-1002",
                                                                                            issueId = 16101,
                                                                                            status = NullTestStatus,
                                                                                            summary = "summaryBlah1",
                                                                                            executedOn = "executedOnBlah1",
                                                                                            executedBy = "executedByDisplayBlah1",
                                                                                            comment = "commentBlah1" }},
                                    ScheduleDef_ {executionStatus = "2", schedule = Schedule {cycleName = "cycleNameBlah2" ,
                                                                                            cycleId = 2,
                                                                                            scheduleId = 76,
                                                                                            issueKey = "LYNX-992",
                                                                                            issueId = 16023,
                                                                                            status = NullTestStatus,
                                                                                            summary = "summaryBlah2",
                                                                                            executedOn = "executedOnBlah2",
                                                                                            executedBy = "executedByDisplayBlah2",
                                                                                            comment = "commentBlah2" }}]
                    }
        s = [r|{"status": { "1": {"id":  1, "color": "#75B000", "desc": "blah", "name": "PASS"},
                            "2": {"id":  2, "color": "#CC3300", "desc": "blah", "name": "FAIL"}}, 
            "schedules": [{"summary": "summaryBlah1",
                            "executedOn": "executedOnBlah1",
                            "cycleName": "cycleNameBlah1",
                            "cycleID": 2,
                            "label": "labelBlah1",
                            "issueId": 16101,
                            "versionName": "versionNameBlah1",
                            "issueID": 16101,
                            "executedByDisplay": "executedByDisplayBlah1",
                            "executionStatus": "1",
                            "projectID": 10500,
                            "htmlComment": "htmlCommentBlah1",
                            "executedBy": "executedByBlah",
                            "component": "",
                            "versionID": -1,
                            "issueKey": "LYNX-1002",
                            "scheduleID": 75,
                            "comment": "commentBlah1"},
                           {"summary": "summaryBlah2",
                            "executedOn": "executedOnBlah2",
                            "cycleName": "cycleNameBlah2",
                            "cycleID": 2,
                            "label": "labelBlah2",
                            "issueId": 16023,
                            "versionName": "versionNameBlah2",
                            "issueID": 16023,
                            "executedByDisplay": "executedByDisplayBlah2",
                            "executionStatus": "2",
                            "projectID": 10500,
                            "htmlComment": "htmlCommentBlah2",
                            "executedBy": "executedByBlah2",
                            "component": "",
                            "versionID": -1,
                            "issueKey": "LYNX-992",
                            "scheduleID": 76,
                            "comment": "commentBlah2"}]
                }
            |]

_schedulesFromCycle :: Cycle_ -> [Schedule]
_schedulesFromCycle Cycle_ {..} = map toS schedules
    where
        toS ScheduleDef_{..} = schedule {status =  fromMaybe NullTestStatus $ _testStatusFromDef <$> M.lookup executionStatus statusDefs}

decodeCycleResponse :: ByteString -> [Schedule]
decodeCycleResponse = _schedulesFromCycle . fromJust . AS.decode 

-----------------------------------
-- TestStep
-----------------------------------
data TestStepInfo = TestStepInfo
    {
        stepInfoId      :: Int,
        stepInfoOrdId   :: Int,
        stepInfoDesc    :: String,
        stepInfoData    :: String,
        stepInfoExpect  :: String
    }deriving (Eq, Show, Read, Generic)

instance Out TestStepInfo

instance AS.FromJSON TestStepInfo where
    parseJSON (AS.Object v) = TestStepInfo <$>
                                v AS..: "id" <*>
                                v AS..: "orderId" <*>
                                v AS..: "step" <*>
                                v AS..: "data" <*>
                                v AS..: "result"
    parseJSON _ = _errObjExpected "TestStepInfo"

case_decodeTestStepInfo ::  Assertion
case_decodeTestStepInfo = Right expected @=? AS.eitherDecode s
    where
        expected =  TestStepInfo 226 1 "stepBlah" "dataBlah" "resultBlah"
        s = [r|{"id": 226, "orderId": 1,
                "step": "stepBlah",
                "data": "dataBlah",
                "result": "resultBlah",
                "htmlStep": "htmlStepBlah",
                "htmlData": "htmlDataBlah",
                "htmlResult": "htmlResultBlah"}
            |]


decodeTestStepsResponse :: ByteString -> [TestStepInfo]
decodeTestStepsResponse = fromJust . AS.decode 



data TestStepResult = TestStepResult
    {
        stepResId      :: Int,
        stepResInfoId  :: Int,
        stepResStatus  :: TestStatus,
        stepResComment :: String
    }deriving (Eq, Show, Read, Generic)

instance Out TestStepResult

instance AS.FromJSON TestStepResult where
    parseJSON (AS.Object v) = do
            stepResId' <- v AS..: "id"
            stepResInfoId' <- v AS..: "stepId" 
            status <- v AS..: "status"
            executionStatus <- v AS..: "executionStatus"
            stepResComment' <- v AS..: "comment"
            return TestStepResult {stepResId = stepResId', stepResInfoId = stepResInfoId', stepResStatus = toS status executionStatus, stepResComment = stepResComment'}
            where
                toS :: String -> [StatusDef_] -> TestStatus
                toS s ds = case find ((read s ==) . _ident) ds of
                            Just d -> _testStatusFromDef d
                            Nothing -> NullTestStatus
    parseJSON _ = _errObjExpected "TestStepResult"

case_decodeTestStepResult ::  Assertion
case_decodeTestStepResult = Right expected @=? AS.eitherDecode s
    where
        expected =  TestStepResult 160 226 TestPass  "commentBlah"
        s = [r|{"id": 160, "executedOn": 1369150244095, "status": "1",
                "comment": "commentBlah",
                "htmlComment": "htmlCommentBlah",
                "executedBy": "executedByBlah", "scheduleId": 21, "stepId": 226,
                "executionStatus": [{"id": 1, "name": "PASS",
                                    "description": "Test step was executed and passed successfully",
                                    "color": "#75B000", "type": 0},
                                    {"id": 2, "name": "FAIL",
                                    "description": "Test step was executed and failed.",
                                    "color": "#CC3300", "type": 0},
                                    {"id": 3, "name": "WIP",
                                    "description": "Test step execution is a work-in-progress.",
                                    "color": "#F2B000", "type": 0},
                                    {"id": 4, "name": "BLOCKED",
                                    "description": "The Test step execution of this test was blocked for some reason.",
                                    "color": "#6693B0", "type": 0},
                                    {"id": -1, "name": "UNEXECUTED",
                                    "description": "The Test step has not yet been executed.",
                                    "color": "#A0A0A0", "type": 0}],
                "readableDefectList": ""}
            |]



decodeTestStepResultsResponse :: ByteString -> [TestStepResult]
decodeTestStepResultsResponse = fromJust . AS.decode 



-------------------------------------------------
-- Debug main
------------------------------------------------
main :: IO ()
main = defaultMain [zephyrJsonTestGroup]
