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
module Jira.ZephyrJson   ( TestStatus(..)
                    , TestStepInfo(..)
                    , TestStepResult(..)
                    , Execution(..)
                    , Cycle(..)
                    , decodeCycleExecutions
                    , decodeTestStepsResponse
                    , decodeTestStepResultsResponse
                    , decodeProjectCyclesResponse
                    , zephyrJsonTestGroup
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
-- import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe(fromMaybe, isJust)
import Control.Applicative(pure, (<$>), (<|>), (<*>))
import Data.List(find)
import Data.ByteString.Lazy.Char8(unpack, ByteString)

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
    parseJSON a = AS.typeMismatch "StatusDef_" a


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
data Execution = Execution
    {
        executionId :: Int,
        issueKey :: String,
        issueId :: Int,
        summary :: String,
        executedOn :: String,
        executedBy :: String,
        status :: TestStatus,
        comment :: String
    }deriving (Eq, Show, Read, Generic)
instance Out Execution

data ExecutionDef_ = ExecutionDef_ {executionStatus :: String, execution :: Execution} deriving (Eq, Show, Read, Generic)
instance Out ExecutionDef_
instance AS.FromJSON ExecutionDef_ where
    parseJSON (AS.Object v) = do
        executionId' <- v AS..: "id"
        issueKey' <- v AS..: "issueKey"
        issueId' <- v AS..: "issueId"
        summary' <- v AS..: "summary"
        executedOn' <- v AS..:? "executedOn" AS..!= ""
        executedBy' <- v AS..:? "executedByDisplay" AS..!= ""
        executionStatus' <- v AS..: "executionStatus"
        comment' <- v AS..:? "htmlComment" AS..!= ""
        return ExecutionDef_ {executionStatus = executionStatus', execution = Execution {executionId = executionId',
                                                                                        issueKey = issueKey',
                                                                                        issueId = issueId',
                                                                                        status = NullTestStatus,
                                                                                        summary = summary',
                                                                                        executedOn = executedOn',
                                                                                        executedBy = executedBy',
                                                                                        comment = comment' }}
    parseJSON a = AS.typeMismatch "ExecutionDef_" a

case_decodeExecutionDef ::  Assertion
case_decodeExecutionDef = Right expected @=? AS.eitherDecode s
    where
        expected =  ExecutionDef_ {executionStatus = "1", execution = Execution { 
                                                                            executionId = 75,
                                                                            issueKey = "LYNX-1002",
                                                                            issueId = 16101,
                                                                            status = NullTestStatus,
                                                                            summary = "summaryBlah",
                                                                            executedOn = "02/May/13 12:14 PM",
                                                                            executedBy = "executedByDisplayBlah",
                                                                            comment = "commentBlah" }}
        s = [r|{"summary": "summaryBlah",
                "id": 75, 
                "executedOn": "02/May/13 12:14 PM",
                "cycleName": "cycleNameBlah", 
                "cycleID": 2,
                "label": "LxSensor", 
                "issueId": 16101,
                "versionName": "Unscheduled", 
                "executedByDisplay": "executedByDisplayBlah", 
                "executionStatus": "1",
                "projectID": 10500, 
                "htmlComment": "commentBlah",
                "executedBy": "handre.stolp", 
                "component": "", 
                "versionID": -1,
                "issueKey": "LYNX-1002", 
                "comment": ""}
            |]


-----------------------------------
-- Cycle_
-----------------------------------
type StatusMap_ = M.Map String StatusDef_
instance Out StatusMap_ where
    doc  = GP.docList . M.toList
    docPrec _  = GP.docList . M.toList

data Cycle_ = Cycle_ {statusDefs :: StatusMap_, executions :: [ExecutionDef_], totalExecutions :: Int} deriving (Eq, Show, Read, Generic)
instance Out Cycle_

instance AS.FromJSON Cycle_ where
    parseJSON (AS.Object v) = do
        statusDefs' <- v AS..: "status"
        executions' <- v AS..: "executions"
        totalExecutions' <- v AS..: "recordsCount"
        return Cycle_ {executions = executions', statusDefs = statusDefs', totalExecutions = totalExecutions'}
    parseJSON a = AS.typeMismatch "Cycle_" a

_executionsFromCycle :: Cycle_ -> [Execution]
_executionsFromCycle Cycle_ {..} = map toS executions
    where
        toS ExecutionDef_{..} = execution {status =  fromMaybe NullTestStatus $ _testStatusFromDef <$> M.lookup executionStatus statusDefs}

decodeCycleExecutions :: ByteString -> Either String ([Execution], Int)
decodeCycleExecutions  = fmap mkTpl . AS.eitherDecode where  mkTpl c = (_executionsFromCycle c, totalExecutions c)

case_decodeCycleSchedules ::  Assertion
case_decodeCycleSchedules = Right expected @=? AS.eitherDecode s
    where
        expected =  Cycle_ 
                    { statusDefs = M.fromList [("1", StatusDef_ {_ident = 1, _desc = "blah", _name="PASS"}), ("2", StatusDef_ {_ident = 2, _desc = "blah", _name="FAIL"})],
                      executions = [ExecutionDef_ {executionStatus = "1", execution = Execution {
                                                                                            executionId = 75,
                                                                                            issueKey = "LYNX-1002",
                                                                                            issueId = 16101,
                                                                                            status = NullTestStatus,
                                                                                            summary = "summaryBlah1",
                                                                                            executedOn = "executedOnBlah1",
                                                                                            executedBy = "executedByDisplayBlah1",
                                                                                            comment = "htmlCommentBlah1" }},
                                    ExecutionDef_ {executionStatus = "2", execution = Execution {
                                                                                            executionId = 76,
                                                                                            issueKey = "LYNX-992",
                                                                                            issueId = 16023,
                                                                                            status = NullTestStatus,
                                                                                            summary = "summaryBlah2",
                                                                                            executedOn = "executedOnBlah2",
                                                                                            executedBy = "executedByDisplayBlah2",
                                                                                            comment = "htmlCommentBlah2" }}],
                      totalExecutions = 34
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
                            "issueId": 16101,
                            "executedByDisplay": "executedByDisplayBlah1",
                            "executionStatus": "1",
                            "projectID": 10500,
                            "htmlComment": "htmlCommentBlah1",
                            "executedBy": "executedByBlah",
                            "component": "",
                            "versionID": -1,
                            "issueKey": "LYNX-1002",
                            "id": 75,
                            "comment": "commentBlah1"},
                           {"summary": "summaryBlah2",
                            "executedOn": "executedOnBlah2",
                            "cycleName": "cycleNameBlah2",
                            "cycleID": 2,
                            "label": "labelBlah2",
                            "issueId": 16023,
                            "versionName": "versionNameBlah2",
                            "issueId": 16023,
                            "executedByDisplay": "executedByDisplayBlah2",
                            "executionStatus": "2",
                            "projectID": 10500,
                            "htmlComment": "htmlCommentBlah2",
                            "executedBy": "executedByBlah2",
                            "component": "",
                            "versionID": -1,
                            "issueKey": "LYNX-992",
                            "id": 76,
                            "comment": "commentBlah2"}],
                "currentlySelectedExecutionId": "", 
                "recordsCount": 34
                }
            |]


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
                                -- should maybe rather use the HTML step, data and result
                                v AS..:? "step" AS..!= "" <*>
                                v AS..:? "data" AS..!= "" <*>
                                v AS..:? "result" AS..!= "" 
    parseJSON a = AS.typeMismatch "TestStepInfo" a

decodeTestStepsResponse :: ByteString -> Either String [TestStepInfo]
decodeTestStepsResponse s = either (\l -> Left (l ++ unpack s)) Right $ AS.eitherDecode s

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
            stepResComment' <- v AS..:? "htmlComment" AS..!= ""
            return TestStepResult {stepResId = stepResId', stepResInfoId = stepResInfoId', stepResStatus = toS status executionStatus, stepResComment = stepResComment'}
            where
                toS :: String -> [StatusDef_] -> TestStatus
                toS s ds = case find ((read s ==) . _ident) ds of
                            Just d -> _testStatusFromDef d
                            Nothing -> NullTestStatus
    parseJSON a = AS.typeMismatch "TestStepResult" a

decodeTestStepResultsResponse :: ByteString -> Either String [TestStepResult]
decodeTestStepResultsResponse = AS.eitherDecode 

case_decodeTestStepResult ::  Assertion
case_decodeTestStepResult = Right expected @=? AS.eitherDecode s
    where
        expected =  TestStepResult 160 226 TestPass  "htmlCommentBlah"
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



-------------------------------------------------
-- Project test cycles
-------------------------------------------------
data Cycle = Cycle
    {
        cycleId :: Int,
        cycleName :: String,
        cycleDesc :: String
    }deriving (Eq, Show, Read, Generic)

instance Out Cycle

instance AS.FromJSON Cycle where
    parseJSON (AS.Object v) = Cycle <$> v AS..:? "id" AS..!= 0 <*> v AS..: "name" <*> v AS..: "description"
    parseJSON a = AS.typeMismatch "Cycle" a

newtype ProjectCycleElem_ = ProjectCycleElem_ {_projCycleElem :: Maybe Cycle}
instance AS.FromJSON ProjectCycleElem_ where
    parseJSON o@(AS.Object _) =  ProjectCycleElem_ . Just <$> AS.parseJSON o
    parseJSON _ = pure $ ProjectCycleElem_ Nothing

decodeProjectCyclesResponse :: ByteString -> Either String [Cycle]
decodeProjectCyclesResponse = fmap (map cmb . filter (isJust . _projCycleElem . snd) . M.toList) . decodeMap
    where
        decodeMap :: ByteString -> Either String (M.Map String ProjectCycleElem_)
        decodeMap = AS.eitherDecode
        cmb :: (String, ProjectCycleElem_) -> Cycle
        cmb (s, ProjectCycleElem_ (Just c)) = c {cycleId = read s}
        cmb _ = error "decodeProjectCyclesResponse - should not be here"

case_decodeProjectCycles ::  Assertion
case_decodeProjectCycles = Right expected @=? decodeProjectCyclesResponse s
    where
        expected =  [ Cycle {cycleId = -1, cycleName = "Ad hoc", cycleDesc = ""}
                    , Cycle {cycleId = 1, cycleName = "Sprint 2 Dry run", cycleDesc = ""}
                    , Cycle {cycleId = 2, cycleName = "Sprint 2 Demonstrations", cycleDesc = "Tests demonstrating the outcomes of sprint 2."}
                    ]
        s = [r|{"-1": {"totalSchedules": 35, "startDate": "", "totalExecutions": 31, "description": "", "environment": "",
                        "executionSummaries": {"executionSummary": [{"count": 4, "statusKey": -1, "statusName": "UNEXECUTED", "statusColor": "#A0A0A0", "statusDescription": "The test has not yet been executed."},
                                                                    {"count": 30, "statusKey": 1, "statusName": "PASS", "statusColor": "#75B000", "statusDescription": "Test was executed and passed successfully."},
                                                                    {"count": 0, "statusKey": 2, "statusName": "FAIL", "statusColor": "#CC3300", "statusDescription": "Test was executed and failed."},
                                                                    {"count": 1, "statusKey": 3, "statusName": "WIP", "statusColor": "#F2B000", "statusDescription": "Test execution is a work-in-progress."},
                                                                    {"count": 0, "statusKey": 4, "statusName": "BLOCKED", "statusColor": "#6693B0", "statusDescription": "The test execution of this test was blocked for some reason."}]},
                        "name": "Ad hoc", "versionId": -1, "endDate": "", "started": "", "build": ""},
                "1": {"totalSchedules": 34, "startDate": "24/Apr/13", "totalExecutions": 34, "description": "", "environment": "",
                    "executionSummaries": {"executionSummary": [{"count": 0, "statusKey": -1, "statusName": "UNEXECUTED", "statusColor": "#A0A0A0", "statusDescription": "The test has not yet been executed."},
                                                                {"count": 34, "statusKey": 1, "statusName": "PASS", "statusColor": "#75B000", "statusDescription": "Test was executed and passed successfully."},
                                                                {"count": 0, "statusKey": 2, "statusName": "FAIL", "statusColor": "#CC3300", "statusDescription": "Test was executed and failed."},
                                                                {"count": 0, "statusKey": 3, "statusName": "WIP", "statusColor": "#F2B000", "statusDescription": "Test execution is a work-in-progress."},
                                                                {"count": 0, "statusKey": 4, "statusName": "BLOCKED", "statusColor": "#6693B0", "statusDescription": "The test execution of this test was blocked for some reason."}]},
                    "name": "Sprint 2 Dry run", "versionId": -1, "endDate": "28/May/13", "started": "true", "build": ""},
                "2": {"totalSchedules": 34, "startDate": "02/May/13", "totalExecutions": 18, "description": "Tests demonstrating the outcomes of sprint 2.", "environment": "",
                    "executionSummaries": {"executionSummary": [{"count": 16, "statusKey": -1, "statusName": "UNEXECUTED", "statusColor": "#A0A0A0", "statusDescription": "The test has not yet been executed."},
                                                                {"count": 15, "statusKey": 1, "statusName": "PASS", "statusColor": "#75B000", "statusDescription": "Test was executed and passed successfully."},
                                                                {"count": 3, "statusKey": 2, "statusName": "FAIL", "statusColor": "#CC3300", "statusDescription": "Test was executed and failed."},
                                                                {"count": 0, "statusKey": 3, "statusName": "WIP", "statusColor": "#F2B000", "statusDescription": "Test execution is a work-in-progress."},
                                                                {"count": 0, "statusKey": 4, "statusName": "BLOCKED", "statusColor": "#6693B0", "statusDescription": "The test execution of this test was blocked for some reason."}]},
                    "name": "Sprint 2 Demonstrations", "versionId": -1, "endDate": "02/May/13", "started": "true", "build": ""},
                "offsetCount": 1, "recordsCount": 2}
            |]
-------------------------------------------------
-- Debug main
------------------------------------------------
-- main :: IO ()
-- main = defaultMain [zephyrJsonTestGroup]
