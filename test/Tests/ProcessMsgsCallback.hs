module Tests.ProcessMsgsCallback where

import qualified Config.Config as Config
import qualified Environment.Environment as Env
import qualified Logic.ProcMsgs.Callback as Logic
import qualified Logic.PureStructs as PureStructs
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testProcessMsgsCallback :: [Test]
testProcessMsgsCallback =
  [testProcessMsgsCallback1]

testProcessMsgsCallback1 :: Test
testProcessMsgsCallback1 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgsCallback1"
        expectedProcessMsgsCallback1
        actualProcessMsgsCallback1
    )

actualProcessMsgsCallback1 :: Maybe Config.Config
actualProcessMsgsCallback1 =
  Env.config
    <$> Logic.processMsgsCallback
      TestData.servicesTel1
      TestData.cbMsg2
      PureStructs.rep5
      11

expectedProcessMsgsCallback1 :: Maybe Config.Config
expectedProcessMsgsCallback1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3
