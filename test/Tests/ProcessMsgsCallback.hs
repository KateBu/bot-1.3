module Tests.ProcessMsgsCallback where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.ProcMsgs.Callback as Logic
import qualified Logic.PureStructs as PureStructs
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessCallbackMsgs :: [Test]
testProcessCallbackMsgs =
  [testProcessCallbackMsgs1]

testProcessCallbackMsgs1 :: Test
testProcessCallbackMsgs1 =
  TestCase
    ( assertEqual
        "Logic.testProcessCallbackMsgs1"
        expectedProcessCallbackMsgs1
        actualProcessCallbackMsgs1
    )

actualProcessCallbackMsgs1 :: Maybe Config.Config
actualProcessCallbackMsgs1 =
  Env.config
    <$> Logic.processCallbackMsgs
      TestData.testEnvTelegram
      TestData.cbMsg2
      PureStructs.setRepeat5
      11

expectedProcessCallbackMsgs1 :: Maybe Config.Config
expectedProcessCallbackMsgs1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3
