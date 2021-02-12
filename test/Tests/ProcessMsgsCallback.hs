module Tests.ProcessMsgsCallback where

import qualified API.PureStructs.Exports as PureStructs
import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.Functions.Callback as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessCallbackMsgs :: [Test]
testProcessCallbackMsgs =
  [testProcessCallbackMsgs1, testProcessCallbackMsgs2, testProcessCallbackMsgs3]

testProcessCallbackMsgs1 :: Test
testProcessCallbackMsgs1 =
  TestCase
    ( assertEqual
        "process callback message without errors"
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

expectedProcessCallbackMsgs1 :: Maybe Config.Config
expectedProcessCallbackMsgs1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testProcessCallbackMsgs2 :: Test
testProcessCallbackMsgs2 =
  TestCase
    ( assertEqual
        "process callback message with errors"
        Nothing
        actualProcessCallbackMsgs2
    )

actualProcessCallbackMsgs2 :: Maybe Config.Config
actualProcessCallbackMsgs2 =
  Env.config
    <$> Logic.processCallbackMsgs
      TestData.testEnvTelegram
      TestData.cbMsg1
      PureStructs.setRepeat5

testProcessCallbackMsgs3 :: Test
testProcessCallbackMsgs3 =
  TestCase
    ( assertEqual
        "process callback message with wrong callback data"
        expectedProcessCallbackMsgs3
        actualProcessCallbackMsgs3
    )

actualProcessCallbackMsgs3 :: Maybe Config.Config
actualProcessCallbackMsgs3 =
  Env.config
    <$> Logic.processCallbackMsgs
      TestData.testEnvVK
      TestData.cbMsg3
      "PureStructs.setRepeat5"

expectedProcessCallbackMsgs3 :: Maybe Config.Config
expectedProcessCallbackMsgs3 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3
