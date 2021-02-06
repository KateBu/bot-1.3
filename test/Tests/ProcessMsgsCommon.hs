module Tests.ProcessMsgsCommon where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.ProcMsgs.Common as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessCommonMsgs :: [Test]
testProcessCommonMsgs = [testProcessCommonMsgs1, testProcessCommonMsgs2]

testProcessCommonMsgs1 :: Test
testProcessCommonMsgs1 =
  TestCase
    ( assertEqual
        "Logic.processCommonMsgs1"
        expectedProcessCommonMsgs1
        actualProcessCommonMsgs1
    )

actualProcessCommonMsgs1 :: Maybe Config.Config
actualProcessCommonMsgs1 =
  Env.config
    <$> Logic.processCommonMsgs
      TestData.testEnvTelegram
      TestData.cmnMsg4
      11

expectedProcessCommonMsgs1 :: Maybe Config.Config
expectedProcessCommonMsgs1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testProcessCommonMsgs2 :: Test
testProcessCommonMsgs2 =
  TestCase
    ( assertEqual
        "Logic.processCommonMsgs2"
        expectedProcessCommonMsgs2
        actualProcessCommonMsgs2
    )

actualProcessCommonMsgs2 :: Maybe Config.Config
actualProcessCommonMsgs2 =
  Env.config
    <$> Logic.processCommonMsgs
      TestData.testEnvVK
      TestData.cmnMsg5
      42

expectedProcessCommonMsgs2 :: Maybe Config.Config
expectedProcessCommonMsgs2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 16
