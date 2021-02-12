module Tests.ProcessMsgsCommon where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.Functions.Common as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessCommonMsgs :: [Test]
testProcessCommonMsgs = [testProcessCommonMsgs1, testProcessCommonMsgs2, testProcessCommonMsgs3]

testProcessCommonMsgs1 :: Test
testProcessCommonMsgs1 =
  TestCase
    ( assertEqual
        "process common message without errors"
        expectedProcessCommonMsgs1
        actualProcessCommonMsgs1
    )

actualProcessCommonMsgs1 :: Maybe Config.Config
actualProcessCommonMsgs1 =
  Env.config
    <$> Logic.processCommonMsgs
      TestData.testEnvTelegram
      TestData.cmnMsg4

expectedProcessCommonMsgs1 :: Maybe Config.Config
expectedProcessCommonMsgs1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testProcessCommonMsgs2 :: Test
testProcessCommonMsgs2 =
  TestCase
    ( assertEqual
        "process common message without errors"
        expectedProcessCommonMsgs2
        actualProcessCommonMsgs2
    )

actualProcessCommonMsgs2 :: Maybe Config.Config
actualProcessCommonMsgs2 =
  Env.config
    <$> Logic.processCommonMsgs
      TestData.testEnvVK
      TestData.cmnMsg5

expectedProcessCommonMsgs2 :: Maybe Config.Config
expectedProcessCommonMsgs2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 16

testProcessCommonMsgs3 :: Test
testProcessCommonMsgs3 =
  TestCase
    ( assertEqual
        "process common message without errors"
        Nothing
        actualProcessCommonMsgs3
    )

actualProcessCommonMsgs3 :: Maybe Config.Config
actualProcessCommonMsgs3 =
  Env.config
    <$> Logic.processCommonMsgs
      TestData.testEnvVK
      TestData.cmnMsg1
