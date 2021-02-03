module Tests.ProcessMsgsCommon where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.ProcMsgs.Common as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessMsgsCommon :: [Test]
testProcessMsgsCommon = [testProcessMsgsCommon1, testProcessMsgsCommon2]

testProcessMsgsCommon1 :: Test
testProcessMsgsCommon1 =
  TestCase
    ( assertEqual
        "Logic.processMsgsCommon1"
        expectedProcessMsgsCommon1
        actualProcessMsgsCommon1
    )

actualProcessMsgsCommon1 :: Maybe Config.Config
actualProcessMsgsCommon1 =
  Env.config
    <$> Logic.processMsgsCommon
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.cmnMsg4
      11

expectedProcessMsgsCommon1 :: Maybe Config.Config
expectedProcessMsgsCommon1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testProcessMsgsCommon2 :: Test
testProcessMsgsCommon2 =
  TestCase
    ( assertEqual
        "Logic.processMsgsCommon2"
        expectedProcessMsgsCommon2
        actualProcessMsgsCommon2
    )

actualProcessMsgsCommon2 :: Maybe Config.Config
actualProcessMsgsCommon2 =
  Env.config
    <$> Logic.processMsgsCommon
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg5
      42

expectedProcessMsgsCommon2 :: Maybe Config.Config
expectedProcessMsgsCommon2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 16
