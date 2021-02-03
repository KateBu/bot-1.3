module Tests.RepeatMsgs where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.ProcMsgs.Common as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testsRepeatMsg :: [Test]
testsRepeatMsg =
  [ testRepeatMsg0,
    testRepeatMsg1,
    testRepeatMsg2,
    testRepeatMsg3
  ]

testRepeatMsg0 :: Test
testRepeatMsg0 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg0"
        actualRepeatMsg0
        expectedRepeatMsg0
    )

actualRepeatMsg0 :: Maybe Config.Config
actualRepeatMsg0 =
  Env.config
    <$> Logic.repeatMsg
      TestData.cmnMsg1
      0
      TestData.servicesTel1
      TestData.testEnvTelegram

expectedRepeatMsg0 :: Maybe Config.Config
expectedRepeatMsg0 = pure $ Env.config TestData.testEnvTelegram

testRepeatMsg1 :: Test
testRepeatMsg1 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg1"
        expectedRepeatMsg1
        actualRepeatMsg1
    )

actualRepeatMsg1 :: Maybe Config.Config
actualRepeatMsg1 =
  Env.config
    <$> Logic.repeatMsg
      TestData.cmnMsg1
      1
      TestData.servicesTel1
      TestData.testEnvTelegram

expectedRepeatMsg1 :: Maybe Config.Config
expectedRepeatMsg1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testRepeatMsg2 :: Test
testRepeatMsg2 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg2"
        expectedRepeatMsg2
        actualRepeatMsg2
    )

actualRepeatMsg2 :: Maybe Config.Config
actualRepeatMsg2 =
  Env.config
    <$> Logic.repeatMsg
      TestData.cmnMsg2
      2
      TestData.servicesVk1
      TestData.testEnvVK

expectedRepeatMsg2 :: Maybe Config.Config
expectedRepeatMsg2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testRepeatMsg3 :: Test
testRepeatMsg3 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg3"
        expectedRepeatMsg3
        actualRepeatMsg3
    )

actualRepeatMsg3 :: Maybe Config.Config
actualRepeatMsg3 =
  Env.config
    <$> Logic.repeatMsg
      TestData.cmnMsg3
      5
      TestData.servicesVk1
      TestData.testEnvVK

expectedRepeatMsg3 :: Maybe Config.Config
expectedRepeatMsg3 = Env.config <$> Env.eSetOffset TestData.testEnvVK 43
