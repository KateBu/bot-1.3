module Tests.ProcessMsgs_ where

import qualified Config.Config as Config
import qualified Exceptions.Exceptions as BotEx
import qualified Logic.Logic as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testProcessMsgs_ :: [Test]
testProcessMsgs_ =
  [ testProcessMsgs_1,
    testProcessMsgs_2,
    testProcessMsgs_3,
    testProcessMsgs_4,
    testProcessMsgs_5,
    testProcessMsgs_6,
    testProcessMsgs_7,
    testProcessMsgs_8,
    testProcessMsgs_9,
    testProcessMsgs_10
  ]

testProcessMsgs_1 :: Test
testProcessMsgs_1 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_1"
        expectedTestProcessMsgs_1
        actualTestProcessMsgs_1
    )

actualTestProcessMsgs_1 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_1 =
  Logic.processMsgs_
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    TestData.emptyMsg1

expectedTestProcessMsgs_1 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_1 = pure . pure $ Config.configSetOffset TestData.testConfigTelegram 1

testProcessMsgs_2 :: Test
testProcessMsgs_2 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_2"
        expectedTestProcessMsgs_2
        actualTestProcessMsgs_2
    )

actualTestProcessMsgs_2 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_2 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.emptyMsg2

expectedTestProcessMsgs_2 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_2 = pure . pure $ Config.configSetOffset TestData.testConfigVK 2

testProcessMsgs_3 :: Test
testProcessMsgs_3 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_3"
        expectedTestProcessMsgs_3
        actualTestProcessMsgs_3
    )

actualTestProcessMsgs_3 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_3 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.commandMsg1

expectedTestProcessMsgs_3 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_3 = pure . pure $ Config.configSetOffset TestData.testConfigVK 1

testProcessMsgs_4 :: Test
testProcessMsgs_4 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_4"
        expectedTestProcessMsgs_3
        actualTestProcessMsgs_3
    )

actualTestProcessMsgs_4 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_4 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.commandMsg5

expectedTestProcessMsgs_4 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_4 = pure . pure $ Config.configSetOffset TestData.testConfigVK 1

testProcessMsgs_5 :: Test
testProcessMsgs_5 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_5"
        expectedTestProcessMsgs_4
        actualTestProcessMsgs_4
    )

actualTestProcessMsgs_5 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_5 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.cbMsg1

expectedTestProcessMsgs_5 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_5 = Logic.processMsgsErr

testProcessMsgs_6 :: Test
testProcessMsgs_6 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_6"
        expectedTestProcessMsgs_5
        actualTestProcessMsgs_5
    )

actualTestProcessMsgs_6 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_6 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.cbMsg2

expectedTestProcessMsgs_6 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_6 = pure . pure $ Config.configSetOffset TestData.testConfigVK 1

testProcessMsgs_7 :: Test
testProcessMsgs_7 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_7"
        expectedTestProcessMsgs_7
        actualTestProcessMsgs_7
    )

actualTestProcessMsgs_7 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_7 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.cmnMsg1

expectedTestProcessMsgs_7 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_7 = Logic.processMsgsErr

testProcessMsgs_8 :: Test
testProcessMsgs_8 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_8"
        expectedTestProcessMsgs_8
        actualTestProcessMsgs_8
    )

actualTestProcessMsgs_8 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_8 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.cmnMsg3

expectedTestProcessMsgs_8 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_8 = pure . pure $ Config.configSetOffset TestData.testConfigVK 1

testProcessMsgs_9 :: Test
testProcessMsgs_9 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_9"
        expectedTestProcessMsgs_9
        actualTestProcessMsgs_9
    )

actualTestProcessMsgs_9 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_9 =
  Logic.processMsgs_
    TestData.testConfigVK
    5
    TestData.testFunction1
    TestData.cmnMsg5

expectedTestProcessMsgs_9 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_9 = pure . pure $ Config.configSetOffset TestData.testConfigVK 3

testProcessMsgs_10 :: Test
testProcessMsgs_10 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_10"
        expectedTestProcessMsgs_10
        actualTestProcessMsgs_10
    )

actualTestProcessMsgs_10 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_10 =
  Logic.processMsgs_
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    TestData.cmnMsg5

expectedTestProcessMsgs_10 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_10 = pure . pure $ Config.configSetOffset TestData.testConfigTelegram 1

testProcessMsgs_11 :: Test
testProcessMsgs_11 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_11"
        expectedTestProcessMsgs_11
        actualTestProcessMsgs_11
    )

actualTestProcessMsgs_11 :: Maybe (Either BotEx.BotException Config.Config)
actualTestProcessMsgs_11 =
  Logic.processMsgs_
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    TestData.cmnMsg4

expectedTestProcessMsgs_11 :: Maybe (Either BotEx.BotException Config.Config)
expectedTestProcessMsgs_11 = pure . pure $ Config.configSetOffset TestData.testConfigTelegram 3
