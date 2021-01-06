module Tests.RepeatMsgs where

import qualified Config.Config as Config
import qualified Exceptions.Exceptions as BotEx
import qualified Logic.Logic as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testsRepeatMsg :: [Test]
testsRepeatMsg = [testRepeatMsg0, testRepeatMsg1, testRepeatMsg2, testRepeatMsg3, testRepeatMsg4]

testRepeatMsg0 :: Test
testRepeatMsg0 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg0"
        expectedRepeatMsg0
        actualRepeatMsg0
    )

actualRepeatMsg0 :: Maybe (Either BotEx.BotException Config.Config)
actualRepeatMsg0 =
  Logic.repeatMsg
    TestData.newHelp
    TestData.cbMsg1
    0
    TestData.testFunction0
    TestData.testConfigVK

expectedRepeatMsg0 :: Maybe (Either BotEx.BotException Config.Config)
expectedRepeatMsg0 = pure $ Right TestData.testConfigVK

testRepeatMsg1 :: Test
testRepeatMsg1 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg1"
        expectedRepeatMsg1
        actualRepeatMsg1
    )

actualRepeatMsg1 :: Maybe (Either BotEx.BotException Config.Config)
actualRepeatMsg1 =
  Logic.repeatMsg
    TestData.newHelp
    TestData.cmnMsg1
    1
    TestData.testFunction0
    TestData.testConfigVK

expectedRepeatMsg1 :: Maybe (Either BotEx.BotException Config.Config)
expectedRepeatMsg1 = pure $ Right $ TestData.testConfigVK {Config.helpMessage = TestData.newHelp}

testRepeatMsg2 :: Test
testRepeatMsg2 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg2"
        expectedRepeatMsg2
        actualRepeatMsg2
    )

actualRepeatMsg2 :: Maybe (Either BotEx.BotException Config.Config)
actualRepeatMsg2 =
  Logic.repeatMsg
    TestData.newHelp
    TestData.commandMsg1
    5
    TestData.testFunction0
    TestData.testConfigTelegram

expectedRepeatMsg2 :: Maybe (Either BotEx.BotException Config.Config)
expectedRepeatMsg2 = pure $ Right $ TestData.testConfigTelegram {Config.helpMessage = TestData.newHelp}

testRepeatMsg3 :: Test
testRepeatMsg3 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg3"
        expectedRepeatMsg3
        actualRepeatMsg3
    )

actualRepeatMsg3 :: Maybe (Either BotEx.BotException Config.Config)
actualRepeatMsg3 =
  Logic.repeatMsg
    TestData.newHelp
    TestData.emptyMsg1
    5
    TestData.testFunction0
    TestData.testConfigTelegram

expectedRepeatMsg3 :: Maybe (Either BotEx.BotException Config.Config)
expectedRepeatMsg3 = Nothing

testRepeatMsg4 :: Test
testRepeatMsg4 =
  TestCase
    ( assertEqual
        "Logic.repeatMsg2"
        expectedRepeatMsg4
        actualRepeatMsg4
    )

actualRepeatMsg4 :: Maybe (Either BotEx.BotException Config.Config)
actualRepeatMsg4 =
  Logic.repeatMsg
    3
    TestData.commandMsg1
    5
    TestData.testFunction1
    TestData.testConfigTelegram

expectedRepeatMsg4 :: Maybe (Either BotEx.BotException Config.Config)
expectedRepeatMsg4 = pure $ Right $ Config.configSetOffset TestData.testConfigTelegram 5
