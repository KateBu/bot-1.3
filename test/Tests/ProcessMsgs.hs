module Tests.ProcessMsgs where

import Data.Maybe ( isJust )
import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Config.Config as Config 
import qualified Logic.PureStructs as PureStructs 
import qualified Exceptions.Exceptions as BotEx 

testProcessMsgs :: [Test]
testProcessMsgs = [testProcessMsgs1,testProcessMsgs2,testProcessMsgs3]

testProcessMsgs1 :: Test 
testProcessMsgs1 = TestCase (assertEqual "Logic.testProcessMsgs1"
    expectedTestProcessMsgs1
    actualTestProcessMsgs1) 

actualTestProcessMsgs1 :: Maybe (Either BotEx.BotException Config.Config) 
actualTestProcessMsgs1 = Logic.processMsgs
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    (Right TestData.allMessages)

expectedTestProcessMsgs1 :: Maybe (Either BotEx.BotException Config.Config) 
expectedTestProcessMsgs1 = Logic.processMsgsErr

withChid :: [PureStructs.PureMessage]
withChid = filter (isJust . PureStructs.mbChatID) TestData.commonMessages

testProcessMsgs2 :: Test 
testProcessMsgs2 = TestCase (assertEqual "Logic.testProcessMsgs2"
    expectedTestProcessMsgs2
    actualTestProcessMsgs2) 

actualTestProcessMsgs2 :: Maybe (Either BotEx.BotException Config.Config) 
actualTestProcessMsgs2 = Logic.processMsgs
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    (Right withChid)

expectedTestProcessMsgs2 :: Maybe (Either BotEx.BotException Config.Config) 
expectedTestProcessMsgs2 = pure . pure $ Config.configSetOffset TestData.testConfigTelegram 1

testProcessMsgs3 :: Test 
testProcessMsgs3 = TestCase (assertEqual "Logic.testProcessMsgs3"
    expectedTestProcessMsgs3
    actualTestProcessMsgs3) 

actualTestProcessMsgs3 :: Maybe (Either BotEx.BotException Config.Config) 
actualTestProcessMsgs3 = Logic.processMsgs
    TestData.testConfigVK
    5
    TestData.testFunction1
    (Right withChid)

expectedTestProcessMsgs3 :: Maybe (Either BotEx.BotException Config.Config) 
expectedTestProcessMsgs3 = pure . pure $ Config.configSetOffset TestData.testConfigVK 3