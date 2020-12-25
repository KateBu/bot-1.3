module Tests where

import Test.HUnit 
import qualified Logic.Logic as Logic 
import qualified Logic.PureStructs as PureStructs 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 
import qualified Logger.LoggerMsgs as LoggerMsgs

testsRepeatMsg :: [Test]
testsRepeatMsg = [testRepeatMsg0, testRepeatMsg1, testRepeatMsg2, testRepeatMsg3]

testRepeatMsg0 :: Test 
testRepeatMsg0 = TestCase (assertEqual "Logic.repeatMsg0"
    expectedRepeatMsg0 
    actualRepeatMsg0) 

actualRepeatMsg0 :: [Either Logger.LogMessage Config.Config]
actualRepeatMsg0 = Logic.repeatMsg 
    TestData.testConfigVK 
    TestData.newHelp 
    TestData.cbMsg1 
    0 
    TestData.testFunction 

expectedRepeatMsg0 :: [Either Logger.LogMessage Config.Config] 
expectedRepeatMsg0 = [Right TestData.testConfigVK]

testRepeatMsg1 :: Test 
testRepeatMsg1 = TestCase (assertEqual "Logic.repeatMsg1"
    expectedRepeatMsg1 
    actualRepeatMsg1) 

actualRepeatMsg1 :: [Either Logger.LogMessage Config.Config]
actualRepeatMsg1 = Logic.repeatMsg 
    TestData.testConfigVK 
    TestData.newHelp
    TestData.cmnMsg1
    1 
    TestData.testFunction 

expectedRepeatMsg1 :: [Either Logger.LogMessage Config.Config] 
expectedRepeatMsg1 = [Right $ TestData.testConfigVK {Config.helpMessage = TestData.newHelp}]

testRepeatMsg2 :: Test 
testRepeatMsg2 = TestCase (assertEqual "Logic.repeatMsg2"
    expectedRepeatMsg2 
    actualRepeatMsg2) 

actualRepeatMsg2 :: [Either Logger.LogMessage Config.Config]
actualRepeatMsg2 = Logic.repeatMsg 
    TestData.testConfigTelegram
    TestData.newHelp
    TestData.commandMsg1
    5 
    TestData.testFunction 

expectedRepeatMsg2 :: [Either Logger.LogMessage Config.Config] 
expectedRepeatMsg2 = [Right $ TestData.testConfigTelegram {Config.helpMessage = TestData.newHelp}]

testRepeatMsg3 :: Test 
testRepeatMsg3 = TestCase (assertEqual "Logic.repeatMsg2"
    expectedRepeatMsg3
    actualRepeatMsg3) 

actualRepeatMsg3 :: [Either Logger.LogMessage Config.Config]
actualRepeatMsg3 = Logic.repeatMsg 
    TestData.testConfigTelegram
    TestData.newHelp
    TestData.emptyMsg1
    5 
    TestData.testFunction 

expectedRepeatMsg3 :: [Either Logger.LogMessage Config.Config] 
expectedRepeatMsg3 = [Left LoggerMsgs.testError]