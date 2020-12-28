module Tests where

import Test.HUnit 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 

testsRepeatMsg :: [Test]
testsRepeatMsg = [testRepeatMsg0, testRepeatMsg1, testRepeatMsg2, testRepeatMsg3]

testRepeatMsg0 :: Test 
testRepeatMsg0 = TestCase (assertEqual "Logic.repeatMsg0"
    expectedRepeatMsg0 
    actualRepeatMsg0) 

actualRepeatMsg0 :: Maybe (Either Logger.LogMessage Config.Config)
actualRepeatMsg0 = Logic.repeatMsg 
    TestData.testConfigVK 
    TestData.newHelp 
    TestData.cbMsg1 
    0 
    TestData.testFunction0 

expectedRepeatMsg0 :: Maybe (Either Logger.LogMessage Config.Config)
expectedRepeatMsg0 = pure $ Right TestData.testConfigVK

testRepeatMsg1 :: Test 
testRepeatMsg1 = TestCase (assertEqual "Logic.repeatMsg1"
    expectedRepeatMsg1 
    actualRepeatMsg1) 

actualRepeatMsg1 :: Maybe (Either Logger.LogMessage Config.Config)
actualRepeatMsg1 = Logic.repeatMsg 
    TestData.testConfigVK 
    TestData.newHelp
    TestData.cmnMsg1
    1 
    TestData.testFunction0 

expectedRepeatMsg1 :: Maybe (Either Logger.LogMessage Config.Config)
expectedRepeatMsg1 = pure $ Right $ TestData.testConfigVK {Config.helpMessage = TestData.newHelp}

testRepeatMsg2 :: Test 
testRepeatMsg2 = TestCase (assertEqual "Logic.repeatMsg2"
    expectedRepeatMsg2 
    actualRepeatMsg2) 

actualRepeatMsg2 :: Maybe (Either Logger.LogMessage Config.Config)
actualRepeatMsg2 = Logic.repeatMsg 
    TestData.testConfigTelegram
    TestData.newHelp
    TestData.commandMsg1
    5 
    TestData.testFunction0 

expectedRepeatMsg2 :: Maybe (Either Logger.LogMessage Config.Config)
expectedRepeatMsg2 = pure $ Right $ TestData.testConfigTelegram {Config.helpMessage = TestData.newHelp}

testRepeatMsg3 :: Test 
testRepeatMsg3 = TestCase (assertEqual "Logic.repeatMsg2"
    expectedRepeatMsg3
    actualRepeatMsg3) 

actualRepeatMsg3 :: Maybe (Either Logger.LogMessage Config.Config)
actualRepeatMsg3 = Logic.repeatMsg 
    TestData.testConfigTelegram
    TestData.newHelp
    TestData.emptyMsg1
    5 
    TestData.testFunction0 

expectedRepeatMsg3 :: Maybe (Either Logger.LogMessage Config.Config)
expectedRepeatMsg3 = Nothing