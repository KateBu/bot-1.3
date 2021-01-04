module Tests.ProcessMsgsCommon where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 

testProcessMsgsCommon :: [Test]
testProcessMsgsCommon = [testProcessMsgsCommon1, testProcessMsgsCommon2, testProcessMsgsCommon3, testProcessMsgsCommon4]

testProcessMsgsCommon1 :: Test 
testProcessMsgsCommon1 = TestCase (assertEqual "Logic.processMsgsCommon1"
    expectedProcessMsgsCommon1 
    actualProcessMsgsCommon1) 

actualProcessMsgsCommon1 :: Maybe (Either Logger.LogMessage Config.Config)
actualProcessMsgsCommon1 = Logic.processMsgsCommon 
    TestData.testConfigTelegram 
    5
    TestData.testFunction1 
    TestData.commandMsg1 
    11

expectedProcessMsgsCommon1 :: Maybe (Either Logger.LogMessage Config.Config)
expectedProcessMsgsCommon1 = pure $ Right $ Config.configSetOffset TestData.testConfigTelegram 1 

testProcessMsgsCommon2 :: Test 
testProcessMsgsCommon2 = TestCase (assertEqual "Logic.processMsgsCommon2"
    expectedProcessMsgsCommon2 
    actualProcessMsgsCommon2) 

actualProcessMsgsCommon2 :: Maybe (Either Logger.LogMessage Config.Config)
actualProcessMsgsCommon2 = Logic.processMsgsCommon 
    TestData.testConfigTelegram 
    5
    TestData.testFunction1 
    TestData.cmnMsg2
    22

expectedProcessMsgsCommon2 :: Maybe (Either Logger.LogMessage Config.Config)
expectedProcessMsgsCommon2 = pure $ Right $ Config.configSetOffset TestData.testConfigTelegram 2 

testProcessMsgsCommon3 :: Test 
testProcessMsgsCommon3 = TestCase (assertEqual "Logic.processMsgsCommon3"
    expectedProcessMsgsCommon3 
    actualProcessMsgsCommon3) 

actualProcessMsgsCommon3 :: Maybe (Either Logger.LogMessage Config.Config)
actualProcessMsgsCommon3 = Logic.processMsgsCommon 
    TestData.testConfigVK
    5
    TestData.testFunction1 
    TestData.cmnMsg2
    33

expectedProcessMsgsCommon3 :: Maybe (Either Logger.LogMessage Config.Config)
expectedProcessMsgsCommon3 = pure $ Right $ Config.configSetOffset TestData.testConfigVK 3 

testProcessMsgsCommon4 :: Test 
testProcessMsgsCommon4 = TestCase (assertEqual "Logic.processMsgsCommon3"
    expectedProcessMsgsCommon4 
    actualProcessMsgsCommon4) 

actualProcessMsgsCommon4 :: Maybe (Either Logger.LogMessage Config.Config)
actualProcessMsgsCommon4 = Logic.processMsgsCommon 
    TestData.testConfigVK
    5
    TestData.testFunction1 
    TestData.cmnMsg2
    10

expectedProcessMsgsCommon4 :: Maybe (Either Logger.LogMessage Config.Config)
expectedProcessMsgsCommon4 = pure $ Right $ Config.configSetOffset TestData.testConfigVK 3 

