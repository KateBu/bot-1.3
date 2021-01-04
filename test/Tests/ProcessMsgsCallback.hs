module Tests.ProcessMsgsCallback where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 
import qualified Logic.PureStructs as PureStructs 

testProcessMsgsCallback :: [Test]
testProcessMsgsCallback = [testProcessMsgsCallback1, testProcessMsgsCallback2
    , testProcessMsgsCallback3, testProcessMsgsCallback4]

testProcessMsgsCallback1 :: Test 
testProcessMsgsCallback1 = TestCase (assertEqual "Logic.testProcessMsgsCallback1"
    expectedProcessMsgsCallback1 
    actualProcessMsgsCallback1) 

actualProcessMsgsCallback1 :: Maybe (Either Logger.LogMessage Config.Config) 
actualProcessMsgsCallback1 = Logic.processMsgsCallback 
    TestData.testConfigTelegram
    5
    TestData.testFunction1
    TestData.cbMsg2 
    PureStructs.rep5
    11

expectedProcessMsgsCallback1 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedProcessMsgsCallback1 = pure . pure $ Config.configSetOffset newConfig 1 where 
    newConfig = Config.setUserRepeat TestData.testConfigTelegram 11 5 

testProcessMsgsCallback2 :: Test 
testProcessMsgsCallback2 = TestCase (assertEqual "Logic.testProcessMsgsCallback2"
    expectedProcessMsgsCallback2 
    actualProcessMsgsCallback2) 

actualProcessMsgsCallback2 :: Maybe (Either Logger.LogMessage Config.Config) 
actualProcessMsgsCallback2 = Logic.processMsgsCallback 
    TestData.testConfigTelegram
    3
    TestData.testFunction1
    TestData.cbMsg5 
    PureStructs.rep3
    42

expectedProcessMsgsCallback2 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedProcessMsgsCallback2 = pure . pure $ Config.configSetOffset newConfig 1 where 
    newConfig = Config.setUserRepeat TestData.testConfigTelegram 42 3 

testProcessMsgsCallback3 :: Test 
testProcessMsgsCallback3 = TestCase (assertEqual "Logic.testProcessMsgsCallback3"
    expectedProcessMsgsCallback3 
    actualProcessMsgsCallback3) 

actualProcessMsgsCallback3 :: Maybe (Either Logger.LogMessage Config.Config) 
actualProcessMsgsCallback3 = Logic.processMsgsCallback 
    TestData.testConfigVK
    3
    TestData.testFunction1
    TestData.cbMsg5 
    PureStructs.rep2
    42

expectedProcessMsgsCallback3 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedProcessMsgsCallback3 = pure . pure $ Config.configSetOffset newConfig 1 where 
    newConfig = Config.setUserRepeat TestData.testConfigVK 42 2 

testProcessMsgsCallback4 :: Test 
testProcessMsgsCallback4 = TestCase (assertEqual "Logic.testProcessMsgsCallback3"
    expectedProcessMsgsCallback4 
    actualProcessMsgsCallback4) 

actualProcessMsgsCallback4 :: Maybe (Either Logger.LogMessage Config.Config) 
actualProcessMsgsCallback4 = Logic.processMsgsCallback 
    TestData.testConfigVK
    3
    TestData.testFunction1
    TestData.cbMsg5 
    "PureStructs.rep1"
    22

expectedProcessMsgsCallback4 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedProcessMsgsCallback4 = pure . pure $ Config.configSetOffset newConfig 1 where 
    newConfig = Config.setUserRepeat TestData.testConfigVK 22 5 