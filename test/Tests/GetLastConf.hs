module Tests.GetLastConf where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 

testGetLastConf :: [Test]
testGetLastConf = [testGetLastConf1, testGetLastConf2, testGetLastConf3]

testGetLastConf1 :: Test 
testGetLastConf1 = TestCase (assertEqual "Logic.getLastConf1"
    expectedGetLastConf1
    actualGetLastConf1) 

actualGetLastConf1 :: Maybe (Either Logger.LogMessage Config.Config) 
actualGetLastConf1 = Logic.getLastConf 
    TestData.testConfigTelegram 
    []

expectedGetLastConf1 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedGetLastConf1 = (pure . pure) TestData.testConfigTelegram 

testGetLastConf2 :: Test 
testGetLastConf2 = TestCase (assertEqual "Logic.getLastConf12"
    expectedGetLastConf2
    actualGetLastConf2) 

actualGetLastConf2 :: Maybe (Either Logger.LogMessage Config.Config) 
actualGetLastConf2 = Logic.getLastConf 
    TestData.testConfigTelegram 
    [Right TestData.testConfigTelegram, Right TestData.testConfigVK]

expectedGetLastConf2 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedGetLastConf2 = (pure . pure) TestData.testConfigVK

testGetLastConf3 :: Test 
testGetLastConf3 = TestCase (assertEqual "Logic.getLastConf13"
    expectedGetLastConf3
    actualGetLastConf3) 

actualGetLastConf3 :: Maybe (Either Logger.LogMessage Config.Config) 
actualGetLastConf3 = Logic.getLastConf 
    TestData.testConfigTelegram 
    [Right TestData.testConfigTelegram, Right TestData.testConfigVK, Left $ Logger.LogMessage Logger.Error "error"]

expectedGetLastConf3 :: Maybe (Either Logger.LogMessage Config.Config) 
expectedGetLastConf3 = pure $ Left $ Logger.LogMessage Logger.Error "error"