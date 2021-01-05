module Tests.ConfigSetOffset where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified TestData as TestData
import qualified Config.Config as Config 

testConfigSetOffset :: [Test]
testConfigSetOffset = [testConfigSetOffset1, testConfigSetOffset2]

testConfigSetOffset1 :: Test 
testConfigSetOffset1 = TestCase (assertEqual "Config.configSetOffset1"
    expectedConfigSetOffset1
    actualConfigSetOffset1)

expectedConfigSetOffset1 :: Config.Config 
expectedConfigSetOffset1 = TestData.testConfigTelegram {Config.botType = Config.TBot (Config.Telegram "token" 5)}

actualConfigSetOffset1 :: Config.Config
actualConfigSetOffset1 = Config.configSetOffset TestData.testConfigTelegram 5 

testConfigSetOffset2 :: Test 
testConfigSetOffset2 = TestCase (assertEqual "Config.configSetOffset2"
    expectedConfigSetOffset2
    actualConfigSetOffset2)

expectedConfigSetOffset2 :: Config.Config 
expectedConfigSetOffset2 = TestData.testConfigVK {Config.botType = Config.VKBot (Config.VK "token" 11 "key" "server" 42)}

actualConfigSetOffset2 :: Config.Config
actualConfigSetOffset2 = Config.configSetOffset TestData.testConfigVK 42 