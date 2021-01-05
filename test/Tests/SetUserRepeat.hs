module Tests.SetUserRepeat where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified Data.Map as Map 
import qualified TestData as TestData
import qualified Config.Config as Config 


testSetUserRepeat :: [Test]
testSetUserRepeat = [testSetUserRepeat1, testSetUserRepeat2, testSetUserRepeat3]

testSetUserRepeat1 :: Test 
testSetUserRepeat1 = TestCase (assertEqual "Config.setUserRepeat1"
    expectedSetUserRepeat1
    actualSetUserRepeat1)

expectedSetUserRepeat1 :: Config.Config 
expectedSetUserRepeat1 = TestData.testConfigTelegram {Config.users = Map.fromList [(11,5), (22,2), (33,3)]}

actualSetUserRepeat1 :: Config.Config   
actualSetUserRepeat1 = Config.setUserRepeat TestData.testConfigTelegram 11 5 

testSetUserRepeat2 :: Test 
testSetUserRepeat2 = TestCase (assertEqual "Config.setUserRepeat2"
    expectedSetUserRepeat2
    actualSetUserRepeat2)

expectedSetUserRepeat2 :: Config.Config 
expectedSetUserRepeat2 = TestData.testConfigVK {Config.users = Map.fromList [(11,1), (22,4), (33,3)]}

actualSetUserRepeat2 :: Config.Config   
actualSetUserRepeat2 = Config.setUserRepeat TestData.testConfigVK 22 4 

testSetUserRepeat3 :: Test 
testSetUserRepeat3 = TestCase (assertEqual "Config.setUserRepeat3"
    expectedSetUserRepeat3
    actualSetUserRepeat3)

expectedSetUserRepeat3 :: Config.Config 
expectedSetUserRepeat3 = TestData.testConfigVK {Config.users = Map.fromList [(11,1), (22,2), (33,3), (42,2)]}

actualSetUserRepeat3 :: Config.Config   
actualSetUserRepeat3 = Config.setUserRepeat TestData.testConfigVK 42 2 