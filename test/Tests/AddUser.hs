module Tests.AddUser where

import Test.HUnit ( assertEqual, Test(TestCase) ) 
import qualified TestData as TestData
import qualified Config.Config as Config 
import qualified Data.Map as Map 

testAddUser :: [Test]
testAddUser = [testAddUser1, testAddUser2]

testAddUser1 :: Test 
testAddUser1 = TestCase (assertEqual "Config.addUser1"
    expectedAddUser1
    actualAddUser1)

expectedAddUser1 :: Config.Config 
expectedAddUser1 = TestData.testConfigTelegram {Config.users = Map.fromList [(11,1), (22,2), (33,3), (15,3)]}

actualAddUser1 :: Config.Config  
actualAddUser1 = Config.addUser 15 3 TestData.testConfigTelegram 

testAddUser2 :: Test 
testAddUser2 = TestCase (assertEqual "Config.addUSer2"
    expectedAddUser2
    actualAddUser2)

expectedAddUser2 :: Config.Config 
expectedAddUser2 = TestData.testConfigVK {Config.users = Map.fromList [(11,1), (22,2), (33,3), (123,42)]}

actualAddUser2 :: Config.Config  
actualAddUser2 = Config.addUser 123 42 TestData.testConfigVK