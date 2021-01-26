module Tests.ConfigGetUid where

import qualified Config.Config as Config
import Test.HUnit ( assertBool, Test(TestCase) ) 
import qualified TestData 

testConfigGetUid :: [Test]
testConfigGetUid = [testConfigGetUid1, testConfigGetUid2]

testConfigGetUid1 :: Test
testConfigGetUid1 =
  TestCase
    ( assertBool
        "Config.configGetUid1"
        testConfigGetUid1'        
    )

testConfigGetUid1' :: Bool 
testConfigGetUid1' = 10 == Config.configGetUid TestData.testConfigTelegram

testConfigGetUid2 :: Test
testConfigGetUid2 =
  TestCase
    ( assertBool
        "Config.configGetUid2"
        testConfigGetUid2'
    )

testConfigGetUid2' :: Bool
testConfigGetUid2' = 5 == Config.configGetUid TestData.testConfigVK 