module Tests.DeleteUser where

import qualified Config.Config as Config
import qualified Data.Map as Map
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testDeleteUser :: [Test]
testDeleteUser = [testDeleteUser1, testDeleteUser2, testDeleteUser3]

testDeleteUser1 :: Test
testDeleteUser1 =
  TestCase
    ( assertEqual
        "Config.deleteUser1"
        expectedDeleteUser1
        actualDeleteUser1
    )

expectedDeleteUser1 :: Config.Config
expectedDeleteUser1 = TestData.testConfigTelegram {Config.users = Map.fromList [(22, 2), (33, 3)]}

actualDeleteUser1 :: Config.Config
actualDeleteUser1 = Config.deleteUser 11 TestData.testConfigTelegram

testDeleteUser2 :: Test
testDeleteUser2 =
  TestCase
    ( assertEqual
        "Config.deleteUser2"
        expectedDeleteUser2
        actualDeleteUser2
    )

expectedDeleteUser2 :: Config.Config
expectedDeleteUser2 = TestData.testConfigVK {Config.users = Map.fromList [(11, 1), (22, 2)]}

actualDeleteUser2 :: Config.Config
actualDeleteUser2 = Config.deleteUser 33 TestData.testConfigVK

testDeleteUser3 :: Test
testDeleteUser3 =
  TestCase
    ( assertEqual
        "Config.deleteUser3"
        expectedDeleteUser3
        actualDeleteUser3
    )

expectedDeleteUser3 :: Config.Config
expectedDeleteUser3 = TestData.testConfigVK

actualDeleteUser3 :: Config.Config
actualDeleteUser3 = Config.deleteUser 42 TestData.testConfigVK
