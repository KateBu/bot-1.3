module Tests.ReWriteUserRepeat where

import qualified Config.Config as Config
import qualified Data.Map as Map
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testReWriteUserRepeat :: [Test]
testReWriteUserRepeat = [testReWriteUserRepeat1, testReWriteUserRepeat2]

testReWriteUserRepeat1 :: Test
testReWriteUserRepeat1 =
  TestCase
    ( assertEqual
        "Config.reWriteUserRepeat1"
        expectedReWriteUserRepeat1
        actualReWriteUserRepeat1
    )

expectedReWriteUserRepeat1 :: Config.Config
expectedReWriteUserRepeat1 = TestData.testConfigTelegram {Config.users = Map.fromList [(11, 5), (22, 2), (33, 3)]}

actualReWriteUserRepeat1 :: Config.Config
actualReWriteUserRepeat1 = Config.reWriteUserRepeat TestData.testConfigTelegram 11 5 1

testReWriteUserRepeat2 :: Test
testReWriteUserRepeat2 =
  TestCase
    ( assertEqual
        "Config.reWriteUserRepeat2"
        expectedReWriteUserRepeat2
        actualReWriteUserRepeat2
    )

expectedReWriteUserRepeat2 :: Config.Config
expectedReWriteUserRepeat2 = TestData.testConfigVK {Config.users = Map.fromList [(11, 1), (22, 2), (33, 1)]}

actualReWriteUserRepeat2 :: Config.Config
actualReWriteUserRepeat2 = Config.reWriteUserRepeat TestData.testConfigVK 33 1 1
