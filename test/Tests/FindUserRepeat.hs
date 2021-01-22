module Tests.FindUserRepeat where

import qualified Config.Config as Config
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

{-
testFindUserRepeat :: [Test]
testFindUserRepeat = [testFindUserRepeat1, testFindUserRepeat2, testFindUserRepeat3]

testFindUserRepeat1 :: Test
testFindUserRepeat1 =
  TestCase
    ( assertEqual
        "Config.findUserRepeat1"
        expectedFindUserRepeat1
        actualFindUserRepeat1
    )

expectedFindUserRepeat1 :: Int
expectedFindUserRepeat1 = 1

actualFindUserRepeat1 :: Int
actualFindUserRepeat1 = Config.findUserRepeat TestData.testConfigTelegram 11

testFindUserRepeat2 :: Test
testFindUserRepeat2 =
  TestCase
    ( assertEqual
        "Config.findUserRepeat2"
        expectedFindUserRepeat2
        actualFindUserRepeat2
    )

expectedFindUserRepeat2 :: Int
expectedFindUserRepeat2 = 2

actualFindUserRepeat2 :: Int
actualFindUserRepeat2 = Config.findUserRepeat TestData.testConfigVK 22

testFindUserRepeat3 :: Test
testFindUserRepeat3 =
  TestCase
    ( assertEqual
        "Config.findUserRepeat2"
        expectedFindUserRepeat3
        actualFindUserRepeat3
    )

expectedFindUserRepeat3 :: Int
expectedFindUserRepeat3 = 3

actualFindUserRepeat3 :: Int
actualFindUserRepeat3 = Config.findUserRepeat TestData.testConfigVK 42
-}