module Tests.ProcessMsgs where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.Main as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData

testProcessMsgs :: [Test]
testProcessMsgs =
  [ testProcessMsgs1,
    testProcessMsgs2,
    testProcessMsgs3,
    testProcessMsgs4,
    testProcessMsgs5,
    testProcessMsgs6,
    testProcessMsgs7
  ]

testProcessMsgs1 :: Test
testProcessMsgs1 =
  TestCase
    ( assertEqual
        "Handling messages without errors"
        expectedTestProcessMsgs1
        actualTestProcessMsgs1
    )

actualTestProcessMsgs1 :: Maybe Config.Config
actualTestProcessMsgs1 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.messagesWithoutErrors

expectedTestProcessMsgs1 :: Maybe Config.Config
expectedTestProcessMsgs1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 16

testProcessMsgs2 :: Test
testProcessMsgs2 =
  TestCase
    ( assertEqual
        "Handling empty messages"
        expectedTestProcessMsgs2
        actualTestProcessMsgs2
    )

actualTestProcessMsgs2 :: Maybe Config.Config
actualTestProcessMsgs2 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      TestData.emptyMessages

expectedTestProcessMsgs2 :: Maybe Config.Config
expectedTestProcessMsgs2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 2

testProcessMsgs3 :: Test
testProcessMsgs3 =
  TestCase
    ( assertEqual
        "Handling command messages without errors"
        expectedTestProcessMsgs3
        actualTestProcessMsgs3
    )

actualTestProcessMsgs3 :: Maybe Config.Config
actualTestProcessMsgs3 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.cmdWithoutErrors

expectedTestProcessMsgs3 :: Maybe Config.Config
expectedTestProcessMsgs3 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 3

testProcessMsgs4 :: Test
testProcessMsgs4 =
  TestCase
    ( assertEqual
        "Handling all command messages"
        expectedTestProcessMsgs4
        actualTestProcessMsgs4
    )

actualTestProcessMsgs4 :: Maybe Config.Config
actualTestProcessMsgs4 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.commandMessages

expectedTestProcessMsgs4 :: Maybe Config.Config
expectedTestProcessMsgs4 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 6

testProcessMsgs5 :: Test
testProcessMsgs5 =
  TestCase
    ( assertEqual
        "Handling callback messages without errors"
        expectedTestProcessMsgs5
        actualTestProcessMsgs5
    )

actualTestProcessMsgs5 :: Maybe Config.Config
actualTestProcessMsgs5 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      TestData.cbWithoutErrors

expectedTestProcessMsgs5 :: Maybe Config.Config
expectedTestProcessMsgs5 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsgs6 :: Test
testProcessMsgs6 =
  TestCase
    ( assertEqual
        "Handling all callback messages"
        Nothing
        actualTestProcessMsgs6
    )

actualTestProcessMsgs6 :: Maybe Config.Config
actualTestProcessMsgs6 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      TestData.callbackMessages


testProcessMsgs7 :: Test
testProcessMsgs7 =
  TestCase
    ( assertEqual
        "Handling common messages without errors"
        expectedTestProcessMsgs7
        actualTestProcessMsgs7
    )

actualTestProcessMsgs7 :: Maybe Config.Config
actualTestProcessMsgs7 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.cmnWithoutErrors

expectedTestProcessMsgs7 :: Maybe Config.Config
expectedTestProcessMsgs7 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 16

testProcessMsgs8 :: Test
testProcessMsgs8 =
  TestCase
    ( assertEqual
        "Handling all common messages"
        Nothing
        actualTestProcessMsgs8
    )

actualTestProcessMsgs8 :: Maybe Config.Config
actualTestProcessMsgs8 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.commonMessages

testProcessMsgs9 :: Test
testProcessMsgs9 =
  TestCase
    ( assertEqual
        "Handling empty list of messages"
        expectedTestProcessMsgs9
        actualTestProcessMsgs9
    )

actualTestProcessMsgs9 :: Maybe Config.Config
actualTestProcessMsgs9 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      []

expectedTestProcessMsgs9 :: Maybe Config.Config
expectedTestProcessMsgs9 = pure $ Env.config TestData.testEnvVK

testProcessMsgs10 :: Test
testProcessMsgs10 =
  TestCase
    ( assertEqual
        "Hamdling all messages"
        Nothing
        actualTestProcessMsgs10
    )

actualTestProcessMsgs10 :: Maybe Config.Config
actualTestProcessMsgs10 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.allMessages
