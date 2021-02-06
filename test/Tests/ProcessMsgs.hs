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
        "Logic.Main.testProcessMsgs1"
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
        "Logic.testProcessMsgs2"
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
        "Logic.testProcessMsgs3"
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
        "Logic.testProcessMsgs4"
        expectedTestProcessMsgs4
        actualTestProcessMsgs4
    )

actualTestProcessMsgs4 :: Maybe Config.Config
actualTestProcessMsgs4 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      TestData.cbWithoutErrors

expectedTestProcessMsgs4 :: Maybe Config.Config
expectedTestProcessMsgs4 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsgs5 :: Test
testProcessMsgs5 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs5"
        expectedTestProcessMsgs5
        actualTestProcessMsgs5
    )

actualTestProcessMsgs5 :: Maybe Config.Config
actualTestProcessMsgs5 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.cmnWithoutErrors

expectedTestProcessMsgs5 :: Maybe Config.Config
expectedTestProcessMsgs5 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 16

testProcessMsgs6 :: Test
testProcessMsgs6 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs6"
        expectedTestProcessMsgs6
        actualTestProcessMsgs6
    )

actualTestProcessMsgs6 :: Maybe Config.Config
actualTestProcessMsgs6 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvVK
      []

expectedTestProcessMsgs6 :: Maybe Config.Config
expectedTestProcessMsgs6 = pure $ Env.config TestData.testEnvVK

testProcessMsgs7 :: Test
testProcessMsgs7 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs7"
        Nothing
        actualTestProcessMsgs7
    )

actualTestProcessMsgs7 :: Maybe Config.Config
actualTestProcessMsgs7 =
  Env.config
    <$> Logic.processMsgs
      TestData.testEnvTelegram
      TestData.allMessages
