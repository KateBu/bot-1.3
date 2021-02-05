module Tests.ProcessMsgs_ where

import qualified Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Logic.Main as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testProcessMsg :: [Test]
testProcessMsg =
  [ testProcessMsg_1,
    testProcessMsg_2,
    testProcessMsg_3,
    testProcessMsg_4,
    testProcessMsg_5,
    testProcessMsg_6,
    testProcessMsg_7,
    testProcessMsg_8,
    testProcessMsg_9
  ]

testProcessMsg_1 :: Test
testProcessMsg_1 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_1"
        expectedTestProcessMsg_1
        actualTestProcessMsg_1
    )

actualTestProcessMsg_1 :: Maybe Config.Config
actualTestProcessMsg_1 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.emptyMsg1

expectedTestProcessMsg_1 :: Maybe Config.Config
expectedTestProcessMsg_1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 1

testProcessMsg_2 :: Test
testProcessMsg_2 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_2"
        expectedTestProcessMsg_2
        actualTestProcessMsg_2
    )

actualTestProcessMsg_2 :: Maybe Config.Config
actualTestProcessMsg_2 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.emptyMsg2

expectedTestProcessMsg_2 :: Maybe Config.Config
expectedTestProcessMsg_2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 2

testProcessMsg_3 :: Test
testProcessMsg_3 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_3"
        expectedTestProcessMsg_3
        actualTestProcessMsg_3
    )

actualTestProcessMsg_3 :: Maybe Config.Config
actualTestProcessMsg_3 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.commandMsg1

expectedTestProcessMsg_3 :: Maybe Config.Config
expectedTestProcessMsg_3 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 1

testProcessMsg_4 :: Test
testProcessMsg_4 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_4"
        expectedTestProcessMsg_4
        actualTestProcessMsg_4
    )

actualTestProcessMsg_4 :: Maybe Config.Config
actualTestProcessMsg_4 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.commandMsg5

expectedTestProcessMsg_4 :: Maybe Config.Config
expectedTestProcessMsg_4 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsg_5 :: Test
testProcessMsg_5 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_5"
        expectedTestProcessMsg_5
        actualTestProcessMsg_5
    )

actualTestProcessMsg_5 :: Maybe Config.Config
actualTestProcessMsg_5 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cbMsg2

expectedTestProcessMsg_5 :: Maybe Config.Config
expectedTestProcessMsg_5 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsg_6 :: Test
testProcessMsg_6 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_6"
        expectedTestProcessMsg_6
        actualTestProcessMsg_6
    )

actualTestProcessMsg_6 :: Maybe Config.Config
actualTestProcessMsg_6 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg3

expectedTestProcessMsg_6 :: Maybe Config.Config
expectedTestProcessMsg_6 = Env.config <$> Env.eSetOffset TestData.testEnvVK 43

testProcessMsg_7 :: Test
testProcessMsg_7 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_7"
        expectedTestProcessMsg_7
        actualTestProcessMsg_7
    )

actualTestProcessMsg_7 :: Maybe Config.Config
actualTestProcessMsg_7 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.cmnMsg5

expectedTestProcessMsg_7 :: Maybe Config.Config
expectedTestProcessMsg_7 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 16

testProcessMsg_8 :: Test
testProcessMsg_8 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_8"
        expectedTestProcessMsg_8
        actualTestProcessMsg_8
    )

actualTestProcessMsg_8 :: Maybe Config.Config
actualTestProcessMsg_8 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg4

expectedTestProcessMsg_8 :: Maybe Config.Config
expectedTestProcessMsg_8 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsg_9 :: Test
testProcessMsg_9 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsg_9"
        Nothing
        actualTestProcessMsg_9
    )

actualTestProcessMsg_9 :: Maybe Config.Config
actualTestProcessMsg_9 =
  Env.config
    <$> Logic.processMsg
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg1


