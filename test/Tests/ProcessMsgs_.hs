module Tests.ProcessMsgs_ where

import qualified Config.Internals as Config
import qualified Environment.Internals as Env
import qualified Logic.Logic as Logic
import Test.HUnit (Test (TestCase), assertEqual)
import qualified TestData as TestData

testProcessMsgs_ :: [Test]
testProcessMsgs_ =
  [ testProcessMsgs_1,
    testProcessMsgs_2,
    testProcessMsgs_3,
    testProcessMsgs_4,
    testProcessMsgs_5,
    testProcessMsgs_6,
    testProcessMsgs_7,
    testProcessMsgs_8
  ]

testProcessMsgs_1 :: Test
testProcessMsgs_1 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_1"
        expectedTestProcessMsgs_1
        actualTestProcessMsgs_1
    )

actualTestProcessMsgs_1 :: Maybe Config.Config
actualTestProcessMsgs_1 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.emptyMsg1

expectedTestProcessMsgs_1 :: Maybe Config.Config
expectedTestProcessMsgs_1 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 1

testProcessMsgs_2 :: Test
testProcessMsgs_2 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_2"
        expectedTestProcessMsgs_2
        actualTestProcessMsgs_2
    )

actualTestProcessMsgs_2 :: Maybe Config.Config
actualTestProcessMsgs_2 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.emptyMsg2

expectedTestProcessMsgs_2 :: Maybe Config.Config
expectedTestProcessMsgs_2 = Env.config <$> Env.eSetOffset TestData.testEnvVK 2

testProcessMsgs_3 :: Test
testProcessMsgs_3 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_3"
        expectedTestProcessMsgs_3
        actualTestProcessMsgs_3
    )

actualTestProcessMsgs_3 :: Maybe Config.Config
actualTestProcessMsgs_3 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.commandMsg1

expectedTestProcessMsgs_3 :: Maybe Config.Config
expectedTestProcessMsgs_3 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 1

testProcessMsgs_4 :: Test
testProcessMsgs_4 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_4"
        expectedTestProcessMsgs_4
        actualTestProcessMsgs_4
    )

actualTestProcessMsgs_4 :: Maybe Config.Config
actualTestProcessMsgs_4 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.commandMsg5

expectedTestProcessMsgs_4 :: Maybe Config.Config
expectedTestProcessMsgs_4 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsgs_5 :: Test
testProcessMsgs_5 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_5"
        expectedTestProcessMsgs_5
        actualTestProcessMsgs_5
    )

actualTestProcessMsgs_5 :: Maybe Config.Config
actualTestProcessMsgs_5 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cbMsg2

expectedTestProcessMsgs_5 :: Maybe Config.Config
expectedTestProcessMsgs_5 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3

testProcessMsgs_6 :: Test
testProcessMsgs_6 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_6"
        expectedTestProcessMsgs_6
        actualTestProcessMsgs_6
    )

actualTestProcessMsgs_6 :: Maybe Config.Config
actualTestProcessMsgs_6 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg3

expectedTestProcessMsgs_6 :: Maybe Config.Config
expectedTestProcessMsgs_6 = Env.config <$> Env.eSetOffset TestData.testEnvVK 43

testProcessMsgs_7 :: Test
testProcessMsgs_7 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_7"
        expectedTestProcessMsgs_7
        actualTestProcessMsgs_7
    )

actualTestProcessMsgs_7 :: Maybe Config.Config
actualTestProcessMsgs_7 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvTelegram
      TestData.servicesTel1
      TestData.cmnMsg5

expectedTestProcessMsgs_7 :: Maybe Config.Config
expectedTestProcessMsgs_7 = Env.config <$> Env.eSetOffset TestData.testEnvTelegram 16

testProcessMsgs_8 :: Test
testProcessMsgs_8 =
  TestCase
    ( assertEqual
        "Logic.testProcessMsgs_8"
        expectedTestProcessMsgs_8
        actualTestProcessMsgs_8
    )

actualTestProcessMsgs_8 :: Maybe Config.Config
actualTestProcessMsgs_8 =
  Env.config
    <$> Logic.processMsgs_
      TestData.testEnvVK
      TestData.servicesVk1
      TestData.cmnMsg4

expectedTestProcessMsgs_8 :: Maybe Config.Config
expectedTestProcessMsgs_8 = Env.config <$> Env.eSetOffset TestData.testEnvVK 3
