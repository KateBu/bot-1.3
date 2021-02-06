module TestData (module Tests) where

import TestData.TestEnvironment as Tests
  ( helpM,
    testConfigTelegram,
    testConfigVK,
    testEnvTelegram,
    testEnvVK,
    testLogger,
  )
import TestData.TestMessages as Tests
  ( allMessages,
    callbackMessages,
    cbMsg1,
    cbMsg2,
    cbMsg3,
    cbMsg4,
    cbMsg5,
    cbMsg6,
    cbWithoutErrors,
    cmdWithoutErrors,
    cmnMsg1,
    cmnMsg2,
    cmnMsg3,
    cmnMsg4,
    cmnMsg5,
    cmnWithoutErrors,
    commandMessages,
    commandMsg1,
    commandMsg2,
    commandMsg3,
    commandMsg4,
    commandMsg5,
    commandMsg6,
    commandMsg7,
    commandMsg8,
    commonMessages,
    emptyMessages,
    emptyMsg1,
    emptyMsg2,
    emptyMsg3,
    emptyMsg4,
    messagesWithoutErrors,
    newHelp,
  )
import TestData.TestServices as Tests ()
