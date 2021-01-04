module TestData (module Tests) where

import TestData.TestConfig as Tests
    ( testConfigVK, testConfigTelegram, users ) 
import TestData.TestMessages as Tests
    ( newHelp,
      allMessages,
      emptyMessages,
      commandMessages,
      callbackMessages,
      commonMessages,
      emptyMsg1,
      emptyMsg2,
      emptyMsg3,
      emptyMsg4,
      commandMsg1,
      commandMsg2,
      commandMsg3,
      commandMsg4,
      commandMsg5,
      commandMsg6,
      commandMsg7,
      commandMsg8,
      cbMsg1,
      cbMsg2,
      cbMsg3,
      cbMsg4,
      cbMsg5,
      cbMsg6,
      cmnMsg1,
      cmnMsg2,
      cmnMsg3,
      cmnMsg4,
      cmnMsg5 ) 
import TestData.TestFunctions as Tests
    ( testFunction0, testFunction1 ) 

