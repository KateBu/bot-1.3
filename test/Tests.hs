module Tests where

import Test.HUnit 
import qualified Logic.Logic as Logic 
import qualified Logic.PureStructs as PureStructs 
import qualified TestData as TestData
import qualified Logger.Logger as Logger 
import qualified Config.Config as Config 

testRepeatMsg0 :: Test 
testRepeatMsg0 = TestCase (assertEqual "Logic.repeatMsg"
    expectedRepeatMsg0 
    actualRepeatMsg0) 

actualRepeatMsg0 :: [Either Logger.LogMessage Config.Config]
actualRepeatMsg0 = Logic.repeatMsg 
    TestData.testConfigVK 
    "new help message" 
    TestData.emptyMsg1 
    0 
    TestData.testFunction 

expectedRepeatMsg0 :: [Either Logger.LogMessage Config.Config] 
expectedRepeatMsg0 = [Right TestData.testConfigVK]




{-
testMakeCallbackResponse :: Test
testMakeCallbackResponse = TestCase (assertEqual "Logic.makeCallbackResponse" 
    expectedMakeCallbackResponse  
    actualMakeCallbackResponse)

actualMakeCallbackResponse :: [PureStructs.PureMessage]
actualMakeCallbackResponse = Logic.makeCallbackResponse <$> TestData.allMessages

expectedMakeCallbackResponse :: [PureStructs.PureMessage]
expectedMakeCallbackResponse = changeMType <$> TestData.allMessages

changeMType :: PureStructs.PureMessage -> PureStructs.PureMessage 
changeMType msg = PureStructs.PureMessage 
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (PureStructs.mbParams msg)

-}