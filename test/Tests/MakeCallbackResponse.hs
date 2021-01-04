module Tests.MakeCallbackResponse where

import Test.HUnit ( assertBool, Test(TestCase) ) 
import qualified Logic.Logic as Logic 
import qualified TestData as TestData
import qualified Logic.PureStructs as PureStructs 

testMakeCallbackResponse :: Test 
testMakeCallbackResponse = TestCase (assertBool "Message type should be MTCommon Message" testMakeCallbackResponse')

testMakeCallbackResponse' :: Bool
testMakeCallbackResponse' = all (== PureStructs.MTCommon "Message") (PureStructs.messageType .
    Logic.makeCallbackResponse <$> TestData.allMessages) 