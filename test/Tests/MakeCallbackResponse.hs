module Tests.MakeCallbackResponse where

import qualified Logic.ProcMsgs.Callback as Logic
import qualified Logic.PureStructs as PureStructs
import Test.HUnit (Test (TestCase), assertBool)
import qualified TestData

testMakeCallbackResponse :: Test
testMakeCallbackResponse = TestCase (assertBool "Message type should be MTCommon Message" testMakeCallbackResponse')

testMakeCallbackResponse' :: Bool
testMakeCallbackResponse' =
  all
    (== PureStructs.MsgTypeCommon "Message")
    ( PureStructs.messageType
        . Logic.makeCallbackResponse
        <$> TestData.allMessages
    )
