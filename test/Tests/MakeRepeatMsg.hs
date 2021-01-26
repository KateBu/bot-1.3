module Tests.MakeRepeatMsg where

import qualified Logic.Logic as Logic
import qualified Logic.PureStructs as PureStructs
import Test.HUnit (Test (TestCase), assertBool)
import qualified TestData 

testMakeRepeatMsg :: [Test]
testMakeRepeatMsg = [testMakeRepeatMsgMsgType, testMakeRepeatMsgText]

testMakeRepeatMsgMsgType :: Test
testMakeRepeatMsgMsgType =
  TestCase
    ( assertBool
        "Message type should be MTCommon"
        testMakeRepeatMsgMsgType'
    )

testMakeRepeatMsgMsgType' :: Bool
testMakeRepeatMsgMsgType' =
  all
    (== PureStructs.MTCommon "Message")
    ( PureStructs.messageType
        . Logic.makeRepeatMsg
        <$> TestData.allMessages
    )

testMakeRepeatMsgText :: Test
testMakeRepeatMsgText =
  TestCase
    ( assertBool
        "Text should be 'repeat text'"
        testMakeRepeatMsgText'
    )

testMakeRepeatMsgText' :: Bool
testMakeRepeatMsgText' = all hasTextParam (Logic.makeRepeatMsg <$> TestData.allMessages)

hasTextParam :: PureStructs.PureMessage -> Bool
hasTextParam msg = Just True == hasParam'
  where
    hasParam' = fmap (elem (PureStructs.ParamsText "text" PureStructs.repeatText)) (PureStructs.mbParams msg)
