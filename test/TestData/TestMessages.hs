module TestData.TestMessages where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

newHelp :: T.Text
newHelp = "new help message"

allMessages :: [PureStructs.PureMessage]
allMessages = mconcat [emptyMessages, commandMessages, commonMessages, callbackMessages]

emptyMessages :: [PureStructs.PureMessage]
emptyMessages = [emptyMsg1, emptyMsg2, emptyMsg3, emptyMsg4]

commandMessages :: [PureStructs.PureMessage]
commandMessages =
  [ commandMsg1,
    commandMsg2,
    commandMsg3,
    commandMsg4,
    commandMsg5,
    commandMsg6,
    commandMsg7,
    commandMsg8
  ]

callbackMessages :: [PureStructs.PureMessage]
callbackMessages = [cbMsg1, cbMsg2, cbMsg3, cbMsg4, cbMsg5, cbMsg6]

commonMessages :: [PureStructs.PureMessage]
commonMessages = [cmnMsg1, cmnMsg2, cmnMsg3, cmnMsg4, cmnMsg5]

emptyMsg1 :: PureStructs.PureMessage
emptyMsg1 =
  PureStructs.PureMessage
    PureStructs.MTEmpty
    0
    Nothing
    Nothing

emptyMsg2 :: PureStructs.PureMessage
emptyMsg2 =
  PureStructs.PureMessage
    PureStructs.MTEmpty
    1
    Nothing
    (Just [])

emptyMsg3 :: PureStructs.PureMessage
emptyMsg3 =
  PureStructs.PureMessage
    PureStructs.MTEmpty
    1
    (Just 0)
    (Just [])

emptyMsg4 :: PureStructs.PureMessage
emptyMsg4 =
  PureStructs.PureMessage
    PureStructs.MTEmpty
    1
    (Just 0)
    Nothing

commandMsg1 :: PureStructs.PureMessage
commandMsg1 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Help)
    0
    Nothing
    Nothing

commandMsg2 :: PureStructs.PureMessage
commandMsg2 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Help)
    2
    (Just 0)
    Nothing

commandMsg3 :: PureStructs.PureMessage
commandMsg3 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Help)
    2
    (Just 0)
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

commandMsg4 :: PureStructs.PureMessage
commandMsg4 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Help)
    2
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

commandMsg5 :: PureStructs.PureMessage
commandMsg5 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Repeat)
    2
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

commandMsg6 :: PureStructs.PureMessage
commandMsg6 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Repeat)
    2
    (Just 1)
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

commandMsg7 :: PureStructs.PureMessage
commandMsg7 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Repeat)
    2
    Nothing
    Nothing

commandMsg8 :: PureStructs.PureMessage
commandMsg8 =
  PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Repeat)
    2
    Nothing
    (Just [])

cbMsg1 :: PureStructs.PureMessage
cbMsg1 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery PureStructs.rep1)
    2
    Nothing
    (Just [])

cbMsg2 :: PureStructs.PureMessage
cbMsg2 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery PureStructs.rep2)
    2
    (Just 11)
    Nothing

cbMsg3 :: PureStructs.PureMessage
cbMsg3 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery PureStructs.rep3)
    2
    (Just 11)
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cbMsg4 :: PureStructs.PureMessage
cbMsg4 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery PureStructs.rep4)
    2
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cbMsg5 :: PureStructs.PureMessage
cbMsg5 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery PureStructs.rep5)
    (-11)
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cbMsg6 :: PureStructs.PureMessage
cbMsg6 =
  PureStructs.PureMessage
    (PureStructs.MTCallbackQuery "somethingElse")
    2
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cmnMsg1 :: PureStructs.PureMessage
cmnMsg1 =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    2
    Nothing
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cmnMsg2 :: PureStructs.PureMessage
cmnMsg2 =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    2
    Nothing
    Nothing

cmnMsg3 :: PureStructs.PureMessage
cmnMsg3 =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    42
    (Just 11)
    Nothing

cmnMsg4 :: PureStructs.PureMessage
cmnMsg4 =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    2
    (Just 33)
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )

cmnMsg5 :: PureStructs.PureMessage
cmnMsg5 =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    15
    (Just 42)
    ( Just
        [ PureStructs.ParamsBool "bool" True,
          PureStructs.ParamsText "text" "some text",
          PureStructs.ParamsDouble "double" 3.14,
          PureStructs.ParamsNum "num" 1024,
          PureStructs.ParamsTextList "textList" ["one", "two", "three"],
          PureStructs.ParamsJSON "json" (object ["json" .= ("testJson" :: T.Text)])
        ]
    )
