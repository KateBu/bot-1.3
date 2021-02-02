module Logic.Structs.PureMessage where

import Data.Aeson (Value)
import qualified Data.Text as T

type UpdateID = Int

type ChatID = Int

type MbCaption = Maybe T.Text

type CallbackText = T.Text 

data MessageType
  = MTEmpty
  | MTUserCommand UCommand
  | MTCallbackQuery CallbackText
  | MTCommon T.Text
  deriving (Show, Eq)

data PureMessage = PureMessage
  { messageType :: MessageType,
    updateID :: UpdateID,
    mbChatID :: Maybe ChatID,
    mbParams :: Maybe [Params]
  }
  deriving (Show, Eq)

data UCommand = Help | Repeat
  deriving (Show, Eq)

data Params
  = ParamsText T.Text T.Text
  | ParamsTextList T.Text [T.Text]
  | ParamsNum T.Text Int
  | ParamsDouble T.Text Double
  | ParamsBool T.Text Bool
  | ParamsJSON T.Text Value
  deriving (Show, Eq)
