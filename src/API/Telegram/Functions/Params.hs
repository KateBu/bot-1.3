module API.Telegram.Functions.Params where

import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

basicParams :: PureStructs.ChatID -> TStructs.MessageInfo -> [PureStructs.Params]
basicParams chatId msgInfo = PureStructs.ParamsNum "chat_id" chatId : mbCaption msgInfo

mbCaption :: TStructs.MessageInfo -> [PureStructs.Params]
mbCaption msgInfo =
  let mbCaption' = TStructs.caption msgInfo
   in maybe [] (pure . PureStructs.ParamsText "caption") mbCaption'

type Key = T.Text

buildTextParams :: Key -> Maybe T.Text -> [PureStructs.Params]
buildTextParams _ Nothing = []
buildTextParams key (Just val) = pure $ PureStructs.ParamsText key val

buildBoolParams :: Key -> Maybe Bool -> [PureStructs.Params]
buildBoolParams _ Nothing = []
buildBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

buildNumParams :: Key -> Maybe Int -> [PureStructs.Params]
buildNumParams _ Nothing = []
buildNumParams key (Just val) = pure $ PureStructs.ParamsNum key val
