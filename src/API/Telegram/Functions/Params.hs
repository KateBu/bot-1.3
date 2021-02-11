module API.Telegram.Functions.Params
  ( buildBasicParams,
    buildTextParams,
    buildBoolParams,
    buildNumParams,
  )
where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.Telegram.Structs.MessageInfo as Telegram
import qualified Data.Text as T

buildBasicParams :: PureStructs.ChatID -> Telegram.MessageInfo -> [PureStructs.Params]
buildBasicParams chatId msgInfo = PureStructs.ParamsNum "chat_id" chatId : buildCaptionParams msgInfo

buildCaptionParams :: Telegram.MessageInfo -> [PureStructs.Params]
buildCaptionParams msgInfo =
  let mbCaption' = Telegram.caption msgInfo
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
