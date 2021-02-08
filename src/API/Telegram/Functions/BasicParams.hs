module API.Telegram.Functions.BasicParams where

import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

basicParams :: PureStructs.ChatID -> TStructs.MessageInfo -> [PureStructs.Params]
basicParams chatId msgInfo = PureStructs.ParamsNum "chat_id" chatId : mbCaption msgInfo

mbCaption :: TStructs.MessageInfo -> [PureStructs.Params]
mbCaption msgInfo =
  let mbCaption' = TStructs.caption msgInfo
   in maybe [] (pure . PureStructs.ParamsText "caption") mbCaption'
