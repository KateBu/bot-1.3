module API.Telegram.Cleaners.GetParams where

import qualified API.Telegram.Structs.MessageInfo as TStructs
import qualified Logic.PureStructs as PureStructs

basicParams :: PureStructs.ChatID -> TStructs.MessageInfo -> [PureStructs.Params]
basicParams chid mInfo = PureStructs.ParamsNum "chat_id" chid : mbCaption mInfo

mbCaption :: TStructs.MessageInfo -> [PureStructs.Params]
mbCaption mInfo =
  let mbCap = TStructs.caption mInfo
   in maybe [] (pure . PureStructs.ParamsText "caption") mbCap
