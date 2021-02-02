module API.Telegram.Cleaners.MakePureMessage where

import API.Telegram.Cleaners.MbMsgType
  ( mbAnimation,
    mbAudio,
    mbContact,
    mbDoc,
    mbLocation,
    mbPhoto,
    mbPoll,
    mbSticker,
    mbTextMessage,
    mbVenue,
    mbVideo,
    mbVoice,
  )
import qualified API.Telegram.Structs.Updates as TStructs
import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Environment.Internals as Env
import qualified Exceptions.Internals as BotEx
import qualified Logic.PureStructs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

telUpdateToPureMessage ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  PureStructs.PureMessage
telUpdateToPureMessage hMsg res = do
  let uid = TStructs.update_id res
  let mbPureMessage = mbMakeCallbackPureMessage res uid <|> mbMakePureMessage hMsg res uid
  fromMaybe (BotEx.throwPureUpdateExcept LoggerMsgs.noUpd) mbPureMessage

mbMakeCallbackPureMessage ::
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
mbMakeCallbackPureMessage res uid = case TStructs.callback_query res of
  Just (TStructs.Callback callback cbData) -> do
    let chid = (TStructs.cb_chid . TStructs.cb_chat) callback
    pure
      ( PureStructs.PureMessage
          (PureStructs.MTCallbackQuery cbData)
          uid
          (Just chid)
          ( Just $
              [ PureStructs.ParamsText "text" (PureStructs.newRepeatText (PureStructs.getNewRep cbData)),
                PureStructs.ParamsNum "chat_id" chid
              ]
          )
      )
  _ -> Nothing

mbMakePureMessage ::
  Env.HelpMessage ->
  TStructs.TelUpdateResult ->
  PureStructs.UpdateID ->
  Maybe PureStructs.PureMessage
mbMakePureMessage hMsg res uid = case TStructs.messageInfo res of
  Nothing -> pure $ PureStructs.PureMessage PureStructs.MTEmpty uid Nothing Nothing
  Just mInfo -> do
    let chid = TStructs.chat_id $ TStructs.chat mInfo
    case makeCommonMessage hMsg uid chid mInfo of
      Nothing -> Nothing
      Just pureMsg -> pure pureMsg

makeCommonMessage ::
  Env.HelpMessage ->
  PureStructs.UpdateID ->
  PureStructs.ChatID ->
  TStructs.MessageInfo ->
  Maybe PureStructs.PureMessage
makeCommonMessage hMsg uid chid mInfo =
  mbAnimation uid chid mInfo
    <|> mbAudio uid chid mInfo
    <|> mbDoc uid chid mInfo
    <|> mbVideo uid chid mInfo
    <|> mbVoice uid chid mInfo
    <|> mbPhoto uid chid mInfo
    <|> mbContact uid chid mInfo
    <|> mbVenue uid chid mInfo
    <|> mbLocation uid chid mInfo
    <|> mbSticker uid chid mInfo
    <|> mbPoll uid chid mInfo
    <|> mbTextMessage hMsg uid chid mInfo
