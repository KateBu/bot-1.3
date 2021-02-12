module Logic.Functions.Callback (processCallbackMsgs) where

import qualified API.PureStructs.Exports as PureStructs
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Services.Main as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs

processCallbackMsgs ::
  (BotEx.MonadThrow m, Handle.Services m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  T.Text ->
  m (Env.Environment m)
processCallbackMsgs env msg callbackData = do
  let newRepeat = PureStructs.getNewRepeatNumber callbackData
  let mbChatId = PureStructs.mbChatID msg
  maybe
    (BotEx.throwOtherException LoggerMsgs.chatIdNotFound)
    (processCallbackMsgs' env msg newRepeat)
    mbChatId

processCallbackMsgs' ::
  (Handle.Services m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processCallbackMsgs' env msg newRepeat chatId = do
  mbUserInDB <- Handle.findUser env chatId
  if isNothing mbUserInDB
    then Handle.addUser env chatId newRepeat
    else Handle.updateUser env chatId newRepeat
  Handle.sendMessage env $ buildCallbackResponse msg

buildCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
buildCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MsgTypeCommon "Message"}
