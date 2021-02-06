module Logic.ProcMsgs.Callback where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.Exports as Handle

processCallbackMsgs ::
  (Monad m, Handle.Services m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processCallbackMsgs env msg callbackData chatId = do
  let newRepeat = PureStructs.getNewRepeatNumber callbackData
  mbUserInDB <- Handle.findUser env chatId
  if isNothing mbUserInDB
    then Handle.addUser env chatId newRepeat
    else Handle.updateUser env chatId newRepeat
  Handle.sendMessage env $ makeCallbackResponse msg

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MsgTypeCommon "Message"}
