module Logic.ProcMsgs.Callback where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.Exports as Handle

processMsgsCallback ::
  Monad m =>
  Handle.SHandle m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processMsgsCallback handle msg callbackData userId = do
  let newRep = PureStructs.getNewRep callbackData
  mbUserInDB <- Handle.findUser handle userId
  if isNothing mbUserInDB
    then Handle.addUser handle userId newRep
    else Handle.updateUser handle userId newRep
  Handle.sendMessage handle $ makeCallbackResponse msg

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"}
