module Logic.ProcMsgs.Callback where

import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Logic.PureStructs as PureStructs

processMsgsCallback ::
  Monad m =>
  Env.Environment m ->
  PureStructs.SendFunction m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processMsgsCallback env function msg callbackData chid = undefined
{- do
  let newRep = PureStructs.getNewRep callbackData
  newEnv <- Env.eSetUserRepeat chid newRep env
  function newEnv (makeCallbackResponse msg)-}

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"}
