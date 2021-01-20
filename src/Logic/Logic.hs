module Logic.Logic where

import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import Logic.ProcMsgs.Callback (processMsgsCallback)
import Logic.ProcMsgs.Common (processMsgsCommon)
import qualified Logic.PureStructs as PureStructs

processMsgs ::
  (Monad m) =>
  Env.Environment m ->
  PureStructs.SendFunction m ->
  [PureStructs.PureMessage] ->
  m (Env.Environment m)
processMsgs env sendFunction msgs =
  mapM (processMsgs_ env sendFunction) msgs >>= getLast env

processMsgs_ ::
  (Monad m) =>
  Env.Environment m ->
  PureStructs.SendFunction m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
processMsgs_ env sendFunction msg = case PureStructs.messageType msg of
  PureStructs.MTEmpty -> undefined--Env.eSetOffset ((succ . PureStructs.updateID) msg) env
  PureStructs.MTUserCommand PureStructs.Help -> sendFunction env msg
  PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction env (makeRepeatMsg msg)
  PureStructs.MTCallbackQuery callbackData -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCallback env sendFunction msg callbackData) mbChid
  PureStructs.MTCommon _ -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCommon env sendFunction msg) mbChid

processMsgsErr :: Monad m => m (Env.Environment m)
processMsgsErr = BotEx.throwOtherException LoggerMsgs.chidNotFound

getLast :: Monad m => Env.Environment m -> [Env.Environment m] -> m (Env.Environment m)
getLast env [] = pure env
getLast _ envs = pure . last $ envs

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [PureStructs.ParamsText "text" PureStructs.repeatText]])
