module Logic.Logic where

import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import Logic.ProcMsgs.Callback (processMsgsCallback)
import Logic.ProcMsgs.Common (processMsgsCommon)
import qualified Logic.PureStructs as PureStructs

processMsgs ::
  (Monad m) =>
  Env.Env m ->
  PureStructs.SendFunction m ->
  [PureStructs.PureMessage] ->
  m (Env.Env m)
processMsgs env sendFunction msgs =
  mapM (processMsgs_ env sendFunction) msgs >>= getLast env

processMsgs_ ::
  (Monad m) =>
  Env.Env m ->
  PureStructs.SendFunction m ->
  PureStructs.PureMessage ->
  m (Env.Env m)
processMsgs_ env sendFunction msg = case PureStructs.messageType msg of
  PureStructs.MTEmpty -> Env.eSetOffset ((succ . PureStructs.updateID) msg) env
  PureStructs.MTUserCommand PureStructs.Help -> sendFunction env msg
  PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction env (makeRepeatMsg msg)
  PureStructs.MTCallbackQuery callbackData -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCallback env sendFunction msg callbackData) mbChid
  PureStructs.MTCommon _ -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCommon env sendFunction msg) mbChid

processMsgsErr :: Monad m => m (Env.Env m)
processMsgsErr = BotEx.throwOtherException LoggerMsgs.chidNotFound

getLast :: Monad m => Env.Env m -> [Env.Env m] -> m (Env.Env m)
getLast env [] = pure env
getLast _ envs = pure . last $ envs

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [PureStructs.ParamsText "text" PureStructs.repeatText]])
