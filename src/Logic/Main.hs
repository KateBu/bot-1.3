module Logic.Main where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Internals as Env
import qualified Environment.Logger.Internals as Logger
import qualified Exceptions.Internals as BotEx
import Logic.ProcMsgs.Callback (processMsgsCallback)
import Logic.ProcMsgs.Common (processMsgsCommon)
import qualified Logic.PureStructs as PureStructs
import qualified Services.SHandle as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs

processMsgs ::
  (BotEx.MonadThrow m) =>
  Env.Environment m ->
  Handle.SHandle m ->
  [PureStructs.PureMessage] ->
  m (Env.Environment m)
processMsgs env _ [] = pure env
processMsgs env handle (x : xs) = do
  env' <- processMsgs_ env handle x
  processMsgs env' handle xs

processMsgs_ ::
  (BotEx.MonadThrow m) =>
  Env.Environment m ->
  Handle.SHandle m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
processMsgs_ env handle msg = do
  logger <- runReaderT Env.eLogger env
  case PureStructs.messageType msg of
    PureStructs.MTEmpty -> do
      Logger.botLog logger LoggerMsgs.emptyMsg
      Env.eSetOffset env ((succ . PureStructs.updateID) msg)
    PureStructs.MTUserCommand PureStructs.Help -> do
      Logger.botLog logger LoggerMsgs.helpCmd
      Handle.sendMessage handle msg
    PureStructs.MTUserCommand PureStructs.Repeat -> do
      Logger.botLog logger LoggerMsgs.repeatCmd
      Handle.sendMessage handle (makeRepeatMsg msg)
    PureStructs.MTCallbackQuery callbackData -> do
      Logger.botLog logger LoggerMsgs.callbackMsg
      let mbChid = PureStructs.mbChatID msg
      maybe processMsgsErr (processMsgsCallback handle msg callbackData) mbChid
    PureStructs.MTCommon _ -> do
      let mbChid = PureStructs.mbChatID msg
      maybe processMsgsErr (processMsgsCommon env handle msg) mbChid

processMsgsErr :: BotEx.MonadThrow m => m (Env.Environment m)
processMsgsErr = BotEx.throwOtherException LoggerMsgs.chidNotFound

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [PureStructs.ParamsText "text" PureStructs.repeatText]])
