module Logic.Main where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import Logic.ProcMsgs.Callback (processCallbackMsgs)
import Logic.ProcMsgs.Common (processCommonMsgs)
import qualified Logic.PureStructs as PureStructs
import qualified Services.Exports as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs

processMsgs ::
  (BotEx.MonadThrow m) =>
  Env.Environment m ->
  Handle.SHandle m ->
  [PureStructs.PureMessage] ->
  m (Env.Environment m)
processMsgs env _ [] = pure env
processMsgs env handle (msg : msgs) = do
  env' <- processMsg env handle msg
  processMsgs env' handle msgs

processMsg ::
  (BotEx.MonadThrow m) =>
  Env.Environment m ->
  Handle.SHandle m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
processMsg env handle msg = do
  logger <- runReaderT Env.eLogger env
  let mbChatId = PureStructs.mbChatID msg
  case PureStructs.messageType msg of
    PureStructs.MsgTypeEmpty -> do
      Logger.botLog logger logEmptyMsg
      Env.eSetOffset env ((succ . PureStructs.updateID) msg)
    PureStructs.MsgTypeUserCommand PureStructs.Help -> do
      Logger.botLog logger logHelpCommandMsg
      Handle.sendMessage handle msg
    PureStructs.MsgTypeUserCommand PureStructs.Repeat -> do
      Logger.botLog logger logRepeatCommandMsg
      Handle.sendMessage handle (makeRepeatMsg msg)
    PureStructs.MsgTypeCallbackQuery callbackData -> do
      Logger.botLog logger logCallbackMsg
      maybe processMsgErr (processCallbackMsgs handle msg callbackData) mbChatId
    PureStructs.MsgTypeCommon _ -> do
      maybe processMsgErr (processCommonMsgs env handle msg) mbChatId
  where
    logEmptyMsg = LoggerMsgs.emptyMsgProcessingInProgress
    logHelpCommandMsg = LoggerMsgs.helpCommandProcessingInProgress
    logRepeatCommandMsg = LoggerMsgs.repeatCommandProcessingInProgress
    logCallbackMsg = LoggerMsgs.callbackMsgProcessingInProgress

processMsgErr :: BotEx.MonadThrow m => m (Env.Environment m)
processMsgErr = BotEx.throwOtherException LoggerMsgs.chatIdNotFound

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MsgTypeCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    repeatParams
  where
    repeatParams =
      mconcat
        [ PureStructs.mbParams msg,
          Just [PureStructs.ParamsText "text" PureStructs.repeatText]
        ]
