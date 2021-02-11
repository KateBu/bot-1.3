module Logic.Main (processMsgs) where

import qualified API.PureStructs.Exports as PureStructs
import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logger.Exports as Logger
import Logic.Functions.Callback (processCallbackMsgs)
import Logic.Functions.Common (processCommonMsgs)
import qualified Services.Main as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs
import qualified TextMessages.RepeatCommandMessages as RepeatCommandMessages

processMsgs ::
  (BotEx.MonadThrow m, Handle.Services m) =>
  Env.Environment m ->
  [PureStructs.PureMessage] ->
  m (Env.Environment m)
processMsgs = foldM processMsg

processMsg ::
  (BotEx.MonadThrow m, Handle.Services m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
processMsg env msg = do
  logger <- runReaderT Env.eLogger env
  let mbChatId = PureStructs.mbChatID msg
  case PureStructs.messageType msg of
    PureStructs.MsgTypeEmpty -> do
      Logger.botLog logger logEmptyMsg
      Env.eSetOffset env ((succ . PureStructs.updateID) msg)
    PureStructs.MsgTypeUserCommand PureStructs.Help -> do
      Logger.botLog logger logHelpCommandMsg
      Handle.sendMessage env msg
    PureStructs.MsgTypeUserCommand PureStructs.Repeat -> do
      Logger.botLog logger logRepeatCommandMsg
      Handle.sendMessage env (buildRepeatMsg msg)
    PureStructs.MsgTypeCallbackQuery callbackData -> do
      Logger.botLog logger logCallbackMsg
      maybe throwError (processCallbackMsgs env msg callbackData) mbChatId
    PureStructs.MsgTypeCommon _ -> do
      maybe throwError (processCommonMsgs env msg) mbChatId
  where
    logEmptyMsg = LoggerMsgs.emptyMsgProcessingInProgress
    logHelpCommandMsg = LoggerMsgs.helpCommandProcessingInProgress
    logRepeatCommandMsg = LoggerMsgs.repeatCommandProcessingInProgress
    logCallbackMsg = LoggerMsgs.callbackMsgProcessingInProgress

throwError :: BotEx.MonadThrow m => m (Env.Environment m)
throwError = BotEx.throwOtherException LoggerMsgs.chatIdNotFound

buildRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
buildRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MsgTypeCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    repeatParams
  where
    repeatParams =
      mconcat
        [ PureStructs.mbParams msg,
          Just [PureStructs.ParamsText "text" RepeatCommandMessages.repeatText]
        ]
