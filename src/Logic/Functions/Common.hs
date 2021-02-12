module Logic.Functions.Common (processCommonMsgs) where

import qualified API.PureStructs.Exports as PureStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Services.Main as Handle
import qualified TextMessages.LoggerMessages as LoggerMsgs

processCommonMsgs ::
  (Handle.Services m, BotEx.MonadThrow m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
processCommonMsgs env msg = do
  defaultRepeat <- runReaderT Env.eRep env
  let mbChatId = PureStructs.mbChatID msg
  maybe
    (BotEx.throwOtherException LoggerMsgs.chatIdNotFound)
    (processCommonMsgs' env msg defaultRepeat)
    mbChatId

processCommonMsgs' ::
  Handle.Services m =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processCommonMsgs' env msg defaultRepeat chatId = do
  mbRepeat <- Handle.findUser env chatId
  let currentRepeat = fromMaybe defaultRepeat mbRepeat
  repeatMsg msg currentRepeat env

repeatMsg ::
  (Handle.Services m) =>
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 env = pure env
repeatMsg msg repeatNumber env = do
  currentEnvironment <- Handle.sendMessage env msg
  repeatMsg msg (repeatNumber -1) currentEnvironment
