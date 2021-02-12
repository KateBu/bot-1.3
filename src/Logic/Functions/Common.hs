module Logic.Functions.Common (processCommonMsgs) where

import qualified API.PureStructs.Exports as PureStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Services.Main as Handle

processCommonMsgs ::
  (Handle.Services m) =>
  Env.Environment m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processCommonMsgs env msg chatId = do
  defaultRepeat <- runReaderT Env.eRep env
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
