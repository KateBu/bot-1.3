module Logic.ProcMsgs.Common where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.Exports as Handle

processCommonMsgs ::
  (Monad m, Handle.Services m) =>
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
  (Monad m, Handle.Services m) =>
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 env = pure env
repeatMsg msg repeatNumber env = do
  Handle.sendMessage env msg
    >>= repeatMsg msg (repeatNumber -1)
