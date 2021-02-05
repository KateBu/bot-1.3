module Logic.ProcMsgs.Common where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.Exports as Handle

processCommonMsgs ::
  Monad m =>
  Env.Environment m ->
  Handle.SHandle m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processCommonMsgs env handle msg chatId = do
  defaultRepeat <- runReaderT Env.eRep env
  mbRepeat <- Handle.findUser handle chatId
  let currentRepeat = fromMaybe defaultRepeat mbRepeat
  repeatMsg msg currentRepeat handle env

repeatMsg ::
  Monad m =>
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  Handle.SHandle m ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg repeatNumber handle _ = do
  Handle.sendMessage handle msg
    >>= repeatMsg msg (repeatNumber -1) handle
