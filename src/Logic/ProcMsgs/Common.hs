module Logic.ProcMsgs.Common where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.SHandle as Handle

processMsgsCommon ::
  Monad m =>
  Env.Environment m ->
  Handle.SHandle m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processMsgsCommon env handle msg userId = do
  defaultRepeat <- runReaderT Env.eRep env
  mbRepeat <- Handle.findUser handle userId
  let rep = fromMaybe defaultRepeat mbRepeat
  repeatMsg msg rep handle env

repeatMsg ::
  Monad m =>
  PureStructs.PureMessage ->
  Env.RepeatNumber ->
  Handle.SHandle m ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg repN handle _ = do
  Handle.sendMessage handle msg
    >>= repeatMsg msg (repN -1) handle
