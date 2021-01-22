module Logic.ProcMsgs.Common where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified Environment.Environment as Env
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
  Int ->
  Handle.SHandle m ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg n handle _ = do
  Handle.sendMessage handle msg
    >>= repeatMsg msg (n -1) handle
