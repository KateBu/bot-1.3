module Logic.ProcMsgs.Common where

import qualified Environment.Environment as Env
import qualified Logic.PureStructs as PureStructs

processMsgsCommon ::
  Monad m =>
  Env.Env m ->
  PureStructs.SendFunction m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Env m)
processMsgsCommon env function msg chid = do
  newRepeat <- Env.eFindUserRepeat chid env
  repeatMsg msg newRepeat function env

repeatMsg ::
  Monad m =>
  PureStructs.PureMessage ->
  Int ->
  PureStructs.SendFunction m ->
  Env.Env m ->
  m (Env.Env m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg n function env = do
  function env msg
    >>= repeatMsg msg (n -1) function
