module Logic.ProcMsgs.Common where

import qualified Environment.Environment as Env
import qualified Logic.PureStructs as PureStructs

processMsgsCommon ::
  Monad m =>
  Env.Environment m ->
  PureStructs.SendFunction m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Env.Environment m)
processMsgsCommon env function msg chid = undefined {-do
  newRepeat <- Env.eFindUserRepeat chid env
  repeatMsg msg newRepeat function env
-}
repeatMsg ::
  Monad m =>
  PureStructs.PureMessage ->
  Int ->
  PureStructs.SendFunction m ->
  Env.Environment m ->
  m (Env.Environment m)
repeatMsg _ 0 _ env = pure env
repeatMsg msg n function env = undefined {- do
  function env msg
    >>= repeatMsg msg (n -1) function-}
