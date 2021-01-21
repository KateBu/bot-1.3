module Services.ServiceHandle.SHandleFunctions where

import Environment.Environment as Env (Environment)
import qualified Logic.PureStructs as PureStructs
import qualified Services.APIHandle.APIHandle as APIHandle
import qualified Services.DBHandle.DBHandle as DBHandle
import Services.ServiceHandle.SHandleStructs (SHandle (hAPI, hDB))

getUpdates :: Monad m => SHandle m -> m [PureStructs.PureMessage]
getUpdates handle = do
  hApiHandle <- hAPI handle
  APIHandle.hGetUpdates hApiHandle

sendMessage ::
  Monad m =>
  SHandle m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
sendMessage handle msgs = do
  hApiHandle <- hAPI handle
  APIHandle.hSendMessage hApiHandle msgs

findUser ::
  Monad m =>
  SHandle m ->
  Int ->
  m (Maybe Int)
findUser handle userId = do
  hDbHandle <- hDB handle
  DBHandle.findUser hDbHandle userId

addUser ::
  Monad m =>
  SHandle m ->
  Int ->
  Int ->
  m ()
addUser handle userId repeat = do
  hDbHandle <- hDB handle
  DBHandle.addUser hDbHandle userId repeat

updateUser ::
  Monad m =>
  SHandle m ->
  Int ->
  Int ->
  m ()
updateUser handle userId repeat = do
  hDbHandle <- hDB handle
  DBHandle.updateUser hDbHandle userId repeat
