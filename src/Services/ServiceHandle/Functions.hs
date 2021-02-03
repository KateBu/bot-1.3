module Services.ServiceHandle.Functions where

import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.API.Handle as APIHandle
import qualified Services.DB.Handle as DBHandle
import Services.ServiceHandle.Structs (SHandle (hAPI, hDB))

getUpdates :: Monad m => SHandle m -> m [PureStructs.PureMessage]
getUpdates handle = do
  hApiHandle <- hAPI handle
  APIHandle.hGetUpdates hApiHandle

sendMessage ::
  Monad m =>
  SHandle m ->
  PureStructs.PureMessage ->
  m (Env.Environment m)
sendMessage handle msg = do
  hApiHandle <- hAPI handle
  APIHandle.hSendMessage hApiHandle msg

findUser ::
  Monad m =>
  SHandle m ->
  PureStructs.ChatID ->
  m (Maybe Env.RepeatNumber)
findUser handle userId = do
  hDbHandle <- hDB handle
  DBHandle.findUser hDbHandle userId

addUser ::
  Monad m =>
  SHandle m ->
  PureStructs.ChatID ->
  Env.RepeatNumber ->
  m ()
addUser handle userId rep = do
  hDbHandle <- hDB handle
  DBHandle.addUser hDbHandle userId rep

updateUser ::
  Monad m =>
  SHandle m ->
  PureStructs.ChatID ->
  Env.RepeatNumber ->
  m ()
updateUser handle userId rep = do
  hDbHandle <- hDB handle
  DBHandle.updateUser hDbHandle userId rep
