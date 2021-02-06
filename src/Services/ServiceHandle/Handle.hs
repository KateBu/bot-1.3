module Services.ServiceHandle.Handle where

import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.API.Handle as APIHandle
import qualified Services.DB.Handle as DBHandle

class (Monad m) => Services m where
  getUpdates :: Env.Environment m -> m [PureStructs.PureMessage]
  sendMessage :: Env.Environment m -> PureStructs.PureMessage -> m (Env.Environment m)
  findUser :: Env.Environment m -> PureStructs.ChatID -> m (Maybe Env.RepeatNumber)
  addUser :: Env.Environment m -> PureStructs.ChatID -> Env.RepeatNumber -> m ()
  updateUser :: Env.Environment m -> PureStructs.ChatID -> Env.RepeatNumber -> m ()

instance Services IO where
  getUpdates = getUpdatesIO
  sendMessage = sendMessageIO
  findUser = findUserIO
  addUser = addUserIO
  updateUser = updateUserIO

getUpdatesIO :: Env.Environment IO -> IO [PureStructs.PureMessage]
getUpdatesIO env =
  APIHandle.withApiHandle env APIHandle.hGetUpdates

sendMessageIO :: Env.Environment IO -> PureStructs.PureMessage -> IO (Env.Environment IO)
sendMessageIO env msg =
  APIHandle.withApiHandle env $ flip APIHandle.hSendMessage msg

findUserIO ::
  Env.Environment IO ->
  PureStructs.ChatID ->
  IO (Maybe Env.RepeatNumber)
findUserIO env chatId =
  DBHandle.withDBHandle env $ flip DBHandle.findUser chatId

addUserIO ::
  Env.Environment IO ->
  PureStructs.ChatID ->
  Env.RepeatNumber ->
  IO ()
addUserIO env chatId repeatNumber =
  DBHandle.withDBHandle env function
  where
    function = \handle -> DBHandle.addUser handle chatId repeatNumber

updateUserIO ::
  Env.Environment IO ->
  PureStructs.ChatID ->
  Env.RepeatNumber ->
  IO ()
updateUserIO env chatId repeatNumber =
  DBHandle.withDBHandle env function
  where
    function = \handle -> DBHandle.updateUser handle chatId repeatNumber
