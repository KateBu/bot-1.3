module Services.ServiceHandle.FunctionsIO where

import qualified Environment.Exports as Env
import qualified Logic.PureStructs as PureStructs
import qualified Services.API.Handle as APIHandle
import qualified Services.DB.Handle as DBHandle

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
  DBHandle.withDBHandle env addFunction
  where
    addFunction = \handle -> DBHandle.addUser handle chatId repeatNumber

updateUserIO ::
  Env.Environment IO ->
  PureStructs.ChatID ->
  Env.RepeatNumber ->
  IO ()
updateUserIO env chatId repeatNumber =
  DBHandle.withDBHandle env updateFunction
  where
    updateFunction = \handle -> DBHandle.updateUser handle chatId repeatNumber
