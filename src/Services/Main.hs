module Services.Main (Services (..)) where

import qualified API.PureStructs.Exports as PureStructs
import qualified Environment.Exports as Env
import Services.FunctionsIO
  ( addUserIO,
    findUserIO,
    getUpdatesIO,
    sendMessageIO,
    updateUserIO,
  )

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
