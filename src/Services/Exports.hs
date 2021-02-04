module Services.Exports (module SHandle) where

import Services.ServiceHandle.Functions as SHandle
  ( addUser,
    findUser,
    getUpdates,
    sendMessage,
    updateUser,
  )
import Services.ServiceHandle.Handle as SHandle
  ( SHandle (..),
    new,
  )
