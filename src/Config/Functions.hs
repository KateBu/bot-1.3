module Config.Functions where

import Config.Struct
  ( Config (..),
    Offset,
    Telegram (Telegram),
    VK (VK),
  )

configSetOffset :: Config -> Offset -> Config
configSetOffset (TBot (Telegram token _)) newOffset =
  TBot $ Telegram token newOffset
configSetOffset (VKBot (VK token group key server _)) newOffset =
  VKBot $ VK token group key server newOffset

configGetUid :: Config -> Offset
configGetUid (VKBot (VK _ _ _ _ updateId)) = updateId
configGetUid (TBot (Telegram _ updateId)) = updateId
