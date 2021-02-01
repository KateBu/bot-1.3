module Config.Functions where

import Config.Struct
  ( Config (..),
    Offset,
    Telegram (Telegram),
    VK (VK),
  )

configSetOffset :: Config -> Offset -> Config
configSetOffset (TBot (Telegram tok _)) newOffset =
  TBot $ Telegram tok newOffset
configSetOffset (VKBot (VK tok group key serv _)) newOffset =
  VKBot $ VK tok group key serv newOffset

configGetUid :: Config -> Offset
configGetUid (VKBot (VK _ _ _ _ uid)) = uid
configGetUid (TBot (Telegram _ uid)) = uid
