module Environment.Config.Functions where

import Environment.Config.Struct
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
