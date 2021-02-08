module Environment.Config.Functions where

import Environment.Config.Struct
  ( Config (..),
    Offset,
    Telegram (Telegram),
    VK (VK),
  )

setOffset :: Config -> Offset -> Config
setOffset (TBot (Telegram token _)) newOffset =
  TBot $ Telegram token newOffset
setOffset (VKBot (VK token group key server _)) newOffset =
  VKBot $ VK token group key server newOffset
