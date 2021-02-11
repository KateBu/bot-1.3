module Config.Functions where

import Config.Struct
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
