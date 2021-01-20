module Config.ConfigFunctions where

import Config.ConfigStruct
    ( VK(VK), Telegram(Telegram), Config(..) ) 

configSetOffset :: Config -> Int -> Config
configSetOffset (TBot (Telegram tok _)) newOffset =
  TBot $ Telegram tok newOffset
configSetOffset (VKBot (VK tok group key serv _)) newOffset =
  VKBot $ VK tok group key serv newOffset

configGetUid :: Config -> Int
configGetUid (VKBot (VK _ _ _ _ uid))= uid
configGetUid (TBot (Telegram _ uid)) = uid
