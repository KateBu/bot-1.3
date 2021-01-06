module Config.ConfigFunctions where

import Config.ConfigStructs
  ( BotType (TBot, VKBot),
    Config (Config, repetition, users),
    Telegram (Telegram),
    VK (VK),
  )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

setUserRepeat :: Config -> Int -> Int -> Config
setUserRepeat config chid newRep =
  let mbChid = Map.lookup chid (users config)
   in maybe (addUser chid newRep config) (reWriteUserRepeat config chid newRep) mbChid

reWriteUserRepeat :: Config -> Int -> Int -> Int -> Config
reWriteUserRepeat config chid newRep _ =
  (addUser chid newRep . deleteUser chid) config

findUserRepeat :: Config -> Int -> Int
findUserRepeat config chid = fromMaybe (repetition config) (Map.lookup chid (users config))

deleteUser :: Int -> Config -> Config
deleteUser chid (Config bt hm rep uss prior) =
  Config bt hm rep (Map.delete chid uss) prior

addUser :: Int -> Int -> Config -> Config
addUser chid newRep (Config bt hm rep uss prior) =
  Config bt hm rep (Map.insert chid newRep uss) prior

configSetOffset :: Config -> Int -> Config
configSetOffset (Config (TBot (Telegram tok _)) hm rep uss prior) newOffset =
  Config (TBot $ Telegram tok newOffset) hm rep uss prior
configSetOffset (Config (VKBot (VK tok group key serv _)) hm rep uss prior) newOffset =
  Config (VKBot $ VK tok group key serv newOffset) hm rep uss prior

configGetUid :: Config -> Int
configGetUid (Config (VKBot (VK _ _ _ _ uid)) _ _ _ _) = uid
configGetUid (Config (TBot (Telegram _ uid)) _ _ _ _) = uid
