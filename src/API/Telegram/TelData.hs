module API.Telegram.TelData where

import qualified Data.Text as T 

import qualified Logic.PureStructs as PureStructs 
import qualified Config.Config as Config 

updateHostPath :: Config.BotType -> Maybe PureStructs.HostPath
updateHostPath (Config.Telegram tok _) = pure $ PureStructs.HostPath "api.telegram.org" ["bot" <> tok,"getUpdates"]
updateHostPath _ = Nothing 

updateParams :: Config.BotType -> [PureStructs.Params]
updateParams (Config.Telegram _ offset) = [PureStructs.ParamsNum "offset" offset
    , PureStructs.ParamsText "timeout" Config.timeOut]
updateParams _ = []

sendHostPath :: Config.BotType -> PureStructs.MessageType -> Maybe PureStructs.HostPath
sendHostPath (Config.Telegram tok _) PureStructs.MTEmpty = undefined 
sendHostPath (Config.Telegram tok _) (PureStructs.MTCallbackQuery txt) = undefined 
sendHostPath (Config.Telegram tok _) (PureStructs.MTUserCommand PureStructs.Help) = pure $
    PureStructs.HostPath "api.telegram.org" [
        "bot"<> tok 
        , "sendMessage"
        ]
sendHostPath (Config.Telegram tok _) (PureStructs.MTUserCommand PureStructs.Repeat) = pure $
    PureStructs.HostPath "api.telegram.org" [
        "bot"<> tok 
        , "sendMessage"
        ]
sendHostPath (Config.Telegram tok _) (PureStructs.MTCommon mType) = pure $ 
    PureStructs.HostPath "api.telegram.org" [
        "bot"<> tok 
        , "send" <> mType
        ]
