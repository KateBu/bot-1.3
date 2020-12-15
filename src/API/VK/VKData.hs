module API.VK.VKData where

import qualified Data.Text as T 
import Data.List.Split ( splitOn )
import System.Random ( newStdGen, Random(random) )

import qualified Logic.PureStructs as PureStructs 
import qualified Config.Config as Config 

updateParams :: Config.BotType -> [PureStructs.Params]
updateParams (Config.VK _ _ key _ ts) = [PureStructs.ParamsText "act" "a_check"
    , PureStructs.ParamsText "key" key
    , PureStructs.ParamsNum "ts" ts
    , PureStructs.ParamsText "wait" Config.timeOut
    ]
updateParams _ = [] 

updateHostPath :: Config.BotType -> Maybe PureStructs.HostPath 
updateHostPath (Config.VK _ _ _ server _) = pure $ let 
    (host,rest) = span (/='/') (drop 8 (T.unpack server))
    path = filter (/="") (splitOn "/" rest) 
    in PureStructs.HostPath (T.pack host) (map T.pack path) 

sendHostPath :: PureStructs.HostPath
sendHostPath = PureStructs.HostPath "api.vk.com" ["method","messages.send"]

sendBasicParams :: Config.BotType -> IO [PureStructs.Params]
sendBasicParams (Config.VK tok _ _ _ _) = do 
    rid <- getRandonId
    pure $ [PureStructs.ParamsNum "random_id" rid
        , PureStructs.ParamsText "v" Config.vkApiVersion
        --, PureStructs.ParamsText "timeout" Config.timeOut
        , PureStructs.ParamsText "access_token" tok
        ]
sendBasicParams _ = pure []

getRandonId :: IO Int
getRandonId = do 
    gen <- newStdGen 
    pure $ ( (fst . random) gen :: Int)