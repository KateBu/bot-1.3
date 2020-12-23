module API.VK.VKData where

import qualified Data.Text as T 
import System.Random ( newStdGen, Random(random) )
import qualified Logic.PureStructs as PureStructs 
import qualified Config.Config as Config 
import qualified API.VK.Structs as VKStructs 

urlToHostPath :: T.Text -> Maybe PureStructs.HostPath 
urlToHostPath url = 
    let 
        hostPath = T.drop 3 (T.dropWhile (/= ':') $ T.takeWhile (/= '?') url)
        (host,rest) = T.span ( /= '/') hostPath 
        path = filter (/= "") (T.splitOn "/" rest)
    in pure $ PureStructs.HostPath host path 

urlToParams :: VKStructs.Url -> Maybe [PureStructs.Params] 
urlToParams url = let 
    params = T.drop 1 $ T.dropWhile (/='?') url 
    keyVals = filter (/="") $ T.splitOn "&" params 
    keys = T.splitOn "=" <$> keyVals   
    in mapM listToParams keys  

updateParams :: Config.BotType -> [PureStructs.Params]
updateParams (Config.VKBot(Config.VK _ _ key _ ts)) = [PureStructs.ParamsText "act" "a_check"
    , PureStructs.ParamsText "key" key
    , PureStructs.ParamsNum "ts" ts
    , PureStructs.ParamsText "wait" Config.timeOut
    , PureStructs.ParamsNum "mode" 2 
    ]
updateParams _ = [] 

updateHostPath :: Config.BotType -> Maybe PureStructs.HostPath 
updateHostPath (Config.VKBot(Config.VK _ _ _ server _)) = pure $ let 
    (host,rest) = T.span (/='/') (T.drop 8 server)
    path = filter (/="") (T.splitOn "/" rest) 
    in PureStructs.HostPath host path
updateHostPath _ = Nothing 

sendHostPath :: PureStructs.HostPath
sendHostPath = PureStructs.HostPath "api.vk.com" ["method","messages.send"]

sendBasicParams :: Config.BotType -> IO [PureStructs.Params]
sendBasicParams (Config.VKBot (Config.VK tok _ _ _ _)) = do 
    rid <- getRandonId
    pure $ [PureStructs.ParamsNum "random_id" rid
        , PureStructs.ParamsText "v" Config.vkApiVersion
        , PureStructs.ParamsText "access_token" tok
        ]
sendBasicParams _ = pure []

getRandonId :: IO Int
getRandonId = do 
    gen <- newStdGen 
    pure $ ( (fst . random) gen :: Int)

uploadPhotoServerHostPath :: PureStructs.HostPath
uploadPhotoServerHostPath = PureStructs.HostPath "api.vk.com" ["method","photos.getMessagesUploadServer"]

getUploadFileParams :: Config.BotType -> [PureStructs.Params]
getUploadFileParams (Config.VKBot(Config.VK tok _ _ _ _)) = 
    [
        PureStructs.ParamsText "v" Config.vkApiVersion
        , PureStructs.ParamsText "access_token" tok
    ]
getUploadFileParams _ = []

uploadPhotoHostPath :: VKStructs.UploadUrl -> Maybe PureStructs.HostPath 
uploadPhotoHostPath (VKStructs.UploadUrl url) = urlToHostPath url 
    
uploadPhotoParams :: VKStructs.UploadUrl -> Maybe [PureStructs.Params] 
uploadPhotoParams (VKStructs.UploadUrl url) = urlToParams url 

listToParams :: [T.Text] -> Maybe PureStructs.Params 
listToParams (key : val : []) = Just $ PureStructs.ParamsText key val 
listToParams _ = Nothing 

savePhotoServerHostPath :: PureStructs.HostPath 
savePhotoServerHostPath = PureStructs.HostPath "api.vk.com" ["method","photos.saveMessagesPhoto"]