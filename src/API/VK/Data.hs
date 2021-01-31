module API.VK.Data where

import qualified API.Wrapper.Structs as WrapStructs
import qualified Config.Internals as Config
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs
import System.Random (Random (random), newStdGen)

updateParams :: Config.Config -> [PureStructs.Params]
updateParams (Config.VKBot (Config.VK _ _ key _ ts)) =
  [ PureStructs.ParamsText "act" "a_check",
    PureStructs.ParamsText "key" key,
    PureStructs.ParamsNum "ts" ts,
    PureStructs.ParamsText "wait" Config.timeOut,
    PureStructs.ParamsNum "mode" 2
  ]
updateParams _ = []

updateHostPath :: Config.Config -> Maybe WrapStructs.HostPath
updateHostPath (Config.VKBot (Config.VK _ _ _ server _)) =
  pure $
    let (host, rest) = T.span (/= '/') (T.drop 8 server)
        path = filter (/= "") (T.splitOn "/" rest)
     in WrapStructs.HostPath host path
updateHostPath _ = Nothing

sendHostPath :: Maybe WrapStructs.HostPath
sendHostPath = pure $ WrapStructs.HostPath "api.vk.com" ["method", "messages.send"]

sendBasicParams :: Config.Config -> IO [PureStructs.Params]
sendBasicParams (Config.VKBot (Config.VK tok _ _ _ _)) = do
  rid <- getRandonId
  pure
    [ PureStructs.ParamsNum "random_id" rid,
      PureStructs.ParamsText "v" Config.vkApiVersion,
      PureStructs.ParamsText "access_token" tok
    ]
sendBasicParams _ = pure []

getRandonId :: IO Int
getRandonId = do
  gen <- newStdGen
  pure (fst . random $ gen :: Int)