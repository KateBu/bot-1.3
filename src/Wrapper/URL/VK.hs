module Wrapper.URL.VK where

import qualified API.PureStructs.Exports as PureStructs
import qualified Config.Exports as Config
import qualified Data.Text as T
import System.Random (Random (random), newStdGen)
import qualified Wrapper.URL.Structs as WrapStructs

buildGetUpdatesParams :: Config.Config -> [PureStructs.Params]
buildGetUpdatesParams (Config.VKBot (Config.VK _ _ key _ ts)) =
  [ PureStructs.ParamsText "act" "a_check",
    PureStructs.ParamsText "key" key,
    PureStructs.ParamsNum "ts" ts,
    PureStructs.ParamsText "wait" Config.timeOut,
    PureStructs.ParamsNum "mode" 2
  ]
buildGetUpdatesParams _ = []

buildGetUpdatesHostPath :: Config.Config -> Maybe WrapStructs.HostPath
buildGetUpdatesHostPath (Config.VKBot (Config.VK _ _ _ server _)) =
  pure $
    let (host, rest) = T.span (/= '/') (T.drop 8 server)
        path = filter (/= "") (T.splitOn "/" rest)
     in WrapStructs.HostPath host path
buildGetUpdatesHostPath _ = Nothing

buildSendHostPath :: Maybe WrapStructs.HostPath
buildSendHostPath = pure $ WrapStructs.HostPath "api.vk.com" ["method", "messages.send"]

buildSendBasicParams :: Config.Config -> IO [PureStructs.Params]
buildSendBasicParams (Config.VKBot (Config.VK token _ _ _ _)) = do
  randomId <- getRandomId
  pure
    [ PureStructs.ParamsNum "random_id" randomId,
      PureStructs.ParamsText "v" Config.vkApiVersion,
      PureStructs.ParamsText "access_token" token
    ]
buildSendBasicParams _ = pure []

getRandomId :: IO Int
getRandomId = do
  generator <- newStdGen
  pure (fst . random $ generator :: Int)
