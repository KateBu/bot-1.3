module API.VK.URL where

import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs
import System.Random (Random (random), newStdGen)
import qualified Wrapper.Structs as WrapStructs

buildGetUpdatesParams :: Env.Config -> [PureStructs.Params]
buildGetUpdatesParams (Env.VKBot (Env.VK _ _ key _ ts)) =
  [ PureStructs.ParamsText "act" "a_check",
    PureStructs.ParamsText "key" key,
    PureStructs.ParamsNum "ts" ts,
    PureStructs.ParamsText "wait" Env.timeOut,
    PureStructs.ParamsNum "mode" 2
  ]
buildGetUpdatesParams _ = []

buildGetUpdatesHostPath :: Env.Config -> Maybe WrapStructs.HostPath
buildGetUpdatesHostPath (Env.VKBot (Env.VK _ _ _ server _)) =
  pure $
    let (host, rest) = T.span (/= '/') (T.drop 8 server)
        path = filter (/= "") (T.splitOn "/" rest)
     in WrapStructs.HostPath host path
buildGetUpdatesHostPath _ = Nothing

buildSendHostPath :: Maybe WrapStructs.HostPath
buildSendHostPath = pure $ WrapStructs.HostPath "api.vk.com" ["method", "messages.send"]

buildSendBasicParams :: Env.Config -> IO [PureStructs.Params]
buildSendBasicParams (Env.VKBot (Env.VK token _ _ _ _)) = do
  randomId <- getRandomId
  pure
    [ PureStructs.ParamsNum "random_id" randomId,
      PureStructs.ParamsText "v" Env.vkApiVersion,
      PureStructs.ParamsText "access_token" token
    ]
buildSendBasicParams _ = pure []

getRandomId :: IO Int
getRandomId = do
  generator <- newStdGen
  pure (fst . random $ generator :: Int)
