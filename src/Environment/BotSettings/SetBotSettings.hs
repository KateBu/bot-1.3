module Environment.BotSettings.SetBotSettings where

import qualified API.VK.Structs.Exports as VKStructs
import Config.Data
  ( vkApiVersion,
    vkLongPollUrl,
  )
import qualified Config.Exports as Config
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Logger.Exports as Logger
import qualified Environment.Structs as Env
import qualified Exceptions.Exports as BotEx
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

setBotTypeSettings ::
  Maybe Env.BotType ->
  Maybe Config.VKGroup ->
  Maybe Config.Token ->
  Maybe Config.Token ->
  IO Config.Config
setBotTypeSettings (Just "VK") (Just group) (Just vkToken) _ = do
  vkSettings <- getVKSettings group vkToken
  vkSettingsScs group vkToken vkSettings
setBotTypeSettings (Just "Telegram") _ _ (Just tToken) =
  pure $ Config.TBot (Config.Telegram tToken 0)
setBotTypeSettings _ _ _ _ = BotEx.throwInitConfigExcept

getVKSettings :: Config.VKGroup -> Config.Token -> IO (T.Text, T.Text, Int)
getVKSettings group tok = do
  respBody <- getLongPollReqBody group tok
  getLongPollInfo respBody

getLongPollReqBody :: Config.VKGroup -> Config.Token -> IO BSL.ByteString
getLongPollReqBody group tok = do
  resBody <-
    try (getResponseBody <$> request) ::
      IO (Either IOException BSL.ByteString)
  either BotEx.throwIOException pure resBody
  where
    request = do
      request' <- parseRequest (makeVkLonpPollUrl group tok)
      httpLBS request'

getLongPollInfo ::
  BSL.ByteString ->
  IO (Config.VKKey, Config.VKServer, Config.Offset)
getLongPollInfo respBody = do
  let eiResponse = eitherDecode respBody :: Either String VKStructs.VKResponse
  either BotEx.throwParseExcept tryMakeVKSettings eiResponse

tryMakeVKSettings :: VKStructs.VKResponse -> IO (Config.VKKey, Config.VKServer, Config.Offset)
tryMakeVKSettings (VKStructs.VKResponse (VKStructs.LongPollResponse key server ts)) = pure (key, server, read ts)
tryMakeVKSettings (VKStructs.VKError (VKStructs.ResponseError errCode errMsg)) =
  BotEx.throwBotExcept $
    BotEx.InitConfigExcept
      ( Logger.makeLogMessage
          LoggerMsgs.initConfigExcept
          errorMessage
      )
  where
    errorMessage =
      "error_code: "
        <> (T.pack . show) errCode
        <> "error_message: "
        <> errMsg
tryMakeVKSettings _ = BotEx.throwParseExcept ""

vkSettingsScs ::
  Config.VKGroup ->
  Config.Token ->
  (Config.VKKey, Config.VKServer, Config.Offset) ->
  IO Config.Config
vkSettingsScs group token (key, serv, ts) = do
  let vk = Config.VK token group key serv ts
  pure $ Config.VKBot vk

makeVkLonpPollUrl :: Config.VKGroup -> Config.Token -> String
makeVkLonpPollUrl group tok =
  vkLongPollUrl
    <> show group
    <> "&access_token="
    <> T.unpack tok
    <> "&v="
    <> T.unpack vkApiVersion
