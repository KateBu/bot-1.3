module Environment.Config.Initialization where

import qualified API.VK.Structs.Exports as VKStructs
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Config.Data as Config
import qualified Environment.Config.Struct as Config
import qualified Environment.Logger.Exports as Logger
import qualified Environment.Structs as Env
import qualified Exceptions.Exports as BotEx
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )
import Text.Read (readEither)
import qualified TextMessages.LoggerMessages as LoggerMsgs

setBotSettings ::
  Maybe Env.BotType ->
  Maybe Config.VKGroup ->
  Maybe Config.Token ->
  Maybe Config.Token ->
  IO Config.Config
setBotSettings (Just "VK") (Just group) (Just vkToken) _ = do
  vkSettings <- buildVKSettings group vkToken
  vkSettingsSuccess group vkToken vkSettings
setBotSettings (Just "Telegram") _ _ (Just telegramToken) =
  pure $ Config.TBot (Config.Telegram telegramToken 0)
setBotSettings _ _ _ _ = BotEx.throwInitConfigExcept

buildVKSettings :: Config.VKGroup -> Config.Token -> IO (T.Text, T.Text, Int)
buildVKSettings group token = do
  longPollResponse <- getLongPollRequestBody group token
  getLongPollInfo longPollResponse

getLongPollRequestBody :: Config.VKGroup -> Config.Token -> IO BSL.ByteString
getLongPollRequestBody group token = do
  eiLongPollResponse <-
    try (getResponseBody <$> request) ::
      IO (Either IOException BSL.ByteString)
  either BotEx.throwIOException pure eiLongPollResponse
  where
    request = do
      request' <- parseRequest (buildVkLonpPollUrl group token)
      httpLBS request'

getLongPollInfo ::
  BSL.ByteString ->
  IO (Config.VKKey, Config.VKServer, Config.Offset)
getLongPollInfo longPollResponse = do
  let eiVKResponse = eitherDecode longPollResponse :: Either String VKStructs.VKResponse
  either BotEx.throwParseExcept buildSettings eiVKResponse

buildSettings :: VKStructs.VKResponse -> IO (Config.VKKey, Config.VKServer, Config.Offset)
buildSettings (VKStructs.VKResponse (VKStructs.LongPollResponse key server ts)) = do
  let eiTs = readEither ts :: Either String Int
  either BotEx.throwParseExcept (longPollResponseSuccess key server) eiTs
buildSettings (VKStructs.VKError (VKStructs.ResponseError errCode errMsg)) =
  BotEx.throwBotExcept $
    BotEx.InitConfigExcept
      ( Logger.makeLogMessage
          LoggerMsgs.vkSettingsFatalError
          errorMessage
      )
  where
    errorMessage =
      "error_code: "
        <> (T.pack . show) errCode
        <> "error_message: "
        <> errMsg
buildSettings _ = BotEx.throwParseExcept ""

vkSettingsSuccess ::
  Config.VKGroup ->
  Config.Token ->
  (Config.VKKey, Config.VKServer, Config.Offset) ->
  IO Config.Config
vkSettingsSuccess group token (key, server, ts) = do
  let vk = Config.VK token group key server ts
  pure $ Config.VKBot vk

buildVkLonpPollUrl :: Config.VKGroup -> Config.Token -> String
buildVkLonpPollUrl group server =
  Config.vkLongPollUrl
    <> show group
    <> "&access_token="
    <> T.unpack server
    <> "&v="
    <> T.unpack Config.vkApiVersion

longPollResponseSuccess ::
  Config.VKKey ->
  Config.VKServer ->
  Config.Offset ->
  IO (Config.VKKey, Config.VKServer, Config.Offset)
longPollResponseSuccess key server offset = pure (key, server, offset)
