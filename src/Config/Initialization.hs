module Config.Initialization (setBotSettings) where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Config.Data as Config
import qualified Config.Struct as Config
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Exceptions.Exports as BotEx
import qualified Logger.Exports as Logger
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )
import Text.Read (readEither)
import qualified TextMessages.LoggerMessages as LoggerMsgs

setBotSettings ::
  Maybe Config.BotType ->
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
  let eiVKResponse = eitherDecode longPollResponse :: Either String VKStructs.Response
  either BotEx.throwParseExcept buildSettings eiVKResponse

buildSettings :: VKStructs.Response -> IO (Config.VKKey, Config.VKServer, Config.Offset)
buildSettings (VKStructs.Response (VKStructs.LongPollResponse key server ts)) = do
  let eiTs = readEither ts :: Either String Int
  either BotEx.throwParseExcept (longPollResponseSuccess key server) eiTs
buildSettings (VKStructs.Error (VKStructs.ResponseError errCode errMsg)) =
  BotEx.throwInitConfigExceptWithMessage $
    Logger.makeLogMessage
      LoggerMsgs.vkSettingsFatalError
      errorMessage
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
