module ConfigOld.MakeConfig.SetBotSetting where

import qualified API.VK.Structs as VKStructs
import ConfigOld.ConfigData as Config (vkApiVersion, vkLongPollUrl)
import qualified ConfigOld.ConfigStructs as Config
import Control.Exception (IOException, try)
import Control.Monad ()
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Logger.Logger as Logger
import qualified Environment.Logger.LoggerMsgs as LoggerMsgs
import qualified Exceptions.Exceptions as BotEx
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )

setBotTypeSettings ::
  Maybe T.Text ->
  Maybe Int ->
  Maybe Config.Token ->
  Maybe Config.Token ->
  IO Config.BotType
setBotTypeSettings (Just "VK") mbGroup mbVKToken _ = do
  vkSettings <- getVKSettings mbGroup mbVKToken
  vkSettingsScs mbGroup mbVKToken vkSettings
setBotTypeSettings (Just "Telegram") _ _ (Just tToken) =
  pure $ Config.TBot (Config.Telegram tToken 0)
setBotTypeSettings _ _ _ _ = BotEx.throwInitConfigExcept

getVKSettings :: Maybe Int -> Maybe T.Text -> IO (T.Text, T.Text, Int)
getVKSettings (Just group) (Just tok) = do
  getLongPollReqBody group tok >>= getLongPollInfo
getVKSettings _ _ = BotEx.throwInitConfigExcept

getLongPollReqBody :: Int -> T.Text -> IO BSL.ByteString
getLongPollReqBody group tok = do
  resBody <-
    try
      (getResponseBody <$> (parseRequest (makeVkLonpPollUrl group tok) >>= httpLBS)) ::
      IO (Either IOException BSL.ByteString)
  either BotEx.throwIOException pure resBody

getLongPollInfo ::
  BSL.ByteString ->
  IO (T.Text, T.Text, Int)
getLongPollInfo respBody = do
  let eiResponse = eitherDecode respBody :: Either String VKStructs.VKResponse
  either BotEx.throwParseExcept tryMakeVKSettings eiResponse

tryMakeVKSettings :: VKStructs.VKResponse -> IO (T.Text, T.Text, Int)
tryMakeVKSettings (VKStructs.VKResponse (VKStructs.LongPollResponse k s t)) = pure (k, s, read t)
tryMakeVKSettings (VKStructs.VKError (VKStructs.ResponseError ec em)) =
  BotEx.throwBotExcept $
    BotEx.InitConfigExcept
      ( Logger.makeLogMessage
          LoggerMsgs.initConfigExcept
          ( "error_code: "
              <> (T.pack . show) ec
              <> "error_message: "
              <> em
          )
      )
tryMakeVKSettings _ = BotEx.throwParseExcept ""

vkSettingsScs ::
  Maybe Int ->
  Maybe Config.Token ->
  (T.Text, T.Text, Int) ->
  IO Config.BotType
vkSettingsScs (Just group) (Just vKToken) (key, serv, ts) = do
  let vk = Config.VK vKToken group key serv ts
  pure $ Config.VKBot vk
vkSettingsScs _ _ _ = BotEx.throwInitConfigExcept

makeVkLonpPollUrl :: Int -> T.Text -> String
makeVkLonpPollUrl group tok =
  vkLongPollUrl
    <> show group
    <> "&access_token="
    <> T.unpack tok
    <> "&v="
    <> T.unpack vkApiVersion
