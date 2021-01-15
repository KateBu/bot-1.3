module Config.MakeConfigFunctions where

import qualified API.VK.Structs as VKStructs
import Config.ConfigData as Config (vkApiVersion, vkLongPollUrl)
import qualified Config.ConfigStructs as Config
  ( BotType (..),
    Config (Config),
    Telegram (Telegram),
    Token,
    VK (VK),
  )
import Control.Exception (IOException, try)
import Control.Monad ()
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )

parseConfig :: String -> IO Config.Config
parseConfig path = do
  getConfigFile path >>= parseConfigFile

getConfigFile :: String -> IO Configurator.Config
getConfigFile path = do
  config <- try $ Configurator.load [Configurator.Required path] :: IO (Either IOException Configurator.Config)
  either BotEx.throwIOException pure config

parseConfigFile ::
  Configurator.Config ->
  IO Config.Config
parseConfigFile conf = do
  botT <- Configurator.lookup conf (T.pack "bot.botType") :: IO (Maybe T.Text)
  rep <- Configurator.lookup conf (T.pack "bot.repetition") :: IO (Maybe Int)
  msg <- Configurator.lookup conf (T.pack "bot.helpMessage") :: IO (Maybe T.Text)
  prior <- Configurator.lookup conf (T.pack "bot.logPriority") :: IO (Maybe String)
  tTok <- Configurator.lookup conf (T.pack "bot.telegramToken") :: IO (Maybe Config.Token)
  vkTok <- Configurator.lookup conf (T.pack "bot.VKToken") :: IO (Maybe Config.Token)
  vkGroup <- Configurator.lookup conf (T.pack "bot.VKGroupID") :: IO (Maybe Int)
  setBotTypeSettings botT vkGroup vkTok tTok >>= initConfig msg rep prior

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

initConfig ::
  Maybe T.Text ->
  Maybe Int ->
  Maybe String ->
  Config.BotType ->
  IO Config.Config
initConfig (Just helpMsg) (Just rep) (Just prior) botType = do
  pure $ Config.Config botType helpMsg (checkRepNumber rep) Map.empty (read prior)
initConfig _ _ _ _ = BotEx.throwInitConfigExcept

makeVkLonpPollUrl :: Int -> T.Text -> String
makeVkLonpPollUrl group tok =
  vkLongPollUrl
    <> show group
    <> "&access_token="
    <> T.unpack tok
    <> "&v="
    <> T.unpack vkApiVersion

checkRepNumber :: Int -> Int
checkRepNumber val
  | val <= 1 = 1
  | val >= 5 = 5
  | otherwise = val
