module API.Telegram.Main (decodePureMessageList) where

import API.Telegram.Functions.Builders (buildPureMessage)
import qualified API.Telegram.Structs.Updates as TStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.Structs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

decodePureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
decodePureMessageList env bytestring = do
  telegramUpdates <- decodeTelegramUpdates env bytestring
  buildPureMessageList env telegramUpdates

decodeTelegramUpdates ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeTelegramUpdates env bytestring = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger logMsg
  let mbTelegramUpdates = decode bytestring :: Maybe TStructs.TelegramUpdates
  maybe (throwUpdateError bytestring) (decodeSuccess logger) mbTelegramUpdates
  where
    logMsg = LoggerMsgs.telegramBytestringDecodingInProgress

decodeSuccess ::
  Logger.Logger IO ->
  TStructs.TelegramUpdates ->
  IO TStructs.TelegramUpdates
decodeSuccess logger telegramUpdates =
  Logger.botLog logger logMsg >> pure telegramUpdates
  where
    logMsg = LoggerMsgs.telegramBytestringDecodingSuccess

buildPureMessageList ::
  Env.Environment IO ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
buildPureMessageList env telegramUpdates = do
  helpMsg <- runReaderT Env.eHelpMsg env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger logMsg
  maybe (BotEx.throwUpdateExcept LoggerMsgs.telegramUpdatesFailed) pure (mbPureMsgs helpMsg)
  where
    logMsg = LoggerMsgs.parseTelelegramMsgSuccess
    mbPureMsgs helpMsg = sequence $ buildPureMessage helpMsg <$> TStructs.result telegramUpdates

throwUpdateError :: BSL.ByteString -> IO TStructs.TelegramUpdates
throwUpdateError bytestring = do
  let eiTelegramError = eitherDecode bytestring :: Either String TStructs.TelegramUpdatesError
  either BotEx.throwParseExcept telegramError eiTelegramError

telegramError :: TStructs.TelegramUpdatesError -> IO TStructs.TelegramUpdates
telegramError err =
  BotEx.throwUpdateExcept
    ( Logger.makeLogMessage
        LoggerMsgs.getUpdateFailed
        errMessage
    )
  where
    errMessage =
      "\n\terror code: " <> (T.pack . show . TStructs.error_code) err
        <> "\n\terror describtion: "
        <> TStructs.description err
