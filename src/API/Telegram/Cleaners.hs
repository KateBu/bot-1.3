module API.Telegram.Cleaners (telByteStringToPureMessageList) where

import API.Telegram.Cleaners.MakePureMessage
  ( telUpdateToPureMessage,
  )
import qualified API.Telegram.Structs.Updates as TStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

telByteStringToPureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
telByteStringToPureMessageList env bytestring = do
  telegramUpdates <- decodeByteString env bytestring
  telUpdatesToPureMessageList env telegramUpdates

decodeByteString ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString env bytestring = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger logMsg
  let mbTelegramUpdates = decode bytestring :: Maybe TStructs.TelegramUpdates
  maybe (getUpdateError bytestring) (decodeSuccess logger) mbTelegramUpdates
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

telUpdatesToPureMessageList ::
  Env.Environment IO ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList env telegramUpdates = do
  helpMsg <- runReaderT Env.eHelpMsg env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger logMsg
  maybe (BotEx.throwUpdateExcept LoggerMsgs.telegramUpdatesFailed) pure (pureMsg helpMsg)
  where
    logMsg = LoggerMsgs.parseTelelegramMsgSuccess
    pureMsg helpMsg = sequence $ telUpdateToPureMessage helpMsg <$> TStructs.result telegramUpdates

getUpdateError :: BSL.ByteString -> IO TStructs.TelegramUpdates
getUpdateError bytestring = do
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
