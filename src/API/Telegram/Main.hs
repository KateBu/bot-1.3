module API.Telegram.Main (decodePureMessageList) where

import API.Telegram.Functions.Builders (buildPureMessage)
import qualified API.Telegram.Structs.Updates as Telegram
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logic.Structs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs

decodePureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
decodePureMessageList env bytestring = do
  updates <- decodeUpdates env bytestring
  buildPureMessageList env updates

decodeUpdates ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO Telegram.Updates
decodeUpdates env bytestring = do
  logger <- runReaderT Env.eLogger env
  Env.botLog logger logMsg
  let mbUpdates = decode bytestring :: Maybe Telegram.Updates
  maybe (throwUpdateError bytestring) (decodeSuccess logger) mbUpdates
  where
    logMsg = LoggerMsgs.telegramBytestringDecodingInProgress

decodeSuccess ::
  Env.Logger IO ->
  Telegram.Updates ->
  IO Telegram.Updates
decodeSuccess logger updates =
  Env.botLog logger logMsg >> pure updates
  where
    logMsg = LoggerMsgs.telegramBytestringDecodingSuccess

buildPureMessageList ::
  Env.Environment IO ->
  Telegram.Updates ->
  IO [PureStructs.PureMessage]
buildPureMessageList env updates = do
  helpMsg <- runReaderT Env.eHelpMsg env
  logger <- runReaderT Env.eLogger env
  Env.botLog logger logMsg
  maybe (BotEx.throwUpdateExcept LoggerMsgs.telegramUpdatesFailed) pure (mbPureMsgs helpMsg)
  where
    logMsg = LoggerMsgs.parseTelelegramMsgSuccess
    mbPureMsgs helpMsg = sequence $ buildPureMessage helpMsg <$> Telegram.result updates

throwUpdateError :: BSL.ByteString -> IO Telegram.Updates
throwUpdateError bytestring = do
  let eiError = eitherDecode bytestring :: Either String Telegram.UpdatesError
  either BotEx.throwParseExcept buildUpdateError eiError

buildUpdateError :: Telegram.UpdatesError -> IO Telegram.Updates
buildUpdateError err =
  BotEx.throwUpdateExcept
    ( Env.makeLogMessage
        LoggerMsgs.getUpdateFailed
        errMessage
    )
  where
    errMessage =
      "\n\terror code: " <> (T.pack . show . Telegram.error_code) err
        <> "\n\terror describtion: "
        <> Telegram.description err
