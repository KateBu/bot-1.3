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
telByteStringToPureMessageList env bytestring =
  decodeByteString env bytestring >>= telUpdatesToPureMessageList env

decodeByteString ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString env bytestring = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.tDecBS
  let mbTelegramUpdates = decode bytestring :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr bytestring) (decodeScs logger) mbTelegramUpdates

decodeScs ::
  Logger.Logger IO ->
  TStructs.TelegramUpdates ->
  IO TStructs.TelegramUpdates
decodeScs logger tUpd =
  Logger.botLog logger LoggerMsgs.telDecBsScs
    >> pure tUpd

telUpdatesToPureMessageList ::
  Env.Environment IO ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList env tUpd = do
  hMsg <- runReaderT Env.eHelpMsg env
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.parseTelMsgScs
  pure $ telUpdateToPureMessage hMsg <$> TStructs.result tUpd

getUpdErr :: BSL.ByteString -> IO TStructs.TelegramUpdates
getUpdErr bytestring = do
  let eiTelegramErr = eitherDecode bytestring :: Either String TStructs.TelegramUpdatesError
  either BotEx.throwParseExcept telError eiTelegramErr

telError :: TStructs.TelegramUpdatesError -> IO TStructs.TelegramUpdates
telError err =
  BotEx.throwUpdateExcept
    ( Logger.makeLogMessage
        LoggerMsgs.getUpdFld
        errMessage
    )
  where
    errMessage =
      "\n\terror code: " <> (T.pack . show . TStructs.error_code) err
        <> "\n\terror describtion: "
        <> TStructs.description err
