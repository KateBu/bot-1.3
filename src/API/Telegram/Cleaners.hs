module API.Telegram.Cleaners (telByteStringToPureMessageList) where

import API.Telegram.Cleaners.MakePureMessage
  ( telUpdateToPureMessage,
  )
import qualified API.Telegram.TStructs.Updates as TStructs
import qualified Config.Config as Config
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

telByteStringToPureMessageList ::
  Config.Config ->
  Logger.Logger ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
telByteStringToPureMessageList config logger eiBS =
  decodeByteString logger eiBS >>= telUpdatesToPureMessageList config

decodeByteString ::
  Logger.Logger ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString logger json = do
  Logger.botLog logger LoggerMsgs.getTelUpdScs
  let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr json) pure mbTelegramUpdates

getUpdErr :: BSL.ByteString -> IO TStructs.TelegramUpdates
getUpdErr json = do
  let eiTelegramErr = eitherDecode json :: Either String TStructs.TelegramUpdatesError
  either BotEx.throwParseExcept telError eiTelegramErr

telError :: TStructs.TelegramUpdatesError -> IO TStructs.TelegramUpdates
telError err =
  BotEx.throwUpdateExcept
    ( Logger.makeLogMessage
        LoggerMsgs.getUpdFld
        ( "\n\terror code: " <> (T.pack . show . TStructs.error_code) err
            <> "\n\terror describtion: "
            <> TStructs.description err
        )
    )

telUpdatesToPureMessageList ::
  Config.Config ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList config tUpd = pure $ (telUpdateToPureMessage config) <$> (TStructs.result tUpd)
