module API.Telegram.Cleaners (telByteStringToPureMessageList) where

import API.Telegram.Cleaners.MakePureMessage
  ( telUpdateToPureMessage,
  )
import qualified API.Telegram.TStructs.Updates as TStructs
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Environment as Env
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

telByteStringToPureMessageList ::
  Env.Env IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
telByteStringToPureMessageList env eiBS =
  decodeByteString env eiBS >>= telUpdatesToPureMessageList env

decodeByteString ::
  Env.Env IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString env json = do
  Env.eLog LoggerMsgs.tDecBS env
  let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr json) pure mbTelegramUpdates

telUpdatesToPureMessageList ::
  Env.Env IO ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList env tUpd = do
  config <- Env.eGetConfig env
  pure $ telUpdateToPureMessage config <$> TStructs.result tUpd

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
