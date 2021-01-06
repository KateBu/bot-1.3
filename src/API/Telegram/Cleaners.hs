module API.Telegram.Cleaners where

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
  Either BotEx.BotException BSL.ByteString ->
  IO (Either BotEx.BotException [PureStructs.PureMessage])
telByteStringToPureMessageList config logger eiBS =
  decodeByteString logger eiBS >>= telUpdatesToPureMessageList config

decodeByteString ::
  Logger.Logger ->
  Either BotEx.BotException BSL.ByteString ->
  IO (Either BotEx.BotException TStructs.TelegramUpdates)
decodeByteString _ (Left err) = pure $ Left err
decodeByteString logger (Right json) = do
  Logger.botLog logger LoggerMsgs.getTelUpdScs
  let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr json) (pure . Right) mbTelegramUpdates

getUpdErr :: BSL.ByteString -> IO (Either BotEx.BotException TStructs.TelegramUpdates)
getUpdErr json = do
  let eiTelegramErr = eitherDecode json :: Either String TStructs.TelegramUpdatesError
  either BotEx.throwParseExcept telError eiTelegramErr

telError :: TStructs.TelegramUpdatesError -> IO (Either BotEx.BotException TStructs.TelegramUpdates)
telError err =
  pure $
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
  Either BotEx.BotException TStructs.TelegramUpdates ->
  IO (Either BotEx.BotException [PureStructs.PureMessage])
telUpdatesToPureMessageList _ (Left err) = pure $ Left err
telUpdatesToPureMessageList config (Right tUpd) = pure $ mapM (telUpdateToPureMessage config) (TStructs.result tUpd)
