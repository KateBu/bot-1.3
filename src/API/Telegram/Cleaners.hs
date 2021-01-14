module API.Telegram.Cleaners (telByteStringToPureMessageList, telByteStringToPureMessageList') where

import API.Telegram.Cleaners.MakePureMessage
  ( telUpdateToPureMessage,
  )
import qualified API.Telegram.TStructs.Updates as TStructs
import qualified Config.Config as Config
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Environment.Environment as Env 
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.Logger as Logger
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

telByteStringToPureMessageList' ::
  Env.Env IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
telByteStringToPureMessageList' env eiBS =
  decodeByteString' env eiBS >>= telUpdatesToPureMessageList' env 

decodeByteString' ::
  Env.Env IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString' env json = do
  runReaderT (Env.eLog LoggerMsgs.tDecBS) env
  let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr json) pure mbTelegramUpdates

telUpdatesToPureMessageList' ::
  Env.Env IO ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList' env tUpd = do 
  config <- runReaderT Env.eGetConfig env   
  pure $ (telUpdateToPureMessage config) <$> (TStructs.result tUpd)

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













telByteStringToPureMessageList ::
  Config.Config ->
  Logger.Logger IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
telByteStringToPureMessageList config logger eiBS =
  decodeByteString logger eiBS >>= telUpdatesToPureMessageList config

decodeByteString ::
  Logger.Logger IO ->
  BSL.ByteString ->
  IO TStructs.TelegramUpdates
decodeByteString logger json = do
  Logger.botLog logger LoggerMsgs.getTelUpdScs
  let mbTelegramUpdates = decode json :: Maybe TStructs.TelegramUpdates
  maybe (getUpdErr json) pure mbTelegramUpdates



telUpdatesToPureMessageList ::
  Config.Config ->
  TStructs.TelegramUpdates ->
  IO [PureStructs.PureMessage]
telUpdatesToPureMessageList config tUpd = pure $ (telUpdateToPureMessage config) <$> (TStructs.result tUpd)
