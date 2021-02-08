module Wrapper.Functions.Decoders where

import qualified API.Telegram.Cleaners as TelCleaners
import qualified API.VK.Cleaners.ToPureMsgList as VKCleaners
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Config.Exports as Config
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.PureStructs as PureStructs
import Network.HTTP.Req
  ( LbsResponse,
    responseBody,
    responseStatusCode,
  )
import qualified TextMessages.LoggerMessages as LoggerMsgs

decodeByteString ::
  LbsResponse ->
  IO BSL.ByteString
decodeByteString response = case responseStatusCode response of
  200 -> pure (responseBody response :: BSL.ByteString)
  err -> BotEx.throwOtherException logMsg
    where
      logMsg = Logger.makeLogMessage LoggerMsgs.badServerResponse ((T.pack . show) err)

decodePureMessages ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
decodePureMessages env bytestring = do
  config <- runReaderT Env.eConfig env
  case config of
    (Config.VKBot _) ->
      VKCleaners.vkByteStringToPureMessageList env bytestring
    (Config.TBot _) ->
      TelCleaners.telByteStringToPureMessageList env bytestring
