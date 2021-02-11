module Wrapper.Functions.Decoders where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.Telegram.Main as Telegram
import qualified API.VK.Main as VK
import qualified Config.Exports as Config
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logger.Exports as Logger
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
      VK.decodePureMessageList env bytestring
    (Config.TBot _) ->
      Telegram.decodePureMessageList env bytestring
