module Wrapper.Main (getUpdates, sendMessage) where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Logic.Structs as PureStructs
import qualified TextMessages.LoggerMessages as LoggerMsgs
import qualified Wrapper.Functions.Actions as WrapFunctions
import qualified Wrapper.Functions.Decoders as WrapFunctions
import qualified Wrapper.Functions.Requests as WrapFunctions
import qualified Wrapper.Functions.URL as WrapFunctions

getUpdates :: Env.Environment IO -> IO [PureStructs.PureMessage]
getUpdates env = do
  updates <- WrapFunctions.getUpdatesRequest env
  decodedBytestring <- WrapFunctions.decodeByteString updates
  WrapFunctions.decodePureMessages env decodedBytestring

sendMessage ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  IO (Env.Environment IO)
sendMessage env msg = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.sendMsgInProgress
  let mbParams = PureStructs.mbParams msg
  maybe (pure env) (sendMessage' env msg) mbParams

sendMessage' ::
  Env.Environment IO ->
  PureStructs.PureMessage ->
  [PureStructs.Params] ->
  IO (Env.Environment IO)
sendMessage' env msg params = do
  config <- runReaderT Env.eConfig env
  basicParams <- WrapFunctions.mbSendOption config
  apiResponse <- WrapFunctions.sendMessageRequest env basicParams params msg
  WrapFunctions.updateEnvironment env msg apiResponse
