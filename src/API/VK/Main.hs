module API.VK.Main (decodePureMessageList) where

import qualified API.PureStructs.Exports as PureStructs
import API.VK.Functions.Builders (buildPureMessage)
import qualified API.VK.Structs.Exports as VK
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Exceptions.Exports as BotEx
import qualified Logger.Exports as Logger
import Text.Read (readMaybe)
import qualified TextMessages.LoggerMessages as LoggerMsgs

decodePureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
decodePureMessageList env byteString = do
  updates <- decodeUpdates env byteString
  buildPureMessageList env updates

decodeUpdates ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO (PureStructs.UpdateID, [VK.UpdateInfo])
decodeUpdates env byteString = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.vkByteStringDecodingInProgress
  let eiUpdates = eitherDecode byteString :: Either String VK.Updates
  either decodeUpdateError (decodeSuccess logger) eiUpdates

decodeSuccess ::
  Logger.Logger IO ->
  VK.Updates ->
  IO (PureStructs.UpdateID, [VK.UpdateInfo])
decodeSuccess _ (VK.UpdateError (VK.UpdateErr errCode _)) = case errCode of
  1 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode1
  2 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode2
  3 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode3
  _ -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode4
decodeSuccess logger (VK.UpdateSuccess updates) = do
  Logger.botLog logger logMsg
  let mbUpdateId = readMaybe $ VK.ts updates :: Maybe Int
  maybe
    (BotEx.throwOtherException LoggerMsgs.readUpdateIdFailed)
    (\updateId -> pure (updateId, VK.updates updates))
    mbUpdateId
  where
    logMsg = LoggerMsgs.vkDecodeByteStringSuccess

buildPureMessageList ::
  Env.Environment IO ->
  (PureStructs.UpdateID, [VK.UpdateInfo]) ->
  IO [PureStructs.PureMessage]
buildPureMessageList env (updateId, updates) = do
  logger <- runReaderT Env.eLogger env
  helpMsg <- runReaderT Env.eHelpMsg env
  Logger.botLog logger LoggerMsgs.parseVKMsgSuccess
  maybe (BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailed) pure (mbPureMsgs helpMsg)
  where
    mbPureMsgs helpMsg = sequence $ buildPureMessage helpMsg updateId <$> updates

decodeUpdateError ::
  String ->
  IO (PureStructs.UpdateID, [VK.UpdateInfo])
decodeUpdateError err =
  BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkUpdatesDecodingFailed $ T.pack err)
