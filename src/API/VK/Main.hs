module API.VK.Main (decodePureMessageList) where

import API.VK.Functions.Builders (buildPureMessage)
import qualified API.VK.Structs.Exports as VKStructs
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Environment.Exports as Env
import qualified Environment.Logger.Exports as Logger
import qualified Exceptions.Exports as BotEx
import qualified Logic.Structs as PureStructs
import Text.Read (readMaybe)
import qualified TextMessages.LoggerMessages as LoggerMsgs

decodePureMessageList ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO [PureStructs.PureMessage]
decodePureMessageList env vkByteString = do
  vkUpdates <- decodeVKUpdates env vkByteString
  buildPureMessageList env vkUpdates

decodeVKUpdates ::
  Env.Environment IO ->
  BSL.ByteString ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeVKUpdates env vkByteString = do
  logger <- runReaderT Env.eLogger env
  Logger.botLog logger LoggerMsgs.vkByteStringDecodingInProgress
  let eiVKUpdates = eitherDecode vkByteString :: Either String VKStructs.VKUpdates
  either decodeUpdateError (decodeSuccess logger) eiVKUpdates

decodeSuccess ::
  Logger.Logger IO ->
  VKStructs.VKUpdates ->
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeSuccess _ (VKStructs.VKUpdateError (VKStructs.UpdateErr errCode _)) = case errCode of
  1 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode1
  2 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode2
  3 -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode3
  _ -> BotEx.throwUpdateExcept LoggerMsgs.vkUpdatesFailedCode4
decodeSuccess logger (VKStructs.VKUpdates updates) = do
  Logger.botLog logger logMsg
  let mbUpdateId = readMaybe $ VKStructs.ts updates :: Maybe Int
  maybe
    (BotEx.throwOtherException LoggerMsgs.readUpdateIdFailed)
    (\updateId -> pure (updateId, VKStructs.updates updates))
    mbUpdateId
  where
    logMsg = LoggerMsgs.vkDecodeByteStringSuccess

buildPureMessageList ::
  Env.Environment IO ->
  (PureStructs.UpdateID, [VKStructs.VKUpdInfo]) ->
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
  IO (PureStructs.UpdateID, [VKStructs.VKUpdInfo])
decodeUpdateError err =
  BotEx.throwUpdateExcept (Logger.makeLogMessage LoggerMsgs.vkUpdatesDecodingFailed $ T.pack err)
