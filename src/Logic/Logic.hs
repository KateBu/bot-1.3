module Logic.Logic where

import qualified Config.Config as Config
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

type SendFunction a m =
  (Config.Config -> a -> PureStructs.PureMessage -> m (Either BotEx.BotException Config.Config))

processMsgs ::
  (Monad m) =>
  Config.Config ->
  a ->
  SendFunction a m ->
  (Either BotEx.BotException [PureStructs.PureMessage]) ->
  m (Either BotEx.BotException Config.Config)
processMsgs _ _ _ (Left err) = pure $ Left err
processMsgs config logger sendFunction (Right msgs) = do
  eiConfs <- mapM (processMsgs_ config logger sendFunction) msgs
  getLastConf config eiConfs

processMsgs_ ::
  Monad m =>
  Config.Config ->
  a ->
  SendFunction a m ->
  PureStructs.PureMessage ->
  m (Either BotEx.BotException Config.Config)
processMsgs_ config logger sendFunction msg = case PureStructs.messageType msg of
  PureStructs.MTEmpty -> pure $ Right (Config.configSetOffset config ((succ . PureStructs.updateID) msg))
  PureStructs.MTUserCommand PureStructs.Help -> sendFunction config logger msg
  PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction config logger (makeRepeatMsg msg)
  PureStructs.MTCallbackQuery callbackData -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCallback config logger sendFunction msg callbackData) mbChid
  PureStructs.MTCommon _ -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCommon config logger sendFunction msg) mbChid

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [(PureStructs.ParamsText "text" PureStructs.repeatText)]])

processMsgsErr :: Monad m => m (Either BotEx.BotException Config.Config)
processMsgsErr = pure $ BotEx.throwOtherException LoggerMsgs.chidNotFound

processMsgsCallback ::
  Monad m =>
  Config.Config ->
  a ->
  SendFunction a m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m (Either BotEx.BotException Config.Config)
processMsgsCallback config logger function msg callbackData chid = do
  let newRep = PureStructs.getNewRep callbackData
  let newConfig = Config.setUserRepeat config chid newRep
  function newConfig logger (makeCallbackResponse msg)

processMsgsCommon ::
  Monad m =>
  Config.Config ->
  a ->
  SendFunction a m ->
  PureStructs.PureMessage ->
  PureStructs.ChatID ->
  m (Either BotEx.BotException Config.Config)
processMsgsCommon config logger function msg chid = do
  let newRepeat = Config.findUserRepeat config chid
  repeatMsg logger msg newRepeat function config

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"}

repeatMsg ::
  Monad m =>
  a ->
  PureStructs.PureMessage ->
  Int ->
  SendFunction a m ->
  Config.Config ->
  m (Either BotEx.BotException Config.Config)
repeatMsg _ _ 0 _ config = pure $ Right config
repeatMsg logger msg n function config = do
  eiConfig <- function config logger msg
  either (pure . Left) (repeatMsg logger msg (n -1) function) eiConfig

getLastConf ::
  Monad m =>
  Config.Config ->
  [Either BotEx.BotException Config.Config] ->
  m (Either BotEx.BotException Config.Config)
getLastConf config [] = pure $ Right config
getLastConf _ eiConfs = either (pure . Left) (pure . Right) (last eiConfs)
