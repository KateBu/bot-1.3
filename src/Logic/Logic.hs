module Logic.Logic where

import qualified Config.Config as Config
import qualified Data.Text as T
import qualified Exceptions.Exceptions as BotEx
import qualified Logger.LoggerMsgs as LoggerMsgs
import qualified Logic.PureStructs as PureStructs

type SendFunction a m =
  (Config.Config -> a -> PureStructs.PureMessage -> m Config.Config)

processMsgs ::
  (Monad m) =>
  Config.Config ->
  a ->
  SendFunction a m ->
  [PureStructs.PureMessage] ->
  m Config.Config
processMsgs config logger sendFunction msgs = do
  eiConfs <- mapM (processMsgs_ config logger sendFunction) msgs
  getLastConf config eiConfs

processMsgs_ ::
  Monad m =>
  Config.Config ->
  a ->
  SendFunction a m ->
  PureStructs.PureMessage ->
  m Config.Config
processMsgs_ config logger sendFunction msg = case PureStructs.messageType msg of
  PureStructs.MTEmpty -> pure $ Config.configSetOffset config ((succ . PureStructs.updateID) msg)
  PureStructs.MTUserCommand PureStructs.Help -> sendFunction config logger msg
  PureStructs.MTUserCommand PureStructs.Repeat -> sendFunction config logger (makeRepeatMsg msg)
  PureStructs.MTCallbackQuery callbackData -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCallback config logger sendFunction msg callbackData) mbChid
  PureStructs.MTCommon _ -> do
    let mbChid = PureStructs.mbChatID msg
    maybe processMsgsErr (processMsgsCommon config logger sendFunction msg) mbChid

processMsgsErr :: Monad m => m Config.Config
processMsgsErr = BotEx.throwOtherException LoggerMsgs.chidNotFound

processMsgsCallback ::
  Monad m =>
  Config.Config ->
  a ->
  SendFunction a m ->
  PureStructs.PureMessage ->
  T.Text ->
  PureStructs.ChatID ->
  m Config.Config
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
  m Config.Config
processMsgsCommon config logger function msg chid = do
  let newRepeat = Config.findUserRepeat config chid
  repeatMsg logger msg newRepeat function config

repeatMsg ::
  Monad m =>
  a ->
  PureStructs.PureMessage ->
  Int ->
  SendFunction a m ->
  Config.Config ->
  m Config.Config
repeatMsg _ _ 0 _ config = pure config
repeatMsg logger msg n function config = do
  function config logger msg
    >>= repeatMsg logger msg (n -1) function

getLastConf ::
  Monad m =>
  Config.Config ->
  [Config.Config] ->
  m Config.Config
getLastConf config [] = pure config
getLastConf _ confs = pure . last $ confs

makeRepeatMsg :: PureStructs.PureMessage -> PureStructs.PureMessage
makeRepeatMsg msg =
  PureStructs.PureMessage
    (PureStructs.MTCommon "Message")
    (PureStructs.updateID msg)
    (PureStructs.mbChatID msg)
    (mconcat [PureStructs.mbParams msg, Just [(PureStructs.ParamsText "text" PureStructs.repeatText)]])

makeCallbackResponse :: PureStructs.PureMessage -> PureStructs.PureMessage
makeCallbackResponse msg = msg {PureStructs.messageType = PureStructs.MTCommon "Message"}
