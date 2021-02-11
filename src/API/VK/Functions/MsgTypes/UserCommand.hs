module API.VK.Functions.MsgTypes.UserCommand (buildCommandMessage) where

import qualified API.VK.Structs.Exports as VK
import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

buildCommandMessage :: VK.Message -> Maybe PureStructs.MessageType
buildCommandMessage msg = VK.msg_text msg >>= buildCommandMessage'

buildCommandMessage' :: T.Text -> Maybe PureStructs.MessageType
buildCommandMessage' "/help" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Help
buildCommandMessage' "/repeat" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Repeat
buildCommandMessage' _ = Nothing
