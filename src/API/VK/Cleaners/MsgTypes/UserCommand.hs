module API.VK.Cleaners.MsgTypes.UserCommand where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

mbUserCommand :: VKStructs.VKMessage -> Maybe PureStructs.MessageType
mbUserCommand vkMsg = VKStructs.msg_text vkMsg >>= mbUserCommand'

mbUserCommand' :: T.Text -> Maybe PureStructs.MessageType
mbUserCommand' "/help" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Help
mbUserCommand' "/repeat" = pure $ PureStructs.MsgTypeUserCommand PureStructs.Repeat
mbUserCommand' _ = Nothing
