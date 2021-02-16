module API.VK.Functions.MessageTypes.Callback (buildCallbackMessage) where

import qualified API.PureStructs.Exports as PureStructs
import qualified API.VK.Structs.Exports as VK
import qualified Data.Text as T

buildCallbackMessage :: VK.Message -> Maybe PureStructs.MessageType
buildCallbackMessage msg = do
  callback <- VK.callback_payload msg
  callbackText <- mbCallbackText callback
  pure $ PureStructs.MsgTypeCallbackQuery callbackText

mbCallbackText :: T.Text -> Maybe PureStructs.CallbackText
mbCallbackText callback = getCallbackText callback PureStructs.callbacks

getCallbackText :: PureStructs.CallbackText -> [PureStructs.CallbackText] -> Maybe PureStructs.CallbackText
getCallbackText _ [] = Nothing
getCallbackText callback (x : xs) =
  if T.isInfixOf x callback
    then Just x
    else getCallbackText callback xs
