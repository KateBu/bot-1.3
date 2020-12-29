module API.Telegram.Cleaners.MbTxt where

import qualified Data.Text as T 
import qualified API.Telegram.Structs as TStructs 
import qualified Logic.PureStructs as PureStructs 
import qualified Config.Config as Config 
import API.Telegram.Cleaners.GetParams ( basicParams )
import API.Telegram.Cleaners.Keyboard
    ( makeKeyboard, TButtons(TButtons) ) 

mbTextMessage :: Config.Config -> PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> Maybe PureStructs.PureMessage  
mbTextMessage config uid chid mInfo = 
    let mbTxt = TStructs.txt mInfo 
    in maybe Nothing (mkTxtMsg config uid chid mInfo) mbTxt
    
mkTxtMsg :: Config.Config -> PureStructs.UpdateID 
    -> PureStructs.ChatID 
    -> TStructs.MessageInfo 
    -> T.Text -> Maybe PureStructs.PureMessage      
mkTxtMsg config uid chid mInfo "/help" = pure $ PureStructs.PureMessage
    (PureStructs.MTUserCommand PureStructs.Help)
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "text" (Config.helpMessage config)])        
mkTxtMsg _ uid chid mInfo "/repeat" = pure $ PureStructs.PureMessage 
    (PureStructs.MTUserCommand PureStructs.Repeat)
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsJSON "reply_markup" (makeKeyboard ((map (map TButtons)) PureStructs.buttons'))
        , PureStructs.ParamsBool "one_time_keyboard" True])   
mkTxtMsg _ uid chid mInfo text = pure $ PureStructs.PureMessage 
    (PureStructs.MTCommon "Message")
    uid 
    (Just chid)
    (Just $ basicParams chid mInfo 
        <> [PureStructs.ParamsText "text" text])   