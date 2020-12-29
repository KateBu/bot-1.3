module API.Telegram.Cleaners.Keyboard where

import Data.Aeson ( object, Value, KeyValue((.=)), ToJSON(toJSON) )
import qualified Logic.PureStructs as PureStructs 

newtype TButtons = TButtons PureStructs.PureButtons 

instance ToJSON TButtons where 
    toJSON (TButtons (PureStructs.PureButtons lbl txt)) = object ["text" .= lbl, "callback_data" .= txt] 

makeKeyboard :: [[TButtons]] -> Value
makeKeyboard btns = object ["inline_keyboard" .= btns]