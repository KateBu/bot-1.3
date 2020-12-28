module API.Telegram.TStructs.Buttons where

import Data.Aeson ( object, KeyValue((.=)), ToJSON(toJSON) ) 
import qualified Data.Text as T 

data Button = Button T.Text T.Text
    deriving Show 

instance ToJSON Button where 
    toJSON (Button btn cbd) = object ["text" .= btn, "callback_data" .= cbd] 

data InlineKeyBoard = InlineKeyBoard [[Button]]
    deriving Show 

instance ToJSON InlineKeyBoard where 
    toJSON (InlineKeyBoard btns) = object ["inline_keyboard" .= btns]