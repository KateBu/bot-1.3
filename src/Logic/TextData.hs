module Logic.TextData where

import qualified Data.Text as T

repeatText :: T.Text
repeatText = "Please, choose how many times you want to get your messages back: "

newRepeatText :: Int -> T.Text
newRepeatText rep = "You chose: " <> (T.pack . show) rep
