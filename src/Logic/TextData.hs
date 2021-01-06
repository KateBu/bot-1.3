module Logic.TextData where

import qualified Data.Text as T

repeatText :: T.Text
repeatText = "Выберите количество повторов: "

newRepeatText :: Int -> T.Text
newRepeatText rep = "Установлено количество повторов: " <> (T.pack . show) rep
