module TextMessages.RepeatCommandMessages where

import qualified Data.Text as T
import qualified Environment.Exports as Env

repeatText :: T.Text
repeatText = "Please, choose how many times you want to get your messages back: "

newRepeatText :: Env.RepeatNumber -> T.Text
newRepeatText repeatNumber = "You chose: " <> (T.pack . show) repeatNumber
