module Logic.Structs.Buttons where

import qualified Data.Text as T
import qualified Environment.Exports as Env
import Logic.Structs.PureMessage (CallbackText)

data PureButtons = PureButtons T.Text T.Text
  deriving (Show)

buttons :: [[PureButtons]]
buttons =
  [ [PureButtons "1" setRepeat1],
    [PureButtons "2" setRepeat2],
    [PureButtons "3" setRepeat3],
    [PureButtons "4" setRepeat4],
    [PureButtons "5" setRepeat5]
  ]

setRepeat1 :: CallbackText
setRepeat1 = "/setRepetition1"

setRepeat2 :: CallbackText
setRepeat2 = "/setRepetition2"

setRepeat3 :: CallbackText
setRepeat3 = "/setRepetition3"

setRepeat4 :: CallbackText
setRepeat4 = "/setRepetition4"

setRepeat5 :: CallbackText
setRepeat5 = "/setRepetition5"

getNewRepeatNumber :: CallbackText -> Env.RepeatNumber
getNewRepeatNumber txt
  | txt == setRepeat1 = 1
  | txt == setRepeat2 = 2
  | txt == setRepeat3 = 3
  | txt == setRepeat4 = 4
  | otherwise = 5
