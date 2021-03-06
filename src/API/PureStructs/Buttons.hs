module API.PureStructs.Buttons where

import API.PureStructs.PureMessage (CallbackText)
import qualified Data.Text as T
import qualified Environment.Exports as Env

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
setRepeat1 = "setRepetition1"

setRepeat2 :: CallbackText
setRepeat2 = "setRepetition2"

setRepeat3 :: CallbackText
setRepeat3 = "setRepetition3"

setRepeat4 :: CallbackText
setRepeat4 = "setRepetition4"

setRepeat5 :: CallbackText
setRepeat5 = "setRepetition5"

getNewRepeatNumber :: CallbackText -> Env.RepeatNumber
getNewRepeatNumber txt
  | txt == setRepeat2 = 2
  | txt == setRepeat3 = 3
  | txt == setRepeat4 = 4
  | txt == setRepeat5 = 5
  | otherwise = 1

callbacks :: [CallbackText]
callbacks =
  [ setRepeat1,
    setRepeat2,
    setRepeat3,
    setRepeat4,
    setRepeat5
  ]
